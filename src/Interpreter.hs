{-# LANGUAGE OverloadedStrings #-}
module Interpreter where
import           Control.Exception          (throwIO)
import           Control.Monad.Except       (MonadError (throwError),
                                             catchError, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT),
                                             ask, local)
import           Control.Monad.Reader.Class
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Scientific            (Scientific, floatingOrInteger)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           HSONValue
import           Native                     (arrayMethods)
import           Parser

testEval exp = runExceptT $ runReaderT (unEval $ eval exp) Map.empty

testInterpret prog = runExceptT $ runReaderT (unEval $ interpret prog) Map.empty

interpret :: Program -> Eval HSONValue
interpret ([], expr) = eval expr
interpret ((VarStmt (Token _ (Just (String name)) _) initializer):stmts, expr) = do
    val <- eval initializer
    local (Map.insert name val) $ interpret (stmts, expr)

eval :: Expr -> Eval HSONValue
eval (ArrowFunctionExpr (ArrowFunction params body)) = asks $ Lambda (Func $ applyLambda body params)

eval (ArrayInitializerExpr (ArrayInitializer bracketTok elems)) = Array . V.fromList <$> mapM eval elems

eval (BinaryExpr (Binary l opTok r)) = do
  left <- eval l
  right <- eval r
  case opTok of
    Token TokenEqualEqual _ _   -> return $ Bool $ valueEq left right
    Token TokenBangEqual _ _    -> return $ Bool $ valueNeq left right
    Token TokenGreater _ _      -> numCmp opTok (>) left right
    Token TokenGreaterEqual _ _ -> numCmp opTok (>=) left right
    Token TokenLess _ _         -> numCmp opTok (<) left right
    Token TokenLessEqual _ _    -> numCmp opTok (<=) left right
    Token TokenMinus _ _        -> numOp opTok (-) left right
    Token TokenStar _ _         -> numOp opTok (*) left right
    Token TokenSlash _ _        -> numOp opTok (/) left right
    Token TokenPlus _ _         -> valuePlus opTok left right
    _                           -> throwError $ UnhandledOperator opTok

eval (CallExpr (Call callee tok args)) = do
  res <- eval callee
  case res of
    Function f   -> do
      args <- mapM eval args
      fn f args `catchError` (throwError . CallError tok)
    Lambda f env -> do
      args <- mapM eval args
      local (const env) $ fn f args
    _ -> throwError $ UncallableExpression tok

eval (ConditionalExpr (Conditional cond matched unmatched)) = do
  condition <- eval cond
  if isTruthy condition then eval matched else eval unmatched

eval (GetExpr (Get expr tok@(Token _ (Just idx) _))) = eval expr >>= access tok idx

eval (GroupingExpr (Grouping expr)) = eval expr

eval (IndexExpr (Index indexed tok index)) = do
  object <- eval indexed
  idx <- eval index
  access tok idx object
eval (LiteralExpr (Literal v)) = return v

eval (LogicalExpr (Logical l opTok r)) = do
  left <- eval l
  right <- eval r
  case opTok of
    Token TokenOrOr _ _   -> if isTruthy left then return left else return right
    Token TokenAndAnd _ _ -> if isTruthy left then return right else return left
    _                     -> throwError $ UnhandledOperator opTok

eval (ObjectInitializerExpr (ObjectInitializer braceTok entries)) = do
  evalEntries <- mapM evalEntry entries
  return $ Object $ Map.fromList evalEntries

eval (UnaryExpr (Unary opTok r)) = do
  right <- eval r
  case opTok of
    Token TokenMinus _ _ -> minusValue opTok right
    Token TokenBang _ _  -> return $ Bool $ not $ isTruthy right

eval (VariableExpr (Variable tok@(Token TokenIdentifier (Just (String s)) _))) = do
  env <- ask
  case Map.lookup s env of
    Just value -> return value
    Nothing    -> throwError $ UndefinedVariable tok

valueEq :: HSONValue -> HSONValue -> Bool
valueEq (String x) (String y) = x == y
valueEq (Number x) (Number y) = x == y
valueEq (Bool x) (Bool y)     = x == y
valueEq Null Null             = True
valueEq _ _                   = False

valueNeq :: HSONValue -> HSONValue -> Bool
valueNeq x y = not $ valueEq x y

numCmp :: Token -> (Scientific -> Scientific -> Bool) -> HSONValue -> HSONValue -> Eval HSONValue
numCmp _ op (Number x) (Number y) =  return $ Bool $ op x y
numCmp opTok _ x (Number y)         = throwError $ TypeError opTok "left operand must be a number"
numCmp opTok _ (Number x) y         = throwError $ TypeError opTok "right operand must be a number"
numCmp opTok _ _ _                  = throwError $ TypeError opTok  "operands must be numbers"

valuePlus :: Token -> HSONValue -> HSONValue -> Eval HSONValue
valuePlus _ (Number x) (Number y) = return $ Number $ x + y
valuePlus _ (String x) (String y) = return $ String $ x <> y
valuePlus _ (Number x) (String y) = return $ String $ T.pack (show x) <> y
valuePlus _ (String x) (Number y) = return $ String $ x <> T.pack (show y)
valuePlus opTok _ _ = throwError $ TypeError opTok "operands must be either string of number"

numOp :: Token -> (Scientific -> Scientific -> Scientific) -> HSONValue -> HSONValue -> Eval HSONValue
numOp _ op (Number x) (Number y) =  return $ Number $ op x y
numOp opTok _ x (Number y)         = throwError $ TypeError opTok "left operand must be a number"
numOp opTok _ (Number x) y         = throwError $ TypeError opTok "right operand must be a number"
numOp opTok _ _ _                  = throwError $ TypeError opTok  "operands must be numbers"

isTruthy :: HSONValue -> Bool
isTruthy (Number v) = v /= 0
isTruthy (String v) = not $ T.null v
isTruthy (Bool v)   = v
isTruthy Null       = False

minusValue :: Token -> HSONValue -> Eval HSONValue
minusValue _ (Number v) = return $ Number $ -1 * v
minusValue opTok _      = throwError $ TypeError opTok "operand must be a number"

evalEntry :: (Token, Expr) -> Eval (T.Text, HSONValue)
evalEntry (Token _ (Just (String k)) _, exp) = do
  v <- eval exp
  return (k, v)

applyLambda :: Expr -> [Token] -> [HSONValue] -> Eval HSONValue
applyLambda expr params args
  | length params /= length args = throwError $ ArgumentCount (length params) args
  | otherwise = do
    closureEnv <- ask
    let params' = map toIdent params
        env = Map.fromList (zip params' args) <> closureEnv in
          local (const env) $ eval expr

toIdent :: Token -> T.Text
toIdent (Token _ (Just (String t)) _) = t

access :: Token -> HSONValue -> HSONValue -> Eval HSONValue
access tok (String name) (Object o) = accessObjectProp tok name o
access tok value (Object o) = throwError $ InvalidIndex tok value "object"
access tok value@(Number idx) (Array arr) = case floatingOrInteger idx of
  Right int  -> accessArrayIdx tok int arr
  Left float -> throwError $ InvalidIndex tok value "array"
access tok (String name) (Array arr) = accessArrayMethod tok name arr
access tok _ _ = throwError $ UndefinedProperty tok

accessObjectProp :: Token -> T.Text -> Map.Map T.Text HSONValue -> Eval HSONValue
accessObjectProp tok name kv = case Map.lookup name kv of
      Just (Method f) -> return $ Function $ f (Object kv)
      Just v          -> return v
      Nothing         -> throwError $ UndefinedProperty tok

accessArrayIdx :: Token -> Int -> V.Vector HSONValue -> Eval HSONValue
accessArrayIdx tok idx arr = case arr V.!? idx of
      Just v  -> return v
      Nothing -> throwError $ IndexOutOfBounds tok idx

accessArrayMethod :: Token -> T.Text -> V.Vector HSONValue -> Eval HSONValue
accessArrayMethod tok name arr = case Map.lookup name arrayMethods of
      Just (Method f) -> return $ Function $ f (Array arr)
      Nothing         -> throwError $ UndefinedProperty tok
