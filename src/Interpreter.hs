{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Interpreter where

import BuiltIn.Array
import BuiltIn.Function (builtInFunctions)
import BuiltIn.Helpers
import BuiltIn.String
import Control.Exception (SomeException, evaluate, throw, try)
import Control.Monad.Except (
  MonadError (throwError),
  catchError,
  runExceptT,
 )
import Control.Monad.IO.Class
import Control.Monad.Reader (
  MonadReader,
  ReaderT (runReaderT),
  ask,
  local,
 )
import Control.Monad.Reader.Class
import qualified Data.Map as Map
import Data.Maybe
import Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import HSONValue
import Parser

runInterpretWithJSON :: HSONValue -> Program -> IO (Either HSONError HSONValue)
runInterpretWithJSON json = runInterpret (mkEnv json)

runInterpretNoJSON :: Program -> IO (Either HSONError HSONValue)
runInterpretNoJSON = runInterpret (mkEnv Null)

mkEnv :: HSONValue -> Environment
mkEnv json = Map.singleton "$" json `Map.union` builtInFunctions

runInterpret :: Environment -> Program -> IO (Either HSONError HSONValue)
runInterpret env prog = runExceptT $ runReaderT (unEval $ interpret prog) env

interpret :: Program -> Eval HSONValue
interpret ([], expr) = eval expr
interpret (stmt : stmts, expr) = do
  case stmt of
    VarDeclStmt (VarDecl (Token _ (Just (String name)) _) initializer) -> do
      val <- eval initializer
      local (Map.insert name val) $ interpret (stmts, expr)
    ObjectDestructureDeclStmt (ObjectDestructureDecl kvs initializer) -> do
      val <- eval initializer
      case val of
        Object o -> local (Map.union $ bindDestObj kvs o) $ interpret (stmts, expr)
        v -> throwError $ UnexpectedType "object" (showType v)
    ArrayDestructureDeclStmt (ArrayDestructureDecl elems initializer) -> do
      val <- eval initializer
      case val of
        Array arr -> local (Map.union $ bindDestArr elems arr) $ interpret (stmts, expr)
        v -> throwError $ UnexpectedType "array" (showType v)

-- binds the keys in a destructured object to the keys in the provided object
-- inserts those bindings in the given environment and returns the environment
bindDestObj ::
  [(Token, Maybe Token)] -> Map.Map T.Text HSONValue -> Environment
bindDestObj
  [ ( tok@(Token TokenIdentifier (Just (String k)) _)
      , ident
      )
    ]
  o = case ident of
    Just (Token TokenIdentifier (Just (String name)) _) -> Map.singleton k (fromMaybe Null (Map.lookup k o))
    Nothing -> Map.singleton k (fromMaybe Null (Map.lookup k o))
bindDestObj
  ( ( tok@(Token TokenIdentifier (Just (String k)) _)
      , ident
      )
      : kvs
    )
  o = case ident of
    Just (Token TokenIdentifier (Just (String name)) _) -> Map.insert name (fromMaybe Null (Map.lookup k o)) $ bindDestObj kvs o
    Nothing -> Map.insert k (fromMaybe Null (Map.lookup k o)) $ bindDestObj kvs o
bindDestObj
  [ ( tok@(Token TokenString (Just (String k)) _)
      , Just (Token TokenIdentifier (Just (String ident)) _)
      )
    ]
  o = Map.singleton ident (fromMaybe Null (Map.lookup k o))
bindDestObj
  ( ( tok@(Token TokenString (Just (String k)) _)
      , Just (Token TokenIdentifier (Just (String ident)) _)
      )
      : kvs
    )
  o = Map.insert ident (fromMaybe Null (Map.lookup k o)) $ bindDestObj kvs o

bindDestArr :: [Token] -> V.Vector HSONValue -> Environment
bindDestArr elems arr = zipBind (map toIdent elems) (V.toList arr)

zipBind :: [T.Text] -> [HSONValue] -> Environment
zipBind [] values = Map.empty
zipBind idents [] = Map.fromList $ map (,Null) idents
zipBind (ident : idents) (value : values) = Map.insert ident value $ zipBind idents values

eval :: Expr -> Eval HSONValue
eval (ArrowFunctionExpr (ArrowFunction params body)) = toClosure params body
eval (ArrayInitializerExpr (ArrayInitializer bracketTok elems)) = Array . V.fromList <$> mapM eval elems
eval (BinaryExpr (Binary l opTok r)) = do
  left <- eval l
  right <- eval r
  case opTok of
    Token TokenEqualEqual _ _ -> return $ Bool $ left == right
    Token TokenBangEqual _ _ -> return $ Bool $ left == right
    Token TokenGreater _ _ -> numCmp opTok (>) left right
    Token TokenGreaterEqual _ _ -> numCmp opTok (>=) left right
    Token TokenLess _ _ -> numCmp opTok (<) left right
    Token TokenLessEqual _ _ -> numCmp opTok (<=) left right
    Token TokenMinus _ _ -> numOp opTok (-) left right
    Token TokenStar _ _ -> numOp opTok (*) left right
    Token TokenSlash _ _ -> numOp opTok (/) left right
    Token TokenPlus _ _ -> valuePlus opTok left right
    _otherToken -> throwError $ UnhandledOperator opTok
eval (CallExpr (Call callee tok args)) = do
  res <- eval callee
  case res of
    Function f -> do
      args <- mapM eval args
      fn f args `catchError` (throwError . CallError tok)
    Closure f env -> do
      args <- mapM eval args
      local (const env) $ fn f args `catchError` (throwError . CallError tok)
    _uncallableValue -> throwError $ UncallableExpression tok
eval (ConditionalExpr (Conditional cond matched unmatched)) = do
  condition <- eval cond
  if isTruthy condition then eval matched else eval unmatched
eval (DollarExpr (Dollar tok)) =
  fromMaybe <$> throwError (UndefinedVariable tok) <*> asks (Map.lookup "$")
eval (GetExpr (Get expr tok@(Token _ (Just idx) _))) = eval expr >>= access tok idx
eval (GroupingExpr (Grouping expr)) = eval expr
eval (IndexExpr (Index indexed tok index)) = do
  object <- eval indexed
  idx <- eval index
  access tok idx object
eval (LiteralExpr (Literal (Token _ (Just v) _))) = return v
eval (LiteralExpr (Literal (Token TokenTrue _ _))) = return $ Bool True
eval (LiteralExpr (Literal (Token TokenFalse _ _))) = return $ Bool False
eval (LiteralExpr (Literal (Token TokenNull _ _))) = return Null
eval (LogicalExpr (Logical l opTok r)) = do
  left <- eval l
  case opTok of
    Token TokenOrOr _ _ -> if isTruthy left then return left else eval r
    Token TokenAndAnd _ _ -> if isTruthy left then eval r else return left
    Token TokenQuestionQuestion _ _ -> if left == Null then eval r else return left
    _unrecognizedOperator -> throwError $ UnhandledOperator opTok
eval (ObjectInitializerExpr (ObjectInitializer braceTok entries)) = do
  evalEntries <- mapM evalEntry entries
  return $ Object $ Map.fromList evalEntries
eval (UnaryExpr (Unary opTok r)) = do
  right <- eval r
  case opTok of
    Token TokenMinus _ _ -> minusValue opTok right
    Token TokenBang _ _ -> return $ Bool $ not $ isTruthy right
    Token TokenBangBang _ _ -> return $ Bool $ isTruthy right
eval (VariableExpr (Variable tok@(Token TokenIdentifier (Just (String s)) _))) =
  fromMaybe <$> throwError (UndefinedVariable tok) <*> asks (Map.lookup s)

numCmp ::
  Token ->
  (Scientific -> Scientific -> Bool) ->
  HSONValue ->
  HSONValue ->
  Eval HSONValue
numCmp _ op (Number x) (Number y) = return $ Bool $ op x y
numCmp opTok _ x (Number y) = throwError $ TypeError opTok "left operand must be a number"
numCmp opTok _ (Number x) y = throwError $ TypeError opTok "right operand must be a number"
numCmp opTok _ _ _ = throwError $ TypeError opTok "operands must be numbers"

valuePlus :: Token -> HSONValue -> HSONValue -> Eval HSONValue
valuePlus _ (Number x) (Number y) = return $ Number $ x + y
valuePlus _ (String x) (String y) = return $ String $ x <> y
valuePlus _ (Number x) (String y) = return $ String $ T.pack (show x) <> y
valuePlus _ (String x) (Number y) = return $ String $ x <> T.pack (show y)
valuePlus opTok _ _ = throwError $ TypeError opTok "operands must be either strings or numbers"

numOp ::
  Token ->
  (Scientific -> Scientific -> Scientific) ->
  HSONValue ->
  HSONValue ->
  Eval HSONValue
numOp _ op (Number x) (Number y) = return $ Number $ op x y
numOp opTok _ x (Number y) = throwError $ TypeError opTok "left operand must be a number"
numOp opTok _ (Number x) y = throwError $ TypeError opTok "right operand must be a number"
numOp opTok _ _ _ = throwError $ TypeError opTok "operands must be numbers"

minusValue :: Token -> HSONValue -> Eval HSONValue
minusValue _ (Number v) = return $ Number $ -1 * v
minusValue opTok _ = throwError $ TypeError opTok "operand must be a number"

evalEntry :: (Token, Maybe Expr) -> Eval (T.Text, HSONValue)
evalEntry (tok@(Token _ (Just (String k)) _), exp) = case exp of
  Just e -> do
    v <- eval e
    return (k, v)
  Nothing -> do
    binding <- asks (Map.lookup k)
    case binding of
      Just v -> return (k, v)
      Nothing -> throwError $ UndefinedVariable tok

toClosure :: [Token] -> Expr -> Eval HSONValue
toClosure params expr =
  asks $ Closure $ Func (toClosure' params expr)

toClosure' :: [Token] -> Expr -> [HSONValue] -> Eval HSONValue
toClosure' params expr args
  | length args < length params = throwError $ ArgumentCount (length params) args
  | otherwise = do
      let params' = map toIdent params
          env = Map.fromList (zip params' args)
       in local (Map.union env) $ eval expr

toIdent :: Token -> T.Text
toIdent (Token _ (Just (String t)) _) = t

access :: Token -> HSONValue -> HSONValue -> Eval HSONValue
access tok (String name) (Object o) = accessObjectProp tok name o
access tok value (Object o) = throwError $ InvalidIndex tok value "object"
access tok value@(Number idx) (Array arr) = case floatingOrInteger idx of
  Right int -> accessArrayIdx tok int arr
  Left float -> throwError $ InvalidIndex tok value "array"
access tok (String name) (Array arr) = accessArrayMethod tok name arr
access tok (String name) (String str) = accessStringMethod tok name str
access tok value@(Number idx) (String str) = case floatingOrInteger idx of
  Right int -> accessStringIdx tok int str
  Left float -> throwError $ InvalidIndex tok value "string"
access tok value indexed = throwError $ InvalidIndex tok value (showType indexed)

accessObjectProp ::
  Token -> T.Text -> Map.Map T.Text HSONValue -> Eval HSONValue
accessObjectProp tok name kv = case Map.lookup name kv of
  Just (Method f) -> return $ Function $ f (Object kv)
  Just v -> return v
  Nothing -> return Null

accessArrayIdx :: Token -> Int -> V.Vector HSONValue -> Eval HSONValue
accessArrayIdx tok idx arr = case arr V.!? idx of
  Just v -> return v
  Nothing -> throwError $ IndexOutOfBounds tok idx

accessArrayMethod :: Token -> T.Text -> V.Vector HSONValue -> Eval HSONValue
accessArrayMethod tok name arr = case Map.lookup name arrayMethods of
  Just (Method f) -> return $ Function $ f (Array arr)
  Nothing -> throwError $ UndefinedProperty name tok

accessStringMethod :: Token -> T.Text -> T.Text -> Eval HSONValue
accessStringMethod tok name str = case Map.lookup name stringMethods of
  Just (Method f) -> return $ Function $ f (String str)
  Nothing -> throwError $ UndefinedProperty name tok

accessStringIdx :: Token -> Int -> T.Text -> Eval HSONValue
accessStringIdx tok idx str = do
  result <- liftIO $ try $ evaluate $ T.index str idx
  case result :: Either SomeException Char of
    Right result -> return $ String $ T.singleton result
    Left _ -> throwError $ IndexOutOfBounds tok idx
