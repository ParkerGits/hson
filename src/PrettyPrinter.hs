module PrettyPrinter where

import Data.Scientific
import qualified Data.Text as T
import HSONValue
import Parser
import Text.PrettyPrint (
  Doc,
  Mode (PageMode),
  Style (..),
  braces,
  brackets,
  char,
  colon,
  comma,
  doubleQuotes,
  float,
  hcat,
  int,
  lbrace,
  parens,
  punctuate,
  rbrace,
  render,
  sep,
  space,
  text,
  (<+>),
 )

hsonStyle :: Style
hsonStyle = Style{ribbonsPerLine = 1.5, mode = PageMode, lineLength = 80}

prettyPrintProg :: Program -> T.Text
prettyPrintProg prog = T.pack $ render $ ppProg prog

ppProg :: Program -> Doc
ppProg (stmts, expr) = sep [sep $ map ppVarStmt stmts, ppExpr expr]

ppVarStmt :: VarStmt -> Doc
ppVarStmt (VarDeclStmt (VarDecl name init)) = (text "let" <+> ppTok name <+> char '=' <+> ppExpr init) <> char ';'
ppVarStmt (ObjectDestructureDeclStmt (ObjectDestructureDecl kv obj)) =
  (text "let" <+> ppDestObject kv <+> char '=' <+> ppExpr obj)
    <> char ';'
ppVarStmt (ArrayDestructureDeclStmt (ArrayDestructureDecl elems arr)) =
  ( text "let"
      <+> brackets (commaSep $ map ppTok elems)
      <+> char '='
      <+> ppExpr arr
  )
    <> char ';'

ppExpr :: Expr -> Doc
ppExpr (ArrayInitializerExpr (ArrayInitializer _ elems)) = brackets $ commaSep $ map ppExpr elems
ppExpr (ArrowFunctionExpr (ArrowFunction params body)) = parens (commaSep $ map ppTok params) <+> text "=>" <+> ppExpr body
ppExpr (BinaryExpr (Binary l op r)) = ppExpr l <+> ppTok op <+> ppExpr r
ppExpr (CallExpr (Call callee _ args)) = ppExpr callee <> parens (commaSep $ map ppExpr args)
ppExpr (ConditionalExpr (Conditional cond matched unmatched)) = ppExpr cond <+> char '?' <+> ppExpr matched <+> char ':' <+> ppExpr unmatched
ppExpr (DollarExpr (Dollar tok)) = ppTok tok
ppExpr (GetExpr (Get obj prop)) = ppExpr obj <> ppTok prop
ppExpr (GroupingExpr (Grouping expr)) = ppExpr expr
ppExpr (IndexExpr (Index indexed _ index)) = ppExpr indexed <> brackets (ppExpr index)
ppExpr (LiteralExpr (Literal tok)) = ppTok tok
ppExpr (LogicalExpr (Logical l op r)) = ppExpr l <+> ppTok op <+> ppExpr r
ppExpr (ObjectInitializerExpr (ObjectInitializer _ entries)) = ppObjectLiteral entries
ppExpr (UnaryExpr (Unary op r)) = ppTok op <> ppExpr r
ppExpr (VariableExpr (Variable name)) = ppTok name

ppTok :: Token -> Doc
ppTok (Token TokenEqual _ _) = char '='
ppTok (Token TokenEqualEqual _ _) = text "=="
ppTok (Token TokenBang _ _) = char '='
ppTok (Token TokenBangEqual _ _) = text "!="
ppTok (Token TokenAndAnd _ _) = text "&&"
ppTok (Token TokenOrOr _ _) = text "||"
ppTok (Token TokenGreater _ _) = char '>'
ppTok (Token TokenGreaterEqual _ _) = text ">="
ppTok (Token TokenLess _ _) = char '<'
ppTok (Token TokenLessEqual _ _) = text "<="
ppTok (Token TokenMinus _ _) = char '-'
ppTok (Token TokenPlus _ _) = char '+'
ppTok (Token TokenSlash _ _) = char '/'
ppTok (Token TokenStar _ _) = char '*'
ppTok (Token TokenLeftBrace _ _) = char '{'
ppTok (Token TokenLeftBracket _ _) = char '['
ppTok (Token TokenLeftParen _ _) = char '('
ppTok (Token TokenString (Just (String s)) _) = doubleQuotes $ text $ T.unpack s
ppTok (Token TokenNumber (Just (Number n)) _) = case floatingOrInteger n of
  Left f -> float f
  Right i -> int i
ppTok (Token TokenIdentifier (Just (String name)) _) = text $ T.unpack name
ppTok (Token TokenLet _ _) = text "let"
ppTok (Token TokenSemicolon _ _) = char ';'
ppTok (Token TokenColon _ _) = char ':'
ppTok (Token TokenQuestion _ _) = char '?'
ppTok (Token TokenTrue _ _) = text "true"
ppTok (Token TokenFalse _ _) = text "false"
ppTok (Token TokenNull _ _) = text "null"
ppTok (Token TokenArrow _ _) = text "=>"
ppTok (Token TokenDollar _ _) = char '$'
ppTok (Token TokenBangBang _ _) = text "!!"
ppTok (Token TokenQuestionQuestion _ _) = text "??"
ppTok (Token TokenPipeForward _ _) = text "|>"

commaSep :: [Doc] -> Doc
commaSep docs = sep $ punctuate comma docs

ppDestObject :: [(Token, Maybe Token)] -> Doc
ppDestObject entries = sep [lbrace, commaSep $ map ppDestEntry entries, rbrace]
 where
  ppDestEntry (ktok, Just vtok) = (ppTok ktok <> colon) <+> ppTok vtok
  ppDestEntry (tok, Nothing) = ppTok tok

ppObjectLiteral :: [(Token, Maybe Expr)] -> Doc
ppObjectLiteral entries = sep [lbrace, commaSep $ map ppEntry entries, rbrace]
 where
  ppEntry (tok, Just expr) = (ppTok tok <> colon) <+> ppExpr expr
  ppEntry (tok, Nothing) = ppTok tok
