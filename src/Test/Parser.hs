module Test.Parser where

import Control.Monad (forM, forM_)
import qualified Data.Text as T
import Parser
import PrettyPrinter
import Test.Arbitrary
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  quickCheck,
  quickCheckResult,
  sample',
  verboseCheck,
  verboseCheckResult,
 )
import Text.PrettyPrint

checkExpressionParser :: Expr -> Bool
checkExpressionParser ast = case runHSONExprParser (prettyPrintExpr ast) of
  Left _ -> False
  Right a -> ast == a

debugSamples = do
  expr <- sample' (arbitrary :: Gen Expr)
  forM_
    expr
    ( \e -> do
        print (prettyPrintExpr e)
        print e
        print (checkExpressionParser e)
    )

runChecks = quickCheckResult checkExpressionParser
