module Main where

import Arbitrary
import Control.Monad (forM, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Distribution.TestSuite
import Parser
import PrettyPrinter
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Args (..),
  Gen,
  Property,
  Testable (property),
  chatty,
  maxDiscardRatio,
  maxShrinks,
  maxSize,
  maxSuccess,
  property,
  quickCheck,
  quickCheckResult,
  quickCheckWithResult,
  replay,
  resize,
  sample',
  stdArgs,
  verbose,
  verboseCheck,
  verboseCheckResult,
  within,
 )
import qualified Test.QuickCheck as QC
import Text.PrettyPrint

main = debugSamples

checkExpressionParser :: Expr -> Property
checkExpressionParser ast = within 50000 $ case runHSONExprParser (prettyPrintExpr ast) of
  Left _ -> False
  Right a -> ast == a

qcArgs =
  Args
    { replay = replay stdArgs
    , maxSuccess = 10000
    , maxSize = 6
    , maxShrinks = maxShrinks stdArgs
    , maxDiscardRatio = maxDiscardRatio stdArgs
    , chatty = True
    }

runChecks = quickCheckWithResult qcArgs checkExpressionParser

debugSamples = do
  expr <- sample' (resize 6 arbitrary :: Gen Expr)
  forM_
    expr
    ( \e -> do
        res <- verboseCheckResult $ checkExpressionParser e
        case res of
          QC.Failure{} -> do
            TIO.putStrLn $
              T.pack $
                show
                  e
            TIO.putStrLn $
              prettyPrintExpr e
          _ -> return ()
    )

ppSamples = do
  expr <- sample' (resize 2 arbitrary :: Gen Expr)
  forM_
    expr
    (TIO.putStrLn . prettyPrintExpr)
