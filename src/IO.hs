module IO where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HSONValue (HSONError (JSONParsingError))
import Interpreter
import JSONParser
import Opts (
  HSONInput (CmdLineIn, HSONFileInput),
  JSONInput (JSONFileInput, NoJSONInput, StdIn),
  Options (doPrintAst),
 )
import Parser

run :: Maybe BL.ByteString -> T.Text -> Options -> IO ()
run json hson opts = case parseHSON hson of
  Left err -> print err
  Right prog -> do
    when (doPrintAst opts) (print prog)
    runProg json prog

runProg :: Maybe BL.ByteString -> Program -> IO ()
runProg Nothing prog = runInterpretNoJSON prog >>= printResult
runProg (Just json) prog = case decode json of
  Left err -> print $ JSONParsingError $ T.pack err
  Right json -> runInterpretWithJSON json prog >>= printResult

printResult :: (Show a) => Either HSONError a -> IO ()
printResult (Left err) = print err
printResult (Right res) = print res

readHSON :: HSONInput -> IO T.Text
readHSON (HSONFileInput path) = TIO.readFile path
readHSON (CmdLineIn str) = return $ T.pack str

readJSON :: JSONInput -> IO (Maybe BL.ByteString)
readJSON (JSONFileInput path) = Just <$> BL.readFile path
readJSON StdIn = Just <$> BL.getContents
readJSON NoJSONInput = return Nothing
