module Main where

import Data.Maybe
import IO (readHSON, readJSON, run)
import Interpreter
import JSONParser (decode)
import Opts (
  HSONInput (CmdLineIn, HSONFileInput),
  Options (Options, hsonInput, jsonInput),
  testOpts,
 )

main = do
  opts <- testOpts
  hsonIn <- readHSON $ hsonInput opts
  jsonIn <- readJSON $ jsonInput opts
  run jsonIn hsonIn
