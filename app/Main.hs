module Main where

import Data.Maybe
import IO (readHSON, readJSON, run)
import Interpreter
import JSONParser (decode)
import Opts (
  HSONInput (CmdLineIn, HSONFileInput),
  Options (hsonInputOpt, jsonInputOpt),
  testOpts,
 )

main = do
  opts <- testOpts
  hsonIn <- readHSON $ hsonInputOpt opts
  jsonIn <- readJSON $ jsonInputOpt opts
  run jsonIn hsonIn opts
