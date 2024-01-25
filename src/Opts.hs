module Opts where

import Options.Applicative

data Options = Options
  { hsonInputOpt :: HSONInput
  , jsonInputOpt :: JSONInput
  , doPrintAst :: Bool
  , doPrettyPrint :: Bool
  , doOmitEval :: Bool
  }

data HSONInput
  = HSONFileInput FilePath
  | CmdLineIn String

data JSONInput
  = JSONFileInput FilePath
  | StdIn
  | NoJSONInput

opts :: Parser Options
opts =
  Options
    <$> (hsonFileInput <|> cmdLineIn)
    <*> (jsonFileInput <|> stdin)
    <*> printParseTree
    <*> prettyPrint
    <*> hideEval

hsonFileInput :: Parser HSONInput
hsonFileInput =
  HSONFileInput
    <$> strOption
      ( long "hf"
          <> long "hfile"
          <> long "hsonfile"
          <> metavar "filename.hson"
          <> help "HSON input file."
      )

cmdLineIn :: Parser HSONInput
cmdLineIn = CmdLineIn <$> argument str (metavar "input hson" <> help "HSON input")

jsonFileInput :: Parser JSONInput
jsonFileInput =
  JSONFileInput
    <$> strOption
      ( long "jf"
          <> long "jfile"
          <> long "jsonfile"
          <> metavar "filename.json"
          <> help "JSON input file."
      )

stdin :: Parser JSONInput
stdin =
  flag
    StdIn
    NoJSONInput
    (long "no-json" <> short 'n' <> help "Run HSON without a JSON input.")

printParseTree :: Parser Bool
printParseTree =
  flag
    False
    True
    (long "ast" <> short 'a' <> help "Print the HSON parse tree.")

prettyPrint :: Parser Bool
prettyPrint =
  flag
    False
    True
    ( long "pretty-print"
        <> short 'p'
        <> help "Pretty print the provided HSON script."
    )

hideEval :: Parser Bool
hideEval =
  flag
    False
    True
    (long "omit-eval" <> short 'o' <> help "Omit the evaluated expression output")

testOpts =
  execParser $
    info
      (opts <**> helper)
      ( fullDesc
          <> progDesc "Parse JSON according to given HSON input."
          <> header "hson - json processing language"
      )
