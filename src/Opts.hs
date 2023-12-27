module Opts where

import Options.Applicative

data Options = Options
  { hsonInput :: HSONInput
  , jsonInput :: JSONInput
  }

data HSONInput
  = HSONFileInput FilePath
  | CmdLineIn String

data JSONInput
  = JSONFileInput FilePath
  | StdIn
  | NoJSONInput

opts :: Parser Options
opts = Options <$> (hsonFileInput <|> cmdLineIn) <*> (jsonFileInput <|> stdin)

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

testOpts =
  execParser $
    info
      (opts <**> helper)
      ( fullDesc
          <> progDesc "Parse JSON according to given HSON input"
          <> header "hson - json processing language"
      )
