module Main where

import Data.Cardano.Seandexer

import Options.Applicative

data Opts = Opts
  { socketPath :: FilePath,
    networkId :: NetworkId,
    byronGenesis :: FilePath,
    shelleyGenesis :: FilePath,
    alonzoGenesis :: FilePath,
    conwayGenesis :: FilePath
  }
  deriving stock (Eq, Show)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseOpts <**> helper) fullDesc

run :: Opts -> IO ()
run = runSeandexer . toSeandexerOpts
  where
    toSeandexerOpts (Opts{..}) =
      SeandexerOpts
        { soSocketPath = socketPath,
          soNetworkId = networkId,
          soByronGenesis = byronGenesis,
          soShelleyGenesis = shelleyGenesis,
          soAlonzoGenesis = alonzoGenesis,
          soConwayGenesis = conwayGenesis
        }

parseOpts :: Parser Opts
parseOpts =
  Opts
    <$> parseSocketPath
    <*> parseNetworkId
    <*> parseByronGenesis
    <*> parseShelleyGenesis
    <*> parseAlonzoGenesis
    <*> parseConwayGenesis

parseSocketPath :: Parser FilePath
parseSocketPath =
  strOption $
    long "socket-path"
      <> short 's'
      <> metavar "PATH"
      <> help "Cardano Node socket path"

parseNetworkId :: Parser NetworkId
parseNetworkId = parseMainnet <|> (mkTestnet <$> parseTestnetMagic)
  where
    parseMainnet =
      flag' Mainnet $
        long "mainnet"
          <> short 'm'
          <> help "Use the mainnet network magic ID"

    parseTestnetMagic =
      option auto $
        long "testnet-magic"
          <> short 't'
          <> help "Use the specified testnet magic ID"

parseByronGenesis :: Parser FilePath
parseByronGenesis =
  strOption $
    long "byron-genesis"
      <> short 'b'
      <> metavar "PATH"
      <> help "Byron Genesis file path"

parseShelleyGenesis :: Parser FilePath
parseShelleyGenesis =
  strOption $
    long "shelley-genesis"
      <> short 'g'
      <> metavar "PATH"
      <> help "Shelley Genesis file path"

parseAlonzoGenesis :: Parser FilePath
parseAlonzoGenesis =
  strOption $
    long "alonzo-genesis"
      <> short 'a'
      <> metavar "PATH"
      <> help "Alonzo Genesis file path"

parseConwayGenesis :: Parser FilePath
parseConwayGenesis =
  strOption $
    long "conway-genesis"
      <> short 'c'
      <> metavar "PATH"
      <> help "Conway Genesis file path"
