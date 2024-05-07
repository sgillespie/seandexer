module Main where

import Data.Cardano.Seandexer

import Options.Applicative

data Opts = Opts
  { socketPath :: FilePath,
    networkId :: NetworkId
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
          soNetworkId = networkId
        }

parseOpts :: Parser Opts
parseOpts =
  Opts <$> parseSocketPath <*> parseNetworkId

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
