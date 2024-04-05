module Main where

import Data.Cardano.Seandexer (SeandexerOpts (..), runSeandexer)

import Options.Applicative

newtype Opts = Opts
  {socketPath :: FilePath}
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
        { soSocketPath = socketPath
        }

parseOpts :: Parser Opts
parseOpts =
  Opts <$> parseSocketPath

parseSocketPath :: Parser FilePath
parseSocketPath =
  strOption $
    long "socket-path"
      <> short 's'
      <> metavar "PATH"
      <> help "Cardano Node socket path"
