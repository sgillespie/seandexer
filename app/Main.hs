module Main where

import Options.Applicative

data Opts = Opts
  {socketPath :: FilePath}
  deriving stock (Eq, Show)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseOpts <**> helper) fullDesc

run :: Opts -> IO ()
run _ =
  putStrLn "Hello Word!"

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
