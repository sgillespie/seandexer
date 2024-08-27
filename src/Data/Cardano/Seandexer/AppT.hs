module Data.Cardano.Seandexer.AppT
  ( AppT (..),
    AppEnv (..),
    mkAppEnv,
    runAppT,
  ) where

import Control.Tracer (Tracer (..))
import System.Console.Concurrent (outputConcurrent)
import System.Console.Regions (ConsoleRegion ())
import System.Console.Regions qualified as Console

newtype AppT m a = AppT
  {unAppT :: ReaderT AppEnv m a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader AppEnv
    )

data AppEnv = AppEnv
  { envProgressTracer :: Tracer IO Text,
    envStdOutTracer :: Tracer IO Text
  }

mkAppEnv :: ConsoleRegion -> IO AppEnv
mkAppEnv region = do
  progressTracer <- consoleRegionTracer region

  pure $
    AppEnv
      { envProgressTracer = progressTracer,
        envStdOutTracer = outputConcurrentTracer
      }

consoleRegionTracer :: ConsoleRegion -> IO (Tracer IO Text)
consoleRegionTracer region =
  pure $ Tracer (Console.setConsoleRegion region)

outputConcurrentTracer :: Tracer IO Text
outputConcurrentTracer =
  Tracer $ outputConcurrent . (<> "\n")

runAppT :: AppEnv -> AppT m a -> m a
runAppT env action = runReaderT (unAppT action) env
