module Data.Cardano.Seandexer.AppT
  ( AppT (..),
    AppEnv (..),
    StandardBlock (),
    StandardTip (),
    StandardLedgerState (),
    mkAppEnv,
    runAppT,
  ) where

import Control.Tracer (Tracer (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock (), StandardCrypto ())
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Block (Tip ())
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
    envStdOutTracer :: Tracer IO Text,
    envLedgerState :: TVar StandardLedgerState
  }

-- | A Cardano block, applied to @StandardCrypto@
type StandardBlock = CardanoBlock StandardCrypto

-- | Tip of the server's chain, applied to @StandardBlock@
type StandardTip = Tip StandardBlock

-- | Extended ledger state, applied to @StandardBlock@
type StandardLedgerState = ExtLedgerState StandardBlock

mkAppEnv :: ProtocolInfo StandardBlock -> ConsoleRegion -> IO AppEnv
mkAppEnv protoInfo region = do
  progressTracer <- consoleRegionTracer region
  ledgerState <- newTVarIO (pInfoInitLedger protoInfo)

  pure $
    AppEnv
      { envProgressTracer = progressTracer,
        envStdOutTracer = outputConcurrentTracer,
        envLedgerState = ledgerState
      }

consoleRegionTracer :: ConsoleRegion -> IO (Tracer IO Text)
consoleRegionTracer region =
  pure $ Tracer (Console.setConsoleRegion region)

outputConcurrentTracer :: Tracer IO Text
outputConcurrentTracer =
  Tracer $ outputConcurrent . (<> "\n")

runAppT :: AppEnv -> AppT m a -> m a
runAppT env action = runReaderT (unAppT action) env
