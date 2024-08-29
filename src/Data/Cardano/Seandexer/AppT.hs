module Data.Cardano.Seandexer.AppT
  ( AppT (..),
    AppEnv (..),
    LedgerEra (..),
    StandardBlock (),
    StandardTip (),
    StandardLedgerState (),
    mkAppEnv,
    runAppT,
  ) where

import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
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
    envLedgerState :: STM.StrictTVar IO StandardLedgerState,
    envProtocolInfo :: ProtocolInfo StandardBlock,
    envStartEra :: LedgerEra
  }

data LedgerEra
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  deriving stock (Eq, Ord, Show)

-- | A Cardano block, applied to @StandardCrypto@
type StandardBlock = CardanoBlock StandardCrypto

-- | Tip of the server's chain, applied to @StandardBlock@
type StandardTip = Tip StandardBlock

-- | Extended ledger state, applied to @StandardBlock@
type StandardLedgerState = ExtLedgerState StandardBlock

mkAppEnv
  :: ProtocolInfo StandardBlock
  -> LedgerEra
  -> ConsoleRegion
  -> IO AppEnv
mkAppEnv protoInfo era region = do
  progressTracer <- consoleRegionTracer region
  ledgerState <- STM.newTVarIO (pInfoInitLedger protoInfo)

  pure $
    AppEnv
      { envProgressTracer = progressTracer,
        envStdOutTracer = outputConcurrentTracer,
        envLedgerState = ledgerState,
        envProtocolInfo = protoInfo,
        envStartEra = era
      }

consoleRegionTracer :: ConsoleRegion -> IO (Tracer IO Text)
consoleRegionTracer region =
  pure $ Tracer (Console.setConsoleRegion region)

outputConcurrentTracer :: Tracer IO Text
outputConcurrentTracer =
  Tracer $ outputConcurrent . (<> "\n")

runAppT :: AppEnv -> AppT m a -> m a
runAppT env action = runReaderT (unAppT action) env
