module Data.Cardano.Seandexer.AppT
  ( AppT (..),
    AppEnv (..),
    ChainSyncHook (..),
    TrimBlocks (..),
    LedgerEra (..),
    StandardBlock (),
    StandardTip (),
    StandardLedgerState (),
    mkAppEnv,
    runAppT,
    runContAppT,
  ) where

import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
import Control.Monad.Cont (ContT (..), runContT)
import Control.Tracer (Tracer (..))
import Ouroboros.Consensus.Cardano.Block (CardanoBlock (), StandardCrypto ())
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Block (Point, Tip ())
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
    envStartEra :: LedgerEra,
    envTrimBlocks :: TrimBlocks
  }

newtype TrimBlocks = TrimBlocks {unTrimBlocks :: Bool}
  deriving newtype (Eq, Show)

data ChainSyncHook
  = RollForward StandardTip StandardBlock
  | RollBackward StandardTip (Point StandardBlock)

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
  -> TrimBlocks
  -> ConsoleRegion
  -> IO AppEnv
mkAppEnv protoInfo era shouldTrim region = do
  progressTracer <- consoleRegionTracer region
  ledgerState <- STM.newTVarIO (pInfoInitLedger protoInfo)

  pure $
    AppEnv
      { envProgressTracer = progressTracer,
        envStdOutTracer = outputConcurrentTracer,
        envLedgerState = ledgerState,
        envProtocolInfo = protoInfo,
        envStartEra = era,
        envTrimBlocks = shouldTrim
      }

consoleRegionTracer :: ConsoleRegion -> IO (Tracer IO Text)
consoleRegionTracer region =
  pure $ Tracer (Console.setConsoleRegion region)

outputConcurrentTracer :: Tracer IO Text
outputConcurrentTracer =
  Tracer $ outputConcurrent . (<> "\n")

runAppT :: AppEnv -> AppT m a -> m a
runAppT env action =
  runReaderT (unAppT action) env

runContAppT :: AppEnv -> ContT r (AppT m) Void -> m r
runContAppT env action =
  runAppT env (runContT action absurd)
