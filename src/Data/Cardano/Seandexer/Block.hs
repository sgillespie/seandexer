module Data.Cardano.Seandexer.Block
  ( applyBlock,
    rollBackward,
  ) where

import Data.Cardano.Seandexer.AppT

import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
import Control.Tracer (Tracer (..))
import Ouroboros.Consensus.Block (BlockNo (..), Point, withOrigin, withOriginToMaybe)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HeaderValidation (AnnTip (..), HeaderState (..))
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Block

applyBlock :: MonadIO m => StandardTip -> StandardBlock -> AppT m ()
applyBlock serverTip clientTip = do
  AppEnv{..} <- ask

  let trace' = liftIO . runTracer envStdOutTracer
      progress' = liftIO . runTracer envProgressTracer

  -- Apply ledger state
  ledger <- liftIO . STM.atomically $ do
    ledgerState <- STM.readTVar envLedgerState
    let ledgerState' = tickThenReapply (ledgerCfg envProtocolInfo) clientTip ledgerState
    STM.writeTVar envLedgerState ledgerState'

    pure ledgerState'

  let blockNo' = getBlockNo clientTip
      tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)

  when (blockNo' `mod` 1000 == 0) $ do
    progress' $
      "Progress: "
        <> show (blockNo' * 100 `div` tip')
        <> "% (block "
        <> show blockNo'
        <> " / "
        <> show tip'
        <> ")"

  when (blockNo' >= tip') $ do
    trace' $ "Reached final ledger state at block: " <> show (getLedgerTip ledger)
    exitSuccess
  where
    getBlockNo block =
      case Block.getHeaderFields block of
        Block.HeaderFields _ blockNo' _ -> Block.unBlockNo blockNo'

    ledgerCfg protoInfo = ExtLedgerCfg (pInfoConfig protoInfo)

getLedgerTip :: StandardLedgerState -> Maybe Word64
getLedgerTip (ExtLedgerState _ header) =
  let (HeaderState tip _) = header
      tip' = withOriginToMaybe tip
      blockNo' = unBlockNo . annTipBlockNo <$> tip'
  in blockNo'

rollBackward :: MonadIO m => StandardTip -> Point StandardBlock -> AppT m ()
rollBackward serverTip point = do
  env <- ask

  let tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)
      trace' = liftIO . runTracer (envStdOutTracer env)

  trace' $ "Rolling backward at point: " <> show point <> ", tip: " <> show tip'
