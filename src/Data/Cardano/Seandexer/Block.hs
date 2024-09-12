{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Cardano.Seandexer.Block
  ( applyBlock,
    rollBackward,
    rollForward,
  ) where

import Data.Cardano.Seandexer.AppT

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx, AlonzoTx (..), AlonzoTxBody (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBodyRaw (..), AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxSeq (AlonzoTxSeq (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxBodyRaw (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Binary.Decoding (mkSized)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..), ConwayTxBodyRaw (..))
import Cardano.Ledger.Core (Value ())
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Mary (ShelleyTxOut)
import Cardano.Ledger.Mary.TxBody (MaryTxBody (..), MaryTxBodyRaw (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..))
import Cardano.Ledger.MemoBytes (MemoBytes (..))
import Cardano.Ledger.SafeHash (SafeToHash ())
import Cardano.Ledger.Shelley.API.Types (ShelleyTx, ShelleyTxOut (..), UTxO (..))
import Cardano.Ledger.Shelley.BlockChain (ShelleyTxSeq (..))
import Cardano.Ledger.Shelley.LedgerState qualified as Ledger
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..), ShelleyTxRaw (..))
import Cardano.Ledger.TxIn (TxIn ())
import Cardano.Ledger.Val (Val ())
import Control.Concurrent.Class.MonadSTM.Strict qualified as STM
import Control.Monad.Cont (ContT (..))
import Control.Tracer (Tracer (..))
import Data.Sequence.Strict (StrictSeq)
import Ouroboros.Consensus.Block (BlockNo (..), Point, withOrigin, withOriginToMaybe)
import Ouroboros.Consensus.Cardano.Block (AlonzoEra, HardForkBlock (..), LedgerState (..))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HeaderValidation (AnnTip (..), HeaderState (..))
import Ouroboros.Consensus.Ledger.Abstract (tickThenReapply)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Eras qualified as Era
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Block

applyBlock :: MonadIO m => ChainSyncHook -> ContT () (AppT m) Void
applyBlock hook = ContT $ \_ ->
  case hook of
    RollForward tip block -> rollForward tip block
    RollBackward tip block -> rollBackward tip block

rollForward :: MonadIO m => StandardTip -> StandardBlock -> AppT m ()
rollForward serverTip clientTip = do
  AppEnv{..} <- ask

  let trace' = liftIO . runTracer envStdOutTracer
      progress' = liftIO . runTracer envProgressTracer
      block = maybeTrimBlock envTrimBlocks clientTip

  -- Apply ledger state
  ledger <- liftIO . STM.atomically $ do
    ledgerState <- STM.readTVar envLedgerState
    let ledgerState' = tickThenReapply (ledgerCfg envProtocolInfo) block ledgerState
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

    -- Verify we have properly cleaned up
    maybeVerifyLedger envTrimBlocks ledger

  when (blockNo' >= tip') $ do
    trace' $ "Reached final ledger state at block: " <> show (getLedgerTip ledger)
    exitSuccess
  where
    getBlockNo block =
      case Block.getHeaderFields block of
        Block.HeaderFields _ blockNo' _ -> Block.unBlockNo blockNo'

    ledgerCfg protoInfo = ExtLedgerCfg (pInfoConfig protoInfo)

    maybeTrimBlock (TrimBlocks False) = id
    maybeTrimBlock (TrimBlocks True) = trimBlock

    maybeVerifyLedger (TrimBlocks False) _ = pure ()
    maybeVerifyLedger (TrimBlocks True) ledger =
      case ledgerState ledger of
        LedgerStateByron _ -> pure ()
        LedgerStateShelley _ -> pure ()
        LedgerStateAllegra _ -> pure ()
        LedgerStateMary maryLedger -> checkLedgerStateMary maryLedger
        LedgerStateAlonzo alonzoLedger -> checkLedgerStateAlonzo alonzoLedger
        LedgerStateBabbage babbageLedger -> checkLedgerStateBabbage babbageLedger
        LedgerStateConway conwayLedger -> checkLedgerStateBabbage conwayLedger

getLedgerTip :: StandardLedgerState -> Maybe Word64
getLedgerTip (ExtLedgerState _ header) =
  let (HeaderState tip _) = header
      tip' = withOriginToMaybe tip
      blockNo' = unBlockNo . annTipBlockNo <$> tip'
  in blockNo'

trimBlock :: StandardBlock -> StandardBlock
trimBlock = \case
  block@(BlockByron _) -> block
  block@(BlockShelley _) -> block
  block@(BlockAllegra _) -> block
  BlockMary block -> BlockMary (trimMaryBlock block)
  BlockAlonzo block -> BlockAlonzo (trimAlonzoBlock block)
  BlockBabbage block -> BlockBabbage (trimBabbageBlock block)
  BlockConway block -> BlockConway (trimConwayBlock block)
  where
    trimMaryBlock = withShelleyBlock trimMaryTxs
    trimAlonzoBlock = withShelleyBlock (withAlonzoTxs trimAlonzoTxBody)
    trimBabbageBlock = withShelleyBlock (withAlonzoTxs trimBabbageTxBody)
    trimConwayBlock = withShelleyBlock (withAlonzoTxs trimConwayTxBody)

withShelleyBlock
  :: (Ledger.TxSeq era -> Ledger.TxSeq era)
  -> ShelleyBlock proto era
  -> ShelleyBlock proto era
withShelleyBlock f ShelleyBlock{..} =
  ShelleyBlock
    { shelleyBlockRaw = blockRaw shelleyBlockRaw,
      shelleyBlockHeaderHash = shelleyBlockHeaderHash
    }
  where
    blockRaw (Block' header txs bytes) = Block' header (f txs) bytes

trimMaryTxs
  :: ShelleyTxSeq Era.StandardMary
  -> ShelleyTxSeq Era.StandardMary
trimMaryTxs (TxSeq' txs body wits meta) = TxSeq' (fmap trimTx txs) body wits meta
  where
    trimTx :: ShelleyTx Era.StandardMary -> ShelleyTx Era.StandardMary
    trimTx (UnsafeShelleyTx (UnsafeMemo tx bytes hash)) =
      UnsafeShelleyTx (UnsafeMemo (trimTxRaw tx) bytes hash)

    trimTxRaw :: ShelleyTxRaw Era.StandardMary -> ShelleyTxRaw Era.StandardMary
    trimTxRaw tx@UnsafeShelleyTxRaw{ustrBody} = tx{ustrBody = trimTxBody ustrBody}

    trimTxBody :: MaryTxBody Era.StandardMary -> MaryTxBody Era.StandardMary
    trimTxBody (UnsafeMaryTxBody (UnsafeMemo raw bytes hash)) =
      UnsafeMaryTxBody (UnsafeMemo (trimRawTxBody raw) bytes hash)

    trimRawTxBody :: MaryTxBodyRaw Era.StandardMary -> MaryTxBodyRaw Era.StandardMary
    trimRawTxBody b@UnsafeMaryTxBodyRaw{..} =
      b{umtbMint = mempty, umtbOutputs = fmap unMultiAsset umtbOutputs}

    unMultiAsset :: ShelleyTxOut Era.StandardMary -> ShelleyTxOut Era.StandardMary
    unMultiAsset (ShelleyTxOut addr val) =
      ShelleyTxOut addr (trimMultiAsset val)

withAlonzoTxs
  :: (AlonzoEraTx era, SafeToHash (Ledger.TxWits era), Ledger.Tx era ~ AlonzoTx era)
  => (Ledger.TxBody era -> Ledger.TxBody era)
  -> AlonzoTxSeq era
  -> AlonzoTxSeq era
withAlonzoTxs f (UnsafeAlonzoTxSeq txs body' wits' meta' isValid') =
  UnsafeAlonzoTxSeq (fmap (withTx f) txs) body' wits' meta' isValid'
  where
    withTx :: (Ledger.TxBody era -> Ledger.TxBody era) -> AlonzoTx era -> AlonzoTx era
    withTx f' AlonzoTx{..} =
      AlonzoTx
        { body = f' body,
          wits = wits,
          isValid = isValid,
          auxiliaryData = auxiliaryData
        }

trimAlonzoTxBody :: AlonzoTxBody Era.StandardAlonzo -> AlonzoTxBody Era.StandardAlonzo
trimAlonzoTxBody (UnsafeAlonzoTxBody (UnsafeMemo raw bytes hash)) =
  UnsafeAlonzoTxBody (UnsafeMemo (trimRawTxBody raw) bytes hash)
  where
    trimRawTxBody :: AlonzoTxBodyRaw Era.StandardAlonzo -> AlonzoTxBodyRaw Era.StandardAlonzo
    trimRawTxBody body@UnsafeAlonzoTxBodyRaw{uatbOutputs} =
      body
        { uatbMint = mempty,
          uatbOutputs = fmap trimAlonzoTxOut uatbOutputs
        }

    trimAlonzoTxOut :: AlonzoTxOut Era.StandardAlonzo -> AlonzoTxOut Era.StandardAlonzo
    trimAlonzoTxOut (AlonzoTxOut addr val datum) =
      AlonzoTxOut addr (trimMultiAsset val) datum

trimMultiAsset :: MaryValue crypto -> MaryValue crypto
trimMultiAsset (MaryValue c _) = MaryValue c mempty

trimBabbageTxBody
  :: BabbageTxBody Era.StandardBabbage
  -> BabbageTxBody Era.StandardBabbage
trimBabbageTxBody (UnsafeBabbageTxBody (UnsafeMemo raw bytes hash)) =
  UnsafeBabbageTxBody (UnsafeMemo (trimRawTxBody raw) bytes hash)
  where
    trimRawTxBody
      :: BabbageTxBodyRaw Era.StandardBabbage -> BabbageTxBodyRaw Era.StandardBabbage
    trimRawTxBody body@UnsafeBabbageTxBodyRaw{ubtbOutputs} =
      body
        { ubtbMint = mempty,
          ubtbOutputs = trimBabbageTxOuts ubtbOutputs
        }

trimBabbageTxOuts
  :: forall crypto era
   . (Crypto crypto, Ledger.EraScript era, Value era ~ MaryValue crypto)
  => StrictSeq (Sized (BabbageTxOut era))
  -> StrictSeq (Sized (BabbageTxOut era))
trimBabbageTxOuts =
  fmap $ \(Sized out _) ->
    mkSized (Ledger.eraProtVerLow @era) (trimOutput out)
  where
    trimOutput :: BabbageTxOut era -> BabbageTxOut era
    trimOutput (BabbageTxOut addr val datum script) =
      BabbageTxOut addr (trimMultiAsset val) datum script

trimConwayTxBody :: ConwayTxBody Era.StandardConway -> ConwayTxBody Era.StandardConway
trimConwayTxBody (UnsafeConwayTxBody (UnsafeMemo raw bytes hash)) =
  UnsafeConwayTxBody (UnsafeMemo (trimRawTxBody raw) bytes hash)
  where
    trimRawTxBody :: ConwayTxBodyRaw Era.StandardConway -> ConwayTxBodyRaw Era.StandardConway
    trimRawTxBody body@UnsafeConwayTxBodyRaw{uctbOutputs} =
      body
        { uctbMint = mempty,
          uctbOutputs = trimBabbageTxOuts uctbOutputs
        }

rollBackward :: MonadIO m => StandardTip -> Point StandardBlock -> AppT m ()
rollBackward serverTip point = do
  env <- ask

  let tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)
      trace' = liftIO . runTracer (envStdOutTracer env)

  trace' $ "Rolling backward at point: " <> show point <> ", tip: " <> show tip'

checkLedgerStateMary
  :: MonadIO io
  => LedgerState (ShelleyBlock proto Era.StandardMary)
  -> AppT io ()
checkLedgerStateMary ledger@ShelleyLedgerState{shelleyLedgerTip} =
  traverse_
    (\txout -> when (hasMultiAsset txout) $ error errMsg)
    (ledgerUtxos ledger)
  where
    hasMultiAsset :: ShelleyTxOut Era.StandardMary -> Bool
    hasMultiAsset (ShelleyTxOut _ (MaryValue _ (MultiAsset m))) = not (null m)

    errMsg = "Encountered MultiAsset at: " <> show shelleyLedgerTip

checkLedgerStateAlonzo
  :: MonadIO io
  => LedgerState (ShelleyBlock proto (AlonzoEra Era.StandardCrypto))
  -> AppT io ()
checkLedgerStateAlonzo ledger@ShelleyLedgerState{shelleyLedgerTip} =
  traverse_
    (\txout -> when (hasMultiAsset txout) $ error errMsg)
    (ledgerUtxos ledger)
  where
    hasMultiAsset :: AlonzoTxOut (AlonzoEra Era.StandardCrypto) -> Bool
    hasMultiAsset (AlonzoTxOut _ (MaryValue _ (MultiAsset m)) _) = not (null m)

    errMsg = "Encountered MultiAsset at: " <> show shelleyLedgerTip

checkLedgerStateBabbage
  :: ( MonadIO io,
       Era era,
       Val (Ledger.Value era),
       Ledger.Value era ~ MaryValue crypto,
       Ledger.TxOut era ~ BabbageTxOut era
     )
  => LedgerState (ShelleyBlock proto era)
  -> AppT io ()
checkLedgerStateBabbage ledger@ShelleyLedgerState{shelleyLedgerTip} =
  traverse_
    (\txout -> when (hasMultiAsset txout) $ error errMsg)
    (ledgerUtxos ledger)
  where
    errMsg = "Encountered MultiAsset at: " <> show shelleyLedgerTip

    hasMultiAsset
      :: (Era era, Val (Ledger.Value era), Ledger.Value era ~ MaryValue crypto)
      => BabbageTxOut era
      -> Bool
    hasMultiAsset (BabbageTxOut _ (MaryValue _ (MultiAsset m)) _ _) = not (null m)

ledgerUtxos
  :: LedgerState (ShelleyBlock proto era)
  -> Map (TxIn (Ledger.EraCrypto era)) (Ledger.TxOut era)
ledgerUtxos ShelleyLedgerState{shelleyLedgerState} =
  let Ledger.NewEpochState{nesEs} = shelleyLedgerState
      Ledger.EpochState{esLState} = nesEs
      Ledger.LedgerState{lsUTxOState} = esLState
      Ledger.UTxOState{utxosUtxo} = lsUTxOState
  in unUTxO utxosUtxo
