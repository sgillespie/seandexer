{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer.ChainSync
  ( StandardBlock (),
    NodeToClientProtocols (),
    RunMiniProtocolWithMinimalCtx (),
    SocketPath (..),
    StandardTip (),
    ShelleyGenesisHash (),
    subscribe,
    protocolInfo,
  ) where

import Data.Cardano.Seandexer.AppT

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Chain.Genesis (Config ())
import Cardano.Chain.Update qualified as Byron
import Cardano.Client.Subscription qualified as Subscription
import Cardano.Crypto.Hash (Blake2b_256 (), Hash ())
import Cardano.Crypto.Hash.Class (castHash)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis ())
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer (..), natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis ())
import Control.Tracer (Tracer (..), contramapM, nullTracer)
import Network.TypedProtocol.Codec (Codec ())
import Network.TypedProtocol.Core qualified as Protocol
import Ouroboros.Consensus.Block (withOrigin)
import Ouroboros.Consensus.Cardano qualified as Consensus
import Ouroboros.Consensus.Cardano.Node qualified as Consensus
import Ouroboros.Consensus.Config (emptyCheckpointsMap)
import Ouroboros.Consensus.Mempool (mkOverrides, noOverridesMeasure)
import Ouroboros.Consensus.Network.NodeToClient qualified as Client
import Ouroboros.Consensus.Node (ProtocolInfo)
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as Consensus
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy ())
import Ouroboros.Network.Block qualified as Block
import Ouroboros.Network.Magic (NetworkMagic ())
import Ouroboros.Network.Mux qualified as Network
import Ouroboros.Network.NodeToClient qualified as Network
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..))
import Ouroboros.Network.Snocket (localAddressFromPath)
import System.Exit (ExitCode ())

-- | Record of node-to-client mini protocols.
type NodeToClientProtocols =
  Network.NodeToClientProtocols
    'Subscription.InitiatorMode
    Subscription.LocalAddress
    LByteString
    IO

-- | 'RunMiniProtocol' with 'MinimalInitiatorContext' and 'ResponderContext'.
--
-- Use to run node-to-client application as well as in some non p2p contexts.
type RunMiniProtocolWithMinimalCtx a b =
  Network.RunMiniProtocolWithMinimalCtx
    'Subscription.InitiatorMode
    Subscription.LocalAddress
    LByteString
    IO
    a
    b

-- | Unix socket address
newtype SocketPath = SocketPath {unSocketPath :: FilePath}
  deriving stock (Eq, Show)

-- | Subscribe to a Cardano node with the `node-to-client` mini-protocol.
subscribe
  :: MonadIO io
  => NetworkMagic
  -> SocketPath
  -> AppT io Void
subscribe networkMagic (SocketPath socketPath) = do
  env <- ask

  liftIO . Network.withIOManager $ \ioManager ->
    Subscription.subscribe
      (Network.localSnocket ioManager)
      networkMagic
      nodeToClientVersions
      (tracers env)
      (subscriptionParams socketPath)
      (protocols env)

nodeToClientVersions
  :: Map
      Consensus.NodeToClientVersion
      (Consensus.BlockNodeToClientVersion StandardBlock)
nodeToClientVersions = Consensus.supportedNodeToClientVersions (Proxy @StandardBlock)

tracers :: AppEnv -> Network.NetworkClientSubcriptionTracers
tracers AppEnv{envStdOutTracer = stdoutTracer} =
  Network.NetworkSubscriptionTracers
    { nsMuxTracer = muxTracer,
      nsHandshakeTracer = handshakeTracer,
      nsErrorPolicyTracer = errorPolicyTracer,
      nsSubscriptionTracer = subscriptionTracer
    }
  where
    muxTracer = nullTracer
    handshakeTracer = contramapM (pure . ("[HandshakeTracer] " <>) . show) stdoutTracer
    errorPolicyTracer = contramapM (pure . ("[ErrorPolicyTracer] " <>) . show) stdoutTracer
    subscriptionTracer = contramapM (pure . ("[SubscriptionTracer] " <>) . show) stdoutTracer

subscriptionParams :: FilePath -> Network.ClientSubscriptionParams ()
subscriptionParams socketPath =
  Network.ClientSubscriptionParams
    { cspAddress = localAddressFromPath socketPath,
      cspConnectionAttemptDelay = Nothing,
      cspErrorPolicies =
        Network.networkErrorPolicies
          <> consensusErrorPolicy (Proxy @StandardBlock)
          <> Network.ErrorPolicies
            { epAppErrorPolicies =
                [ Network.ErrorPolicy $ \(_ :: ExitCode) -> Just Network.Throw
                ],
              epConErrorPolicies = []
            }
    }

protocols
  :: AppEnv
  -> Consensus.NodeToClientVersion
  -> Consensus.BlockNodeToClientVersion StandardBlock
  -> NodeToClientProtocols () Void
protocols env clientVersion blockVersion =
  Network.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol' env blockVersion clientVersion,
      localTxSubmissionProtocol = localTxSubmissionProtocol' blockVersion clientVersion,
      localStateQueryProtocol = localStateQueryProtocol' blockVersion clientVersion,
      localTxMonitorProtocol = localTxMonitorProtocol' blockVersion clientVersion
    }

localChainSyncProtocol'
  :: AppEnv
  -> Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localChainSyncProtocol' env blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cChainSyncCodec (codecs blockVersion clientVersion)
    peer = mkChainSyncPeer env

mkChainSyncPeer
  :: (MonadIO m, Monad m)
  => AppEnv
  -> Protocol.Peer
      (ChainSync StandardBlock (Block.Point StandardBlock) (Block.Tip StandardBlock))
      'Protocol.AsClient
      StIdle
      m
      ()
mkChainSyncPeer env =
  ChainSync.chainSyncClientPeer $
    ChainSync.ChainSyncClient (mkChainSyncClient env)

mkChainSyncClient
  :: (MonadIO m, Monad m)
  => AppEnv
  -> m
      ( ChainSync.ClientStIdle
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
      )
mkChainSyncClient env =
  pure $ ChainSync.SendMsgFindIntersect [Block.genesisPoint] (mkFindIntersectClient env)

mkFindIntersectClient
  :: (MonadIO m, Monad m)
  => AppEnv
  -> ChainSync.ClientStIntersect
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkFindIntersectClient env =
  ChainSync.ClientStIntersect
    { recvMsgIntersectFound = intersectFound,
      recvMsgIntersectNotFound = intersectNotFound
    }
  where
    intersectFound
      :: MonadIO m
      => Block.Point StandardBlock
      -> Block.Tip StandardBlock
      -> ChainSync.ChainSyncClient
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
    intersectFound point tip =
      ChainSync.ChainSyncClient $ mkRequestNextClient (Just point) tip

    intersectNotFound
      :: MonadIO m
      => Block.Tip StandardBlock
      -> ChainSync.ChainSyncClient
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
    intersectNotFound serverTip = ChainSync.ChainSyncClient (mkRequestNextClient Nothing serverTip)

    mkRequestNextClient
      :: MonadIO m
      => Maybe (Block.Point StandardBlock)
      -> Block.Tip StandardBlock
      -> m
          ( ChainSync.ClientStIdle
              StandardBlock
              (Block.Point StandardBlock)
              (Block.Tip StandardBlock)
              m
              ()
          )
    mkRequestNextClient clientPoint serverTip = do
      let tip' = Block.getTipBlockNo serverTip
          trace' = liftIO . runTracer (envStdOutTracer env)

      case clientPoint of
        Just point' ->
          trace' $ "Found intersection at point: " <> show point' <> ", tip: " <> show tip'
        Nothing ->
          trace' $ "No intersection found at tip: " <> show tip'

      pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext env)

mkClientStNext
  :: MonadIO m
  => AppEnv
  -> ChainSync.ClientStNext
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkClientStNext env =
  ChainSync.ClientStNext
    { recvMsgRollForward = rollForward,
      recvMsgRollBackward = rollBackward
    }
  where
    rollForward
      :: MonadIO m
      => StandardBlock
      -> Block.Tip StandardBlock
      -> ChainSync.ChainSyncClient
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
    rollForward clientTip serverTip =
      ChainSync.ChainSyncClient $ mkRequestNextClient (Left clientTip) serverTip

    rollBackward
      :: MonadIO m
      => Block.Point StandardBlock
      -> Block.Tip StandardBlock
      -> ChainSync.ChainSyncClient
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
    rollBackward clientPoint serverTip =
      ChainSync.ChainSyncClient $ mkRequestNextClient (Right clientPoint) serverTip

    mkRequestNextClient
      :: MonadIO m
      => Either StandardBlock (Block.Point StandardBlock)
      -> Block.Tip StandardBlock
      -> m
          ( ChainSync.ClientStIdle
              StandardBlock
              (Block.Point StandardBlock)
              (Block.Tip StandardBlock)
              m
              ()
          )
    mkRequestNextClient clientTip serverTip =
      case clientTip of
        Left block -> do
          runAppT env $ processBlock serverTip block

          pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext env)
        Right point -> do
          reportRollback point
          pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext env)
      where
        tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)
        trace' = liftIO . runTracer (envStdOutTracer env)

        reportRollback point =
          trace' $ "Rolling backward at point: " <> show point <> ", tip: " <> show tip'

localTxSubmissionProtocol'
  :: Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localTxSubmissionProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cTxSubmissionCodec (codecs blockVersion clientVersion)
    peer = Network.localTxSubmissionPeerNull

localStateQueryProtocol'
  :: Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localStateQueryProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cStateQueryCodec (codecs blockVersion clientVersion)
    peer = Network.localStateQueryPeerNull

localTxMonitorProtocol'
  :: Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localTxMonitorProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cTxMonitorCodec (codecs blockVersion clientVersion)
    peer = Network.localTxMonitorPeerNull

mkInitiatorProtocolOnly
  :: ( Show failure,
       forall (st' :: ps). Show (Protocol.ClientHasAgency st'),
       forall (st' :: ps). Show (Protocol.ServerHasAgency st'),
       ShowProxy ps
     )
  => Codec ps failure IO LByteString
  -> Protocol.Peer ps pr st IO a
  -> RunMiniProtocolWithMinimalCtx a Void
mkInitiatorProtocolOnly codec peer =
  Subscription.InitiatorProtocolOnly $
    Network.mkMiniProtocolCbFromPeer $
      const (tracer, codec, peer)
  where
    tracer = nullTracer

codecs
  :: Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> Client.ClientCodecs StandardBlock IO
codecs = Client.clientCodecs codecConfig
  where
    codecConfig = pClientInfoCodecConfig cfg
    cfg = Consensus.protocolClientInfoCardano mainnetEpochSlots

processBlock :: MonadIO m => StandardTip -> StandardBlock -> AppT m ()
processBlock serverTip clientTip = do
  trace' <- asks (runTracer . envStdOutTracer)
  progress <- asks (runTracer . envProgressTracer)

  let blockNo' = getBlockNo clientTip
      tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)

  when (blockNo' `mod` 10000 == 0) $
    liftIO . progress $
      "Progress: "
        <> show (blockNo' * 100 `div` tip')
        <> "% (block "
        <> show blockNo'
        <> " / "
        <> show tip'
        <> ")"

  when (blockNo' >= tip') $ do
    liftIO . trace' $ "Reached final block: " <> show blockNo'
    exitSuccess
  where
    getBlockNo block =
      case Block.getHeaderFields block of
        Block.HeaderFields _ blockNo' _ -> Block.unBlockNo blockNo'

type ByronConfig = Config
type ShelleyGenesisHash = Hash Blake2b_256 ByteString

protocolInfo
  :: ByronConfig
  -> ShelleyGenesisHash
  -> ShelleyGenesis StandardCrypto
  -> AlonzoGenesis
  -> ConwayGenesis StandardCrypto
  -> ProtocolInfo StandardBlock
protocolInfo byronCfg shelleyGenesisHash shelleyGenesis alonzoGenesis conwayGenesis =
  fst $ Consensus.protocolInfoCardano @StandardCrypto @IO protoParams
  where
    protoParams :: Consensus.CardanoProtocolParams StandardCrypto
    protoParams =
      Consensus.CardanoProtocolParams
        { paramsByron =
            Consensus.ProtocolParamsByron
              { byronGenesis = byronCfg,
                byronPbftSignatureThreshold = Nothing,
                byronProtocolVersion = Byron.ProtocolVersion 3 0 0,
                byronSoftwareVersion = Byron.SoftwareVersion (Byron.ApplicationName "cardano-sl") 1,
                byronLeaderCredentials = Nothing,
                byronMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsShelleyBased =
            Consensus.ProtocolParamsShelleyBased
              { shelleyBasedInitialNonce = Nonce (castHash shelleyGenesisHash),
                shelleyBasedLeaderCredentials = []
              },
          paramsShelley =
            Consensus.ProtocolParamsShelley
              { shelleyProtVer = ProtVer (natVersion @3) 0,
                shelleyMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsAllegra =
            Consensus.ProtocolParamsAllegra
              { allegraProtVer = ProtVer (natVersion @4) 0,
                allegraMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsMary =
            Consensus.ProtocolParamsMary
              { maryProtVer = ProtVer (natVersion @5) 0,
                maryMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsAlonzo =
            Consensus.ProtocolParamsAlonzo
              { alonzoProtVer = ProtVer (natVersion @7) 0,
                alonzoMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsBabbage =
            Consensus.ProtocolParamsBabbage
              { babbageProtVer = ProtVer (natVersion @9) 0,
                babbageMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          paramsConway =
            Consensus.ProtocolParamsConway
              { conwayProtVer = ProtVer (natVersion @10) 0,
                conwayMaxTxCapacityOverrides = mkOverrides noOverridesMeasure
              },
          ledgerTransitionConfig =
            mkLatestTransitionConfig shelleyGenesis alonzoGenesis conwayGenesis,
          hardForkTriggers =
            Consensus.CardanoHardForkTriggers'
              { triggerHardForkShelley = Consensus.TriggerHardForkAtVersion 2,
                triggerHardForkAllegra = Consensus.TriggerHardForkAtVersion 3,
                triggerHardForkMary = Consensus.TriggerHardForkAtVersion 4,
                triggerHardForkAlonzo = Consensus.TriggerHardForkAtVersion 5,
                triggerHardForkBabbage = Consensus.TriggerHardForkAtVersion 7,
                triggerHardForkConway = Consensus.TriggerHardForkAtVersion 9
              },
          checkpoints = emptyCheckpointsMap
        }
