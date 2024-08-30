{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Data.Cardano.Seandexer.ChainSync
  ( NodeToClientProtocols (),
    RunMiniProtocolWithMinimalCtx (),
    SocketPath (..),
    subscribe,
  ) where

import Data.Cardano.Seandexer.AppT

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Client.Subscription qualified as Subscription
import Control.Monad.Cont (ContT (..))
import Control.Tracer (Tracer (..), contramapM, nullTracer)
import Network.TypedProtocol.Codec (Codec ())
import Network.TypedProtocol.Core qualified as Protocol
import Ouroboros.Consensus.Cardano.Node qualified as Consensus
import Ouroboros.Consensus.Network.NodeToClient qualified as Client
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
  :: NetworkMagic
  -> SocketPath
  -> ContT () (AppT IO) ChainSyncHook
subscribe networkMagic (SocketPath socketPath) =
  ContT $ \next -> do
    env <- ask

    void . liftIO . Network.withIOManager $ \ioManager ->
      Subscription.subscribe
        (Network.localSnocket ioManager)
        networkMagic
        nodeToClientVersions
        (tracers env)
        (subscriptionParams socketPath)
        (protocols next env)

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
  :: (ChainSyncHook -> AppT IO ())
  -> AppEnv
  -> Consensus.NodeToClientVersion
  -> Consensus.BlockNodeToClientVersion StandardBlock
  -> NodeToClientProtocols () Void
protocols next env clientVersion blockVersion =
  Network.NodeToClientProtocols
    { localChainSyncProtocol =
        localChainSyncProtocol' next env blockVersion clientVersion,
      localTxSubmissionProtocol = localTxSubmissionProtocol' blockVersion clientVersion,
      localStateQueryProtocol = localStateQueryProtocol' blockVersion clientVersion,
      localTxMonitorProtocol = localTxMonitorProtocol' blockVersion clientVersion
    }

localChainSyncProtocol'
  :: (ChainSyncHook -> AppT IO ())
  -> AppEnv
  -> Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localChainSyncProtocol' next env blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cChainSyncCodec (codecs blockVersion clientVersion)
    peer = mkChainSyncPeer next env

mkChainSyncPeer
  :: (MonadIO m, Monad m)
  => (ChainSyncHook -> AppT m ())
  -> AppEnv
  -> Protocol.Peer
      (ChainSync StandardBlock (Block.Point StandardBlock) (Block.Tip StandardBlock))
      'Protocol.AsClient
      StIdle
      m
      ()
mkChainSyncPeer next env =
  ChainSync.chainSyncClientPeer $
    ChainSync.ChainSyncClient (mkChainSyncClient next env)

mkChainSyncClient
  :: (MonadIO m, Monad m)
  => (ChainSyncHook -> AppT m ())
  -> AppEnv
  -> m
      ( ChainSync.ClientStIdle
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
      )
mkChainSyncClient next env =
  pure $
    ChainSync.SendMsgFindIntersect [Block.genesisPoint] (mkFindIntersectClient next env)

mkFindIntersectClient
  :: forall m
   . MonadIO m
  => (ChainSyncHook -> AppT m ())
  -> AppEnv
  -> ChainSync.ClientStIntersect
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkFindIntersectClient next env =
  ChainSync.ClientStIntersect
    { recvMsgIntersectFound = intersectFound,
      recvMsgIntersectNotFound = intersectNotFound
    }
  where
    intersectFound
      :: Block.Point StandardBlock
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
      :: Block.Tip StandardBlock
      -> ChainSync.ChainSyncClient
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
    intersectNotFound serverTip = ChainSync.ChainSyncClient (mkRequestNextClient Nothing serverTip)

    mkRequestNextClient
      :: Maybe (Block.Point StandardBlock)
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

      pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext next env)

mkClientStNext
  :: forall m
   . MonadIO m
  => (ChainSyncHook -> AppT m ())
  -> AppEnv
  -> ChainSync.ClientStNext
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkClientStNext next env =
  ChainSync.ClientStNext
    { recvMsgRollForward = rollForward,
      recvMsgRollBackward = rollBackward
    }
  where
    rollForward
      :: StandardBlock
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
      :: Block.Point StandardBlock
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
      :: Either StandardBlock (Block.Point StandardBlock)
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
          runAppT env (next $ RollForward serverTip block)
          pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext next env)
        Right point -> do
          runAppT env (next $ RollBackward serverTip point)
          pure $ ChainSync.SendMsgRequestNext (pure ()) (mkClientStNext next env)

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
