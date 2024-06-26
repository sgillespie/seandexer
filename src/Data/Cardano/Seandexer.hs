{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer
  ( SeandexerOpts (..),
    NetworkId (..),
    runSeandexer,
    mkTestnet,
  ) where

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Client.Subscription hiding (NodeToClientProtocols ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Tracer (contramapM, nullTracer, stdoutTracer)
import Network.TypedProtocol.Codec (Codec ())
import Network.TypedProtocol.Core qualified as Protocol
import Ouroboros.Consensus.Cardano (CardanoBlock ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Network.NodeToClient qualified as Client
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as Consensus
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy ())
import Ouroboros.Network.Block (Point (), Tip (..), genesisPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Network
import Ouroboros.Network.NodeToClient qualified as Network
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..))
import Ouroboros.Network.Snocket (localAddressFromPath)

data SeandexerOpts = SeandexerOpts
  { soSocketPath :: FilePath,
    soNetworkId :: NetworkId
  }
  deriving stock (Eq, Show)

type BlockVersion = CardanoBlock StandardCrypto

type NodeToClientProtocols =
  Network.NodeToClientProtocols
    'InitiatorMode
    LocalAddress
    LByteString
    IO

type RunMiniProtocolWithMinimalCtx x y =
  Network.RunMiniProtocolWithMinimalCtx
    'InitiatorMode
    LocalAddress
    LByteString
    IO
    x
    y

runSeandexer :: SeandexerOpts -> IO ()
runSeandexer SeandexerOpts{..} =
  Network.withIOManager $ \ioManager -> do
    void $
      subscribe
        (localSnocket' ioManager)
        (networkMagic soNetworkId)
        nodeToClientVersions
        tracers
        (subscriptionParams soSocketPath)
        protocols

localSnocket' :: Network.IOManager -> Network.LocalSnocket
localSnocket' = Network.localSnocket

data NetworkId
  = Mainnet
  | Testnet NetworkMagic
  deriving stock (Eq, Show)

mkTestnet :: Word32 -> NetworkId
mkTestnet = Testnet . NetworkMagic

networkMagic :: NetworkId -> NetworkMagic
networkMagic Mainnet = NetworkMagic 764824073
networkMagic (Testnet m) = m

nodeToClientVersions
  :: Map
      Consensus.NodeToClientVersion
      (Consensus.BlockNodeToClientVersion BlockVersion)
nodeToClientVersions = Consensus.supportedNodeToClientVersions (Proxy @BlockVersion)

tracers :: Network.NetworkClientSubcriptionTracers
tracers =
  Network.NetworkSubscriptionTracers
    { nsMuxTracer = muxTracer,
      nsHandshakeTracer = handshakeTracer,
      nsErrorPolicyTracer = errorPolicyTracer,
      nsSubscriptionTracer = subscriptionTracer
    }
  where
    muxTracer = contramapM (pure . ("[MuxTracer] " <>) . show) stdoutTracer
    handshakeTracer = contramapM (pure . ("[HandshakeTracer] " <>) . show) stdoutTracer
    errorPolicyTracer = contramapM (pure . ("[ErrorPolicyTracer] " <>) . show) stdoutTracer
    subscriptionTracer = contramapM (pure . ("[SubscriptionTracer] " <>) . show) stdoutTracer

subscriptionParams :: FilePath -> Network.ClientSubscriptionParams ()
subscriptionParams socketPath =
  Network.ClientSubscriptionParams
    { cspAddress = localAddressFromPath socketPath,
      cspConnectionAttemptDelay = Nothing,
      cspErrorPolicies =
        Network.networkErrorPolicies <> consensusErrorPolicy (Proxy @BlockVersion)
    }

protocols
  :: Consensus.NodeToClientVersion
  -> Consensus.BlockNodeToClientVersion BlockVersion
  -> NodeToClientProtocols () Void
protocols clientVersion blockVersion =
  Network.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol' blockVersion clientVersion,
      localTxSubmissionProtocol = localTxSubmissionProtocol' blockVersion clientVersion,
      localStateQueryProtocol = localStateQueryProtocol' blockVersion clientVersion,
      localTxMonitorProtocol = localTxMonitorProtocol' blockVersion clientVersion
    }

localChainSyncProtocol'
  :: Consensus.BlockNodeToClientVersion BlockVersion
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localChainSyncProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cChainSyncCodec (codecs blockVersion clientVersion)
    peer = mkChainSyncPeer

mkChainSyncPeer
  :: forall (header :: Type) (block :: Type) (tip :: Type) m a
   . (MonadIO m, Monad m)
  => Protocol.Peer
      (ChainSync header (Point block) (Tip tip))
      'Protocol.AsClient
      StIdle
      m
      a
mkChainSyncPeer = ChainSync.chainSyncClientPeer (ChainSync.ChainSyncClient mkChainSyncClient)

mkChainSyncClient
  :: (MonadIO m, Monad m)
  => m (ChainSync.ClientStIdle header (Point block) (Tip tip) m a)
mkChainSyncClient =
  pure $ ChainSync.SendMsgFindIntersect [genesisPoint] mkFindIntersectClient

mkFindIntersectClient
  :: (MonadIO m, Monad m)
  => ChainSync.ClientStIntersect header (Point block) (Tip tip) m a
mkFindIntersectClient =
  ChainSync.ClientStIntersect
    { recvMsgIntersectFound = const mkChainSyncClient',
      recvMsgIntersectNotFound = mkChainSyncClient'
    }

mkChainSyncClient'
  :: Monad m
  => Tip tip
  -> ChainSync.ChainSyncClient header (Point block) (Tip tip) m a
mkChainSyncClient' tip = ChainSync.ChainSyncClient (mkRequestNextClient tip)

mkRequestNextClient
  :: Monad m
  => Tip tip
  -> m (ChainSync.ClientStIdle header (Point block) (Tip tip) m a)
mkRequestNextClient _ = do
  pure $ ChainSync.SendMsgRequestNext (pure ()) mkClientStNext

mkClientStNext
  :: Monad m
  => ChainSync.ClientStNext header (Point block) (Tip tip) m a
mkClientStNext =
  ChainSync.ClientStNext
    { recvMsgRollForward = const mkChainSyncClient',
      recvMsgRollBackward = const mkChainSyncClient'
    }

localTxSubmissionProtocol'
  :: Consensus.BlockNodeToClientVersion BlockVersion
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localTxSubmissionProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cTxSubmissionCodec (codecs blockVersion clientVersion)
    peer = Network.localTxSubmissionPeerNull

localStateQueryProtocol'
  :: Consensus.BlockNodeToClientVersion BlockVersion
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localStateQueryProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cStateQueryCodec (codecs blockVersion clientVersion)
    peer = Network.localStateQueryPeerNull

localTxMonitorProtocol'
  :: Consensus.BlockNodeToClientVersion BlockVersion
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
  -> Protocol.Peer ps pr st IO ()
  -> RunMiniProtocolWithMinimalCtx () Void
mkInitiatorProtocolOnly codec peer =
  InitiatorProtocolOnly $
    Network.mkMiniProtocolCbFromPeer $
      const (tracer, codec, peer)
  where
    tracer = nullTracer

codecs
  :: Consensus.BlockNodeToClientVersion BlockVersion
  -> Consensus.NodeToClientVersion
  -> Client.ClientCodecs BlockVersion IO
codecs = Client.clientCodecs codecConfig
  where
    codecConfig = pClientInfoCodecConfig cfg
    cfg = protocolClientInfoCardano mainnetEpochSlots
