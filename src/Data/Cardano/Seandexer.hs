{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer
  ( SeandexerOpts (..),
    runSeandexer,
  ) where

import Cardano.Chain.Epoch.File (mainnetEpochSlots)
import Cardano.Client.Subscription hiding (NodeToClientProtocols ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Control.Tracer (contramapM, nullTracer, stdoutTracer)
import Network.TypedProtocol.Codec (Codec ())
import Network.TypedProtocol.Core (ClientHasAgency (), Peer (), ServerHasAgency (..))
import Ouroboros.Consensus.Cardano (CardanoBlock ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Network.NodeToClient
  ( ClientCodecs (),
    Codecs' (..),
    clientCodecs,
  )
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy ())
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Network
import Ouroboros.Network.NodeToClient qualified as Network
import Ouroboros.Network.Snocket (localAddressFromPath)

newtype SeandexerOpts = SeandexerOpts
  {soSocketPath :: FilePath}
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
        networkMagic
        nodeToClientVersions
        tracers
        (subscriptionParams soSocketPath)
        protocols

localSnocket' :: Network.IOManager -> Network.LocalSnocket
localSnocket' = Network.localSnocket

networkMagic :: NetworkMagic
networkMagic = NetworkMagic 764_824_073 -- Mainnet

nodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion BlockVersion)
nodeToClientVersions = supportedNodeToClientVersions (Proxy @BlockVersion)

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
  :: NodeToClientVersion
  -> BlockNodeToClientVersion BlockVersion
  -> NodeToClientProtocols () Void
protocols clientVersion blockVersion =
  Network.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol' blockVersion clientVersion,
      localTxSubmissionProtocol = localTxSubmissionProtocol' blockVersion clientVersion,
      localStateQueryProtocol = localStateQueryProtocol' blockVersion clientVersion,
      localTxMonitorProtocol = localTxMonitorProtocol' blockVersion clientVersion
    }

localChainSyncProtocol'
  :: BlockNodeToClientVersion BlockVersion
  -> NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localChainSyncProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = cChainSyncCodec (codecs blockVersion clientVersion)
    peer = Network.chainSyncPeerNull

localTxSubmissionProtocol'
  :: BlockNodeToClientVersion BlockVersion
  -> NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localTxSubmissionProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = cTxSubmissionCodec (codecs blockVersion clientVersion)
    peer = Network.localTxSubmissionPeerNull

localStateQueryProtocol'
  :: BlockNodeToClientVersion BlockVersion
  -> NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localStateQueryProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = cStateQueryCodec (codecs blockVersion clientVersion)
    peer = Network.localStateQueryPeerNull

localTxMonitorProtocol'
  :: BlockNodeToClientVersion BlockVersion
  -> NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localTxMonitorProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = cTxMonitorCodec (codecs blockVersion clientVersion)
    peer = Network.localTxMonitorPeerNull

mkInitiatorProtocolOnly
  :: ( Show failure,
       forall (st' :: ps). Show (ClientHasAgency st'),
       forall (st' :: ps). Show (ServerHasAgency st'),
       ShowProxy ps
     )
  => Codec ps failure IO LByteString
  -> Peer ps pr st IO ()
  -> RunMiniProtocolWithMinimalCtx () Void
mkInitiatorProtocolOnly codec peer =
  InitiatorProtocolOnly $
    Network.mkMiniProtocolCbFromPeer $
      const (tracer, codec, peer)
  where
    tracer = nullTracer

codecs
  :: BlockNodeToClientVersion BlockVersion
  -> NodeToClientVersion
  -> ClientCodecs BlockVersion IO
codecs = clientCodecs codecConfig
  where
    codecConfig = pClientInfoCodecConfig cfg
    cfg = protocolClientInfoCardano mainnetEpochSlots
