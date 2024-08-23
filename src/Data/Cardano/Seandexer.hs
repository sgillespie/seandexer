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
import Data.Text.IO qualified as Text
import Network.TypedProtocol.Codec (Codec ())
import Network.TypedProtocol.Core qualified as Protocol
import Ouroboros.Consensus.Block (withOrigin)
import Ouroboros.Consensus.Cardano (CardanoBlock ())
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Network.NodeToClient qualified as Client
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as Consensus
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolClientInfo (..))
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Util (ShowProxy ())
import Ouroboros.Network.Block qualified as Block
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux qualified as Network
import Ouroboros.Network.NodeToClient qualified as Network
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..))
import Ouroboros.Network.Snocket (localAddressFromPath)
import System.Exit (ExitCode)

data SeandexerOpts = SeandexerOpts
  { soSocketPath :: FilePath,
    soNetworkId :: NetworkId
  }
  deriving stock (Eq, Show)

type StandardBlock = CardanoBlock StandardCrypto

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
      (Consensus.BlockNodeToClientVersion StandardBlock)
nodeToClientVersions = Consensus.supportedNodeToClientVersions (Proxy @StandardBlock)

tracers :: Network.NetworkClientSubcriptionTracers
tracers =
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
  :: Consensus.NodeToClientVersion
  -> Consensus.BlockNodeToClientVersion StandardBlock
  -> NodeToClientProtocols () Void
protocols clientVersion blockVersion =
  Network.NodeToClientProtocols
    { localChainSyncProtocol = localChainSyncProtocol' blockVersion clientVersion,
      localTxSubmissionProtocol = localTxSubmissionProtocol' blockVersion clientVersion,
      localStateQueryProtocol = localStateQueryProtocol' blockVersion clientVersion,
      localTxMonitorProtocol = localTxMonitorProtocol' blockVersion clientVersion
    }

localChainSyncProtocol'
  :: Consensus.BlockNodeToClientVersion StandardBlock
  -> Consensus.NodeToClientVersion
  -> RunMiniProtocolWithMinimalCtx () Void
localChainSyncProtocol' blockVersion clientVersion =
  mkInitiatorProtocolOnly codec peer
  where
    codec = Client.cChainSyncCodec (codecs blockVersion clientVersion)
    peer = mkChainSyncPeer

mkChainSyncPeer
  :: (MonadIO m, Monad m)
  => Protocol.Peer
      (ChainSync StandardBlock (Block.Point StandardBlock) (Block.Tip StandardBlock))
      'Protocol.AsClient
      StIdle
      m
      ()
mkChainSyncPeer = ChainSync.chainSyncClientPeer (ChainSync.ChainSyncClient mkChainSyncClient)

mkChainSyncClient
  :: (MonadIO m, Monad m)
  => m
      ( ChainSync.ClientStIdle
          StandardBlock
          (Block.Point StandardBlock)
          (Block.Tip StandardBlock)
          m
          ()
      )
mkChainSyncClient =
  pure $ ChainSync.SendMsgFindIntersect [Block.genesisPoint] mkFindIntersectClient

mkFindIntersectClient
  :: (MonadIO m, Monad m)
  => ChainSync.ClientStIntersect
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkFindIntersectClient =
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

      liftIO . Text.putStrLn $
        case clientPoint of
          Just point' ->
            "Found intersection at point: " <> show point' <> ", tip: " <> show tip'
          Nothing ->
            "No intersection found at tip: " <> show tip'

      pure $ ChainSync.SendMsgRequestNext (pure ()) mkClientStNext

mkClientStNext
  :: MonadIO m
  => ChainSync.ClientStNext
      StandardBlock
      (Block.Point StandardBlock)
      (Block.Tip StandardBlock)
      m
      ()
mkClientStNext =
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
          let blockNo' = getBlockNo block

          if blockNo' >= tip'
            then do
              liftIO . Text.putStrLn $ "Reached final block: " <> show blockNo'
              exitSuccess
            else
              pure $ ChainSync.SendMsgRequestNext (pure ()) mkClientStNext
        Right point -> do
          reportRollback point
          pure $ ChainSync.SendMsgRequestNext (pure ()) mkClientStNext
      where
        tip' = withOrigin 0 Block.unBlockNo (Block.getTipBlockNo serverTip)

        getBlockNo block =
          case Block.getHeaderFields block of
            Block.HeaderFields _ blockNo' _ -> Block.unBlockNo blockNo'

        reportRollback point =
          liftIO . Text.putStrLn $
            "Rolling backward at point: " <> show point <> ", tip: " <> show tip'

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
  InitiatorProtocolOnly $
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
    cfg = protocolClientInfoCardano mainnetEpochSlots
