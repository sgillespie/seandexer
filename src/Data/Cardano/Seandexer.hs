{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer
  ( SeandexerOpts (..),
    NetworkId (..),
    runSeandexer,
    mkTestnet,
  ) where

import Data.Cardano.Seandexer.AppT
import Data.Cardano.Seandexer.ChainSync
import Data.Cardano.Seandexer.Config (mkProtocolInfo)

import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import Ouroboros.Consensus.Node (ProtocolInfo ())
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import System.Console.Regions qualified as Console

data SeandexerOpts = SeandexerOpts
  { soSocketPath :: FilePath,
    soNetworkId :: NetworkId,
    soByronGenesis :: FilePath,
    soShelleyGenesis :: FilePath,
    soAlonzoGenesis :: FilePath,
    soConwayGenesis :: FilePath
  }
  deriving stock (Eq, Show)

data NetworkId
  = Mainnet
  | Testnet NetworkMagic
  deriving stock (Eq, Show)

runSeandexer :: SeandexerOpts -> IO ()
runSeandexer opts@SeandexerOpts{..} = Console.displayConsoleRegions $ do
  Console.withConsoleRegion Console.Linear $ \region -> do
    protoInfo <- protoInfoFromOpts opts
    env <- mkAppEnv protoInfo region

    runAppT env . void $
      subscribe (networkMagic soNetworkId) (SocketPath soSocketPath)

protoInfoFromOpts :: SeandexerOpts -> IO (ProtocolInfo StandardBlock)
protoInfoFromOpts SeandexerOpts{..} =
  mkProtocolInfo
    (requiresNetworkMagic soNetworkId)
    soByronGenesis
    soShelleyGenesis
    soAlonzoGenesis
    soConwayGenesis

mkTestnet :: Word32 -> NetworkId
mkTestnet = Testnet . NetworkMagic

networkMagic :: NetworkId -> NetworkMagic
networkMagic Mainnet = NetworkMagic 764824073
networkMagic (Testnet m) = m

requiresNetworkMagic :: NetworkId -> RequiresNetworkMagic
requiresNetworkMagic Mainnet = RequiresNoMagic
requiresNetworkMagic _ = RequiresNoMagic
