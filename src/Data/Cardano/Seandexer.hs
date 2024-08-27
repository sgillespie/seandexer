{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer
  ( SeandexerOpts (..),
    NetworkId (..),
    runSeandexer,
    mkTestnet,
  ) where

import Data.Cardano.Seandexer.AppT (mkAppEnv, runAppT)
import Data.Cardano.Seandexer.ChainSync (SocketPath (..), subscribe)

import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import System.Console.Regions qualified as Console

data SeandexerOpts = SeandexerOpts
  { soSocketPath :: FilePath,
    soNetworkId :: NetworkId
  }
  deriving stock (Eq, Show)

data NetworkId
  = Mainnet
  | Testnet NetworkMagic
  deriving stock (Eq, Show)

runSeandexer :: SeandexerOpts -> IO ()
runSeandexer SeandexerOpts{..} = Console.displayConsoleRegions $ do
  Console.withConsoleRegion Console.Linear $ \region -> do
    env <- mkAppEnv region

    runAppT env . void $
      subscribe (networkMagic soNetworkId) (SocketPath soSocketPath)

mkTestnet :: Word32 -> NetworkId
mkTestnet = Testnet . NetworkMagic

networkMagic :: NetworkId -> NetworkMagic
networkMagic Mainnet = NetworkMagic 764824073
networkMagic (Testnet m) = m
