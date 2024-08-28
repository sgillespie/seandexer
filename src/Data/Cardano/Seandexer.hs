{-# LANGUAGE QuantifiedConstraints #-}

module Data.Cardano.Seandexer
  ( SeandexerOpts (..),
    NetworkId (..),
    runSeandexer,
    mkTestnet,
  ) where

import Data.Cardano.Seandexer.AppT
import Data.Cardano.Seandexer.ChainSync

import Cardano.Chain.Genesis qualified as Byron
import Cardano.Crypto.Hash (Blake2b_256, Hash, hashWith)
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic (..))
import Data.Aeson qualified as Aeson
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
    proto <- mkProtocolInfo opts
    env <- mkAppEnv proto region

    runAppT env . void $
      subscribe (networkMagic soNetworkId) (SocketPath soSocketPath)

mkProtocolInfo :: SeandexerOpts -> IO (ProtocolInfo StandardBlock)
mkProtocolInfo SeandexerOpts{..} =
  protocolInfo
    <$> readByronGenesis
    <*> readGenesisHash soShelleyGenesis
    <*> decodeFileStrictIO soShelleyGenesis
    <*> decodeFileStrictIO soAlonzoGenesis
    <*> decodeFileStrictIO soConwayGenesis
  where
    readByronGenesis :: IO Byron.Config
    readByronGenesis = do
      (_, Byron.GenesisHash hash) <- liftEitherT (Byron.readGenesisData soByronGenesis)
      liftEitherT $
        Byron.mkConfigFromFile (requiresNetworkMagic soNetworkId) soByronGenesis hash

    readGenesisHash :: FilePath -> IO (Hash Blake2b_256 ByteString)
    readGenesisHash genesis =
      hashWith id <$> readFileBS genesis

    decodeFileStrictIO :: Aeson.FromJSON a => FilePath -> IO a
    decodeFileStrictIO genesis = do
      res <- Aeson.eitherDecodeFileStrict' genesis
      either fail pure res

    liftEitherT :: Show e => ExceptT e IO a -> IO a
    liftEitherT act = either (error . show) pure =<< runExceptT act

mkTestnet :: Word32 -> NetworkId
mkTestnet = Testnet . NetworkMagic

networkMagic :: NetworkId -> NetworkMagic
networkMagic Mainnet = NetworkMagic 764824073
networkMagic (Testnet m) = m

requiresNetworkMagic :: NetworkId -> RequiresNetworkMagic
requiresNetworkMagic Mainnet = RequiresNoMagic
requiresNetworkMagic _ = RequiresNoMagic
