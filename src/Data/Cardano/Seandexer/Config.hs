module Data.Cardano.Seandexer.Config
  ( ByronConfig (),
    ShelleyGenesisHash (),
    ByronGenesisFile (),
    ShelleyGenesisFile (),
    AlonzoGenesisFile (),
    ConwayGenesisFile (),
    mkProtocolInfo,
    protocolInfo,
  ) where

import Data.Cardano.Seandexer.AppT (StandardBlock ())

import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.Update qualified as Byron
import Cardano.Crypto.Hash (Blake2b_256 (), Hash (), hashWith)
import Cardano.Crypto.Hash.Class (castHash)
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis ())
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.BaseTypes (Nonce (..), ProtVer (..), natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis ())
import Data.Aeson qualified as Aeson
import Ouroboros.Consensus.Cardano qualified as Consensus
import Ouroboros.Consensus.Cardano.Node qualified as Consensus
import Ouroboros.Consensus.Config (emptyCheckpointsMap)
import Ouroboros.Consensus.Mempool (mkOverrides, noOverridesMeasure)
import Ouroboros.Consensus.Node (ProtocolInfo)
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

type ByronConfig = Byron.Config

type ShelleyGenesisHash = Hash Blake2b_256 ByteString

type ByronGenesisFile = FilePath
type ShelleyGenesisFile = FilePath
type AlonzoGenesisFile = FilePath
type ConwayGenesisFile = FilePath

mkProtocolInfo
  :: RequiresNetworkMagic
  -> ByronGenesisFile
  -> ShelleyGenesisFile
  -> AlonzoGenesisFile
  -> ConwayGenesisFile
  -> IO (ProtocolInfo StandardBlock)
mkProtocolInfo requiresMagic byronGenesis shelleyGenesis alonzoGenesis conwayGenesis =
  protocolInfo
    <$> readByronGenesis
    <*> readGenesisHash shelleyGenesis
    <*> decodeFileStrictIO shelleyGenesis
    <*> decodeFileStrictIO alonzoGenesis
    <*> decodeFileStrictIO conwayGenesis
  where
    readByronGenesis :: IO Byron.Config
    readByronGenesis = do
      (_, Byron.GenesisHash hash) <- liftEitherT (Byron.readGenesisData byronGenesis)
      liftEitherT $
        Byron.mkConfigFromFile requiresMagic byronGenesis hash

    readGenesisHash :: FilePath -> IO (Hash Blake2b_256 ByteString)
    readGenesisHash genesis =
      hashWith id <$> readFileBS genesis

    decodeFileStrictIO :: Aeson.FromJSON a => FilePath -> IO a
    decodeFileStrictIO genesis = do
      res <- Aeson.eitherDecodeFileStrict' genesis
      either fail pure res

    liftEitherT :: Show e => ExceptT e IO a -> IO a
    liftEitherT act = either (error . show) pure =<< runExceptT act

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
