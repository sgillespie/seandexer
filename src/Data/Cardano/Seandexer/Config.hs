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

import Data.Cardano.Seandexer.AppT (LedgerEra (..), StandardBlock ())

import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.Update qualified as Byron
import Cardano.Crypto.Hash (Blake2b_256 (), Hash (), hashWith)
import Cardano.Crypto.Hash.Class (castHash)
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis ())
import Cardano.Ledger.Api.Transition (mkLatestTransitionConfig)
import Cardano.Ledger.BaseTypes (EpochNo (..), Nonce (..), ProtVer (..), natVersion)
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
  -> LedgerEra
  -> ByronGenesisFile
  -> ShelleyGenesisFile
  -> AlonzoGenesisFile
  -> ConwayGenesisFile
  -> IO (ProtocolInfo StandardBlock)
mkProtocolInfo requiresMagic startEra byronGenesis shelleyGenesis alonzoGenesis conwayGenesis =
  protocolInfo startEra
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
  :: LedgerEra
  -> ByronConfig
  -> ShelleyGenesisHash
  -> ShelleyGenesis StandardCrypto
  -> AlonzoGenesis
  -> ConwayGenesis StandardCrypto
  -> ProtocolInfo StandardBlock
protocolInfo startEra byronCfg shelleyGenesisHash shelleyGenesis alonzoGenesis conwayGenesis =
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
              { triggerHardForkShelley = triggerHardFork Shelley startEra,
                triggerHardForkAllegra = triggerHardFork Allegra startEra,
                triggerHardForkMary = triggerHardFork Mary startEra,
                triggerHardForkAlonzo = triggerHardFork Alonzo startEra,
                triggerHardForkBabbage = triggerHardFork Babbage startEra,
                triggerHardForkConway = triggerHardFork Conway startEra
              },
          checkpoints = emptyCheckpointsMap
        }

triggerHardFork :: LedgerEra -> LedgerEra -> Consensus.TriggerHardFork
triggerHardFork hfEra start
  | hfEra <= start = Consensus.TriggerHardForkAtEpoch (EpochNo 0)
  | otherwise = triggerHardFork' hfEra
  where
    triggerHardFork' Byron = error "Should not happen!"
    triggerHardFork' Shelley = Consensus.TriggerHardForkAtVersion 2
    triggerHardFork' Allegra = Consensus.TriggerHardForkAtVersion 3
    triggerHardFork' Mary = Consensus.TriggerHardForkAtVersion 4
    triggerHardFork' Alonzo = Consensus.TriggerHardForkAtVersion 5
    triggerHardFork' Babbage = Consensus.TriggerHardForkAtVersion 7
    triggerHardFork' Conway = Consensus.TriggerHardForkAtVersion 9
