module Backend.IndividualEncounterParticipant.Model exposing (..)

import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessEncounterType)
import Backend.Entities exposing (..)
import Backend.NutritionEncounter.Model exposing (NutritionEncounterType)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias IndividualEncounterParticipant =
    { person : PersonId
    , encounterType : IndividualEncounterType
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , eddDate : Maybe NominalDate
    , dateConcluded : Maybe NominalDate
    , outcome : Maybe IndividualEncounterParticipantOutcome
    , deliveryLocation : Maybe DeliveryLocation
    , newborn : Maybe PersonId
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyIndividualEncounterParticipant : NominalDate -> PersonId -> IndividualEncounterType -> HealthCenterId -> IndividualEncounterParticipant
emptyIndividualEncounterParticipant currentDate personId type_ healthCenterId =
    IndividualEncounterParticipant personId type_ currentDate Nothing Nothing Nothing Nothing Nothing Nothing False (Just healthCenterId)


type IndividualParticipantExtraData
    = AcuteIllnessData AcuteIllnessEncounterType
    | AntenatalData PrenatalEncounterType
    | HealthyStartData PrenatalEncounterType
    | WellChildData WellChildEncounterType
    | NutritionData NutritionEncounterType
    | NoIndividualParticipantExtraData


type IndividualParticipantInitiator
    = InitiatorParticipantsPage
    | InitiatorPatientRecord PatientRecordInitiator PersonId


type alias Model =
    { updateIndividualEncounterParticipant : WebData ()
    }


type Msg
    = ClosePrenatalSession NominalDate PregnancyOutcome DeliveryLocation
    | CloseAcuteIllnessSession AcuteIllnessOutcome
    | CloseTuberculosisSession TuberculosisOutcome
    | CloseHIVSession HIVOutcome
    | SetEddDate NominalDate
    | SetNewborn PersonId
    | HandleUpdatedIndividualEncounterParticipant (WebData ())


emptyModel : Model
emptyModel =
    { updateIndividualEncounterParticipant = NotAsked
    }


type IndividualEncounterType
    = AcuteIllnessEncounter
    | AntenatalEncounter
    | HealthyStartEncounter
    | ChildScoreboardEncounter
    | HIVEncounter
    | HomeVisitEncounter
    | NCDEncounter
    | NutritionEncounter
    | TuberculosisEncounter
    | WellChildEncounter
      -- @todo : can be removed?
    | InmmunizationEncounter


type DeliveryLocation
    = FacilityDelivery
    | HomeDelivery


type IndividualEncounterParticipantOutcome
    = AcuteIllness AcuteIllnessOutcome
    | Pregnancy PregnancyOutcome
    | Tuberculosis TuberculosisOutcome
    | HIV HIVOutcome


type AcuteIllnessOutcome
    = OutcomeIllnessResolved
    | OutcomeLostToFollowUp
    | OutcomeMovedOutsideCA
    | OutcomePatientDied
    | OutcomeReferredToHC
    | OutcomeOther


allAcuteIllnessOutcome : List AcuteIllnessOutcome
allAcuteIllnessOutcome =
    [ OutcomeIllnessResolved
    , OutcomeLostToFollowUp
    , OutcomeMovedOutsideCA
    , OutcomePatientDied
    , OutcomeReferredToHC
    , OutcomeOther
    ]


type PregnancyOutcome
    = OutcomeLiveAtTerm
    | OutcomeLivePreTerm
    | OutcomeStillAtTerm
    | OutcomeStillPreTerm
    | OutcomeAbortions


allPregnancyOutcome : List PregnancyOutcome
allPregnancyOutcome =
    [ OutcomeLiveAtTerm
    , OutcomeLivePreTerm
    , OutcomeStillAtTerm
    , OutcomeStillPreTerm
    , OutcomeAbortions
    ]


type TuberculosisOutcome
    = TuberculosisOutcomeNotDiagnosed


type HIVOutcome
    = HIVOutcomeNotDiagnosed
