module Backend.IndividualEncounterParticipant.Model exposing (..)

import Backend.Entities exposing (..)
import Date exposing (Date)
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
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


type alias Model =
    { closePrenatalSession : WebData ()
    , closeAcuteIllnessSession : WebData ()
    , setEddDate : WebData ()
    }


type Msg
    = ClosePrenatalSession Date PregnancyOutcome Bool
    | HandleClosedPrenatalSession (WebData ())
    | CloseAcuteIllnessSession AcuteIllnessOutcome
    | HandleClosedAcuteIllnessSession (WebData ())
    | SetEddDate NominalDate
    | HandleSetEddDate (WebData ())


emptyModel : Model
emptyModel =
    { closePrenatalSession = NotAsked
    , closeAcuteIllnessSession = NotAsked
    , setEddDate = NotAsked
    }


type IndividualEncounterType
    = AcuteIllnessEncounter
    | AntenatalEncounter
    | HomeVisitEncounter
    | InmmunizationEncounter
    | NutritionEncounter


type DeliveryLocation
    = FacilityDelivery
    | HomeDelivery


type IndividualEncounterParticipantOutcome
    = AcuteIllness AcuteIllnessOutcome
    | Pregnancy PregnancyOutcome


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
