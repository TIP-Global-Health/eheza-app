module Backend.IndividualEncounterParticipant.Model exposing (..)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounterType)
import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
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
    | NoIndividualParticipantExtraData


type alias Model =
    { closePrenatalSession : WebData ()
    , closeAcuteIllnessSession : WebData ()
    , setEddDate : WebData ()
    , setNewborn : WebData ()
    }


type Msg
    = ClosePrenatalSession Date PregnancyOutcome DeliveryLocation
    | HandleClosedPrenatalSession (WebData ())
    | CloseAcuteIllnessSession AcuteIllnessOutcome
    | HandleClosedAcuteIllnessSession (WebData ())
    | SetEddDate NominalDate
    | HandleSetEddDate (WebData ())
    | SetNewborn PersonId
    | HandleSetNewborn (WebData ())


emptyModel : Model
emptyModel =
    { closePrenatalSession = NotAsked
    , closeAcuteIllnessSession = NotAsked
    , setEddDate = NotAsked
    , setNewborn = NotAsked
    }


type IndividualEncounterType
    = AcuteIllnessEncounter
    | AntenatalEncounter
    | HomeVisitEncounter
    | InmmunizationEncounter
    | NutritionEncounter
    | WellChildEncounter


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
