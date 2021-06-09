module Pages.GlobalCaseManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Measurement.Model exposing (FollowUpOption(..), FollowUpValue, NutritionAssesment, PrenatalFollowUpValue)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { encounterTypeFilter : Maybe IndividualEncounterType
    , dialogState : Maybe FollowUpEncounterDataType
    }


emptyModel : Model
emptyModel =
    { encounterTypeFilter = Nothing
    , dialogState = Nothing
    }


type FollowUpDueOption
    = DueToday
    | DueThisWeek
    | DueThisMonth
    | DueNextMonth
    | OverDue


type alias NutritionFollowUpItem =
    { dateMeasured : NominalDate
    , personName : String
    , value : FollowUpValue
    }


type alias NutritionFollowUpEntry =
    { personId : PersonId
    , item : NutritionFollowUpItem
    }


type alias AcuteIllnessFollowUpItem =
    { dateMeasured : NominalDate
    , personName : String
    , encounterId : Maybe AcuteIllnessEncounterId

    -- Since there may be multiple encounters during same day,
    -- we need to store sequence number, to be able to order
    -- follow ups correctly.
    , encounterSequenceNumber : Int
    , value : EverySet FollowUpOption
    }


type alias AcuteIllnessFollowUpEntry =
    { participantId : IndividualEncounterParticipantId
    , personId : PersonId
    , item : AcuteIllnessFollowUpItem
    , newEncounterSequenceNumber : Int
    , diagnosis : AcuteIllnessDiagnosis
    }


type alias PrenatalFollowUpItem =
    { dateMeasured : NominalDate
    , personName : String
    , encounterId : Maybe PrenatalEncounterId
    , value : PrenatalFollowUpValue
    }


type alias PrenatalFollowUpEntry =
    { participantId : IndividualEncounterParticipantId
    , personId : PersonId
    , item : PrenatalFollowUpItem
    , encounterType : PrenatalEncounterType
    , hasNurseEncounter : Bool
    }


type FollowUpEncounterDataType
    = FollowUpNutrition FollowUpNutritionData
    | FollowUpAcuteIllness FollowUpAcuteIllnessData
    | FollowUpPrenatal FollowUpPrenatalData


type alias FollowUpNutritionData =
    { personId : PersonId
    , personName : String
    }


type alias FollowUpAcuteIllnessData =
    { personId : PersonId
    , personName : String
    , participantId : IndividualEncounterParticipantId
    , sequenceNumber : Int
    }


type alias FollowUpPrenatalData =
    { personId : PersonId
    , personName : String
    , participantId : IndividualEncounterParticipantId
    , encounterType : PrenatalEncounterType
    , hasNurseEncounter : Bool
    , dateMeasured : NominalDate
    }


type Msg
    = SetActivePage Page
    | SetEncounterTypeFilter (Maybe IndividualEncounterType)
    | SetDialogState (Maybe FollowUpEncounterDataType)
    | StartFollowUpEncounter FollowUpEncounterDataType
    | StartPrenatalFollowUpEncounter IndividualEncounterParticipantId Bool PrenatalEncounterType
