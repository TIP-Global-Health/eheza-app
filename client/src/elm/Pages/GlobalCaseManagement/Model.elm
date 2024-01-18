module Pages.GlobalCaseManagement.Model exposing (..)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FollowUpOption, FollowUpValue, PrenatalFollowUpValue)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { filter : Maybe CaseManagementFilter
    , dialogState : Maybe FollowUpEncounterDataType
    }


emptyModel : Model
emptyModel =
    { filter = Nothing
    , dialogState = Nothing
    }


type CaseManagementFilter
    = FilterAcuteIllness
    | FilterAntenatal
    | FilterNutrition
    | FilterContactsTrace
    | FilterPrenatalLabs
    | FilterNCDLabs
    | FilterImmunization


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


type alias ImmunizationFollowUpItem =
    { dateMeasured : NominalDate
    , dueDate : NominalDate
    , personName : String
    }


type alias ImmunizationFollowUpEntry =
    { personId : PersonId
    , item : ImmunizationFollowUpItem
    }


type FollowUpEncounterDataType
    = FollowUpNutrition FollowUpNutritionData
    | FollowUpAcuteIllness FollowUpAcuteIllnessData
    | FollowUpPrenatal FollowUpPrenatalData
    | FollowUpImmunization FollowUpImmunizationData
    | CaseManagementContactsTracing


type alias FollowUpNutritionData =
    { personId : PersonId
    , personName : String
    }


type alias FollowUpImmunizationData =
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


type alias ContactsTracingEntryData =
    { itemId : AcuteIllnessTraceContactId
    , personName : String
    , phoneNumber : String
    , reporterName : String
    , lastFollowUpDate : Maybe NominalDate
    }


type alias PrenatalLabsEntryData =
    { personId : PersonId
    , personName : String
    , encounterId : PrenatalEncounterId
    , state : LabsEntryState
    , label : String
    }


type LabsEntryState
    = LabsEntryPending
    | LabsEntryClosingSoon
    | LabsEntryReadyForReview


type alias NCDLabsEntryData =
    { personId : PersonId
    , personName : String
    , encounterId : NCDEncounterId
    , state : LabsEntryState
    , label : String
    }


type alias FollowUpPatients =
    { nutrition : List PersonId
    , acuteIllness : List PersonId
    , prenatal : List PersonId
    , immunization : List PersonId
    }


type Msg
    = SetActivePage Page
    | SetFilter (Maybe CaseManagementFilter)
    | SetDialogState (Maybe FollowUpEncounterDataType)
    | StartFollowUpEncounter FollowUpEncounterDataType
    | StartPrenatalFollowUpEncounter IndividualEncounterParticipantId Bool PrenatalEncounterType
