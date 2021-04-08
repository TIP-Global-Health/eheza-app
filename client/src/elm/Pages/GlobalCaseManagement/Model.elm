module Pages.GlobalCaseManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Measurement.Model exposing (FollowUpValue, NutritionAssesment)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { encounterTypeFilter : Maybe IndividualEncounterType
    , dialogState : Maybe FollowUpEncounterData
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
    | OverDue


type alias FollowUpItem =
    { dateMeasured : NominalDate
    , value : FollowUpValue
    }


type alias FollowUpEncounterData =
    { encounterType : IndividualEncounterType
    , personId : PersonId
    }


type Msg
    = SetActivePage Page
    | SetEncounterTypeFilter (Maybe IndividualEncounterType)
    | SetDialogState (Maybe FollowUpEncounterData)
    | StartFollowUpEncounter FollowUpEncounterData
