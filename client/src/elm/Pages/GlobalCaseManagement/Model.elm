module Pages.GlobalCaseManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (HealthCenterId, VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Measurement.Model exposing (FollowUpValue, NutritionAssesment)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { encounterTypeFilter : Maybe IndividualEncounterType
    }


emptyModel : Model
emptyModel =
    { encounterTypeFilter = Nothing
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


type Msg
    = SetEncounterTypeFilter (Maybe IndividualEncounterType)
    | SetActivePage Page
