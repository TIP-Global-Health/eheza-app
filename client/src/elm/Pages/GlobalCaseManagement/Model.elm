module Pages.GlobalCaseManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (HealthCenterId, VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Pages.Page exposing (Page)


type alias Model =
    { encounterTypeFilter : Maybe IndividualEncounterType
    }


emptyModel : Model
emptyModel =
    { encounterTypeFilter = Nothing
    }


type alias FollowUpCase =
    { name : String
    , dueOption : FollowUpDueOption
    , assessment : String
    }


type FollowUpDueOption
    = DueToday
    | DueThisWeek
    | DueThisMonth
    | OverDue


type Msg
    = SetEncounterTypeFilter (Maybe IndividualEncounterType)
    | SetActivePage Page
