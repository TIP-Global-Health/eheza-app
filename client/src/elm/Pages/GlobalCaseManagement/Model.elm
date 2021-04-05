module Pages.GlobalCaseManagement.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (HealthCenterId, VillageId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Pages.Page exposing (Page)


type alias Model =
    { encounterType : Maybe IndividualEncounterType
    }


emptyModel : Model
emptyModel =
    { encounterType = Nothing
    }


type Msg
    = SetEncounterType (Maybe IndividualEncounterType)
    | SetActivePage Page
