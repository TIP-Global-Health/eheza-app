module Pages.MessagingCenter.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { -- filter : Maybe CaseManagementFilter
      -- , dialogState : Maybe FollowUpEncounterDataType
    }


emptyModel : Model
emptyModel =
    { --  filter = Nothing
      -- , dialogState = Nothing
    }


type Msg
    = SetActivePage Page
