module Backend.Completion.Model exposing (..)

import App.Types exposing (Site)
import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (Value)


type alias CompletionData =
    { site : Site
    , entityName : String
    , entityType : SelectedEntity
    }


type SelectedEntity
    = EntityGlobal
    | EntityHealthCenter


type Msg
    = SetData Value
