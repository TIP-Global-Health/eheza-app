module Pages.CompletionMenu.Model exposing (Model, Msg(..), emptyModel)

import Backend.Components.Model exposing (HealthCenterId)
import Pages.Components.Types exposing (PopulationSelectionOption)


type alias Model =
    { populationSelection : Maybe PopulationSelectionOption
    , selectedHealthCenter : Maybe HealthCenterId
    , selected : Bool
    }


emptyModel : Model
emptyModel =
    { populationSelection = Nothing
    , selectedHealthCenter = Nothing
    , selected = False
    }


type Msg
    = SetPopulationSelection String
    | SetHealthCenter String
    | SelectionMade
