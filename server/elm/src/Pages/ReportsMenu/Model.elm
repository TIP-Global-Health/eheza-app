module Pages.ReportsMenu.Model exposing (Model, Msg(..), emptyModel)

import Backend.Components.Model exposing (HealthCenterId)
import Pages.Components.Model exposing (DemographicsSelection, emptyDemographicsSelection)
import Pages.Components.Types exposing (PopulationSelectionOption)


type alias Model =
    { populationSelection : Maybe PopulationSelectionOption
    , selectedDemographics : DemographicsSelection
    , selectedHealthCenter : Maybe HealthCenterId
    , selected : Bool
    }


emptyModel : Model
emptyModel =
    { populationSelection = Nothing
    , selectedDemographics = emptyDemographicsSelection
    , selectedHealthCenter = Nothing
    , selected = False
    }


type Msg
    = SetPopulationSelection String
    | SetGeoLocation (String -> DemographicsSelection -> DemographicsSelection) String
    | SetHealthCenter String
    | SelectionMade
