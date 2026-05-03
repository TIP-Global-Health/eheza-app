module Pages.ScoreboardMenu.Model exposing (Model, Msg(..), emptyModel)

import Pages.Components.Model exposing (DemographicsSelection, emptyDemographicsSelection)


type alias Model =
    { selectedDemographics : DemographicsSelection
    , selected : Bool
    }


emptyModel : Model
emptyModel =
    { selectedDemographics = emptyDemographicsSelection
    , selected = False
    }


type Msg
    = SetGeoLocation (String -> DemographicsSelection -> DemographicsSelection) String
    | SelectionMade
