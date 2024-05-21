module Pages.ScoreboardMenu.Model exposing (..)

import Utils.GeoLocation exposing (GeoLocationId)


type alias Model =
    { province : Maybe GeoLocationId
    , district : Maybe GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    , selected : Bool
    }


emptyModel : Model
emptyModel =
    { province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , selected = False
    }


type alias ViewSelectionValue =
    { province : GeoLocationId
    , district : GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


type Msg
    = SetGeoLocation (String -> Model -> Model) String
    | SelectionMade
