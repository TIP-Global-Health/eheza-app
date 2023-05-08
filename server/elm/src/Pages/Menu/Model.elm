module Pages.Menu.Model exposing (..)

import Utils.GeoLocation exposing (GeoLocationId)


type alias Model =
    { province : Maybe GeoLocationId
    , district : Maybe GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


emptyModel : Model
emptyModel =
    { province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
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
