module Pages.Scoreboard.Model exposing (..)

import AssocList
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Utils.GeoLocation exposing (GeoLocationId)


type alias Model =
    { form : ViewSelectionForm }


emptyModel : Model
emptyModel =
    { form = emptyViewSelectionForm }


type DisplayMode
    = DisplayViewSelection
    | DisplayResultTable ViewSelectionValue


type alias ViewSelectionForm =
    { province : Maybe GeoLocationId
    , district : Maybe GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


emptyViewSelectionForm : ViewSelectionForm
emptyViewSelectionForm =
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
    = SetGeoLocation (String -> ViewSelectionForm -> ViewSelectionForm) String
