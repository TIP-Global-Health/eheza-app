module Pages.Scoreboard.Model exposing (..)

import AssocList
import Dict exposing (Dict)
import Json.Decode exposing (Value)


type alias Model =
    { form : ViewSelectionForm }


emptyModel : Model
emptyModel =
    { form = emptyViewSelectionForm }


type DisplayMode
    = DisplayViewSelection
    | DisplayResultTable ViewSelectionValue


type alias ViewSelectionForm =
    { province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
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
    { province : String
    , district : String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    }


type Msg
    = NoOp
