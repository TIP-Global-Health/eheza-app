module Pages.PrenatalActivity.Model exposing
    ( LmpRange(..)
    , Model
    , Msg(..)
    , PregnancyDatingForm
    , decodeLmpRange
    , emptyModel
    , encodeLmpRange
    )

import Backend.Entities exposing (..)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { pregnancyDatingForm : PregnancyDatingForm
    }


emptyModel : Model
emptyModel =
    { pregnancyDatingForm = emptyPregnancyDatingForm
    }


type alias PregnancyDatingForm =
    { lmpRange : Maybe LmpRange
    , lmpDate : Maybe Date
    , lmpDateConfident : Maybe Bool
    , isDateSelectorOpen : Bool
    }


emptyPregnancyDatingForm : PregnancyDatingForm
emptyPregnancyDatingForm =
    PregnancyDatingForm Nothing Nothing Nothing False


type LmpRange
    = OneMonth
    | ThreeMonth
    | SixMonth


encodeLmpRange : LmpRange -> String
encodeLmpRange range =
    case range of
        OneMonth ->
            "one-month"

        ThreeMonth ->
            "three-month"

        SixMonth ->
            "six-month"


decodeLmpRange : String -> Maybe LmpRange
decodeLmpRange s =
    case s of
        "one-month" ->
            Just OneMonth

        "three-month" ->
            Just ThreeMonth

        "six-month" ->
            Just SixMonth

        _ ->
            Nothing


type Msg
    = SetActivePage Page
      -- PregnancyDatingMsgs
    | ToggleDateSelector
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpRange String
