module Pages.PregnancyOutcome.Model exposing (Model, Msg(..), PregnancyOutcome(..), emptyModel)

import Backend.Entities exposing (..)
import Date exposing (Date)
import Pages.Page exposing (Page)


type alias Model =
    { pregnancyConcluded : Maybe Date
    , isHomeDelivery : Maybe Bool
    , isDateSelectorOpen : Bool
    }


type Msg
    = SetActivePage Page


emptyModel : Model
emptyModel =
    { pregnancyConcluded = Nothing
    , isHomeDelivery = Nothing
    , isDateSelectorOpen = False
    }


type PregnancyOutcome
    = OutcomeLiveAtTerm
    | OutcomeLivePreTerm
    | OutcomeStillAtTerm
    | OutcomeStillPreTerm
    | OutcomeAbortions
