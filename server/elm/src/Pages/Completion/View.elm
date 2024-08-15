module Pages.Completion.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList as Dict exposing (Dict)
import Backend.Completion.Model exposing (CompletionData)
import Backend.Model exposing (ModelBackend)
import Date exposing (Interval(..), Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, customFormatDDMMYYYY, formatDDMMYYYY, sortByDateDesc)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Completion.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Round
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> String -> ModelBackend -> Model -> Html Msg
view language currentDate themePath modelBackend model =
    case modelBackend.completionData of
        Just (Ok data) ->
            viewCompletionData language currentDate themePath data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewCompletionData : Language -> NominalDate -> String -> CompletionData -> Model -> Html Msg
viewCompletionData language currentDate themePath data model =
    text "viewCompletionData"
