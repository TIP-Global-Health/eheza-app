module Pages.StockManagement.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortEncounterTuplesDesc)
import Date exposing (Month, Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.StockManagement.Model exposing (..)
import Pages.Utils
    exposing
        ( taskCompleted
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSelectListInput
        )
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurseId nurse db model =
    let
        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ text <| translate language Translate.StockManagement ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    div [ class "page-activity stock-management" ]
        [ header
        , div [] [ text "Pages.StockManagement.View" ]
        ]
