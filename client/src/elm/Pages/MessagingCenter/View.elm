module Pages.MessagingCenter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuplesDesc)
import Date exposing (Month, Unit(..), isBetween, numberToMonth)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurseId nurse db model =
    text "Pages.MessagingCenter.View"
