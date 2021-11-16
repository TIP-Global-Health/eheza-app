module Pages.TraceContact.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (..)
import Translate exposing (Language, TranslationId, translate)


view : Language -> NominalDate -> AcuteIllnessTraceContactId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    text "Hello"
