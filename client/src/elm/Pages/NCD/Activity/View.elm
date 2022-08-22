module Pages.NCD.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Utils exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.NCD.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( viewLabel
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    text "Pages.NCD.Activity.View"
