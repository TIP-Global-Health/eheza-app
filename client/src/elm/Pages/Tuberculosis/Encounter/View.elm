module Pages.Tuberculosis.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (NCDASign(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Encounter.Model exposing (..)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> TuberculosisEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site id db model =
    text "Pages.Tuberculosis.Encounter.View.view"
