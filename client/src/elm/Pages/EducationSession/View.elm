module Pages.EducationSession.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.EducationSession.Model exposing (..)
import Pages.Utils exposing (viewEncounterActionButton)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> EducationSessionId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    text "@todo Pages.EducationSession.View.view"
