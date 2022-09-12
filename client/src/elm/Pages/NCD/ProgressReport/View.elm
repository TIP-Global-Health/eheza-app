module Pages.NCD.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.NCD.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    text "Pages.NCD.ProgressReport.View"
