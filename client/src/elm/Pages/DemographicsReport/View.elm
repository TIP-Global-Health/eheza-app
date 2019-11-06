module Pages.DemographicsReport.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html Msg
view language currentDate personId db =
    div [] [ text "DemographicsReport page" ]
