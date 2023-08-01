module Pages.ChildScoreboard.Report.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.ChildScoreboard.Report.Model exposing (Model, Msg)
import Translate exposing (Language, TranslationId, translate, translateText)
import Translate.Model exposing (Language(..))


view : Language -> NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    text "@todo:  Pages.ChildScoreboard.Report.View.view"
