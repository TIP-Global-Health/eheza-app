module Pages.ProgressReport.View exposing (view)

import Activity.Utils exposing (mandatoryActivitiesCompleted)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.ProgressReport.Model exposing (..)
import Pages.WellChildProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChildProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> Bool -> PersonId -> ( SessionId, EditableSession ) -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores isChw childId ( sessionId, session ) db model =
    let
        childData =
            getChild childId session.offlineSession
                |> Maybe.map (\child -> Success ( childId, child ))
                |> Maybe.withDefault NotAsked

        initiator =
            InitiatorNutritionGroup sessionId childId

        mandatoryNutritionAssessmentMeasurementsTaken =
            mandatoryActivitiesCompleted currentDate zscores session.offlineSession childId isChw db
    in
    viewWebData language
        (viewProgressReport
            language
            currentDate
            zscores
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            model.diagnosisMode
            SetActivePage
            SetDiagnosisMode
            Nothing
        )
        identity
        childData
