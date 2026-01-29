module Pages.ProgressReport.View exposing (view)

import Activity.Utils exposing (mandatoryActivitiesCompleted)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.ProgressReport.Model exposing (Model, Msg(..))
import Pages.WellChild.ProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChild.ProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> PersonId
    -> ( SessionId, EditableSession )
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features isChw childId ( sessionId, session ) db model =
    let
        childData =
            getChild childId session.offlineSession
                |> Maybe.map (\child -> Success ( childId, child ))
                |> Maybe.withDefault NotAsked

        initiator =
            InitiatorNutritionGroup sessionId childId

        mandatoryNutritionAssessmentMeasurementsTaken =
            mandatoryActivitiesCompleted currentDate zscores features session.offlineSession childId isChw db

        componentsConfig =
            Just { setReportComponentsMsg = SetReportComponents }

        bottomActionData =
            Just <|
                { showEndEncounterDialog = False
                , allowEndEncounter = False
                , closeEncounterMsg = NoOp
                , setEndEncounterDialogStateMsg = always NoOp
                , startEncounterMsg = NoOp
                }
    in
    viewWebData language
        (viewProgressReport
            language
            currentDate
            zscores
            site
            features
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            model.diagnosisMode
            model.reportToWhatsAppDialog
            model.reportTab
            SetActivePage
            SetReportTab
            SetDiagnosisMode
            MsgReportToWhatsAppDialog
            componentsConfig
            model.components
            bottomActionData
        )
        identity
        childData
