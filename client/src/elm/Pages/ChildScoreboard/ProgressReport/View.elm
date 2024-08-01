module Pages.ChildScoreboard.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.ChildScoreboardActivity.Utils exposing (allActivities)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.ChildScoreboard.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.ChildScoreboard.ProgressReport.Model exposing (Model, Msg(..))
import Pages.WellChild.ProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChild.ProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site, SiteFeature)
import Translate.Model exposing (Language)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> ChildScoreboardEncounterId
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features id db model =
    let
        isChw =
            True

        encounter =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.withDefault NotAsked

        participant =
            RemoteData.andThen
                (\encounter_ ->
                    Dict.get encounter_.participant db.individualParticipants
                        |> Maybe.withDefault NotAsked
                )
                encounter

        childData =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (\child_ -> ( participant_.person, child_ ))
                    )

        assembledData =
            generateAssembledData site id db
                |> RemoteData.toMaybe

        ( bottomActionData, mandatoryNutritionAssessmentMeasurementsTaken ) =
            Maybe.map2
                (\assembled _ ->
                    let
                        ( _, pendingActivities ) =
                            List.filter (expectActivity currentDate site assembled) allActivities
                                |> List.partition (activityCompleted currentDate site assembled db)
                    in
                    ( Just <|
                        { showEndEncounterDialog = model.showAIEncounterPopup
                        , allowEndEncounter = List.isEmpty pendingActivities
                        , closeEncounterMsg = CloseEncounter id
                        , setEndEncounterDialogStateMsg = always ShowAIEncounterPopup
                        , startEncounterMsg = NoOp
                        }
                    , True
                    )
                )
                assembledData
                (RemoteData.toMaybe childData)
                |> Maybe.withDefault ( Nothing, False )

        initiator =
            InitiatorChildScoreboard id

        componentsConfig =
            Just { setReportComponentsMsg = SetReportComponents }
    in
    viewWebData language
        (viewProgressReport language
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
