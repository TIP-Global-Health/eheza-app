module Pages.Nutrition.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.Nutrition.Activity.Utils exposing (mandatoryActivitiesCompleted)
import Pages.Nutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Nutrition.Encounter.View exposing (allowEndingEncounter, partitionActivities)
import Pages.Nutrition.ProgressReport.Model exposing (Model, Msg(..))
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
    -> NutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features id isChw db model =
    let
        encounter =
            Dict.get id db.nutritionEncounters
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
            generateAssembledData id db
                |> RemoteData.toMaybe

        ( bottomActionData, mandatoryNutritionAssessmentMeasurementsTaken ) =
            Maybe.map2
                (\assembled ( _, child ) ->
                    let
                        ( _, pendingActivities ) =
                            partitionActivities currentDate zscores features isChw db assembled
                    in
                    ( Just <|
                        { showEndEncounterDialog = model.showEndEncounterDialog
                        , allowEndEncounter = allowEndingEncounter isChw pendingActivities
                        , closeEncounterMsg = CloseEncounter id
                        , setEndEncounterDialogStateMsg = SetEndEncounterDialogState
                        , startEncounterMsg = NoOp
                        }
                    , mandatoryActivitiesCompleted currentDate zscores features child isChw assembled db
                    )
                )
                assembledData
                (RemoteData.toMaybe childData)
                |> Maybe.withDefault ( Nothing, False )

        initiator =
            InitiatorNutritionIndividual id

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
