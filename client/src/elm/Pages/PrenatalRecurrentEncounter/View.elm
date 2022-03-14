module Pages.PrenatalRecurrentEncounter.View exposing (view)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (ObstetricHistoryValue, PrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateHighRiskAlertData
        , generateHighSeverityAlertData
        , getActivityIcon
        , getRecurrentActivityIcon
        )
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounter
        , PrenatalEncounterType(..)
        , PrenatalProgressReportInitiator(..)
        , RecordPreganancyInitiator(..)
        )
import Date exposing (Interval(..))
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (generateAssembledData)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.RecurrentActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.PrenatalRecurrentEncounter.Model exposing (..)
import Pages.PrenatalRecurrentEncounter.Utils exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id model) identity data


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id model data =
    let
        header =
            viewHeader language

        content =
            viewContent language currentDate data model
    in
    div [ class "page-encounter prenatal" ]
        [ header
        , content
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter False ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AssembledData -> Model -> Html Msg
viewContent language currentDate data model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False data (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate data model


viewMainPageContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate data model =
    let
        ( completedActivities, pendingActivities ) =
            List.filter (expectActivity currentDate data) allActivities
                |> List.partition (activityCompleted currentDate data)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.Reports

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
                ]

        viewCard activity =
            let
                ( label, icon ) =
                    ( Translate.PrenatalRecurrentActivitiesTitle activity, getRecurrentActivityIcon activity )
            in
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <|
                        SetActivePage <|
                            UserPage <|
                                PrenatalRecurrentActivityPage data.id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ icon ] [] ]
                , div [ class "content" ]
                    [ p [] [ text <| String.toUpper <| translate language label ] ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        viewReportLink labelTransId redirectPage =
            div
                [ class "report-wrapper"
                , onClick <| SetActivePage redirectPage
                ]
                [ div [ class "icon-progress-report" ] []
                , div [ class "report-text" ]
                    [ div [ class "report-label" ] [ text <| translate language labelTransId ]
                    , div [ class "report-link" ] [ text <| translate language Translate.View ]
                    ]
                ]

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink Translate.ClinicalProgressReport (UserPage <| ClinicalProgressReportPage (InitiatorEncounterPage data.id) data.id)
                    , viewReportLink Translate.DemographicsReport (UserPage <| DemographicsReportPage (InitiatorEncounterPage data.id) data.participant.person)
                    ]

            else
                div [ class "full content" ]
                    [ div [ class "wrap-cards" ]
                        [ div [ class "ui four cards" ] <|
                            if List.isEmpty selectedActivities then
                                [ span [] [ text emptySectionMessage ] ]

                            else
                                List.map viewCard selectedActivities
                        ]
                    ]

        content =
            div [ class "ui full segment" ]
                [ innerContent ]
    in
    [ tabs
    , content
    ]
