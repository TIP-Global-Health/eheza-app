module Pages.Prenatal.RecurrentEncounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isLabTechnician)
import Backend.PrenatalActivity.Utils
    exposing
        ( getRecurrentActivityIcon
        )
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalProgressReportInitiator(..)
        )
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.RecurrentActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.Prenatal.RecurrentEncounter.Model exposing (..)
import Pages.Prenatal.RecurrentEncounter.Utils exposing (..)
import Pages.Utils exposing (viewReportLink)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Nurse -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurse id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate nurse id model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Nurse -> PrenatalEncounterId -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate nurse id model assembled =
    let
        header =
            viewHeader language

        content =
            viewContent language currentDate nurse assembled model
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


viewContent : Language -> NominalDate -> Nurse -> AssembledData -> Model -> Html Msg
viewContent language currentDate nurse assembled model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate nurse assembled model


viewMainPageContent : Language -> NominalDate -> Nurse -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate nurse assembled model =
    let
        isLabTech =
            isLabTechnician nurse

        ( completedActivities, pendingActivities ) =
            getAllActivities isLabTech
                |> List.filter (expectActivity currentDate assembled)
                |> List.partition (activityCompleted currentDate assembled)

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
            activityCard language label icon (SetActivePage <| UserPage <| PrenatalRecurrentActivityPage assembled.id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink language
                        Translate.ClinicalProgressReport
                        (SetActivePage <|
                            UserPage <|
                                ClinicalProgressReportPage (InitiatorRecurrentEncounterPage assembled.id) assembled.id
                        )
                    , viewReportLink language
                        Translate.DemographicsReport
                        (SetActivePage <|
                            UserPage <|
                                DemographicsReportPage (InitiatorRecurrentEncounterPage assembled.id) assembled.participant.person
                        )
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
                [ innerContent
                , div [ class "actions" ]
                    [ button
                        [ class "ui fluid primary button"
                        , onClick (SetActivePage <| UserPage GlobalCaseManagementPage)
                        ]
                        [ text <| translate language Translate.LeaveEncounter ]
                    ]
                ]
    in
    [ tabs
    , content
    ]
