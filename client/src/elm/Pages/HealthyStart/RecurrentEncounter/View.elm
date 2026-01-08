module Pages.HealthyStart.RecurrentEncounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.HealthyStartActivity.Utils
    exposing
        ( getRecurrentActivityIcon
        )
import Backend.HealthyStartEncounter.Model
    exposing
        ( HealthyStartProgressReportInitiator(..)
        )
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isLabTechnician)
import Gizra.Html exposing (showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.HealthyStart.Encounter.Utils exposing (generateAssembledData)
import Pages.HealthyStart.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.HealthyStart.Model exposing (AssembledData)
import Pages.HealthyStart.RecurrentActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.HealthyStart.RecurrentEncounter.Model exposing (..)
import Pages.HealthyStart.RecurrentEncounter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewCustomAction, viewReportLink)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Nurse -> HealthyStartEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurse id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate nurse id model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Nurse -> HealthyStartEncounterId -> Model -> AssembledData -> Html Msg
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
                |> List.filter (expectActivity currentDate isLabTech assembled)
                |> List.partition (activityCompleted currentDate isLabTech assembled)

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
                    |> showIf (not isLabTech)
                ]

        viewCard activity =
            let
                ( label, icon ) =
                    ( Translate.HealthyStartRecurrentActivitiesTitle activity, getRecurrentActivityIcon activity )
            in
            activityCard language label icon (SetActivePage <| UserPage <| HealthyStartRecurrentActivityPage assembled.id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        content =
            let
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

                ( label, action ) =
                    let
                        leaveEncounterTuple =
                            ( Translate.LeaveEncounter, SetActivePage <| UserPage GlobalCaseManagementPage )
                    in
                    if not isLabTech && List.isEmpty pendingActivities then
                        -- Nurse has completed all activities => end the
                        -- encounter (by setting resolution date to today).
                        Maybe.map2
                            (\( resultsId, _ ) value ->
                                ( Translate.EndEncounter
                                , ConcludeEncounter
                                    assembled.participant.person
                                    assembled.id
                                    resultsId
                                    value
                                )
                            )
                            assembled.measurements.labsResults
                            (getMeasurementValueFunc assembled.measurements.labsResults)
                            |> Maybe.withDefault leaveEncounterTuple

                    else
                        leaveEncounterTuple
            in
            div [ class "ui full segment" ]
                [ innerContent
                , viewCustomAction language action False label
                ]
    in
    [ tabs
    , content
    ]
