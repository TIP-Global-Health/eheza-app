module Pages.AcuteIllnessEncounter.View exposing (view, viewPersonDetailsWithAlert)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AcuteIllnessActivity.Utils exposing (getActivityIcon, getAllActivities)
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.AcuteIllnessEncounter.Model exposing (..)
import Pages.AcuteIllnessEncounter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (viewEndEncounterDialog)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        data =
            generateAssembledData id db

        header =
            viewWebData language (viewHeader language) identity data

        content =
            viewWebData language (viewContent language currentDate id model) identity data

        endEncounterDialog =
            if model.showEndEncounetrDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter acute-illness" ] <|
        [ header
        , content
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Model -> AssembledData -> Html Msg
viewContent language currentDate id model data =
    let
        isSuspected =
            suspectedCovid19Case data.measurements
    in
    (viewPersonDetailsWithAlert language currentDate data.person isSuspected model.showAlertsDialog SetAlertsDialogState
        :: viewMainPageContent language currentDate id data isSuspected model
    )
        |> div [ class "ui unstackable items" ]


viewPersonDetailsWithAlert : Language -> NominalDate -> Person -> Bool -> Bool -> (Bool -> msg) -> Html msg
viewPersonDetailsWithAlert language currentDate person isSuspected isDialogOpen setAlertsDialogStateMsg =
    let
        alertSign =
            if isSuspected then
                div
                    [ class "alerts"
                    , onClick <| setAlertsDialogStateMsg True
                    ]
                    [ img [ src "assets/images/exclamation-red.png" ] [] ]

            else
                emptyNode
    in
    div [ class "item" ] <|
        viewPersonDetails language currentDate person
            ++ [ alertSign
               , viewModal <|
                    alertsDialog language
                        isDialogOpen
                        setAlertsDialogStateMsg
               ]


alertsDialog : Language -> Bool -> (Bool -> msg) -> Maybe (Html msg)
alertsDialog language isOpen setAlertsDialogStateMsg =
    if isOpen then
        let
            sectionLabel title =
                div [ class "section-label-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "section-label" ] [ text <| translate language title ++ ":" ]
                    ]
        in
        Just <|
            div [ class "ui active modal alerts-dialog" ]
                [ div [ class "content" ]
                    [ div [ class "high-severity-alerts" ]
                        [ sectionLabel Translate.HighSeverityAlerts
                        , div [ class "section-items" ]
                            [ div [ class "alert" ]
                                [ div [ class "alert-text upper" ] [ text <| "- " ++ translate language Translate.SuspectedCovid19CaseAlert ++ "." ]
                                , div [ class "alert-helper" ] [ text <| translate language Translate.SuspectedCovid19CaseAlertHelper ++ "." ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| setAlertsDialogStateMsg False
                        ]
                        [ text <| translate language Translate.Close ]
                    ]
                ]

    else
        Nothing


viewMainPageContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AssembledData -> Bool -> Model -> List (Html Msg)
viewMainPageContent language currentDate id data isSuspected model =
    let
        measurements =
            data.measurements

        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.partition
                    (\activity ->
                        case activity of
                            AcuteIllnessSymptoms ->
                                isJust measurements.symptomsGeneral
                                    && isJust measurements.symptomsRespiratory
                                    && isJust measurements.symptomsGI

                            AcuteIllnessPhysicalExam ->
                                isJust measurements.vitals

                            AcuteIllnessPriorTreatment ->
                                -- Todo
                                False

                            AcuteIllnessLaboratory ->
                                isJust measurements.malariaTesting

                            AcuteIllnessExposure ->
                                exposureTasksCompleted measurements isSuspected
                    )

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard activity =
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| AcuteIllnessActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.AcuteIllnessActivityTitle activity
                            |> translate language
                            |> String.toUpper
                            |> text
                        ]
                    ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

        innerContent =
            div [ class "full content" ]
                [ div [ class "wrap-cards" ]
                    [ div [ class "ui four cards" ] <|
                        if List.isEmpty selectedActivities then
                            [ span [] [ text emptySectionMessage ] ]

                        else
                            List.map viewCard selectedActivities
                    ]
                ]

        allowEndEcounter =
            if isSuspected then
                isJust measurements.isolation && isJust measurements.hcContact

            else
                List.isEmpty pendingActivities

        endEcounterButtonAttributes =
            if allowEndEcounter then
                [ class "ui fluid primary button"
                , onClick <| SetEndEncounterDialogState True
                ]

            else
                [ class "ui fluid primary button disabled" ]

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , div [ class "actions" ]
                    [ button
                        endEcounterButtonAttributes
                        [ text <| translate language Translate.EndEncounter ]
                    ]
                ]
    in
    [ tabs
    , content
    ]
