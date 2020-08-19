module Pages.AcuteIllnessEncounter.View exposing (splitActivities, view, viewEndEncounterButton, viewPersonDetailsWithAlert, warningPopup)

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
        , viewModal <|
            warningPopup language
                model.warningPopupState
                SetWarningPopupState
        ]


warningPopup : Language -> Maybe AcuteIllnessDiagnosis -> (Maybe AcuteIllnessDiagnosis -> msg) -> Maybe (Html msg)
warningPopup language maybeDiagnosis setStateMsg =
    maybeDiagnosis
        |> Maybe.map
            (\diagnosis ->
                let
                    infoHeading =
                        [ div [ class "popup-heading" ] [ text <| translate language Translate.Assessment ++ ":" ] ]

                    warningHeading =
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]

                    ( heading, content, color ) =
                        case diagnosis of
                            DiagnosisCovid19 ->
                                ( warningHeading
                                , [ div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseIsolate ]
                                  , div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseContactHC ]
                                  ]
                                , "red"
                                )

                            _ ->
                                ( infoHeading, [], "blue" )
                in
                div [ class <| "ui active modal diagnosis-popup " ++ color ]
                    [ div [ class "content" ] <|
                        [ div [ class "popup-heading-wrapper" ] heading
                        , div [ class "popup-title" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
                        ]
                            ++ content
                    , div
                        [ class "actions" ]
                        [ button
                            [ class <| "ui primary fluid button " ++ color
                            , onClick <| setStateMsg Nothing
                            ]
                            [ text <| translate language Translate.Continue ]
                        ]
                    ]
            )


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
        diagnosis =
            resolveAcuteIllnessDiagnosis currentDate data.person data.measurements
    in
    (viewPersonDetailsWithAlert language currentDate data.person diagnosis model.showAlertsDialog SetAlertsDialogState
        :: viewMainPageContent language currentDate id data diagnosis model
    )
        |> div [ class "ui unstackable items" ]


viewPersonDetailsWithAlert : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> Bool -> (Bool -> msg) -> Html msg
viewPersonDetailsWithAlert language currentDate person diagnosis isDialogOpen setAlertsDialogStateMsg =
    let
        alertSign =
            if diagnosis == Just DiagnosisCovid19 then
                div
                    [ class "alerts"
                    , onClick <| setAlertsDialogStateMsg True
                    ]
                    [ img [ src "assets/images/exclamation-red.png" ] [] ]

            else
                emptyNode

        diagnosisTranslationId =
            Maybe.map Translate.AcuteIllnessDiagnosis diagnosis
    in
    div [ class "item" ] <|
        viewPersonDetails language currentDate person diagnosisTranslationId
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


viewMainPageContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AssembledData -> Maybe AcuteIllnessDiagnosis -> Model -> List (Html Msg)
viewMainPageContent language currentDate id data diagnosis model =
    let
        measurements =
            data.measurements

        ( completedActivities, pendingActivities ) =
            splitActivities currentDate data diagnosis

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.ProgressReport

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetActivePage (UserPage (AcuteIllnessProgressReportPage id)))
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
                    [ viewReportLink Translate.ClinicalProgressReport (UserPage <| AcuteIllnessProgressReportPage data.id)
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
                , viewEndEncounterButton language measurements pendingActivities diagnosis SetEndEncounterDialogState
                ]
    in
    [ tabs
    , content
    ]


splitActivities : NominalDate -> AssembledData -> Maybe AcuteIllnessDiagnosis -> ( List AcuteIllnessActivity, List AcuteIllnessActivity )
splitActivities currentDate data diagnosis =
    getAllActivities
        |> List.filter (expectActivity currentDate data.person data.measurements diagnosis)
        |> List.partition (activityCompleted currentDate data.person data.measurements diagnosis)


viewEndEncounterButton : Language -> AcuteIllnessMeasurements -> List AcuteIllnessActivity -> Maybe AcuteIllnessDiagnosis -> (Bool -> msg) -> Html msg
viewEndEncounterButton language measurements pendingActivities diagnosis setDialogStateMsgs =
    let
        allowEndEcounter =
            if diagnosis == Just DiagnosisCovid19 then
                isJust measurements.isolation && isJust measurements.hcContact

            else if isJust diagnosis then
                case pendingActivities of
                    [] ->
                        True

                    [ AcuteIllnessPriorTreatment ] ->
                        True

                    _ ->
                        False

            else
                List.isEmpty pendingActivities

        attributes =
            if allowEndEcounter then
                [ class "ui fluid primary button"
                , onClick <| setDialogStateMsgs True
                ]

            else
                [ class "ui fluid primary button disabled" ]
    in
    div [ class "actions" ]
        [ button
            attributes
            [ text <| translate language Translate.EndEncounter ]
        ]
