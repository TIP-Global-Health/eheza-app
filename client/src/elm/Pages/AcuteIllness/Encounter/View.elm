module Pages.AcuteIllness.Encounter.View exposing (allowEndingEcounter, partitionActivities, view, viewPersonDetailsWithAlert, warningPopup)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter, AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.AcuteIllness.Activity.Utils
    exposing
        ( activityCompleted
        , expectActivity
        , muacRedOnSubsequentVisit
        , noImprovementOnSubsequentVisit
        , respiratoryRateElevated
        , sendToHCOnSubsequentVisitByNutrition
        )
import Pages.AcuteIllness.Encounter.Model exposing (..)
import Pages.AcuteIllness.Encounter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails, viewReportLink)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        assembled =
            generateAssembledData currentDate id isChw db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw db model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate id isChw model assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter acute-illness" ]
        [ header
        , content
        , viewModal endEncounterDialog
        , viewModal <|
            warningPopup language
                currentDate
                isChw
                model.warningPopupState
                SetWarningPopupState
                assembled
        ]


warningPopup : Language -> NominalDate -> Bool -> Maybe AcuteIllnessDiagnosis -> (Maybe AcuteIllnessDiagnosis -> msg) -> AssembledData -> Maybe (Html msg)
warningPopup language currentDate isChw state setStateMsg assembled =
    state
        |> Maybe.map
            (\diagnosis ->
                if assembled.initialEncounter then
                    viewWarningPopupFirstEncounter language isChw setStateMsg diagnosis

                else
                    viewWarningPopupSubsequentEncounter language currentDate setStateMsg diagnosis assembled
            )


viewWarningPopupFirstEncounter : Language -> Bool -> (Maybe AcuteIllnessDiagnosis -> msg) -> AcuteIllnessDiagnosis -> Html msg
viewWarningPopupFirstEncounter language isChw setStateMsg diagnosis =
    let
        infoHeading =
            [ div [ class "popup-heading" ] [ text <| translate language Translate.Assessment ++ ":" ] ]

        warningHeading =
            [ img [ src "assets/images/exclamation-red.png" ] []
            , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
            ]

        ( heading, content ) =
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    ( warningHeading
                    , if isChw then
                        [ div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseIsolate ]
                        , div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CaseContactHC ]
                        ]

                      else
                        [ div [ class "popup-action" ] [ text <| translate language Translate.SuspectedCovid19CasePerformRapidTest ] ]
                    )

                _ ->
                    ( infoHeading, [] )
    in
    div [ class "ui active modal diagnosis-popup" ]
        [ div [ class "content" ] <|
            [ div [ class "popup-heading-wrapper" ] heading
            , div [ class "popup-title" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
            ]
                ++ content
        , div
            [ class "actions" ]
            [ button
                [ class "ui primary fluid button"
                , onClick <| setStateMsg Nothing
                ]
                [ text <| translate language Translate.Continue ]
            ]
        ]


viewWarningPopupSubsequentEncounter : Language -> NominalDate -> (Maybe AcuteIllnessDiagnosis -> msg) -> AcuteIllnessDiagnosis -> AssembledData -> Html msg
viewWarningPopupSubsequentEncounter language currentDate setStateMsg diagnosis assembled =
    let
        isImproving =
            not <| noImprovementOnSubsequentVisit currentDate assembled.person assembled.measurements

        respiratoryDistress =
            if respiratoryRateElevated currentDate assembled.person assembled.measurements then
                p [] [ text <| translate language Translate.RespiratoryDistress ]

            else
                emptyNode

        severeAcuteMalnutrition =
            if muacRedOnSubsequentVisit assembled.measurements then
                p [] [ text <| translate language Translate.SevereAcuteMalnutrition ]

            else
                emptyNode

        malnutritionWithComplications =
            if sendToHCOnSubsequentVisitByNutrition assembled.measurements then
                p [] [ text <| translate language Translate.MalnutritionWithComplications ]

            else
                emptyNode
    in
    div [ classList [ ( "ui active modal diagnosis-popup", True ), ( "blue", isImproving ) ] ]
        [ div [ class "content" ] <|
            [ div [ class "popup-heading-wrapper" ]
                [ div [ class "popup-heading" ] [ text <| translate language Translate.Assessment ++ ":" ] ]
            , div [ class "popup-title" ]
                [ p [] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
                , respiratoryDistress
                , severeAcuteMalnutrition
                , malnutritionWithComplications
                , p [] [ text <| translate language <| Translate.ConditionImproving isImproving ]
                ]
            ]
        , div
            [ class "actions" ]
            [ button
                [ class "ui primary fluid button"
                , onClick <| setStateMsg Nothing
                ]
                [ text <| translate language Translate.Continue ]
            ]
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language assembled =
    let
        label =
            if assembled.initialEncounter then
                Translate.IndividualEncounterLabel Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter True
                    |> translate language
                    |> text
                    |> List.singleton

            else
                let
                    diagnosisLabel =
                        Maybe.map Tuple.second assembled.diagnosis
                            |> Maybe.map (Translate.AcuteIllnessDiagnosis >> translate language)
                            |> Maybe.withDefault ""

                    subsequentDiagnosisLabel =
                        Translate.IndividualEncounterSubsequentVisit Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                            |> translate language
                in
                [ div [] [ text <| subsequentDiagnosisLabel ++ ":" ]
                , div [] [ text diagnosisLabel ]
                ]
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            label
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage InitiatorParticipantsPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewContent language currentDate id isChw model assembled =
    (viewPersonDetailsWithAlert language currentDate isChw assembled model.showAlertsDialog SetAlertsDialogState
        :: viewMainPageContent language currentDate id isChw assembled model
    )
        |> div [ class "ui unstackable items" ]


viewPersonDetailsWithAlert : Language -> NominalDate -> Bool -> AssembledData -> Bool -> (Bool -> msg) -> Html msg
viewPersonDetailsWithAlert language currentDate isChw assembled isDialogOpen setAlertsDialogStateMsg =
    let
        diagnosis =
            Maybe.map Tuple.second assembled.diagnosis

        diagnosisTranslationId =
            Maybe.map Translate.AcuteIllnessDiagnosis diagnosis

        alertSection =
            if isChw && diagnosis == Just DiagnosisCovid19Suspect then
                [ div
                    [ class "alerts"
                    , onClick <| setAlertsDialogStateMsg True
                    ]
                    [ img [ src "assets/images/exclamation-red.png" ] [] ]
                , viewModal <|
                    alertsDialog language
                        isDialogOpen
                        setAlertsDialogStateMsg
                ]

            else
                []
    in
    div [ class "item" ] <|
        viewPersonDetails language currentDate assembled.person diagnosisTranslationId
            ++ alertSection


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


viewMainPageContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate id isChw assembled model =
    let
        measurements =
            assembled.measurements

        ( completedActivities, pendingActivities ) =
            partitionActivities currentDate isChw assembled

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
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetActivePage (UserPage (AcuteIllnessProgressReportPage InitiatorEncounterPage id)))
                ]

        viewCard activity =
            activityCard language
                (Translate.AcuteIllnessActivityTitle activity)
                (getActivityIcon activity)
                (SetActivePage <| UserPage <| AcuteIllnessActivityPage id activity)

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
                                AcuteIllnessProgressReportPage InitiatorEncounterPage assembled.id
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

        allowEndEncounter =
            allowEndingEcounter currentDate isChw assembled pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEndEncounterButton language allowEndEncounter SetEndEncounterDialogState
                ]
    in
    [ tabs
    , content
    ]


partitionActivities : NominalDate -> Bool -> AssembledData -> ( List AcuteIllnessActivity, List AcuteIllnessActivity )
partitionActivities currentDate isChw assembled =
    getAllActivities assembled.initialEncounter
        |> List.filter (expectActivity currentDate isChw assembled)
        |> List.partition (activityCompleted currentDate isChw assembled)


allowEndingEcounter : NominalDate -> Bool -> AssembledData -> List AcuteIllnessActivity -> Bool
allowEndingEcounter currentDate isChw assembled pendingActivities =
    if not assembled.initialEncounter then
        List.isEmpty pendingActivities

    else if expectActivity currentDate isChw assembled AcuteIllnessNextSteps then
        activityCompleted currentDate isChw assembled AcuteIllnessNextSteps

    else
        List.isEmpty pendingActivities
