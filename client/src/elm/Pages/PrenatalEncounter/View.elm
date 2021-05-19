module Pages.PrenatalEncounter.View exposing (generateActivityData, view, viewMotherAndMeasurements, viewPersonDetails)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (ObstetricHistoryValue, PrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateHighRiskAlertData
        , generateHighSeverityAlertData
        , generateRecurringHighSeverityAlertData
        , getActivityIcon
        )
import Backend.PrenatalEncounter.Model
    exposing
        ( ClinicalProgressReportInitiator(..)
        , PrenatalEncounter
        , PrenatalEncounterType(..)
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
import Pages.PrenatalEncounter.Model exposing (..)
import Pages.PrenatalEncounter.Utils exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw model) identity data


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw model data =
    let
        header =
            viewHeader language data

        content =
            viewContent language currentDate isChw data model
    in
    div [ class "page-encounter prenatal" ]
        [ header
        , content
        , viewModal <|
            viewChwWarningPopup language data model
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.IndividualEncounterLabel AntenatalEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Bool -> AssembledData -> Model -> Html Msg
viewContent language currentDate isChw data model =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate isChw data (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewMainPageContent language currentDate data model


viewChwWarningPopup : Language -> AssembledData -> Model -> Maybe (Html Msg)
viewChwWarningPopup language data model =
    if model.showWarningForChw then
        Just <|
            div [ class "ui tiny active modal" ]
                [ div [ class "content" ]
                    [ span [ class "person-name" ] [ text data.person.name ]
                    , text " "
                    , text <| translate language Translate.PatientNotYetSeenAtHCLabel
                    , text "."
                    ]
                , div [ class "actions" ]
                    [ div [ class "two ui buttons" ]
                        [ button
                            [ class "ui primary fluid button"
                            , onClick <| SetChwWarningVisible False
                            ]
                            [ text <| translate language Translate.OK ]
                        ]
                    ]
                ]

    else
        Nothing


viewMotherAndMeasurements : Language -> NominalDate -> Bool -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> List (Html msg)
viewMotherAndMeasurements language currentDate isChw data alertsDialogData =
    [ viewMotherDetails language currentDate isChw data alertsDialogData
    , viewMeasurements language currentDate data.globalLmpDate data.globalObstetricHistory
    ]


viewMotherDetails : Language -> NominalDate -> Bool -> AssembledData -> Maybe ( Bool, Bool -> msg ) -> Html msg
viewMotherDetails language currentDate isChw data alertsDialogData =
    let
        mother =
            data.person

        firstEncounterMeasurements =
            getFirstEncounterMeasurements isChw data

        highRiskAlertsData =
            allHighRiskFactors
                |> List.filterMap (generateHighRiskAlertData language firstEncounterMeasurements)

        highSeverityAlertsData =
            allHighSeverityAlerts
                |> List.filterMap (generateHighSeverityAlertData language currentDate isChw data)

        recurringHighSeverityAlertsData =
            allRecurringHighSeverityAlerts
                |> List.map (generateRecurringHighSeverityAlertData language currentDate isChw data)
                |> List.filter (List.isEmpty >> not)

        alertsDialogSection =
            alertsDialogData
                |> Maybe.map
                    (\( isDialogOpen, setAlertsDialogStateMsg ) ->
                        let
                            alertSign =
                                if List.isEmpty highRiskAlertsData && List.isEmpty highSeverityAlertsData && List.isEmpty recurringHighSeverityAlertsData then
                                    emptyNode

                                else
                                    div
                                        [ class "alerts"
                                        , onClick <| setAlertsDialogStateMsg True
                                        ]
                                        [ img [ src "assets/images/exclamation-red.png" ] [] ]

                            dialog =
                                viewModal <|
                                    alertsDialog language
                                        highRiskAlertsData
                                        highSeverityAlertsData
                                        recurringHighSeverityAlertsData
                                        isDialogOpen
                                        setAlertsDialogStateMsg
                        in
                        [ alertSign
                        , dialog
                        ]
                    )
                |> Maybe.withDefault []
    in
    div [ class "item" ] <|
        viewPersonDetails language currentDate mother Nothing
            ++ alertsDialogSection


viewPersonDetails : Language -> NominalDate -> Person -> Maybe TranslationId -> List (Html msg)
viewPersonDetails language currentDate person maybeDiagnosisTranslationId =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
    , div [ class "content person-details" ]
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , maybeAge
            |> Maybe.map
                (\age ->
                    p [ class "age-wrapper" ]
                        [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                        , span [] [ text age ]
                        ]
                )
            |> Maybe.withDefault emptyNode
        , maybeDiagnosisTranslationId
            |> Maybe.map
                (\diagnosis ->
                    div
                        [ classList
                            [ ( "diagnosis-wrapper", True )
                            , ( "covid-19", diagnosis == Translate.AcuteIllnessDiagnosis DiagnosisCovid19 )
                            ]
                        ]
                        [ div [ class "label upper" ] [ text <| translate language Translate.Diagnosis ++ ":" ]
                        , div [ class "diagnosis" ] [ text <| translate language diagnosis ]
                        ]
                )
            |> Maybe.withDefault emptyNode
        ]
    ]


alertsDialog :
    Language
    -> List String
    -> List ( String, String )
    -> List (List ( String, String, String ))
    -> Bool
    -> (Bool -> msg)
    -> Maybe (Html msg)
alertsDialog language highRiskAlertsData highSeverityAlertsData recurringHighSeverityAlertsData isOpen setAlertsDialogStateMsg =
    if isOpen then
        let
            sectionLabel title =
                div [ class "section-label-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "section-label" ] [ text <| translate language title ++ ":" ]
                    ]

            viewAlertWithValue message value =
                div [ class "alert" ]
                    [ span [ class "alert-text" ] [ text <| "- " ++ message ++ ":" ]
                    , span [ class "alert-value" ] [ text value ]
                    ]

            viewAlertWithValueAndDate ( message, value, date ) =
                div [ class "alert" ]
                    [ span [ class "alert-text" ] [ text <| "- " ++ message ++ ":" ]
                    , span [ class "alert-value" ] [ text value ]
                    , span [ class "alert-date" ] [ text <| "(" ++ date ++ ")" ]
                    ]

            viewAlertWithoutValue message =
                div [ class "alert" ] [ text <| "- " ++ message ]

            viewHighSeverityAlert ( message, value ) =
                if value == "" then
                    viewAlertWithoutValue message

                else
                    viewAlertWithValue message value

            highRiskAlerts =
                highRiskAlertsData
                    |> List.map viewAlertWithoutValue

            highSeverityAlerts =
                highSeverityAlertsData
                    |> List.map viewHighSeverityAlert

            viewRecurringHighSeverityAlert alertsList =
                List.map viewAlertWithValueAndDate alertsList

            recurringHighSeverityAlerts =
                recurringHighSeverityAlertsData
                    |> List.map viewRecurringHighSeverityAlert
                    |> List.concat

            highSeverityAlertsEmpty =
                List.isEmpty highSeverityAlerts && List.isEmpty recurringHighSeverityAlerts
        in
        Just <|
            div [ class "ui active modal alerts-dialog" ]
                [ div [ class "content" ]
                    [ div [ class "high-risk-alerts" ]
                        [ sectionLabel Translate.HighRiskFactors
                        , highRiskAlerts
                            |> div [ class "section-items" ]
                        ]
                        |> showIf (List.isEmpty highRiskAlerts |> not)
                    , div [ class "high-severity-alerts" ]
                        [ sectionLabel Translate.HighSeverityAlerts
                        , highSeverityAlerts
                            ++ recurringHighSeverityAlerts
                            |> div [ class "section-items" ]
                        ]
                        |> showIf (not highSeverityAlertsEmpty)
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


viewMeasurements : Language -> NominalDate -> Maybe NominalDate -> Maybe ObstetricHistoryValue -> Html any
viewMeasurements language currentDate lmpDate obstetricHistory =
    let
        ( edd, ega ) =
            generateEDDandEGA language currentDate ( "--/--/----", "----" ) lmpDate

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value
                    , generatePara value
                    )
                )
                obstetricHistory
    in
    div [ class "item measurements" ]
        [ div [ class "ui edd" ]
            [ div [ class "label" ] [ text <| translate language Translate.Edd ++ ":" ]
            , div [ class "value" ] [ text edd ]
            ]
        , div [ class "ui ega" ]
            [ div [ class "label" ] [ text <| translate language Translate.Ega ++ ":" ]
            , div [ class "value" ] [ text ega ]
            ]
        , div [ class "ui gravida" ]
            [ div [ class "label" ] [ text <| translate language Translate.Gravida ++ ":" ]
            , div [ class "value" ] [ text gravida ]
            ]
        , div [ class "ui para" ]
            [ div [ class "label" ] [ text <| translate language Translate.Para ++ ":" ]
            , div [ class "value" ] [ text para ]
            ]
        ]


viewMainPageContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate data model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities data
                |> List.filter (expectActivity currentDate data)
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
                    generateActivityData activity data

                navigationAction =
                    case activity of
                        PregnancyOutcome ->
                            [ onClick <|
                                SetActivePage <|
                                    UserPage <|
                                        PregnancyOutcomePage (InitiatorPostpartumEncounter data.id) data.encounter.participant
                            ]

                        _ ->
                            [ onClick <|
                                SetActivePage <|
                                    UserPage <|
                                        PrenatalActivityPage data.id activity
                            ]
            in
            div [ class "card" ]
                [ div (class "image" :: navigationAction)
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
                    [ viewReportLink Translate.ClinicalProgressReport (UserPage <| ClinicalProgressReportPage InitiatorEncounterPage data.id)
                    , viewReportLink Translate.DemographicsReport (UserPage <| DemographicsReportPage data.id)
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

        allowEndEcounter =
            case pendingActivities of
                -- Either all activities are completed
                [] ->
                    True

                -- Or only one none mandatory activity remains
                [ PrenatalPhoto ] ->
                    True

                _ ->
                    False

        endEcounterButtonAttributes =
            if allowEndEcounter then
                [ class "ui fluid primary button"
                , onClick <| CloseEncounter data.id
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


generateActivityData : PrenatalActivity -> AssembledData -> ( TranslationId, String )
generateActivityData activity data =
    case activity of
        NextSteps ->
            if noDangerSigns data && data.encounter.encounterType /= ChwPostpartumEncounter then
                ( Translate.AppointmentConfirmation, "appointment-confirmation" )

            else
                ( Translate.PrenatalActivitiesTitle NextSteps, getActivityIcon activity )

        _ ->
            ( Translate.PrenatalActivitiesTitle activity, getActivityIcon activity )
