module Pages.PrenatalEncounter.View exposing (view, viewMotherAndMeasurements)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import Date.Extra as Date exposing (Interval(Day))
import EveryDict
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.Model exposing (..)
import Pages.PrenatalEncounter.Utils exposing (..)
import PrenatalActivity.Model exposing (..)
import PrenatalActivity.Utils exposing (generateHighRiskAlertData, generateHighSeverityAlertData, getActivityIcon, getAllActivities)
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date exposing (date)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (script, tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


type alias FetchedData =
    { encounter : PrenatalEncounter
    , participant : PrenatalParticipant
    , person : Person
    , measurements : PrenatalMeasurements
    , id : PrenatalEncounterId
    }


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        encounter =
            EveryDict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            EveryDict.get id db.prenatalMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        EveryDict.get encounter.participant db.prenatalParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant ->
                        EveryDict.get participant.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        data =
            RemoteData.map FetchedData encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success id)

        header =
            viewWebData language (viewHeader language) identity data

        content =
            viewWebData language (viewContent language currentDate model) identity data
    in
    div [ class "page-prenatal-encounter" ] <|
        [ header
        , content
        ]


viewContent : Language -> NominalDate -> Model -> FetchedData -> Html Msg
viewContent language currentDate model data =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate data.person data.measurements model.showAlertsDialog SetAlertsDialogState
            ++ viewMainPageContent language currentDate data model


viewHeader : Language -> FetchedData -> Html Msg
viewHeader language data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PrenatalEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewMotherAndMeasurements : Language -> NominalDate -> Person -> PrenatalMeasurements -> Bool -> (Bool -> msg) -> List (Html msg)
viewMotherAndMeasurements language currentDate mother measurements isDialogOpen setAlertsDialogStateMsg =
    [ viewMotherDetails language currentDate mother measurements isDialogOpen setAlertsDialogStateMsg
    , viewMeasurements language currentDate measurements
    ]


viewMotherDetails : Language -> NominalDate -> Person -> PrenatalMeasurements -> Bool -> (Bool -> msg) -> Html msg
viewMotherDetails language currentDate mother measurements isDialogOpen setAlertsDialogStateMsg =
    let
        highRiskAlertsData =
            allHighRiskFactors
                |> List.filterMap (generateHighRiskAlertData language measurements)

        highSeverityAlertsData =
            allHighSeverityAlerts
                |> List.filterMap (generateHighSeverityAlertData language measurements)

        alertSign =
            if List.isEmpty highRiskAlertsData && List.isEmpty highSeverityAlertsData then
                emptyNode

            else
                div
                    [ class "alerts"
                    , onClick <| setAlertsDialogStateMsg True
                    ]
                    [ img [ src "assets/images/exclamation-red.png" ] [] ]
    in
    div [ class "item" ]
        [ div [ class "ui image" ]
            [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text mother.name ]
            , showMaybe <|
                Maybe.map
                    (\age ->
                        p [ class "age-wrapper" ]
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                            , span [] [ text <| translate language <| Translate.YearsOld age ]
                            ]
                    )
                    (ageInYears currentDate mother)
            ]
        , alertSign
        , viewModal <| alertsDialog language highRiskAlertsData highSeverityAlertsData isDialogOpen setAlertsDialogStateMsg
        ]


alertsDialog : Language -> List String -> List ( String, String ) -> Bool -> (Bool -> msg) -> Maybe (Html msg)
alertsDialog language highRiskAlertsData highSeverityAlertsData isOpen setAlertsDialogStateMsg =
    if isOpen then
        let
            sectionLabel title =
                div [ class "section-label-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "section-label" ] [ text <| translate language title ++ ":" ]
                    ]

            viewHighSeverityAlert ( message, value ) =
                div [ class "alert" ]
                    [ span [ class "alert-text" ] [ text <| "- " ++ message ++ ":" ]
                    , span [ class "alert-value" ] [ text value ]
                    ]

            highRiskAlerts =
                highRiskAlertsData
                    |> List.map (\message -> div [ class "alert" ] [ text <| "- " ++ message ])

            highSeverityAlerts =
                highSeverityAlertsData
                    |> List.map viewHighSeverityAlert
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
                            |> div [ class "section-items" ]
                        ]
                        |> showIf (List.isEmpty highSeverityAlerts |> not)
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


viewMeasurements : Language -> NominalDate -> PrenatalMeasurements -> Html any
viewMeasurements language currentDate measurements =
    let
        ( edd, ega ) =
            measurements.lastMenstrualPeriod
                |> Maybe.map (Tuple.second >> .value >> .date)
                |> generateEDDandEGA language currentDate ( "--/--/----", "----" )

        obstetricHistoryValue =
            measurements.obstetricHistory
                |> Maybe.map (Tuple.second >> .value)

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value.termPregnancy value.preTermPregnancy value.currentlyPregnant
                    , generatePara value.termPregnancy value.preTermPregnancy value.abortions value.liveChildren
                    )
                )
                obstetricHistoryValue
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


viewMainPageContent : Language -> NominalDate -> FetchedData -> Model -> List (Html Msg)
viewMainPageContent language currentDate data model =
    let
        ( completedActivities, pendingActivities ) =
            List.partition
                (\activity ->
                    case activity of
                        PregnancyDating ->
                            isJust data.measurements.lastMenstrualPeriod

                        History ->
                            isJust data.measurements.obstetricHistory
                                && isJust data.measurements.obstetricHistoryStep2
                                && isJust data.measurements.medicalHistory
                                && isJust data.measurements.socialHistory

                        Examination ->
                            isJust data.measurements.vitals
                                && isJust data.measurements.nutrition
                                && isJust data.measurements.corePhysicalExam
                                && isJust data.measurements.obstetricalExam
                                && isJust data.measurements.breastExam

                        FamilyPlanning ->
                            isJust data.measurements.familyPlanning

                        PatientProvisions ->
                            isJust data.measurements.medication && isJust data.measurements.resource

                        DangerSigns ->
                            isJust data.measurements.dangerSigns
                )
                getAllActivities

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
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| PrenatalActivityPage data.id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.PrenatalActivitiesTitle activity
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
                    ( [], "Under construction..." )

        activities =
            div [ class "ui full segment" ]
                [ div
                    [ class "full content" ]
                    [ div [ class "wrap-cards" ]
                        [ div [ class "ui four cards" ] <|
                            if List.isEmpty selectedActivities then
                                [ span [] [ text emptySectionMessage ] ]

                            else
                                List.map viewCard selectedActivities
                        ]
                    ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui fluid primary button"
                        , onClick <| CloseEncounter data.id
                        ]
                        [ text <| translate language Translate.EndEncounter ]
                    ]
                ]
    in
    [ tabs
    , activities
    ]
