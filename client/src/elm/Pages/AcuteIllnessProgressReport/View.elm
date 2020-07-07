module Pages.AcuteIllnessProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInYears, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis, AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (generateAssembledData, resolveAcuteIllnessDiagnosis)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 180
    , height = 180
    }


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate id db =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id) identity data


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AssembledData -> Html Msg
viewContent language currentDate id data =
    let
        diagnosis =
            resolveAcuteIllnessDiagnosis currentDate data.person data.measurements
    in
    div [ class "page-report acute-illness" ]
        [ div
            [ class "ui report unstackable items" ]
            [ viewHeader language currentDate id
            , viewPersonInfo language currentDate data.person data.measurements
            , viewAssessmentPane language currentDate diagnosis
            , viewSymptomsPane language currentDate data.measurements
            , viewPhysicalExamPane language currentDate data.measurements
            , viewActionsTakenPane language currentDate diagnosis data.measurements
            ]
        ]


viewHeader : Language -> NominalDate -> AcuteIllnessEncounterId -> Html Msg
viewHeader language currentDate id =
    div [ class "report-header" ]
        [ a
            [ class "icon-back"
            , onClick <| SetActivePage (UserPage (AcuteIllnessEncounterPage id))
            ]
            []
        , h1 [ class "ui report header" ]
            [ text <| translate language Translate.ProgressReport ]
        , p [ class "date" ]
            [ text <| translate language Translate.CurrentIllnessBegan
            , text " - "
            , text <| renderDate language currentDate
            ]
        ]


viewPersonInfo : Language -> NominalDate -> Person -> AcuteIllnessMeasurements -> Html Msg
viewPersonInfo language currentDate person measurements =
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

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ": " ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div
        [ class "item person-details" ]
        [ div [ class "ui image" ]
            [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width
            ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text person.name ]
            , viewAge
            , viewVillage
            ]
        ]


viewAssessmentPane : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> Html Msg
viewAssessmentPane language currentDate diagnosis =
    let
        assessment =
            diagnosis
                |> Maybe.map (Translate.AcuteIllnessDiagnosisWarning >> translate language >> text >> List.singleton)
                |> Maybe.withDefault []
    in
    div [ class "pane assessment" ]
        [ viewItemHeading language Translate.Assessment "blue"
        , div [ class "pane-content" ] assessment
        ]


viewSymptomsPane : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewSymptomsPane language currentDate measurements =
    let
        symptomsDictToList dict =
            Dict.toList dict
                |> List.filterMap
                    (\( symptom, count ) ->
                        if count > 0 then
                            Just symptom

                        else
                            Nothing
                    )

        symptomsGeneral =
            measurements.symptomsGeneral
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> symptomsDictToList
                        >> List.map (\symptom -> li [ class "general" ] [ text <| translate language (Translate.SymptomsGeneralSign symptom) ])
                    )
                |> Maybe.withDefault []

        symptomsRespiratory =
            measurements.symptomsRespiratory
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> symptomsDictToList
                        >> List.map (\symptom -> li [ class "respiratory" ] [ text <| translate language (Translate.SymptomsRespiratorySign symptom) ])
                    )
                |> Maybe.withDefault []

        symptomsGI =
            measurements.symptomsGI
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> .signs
                        >> symptomsDictToList
                        >> List.map (\symptom -> li [ class "gi" ] [ text <| translate language (Translate.SymptomsGISignAbbrev symptom) ])
                    )
                |> Maybe.withDefault []

        values =
            [ ( currentDate, symptomsGeneral ++ symptomsRespiratory ++ symptomsGI ) ]

        symptomsTable =
            values
                |> List.map
                    (\( date, symptoms ) ->
                        div [ class "symptoms-table-row" ]
                            [ div [ class "date" ] [ formatDDMMYY date |> text ]
                            , ul [] symptoms
                            ]
                    )
                |> div [ class "symptoms-table" ]
    in
    div [ class "pane symptoms" ]
        [ viewItemHeading language Translate.Symptoms "blue"
        , symptomsTable
        ]


viewPhysicalExamPane : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewPhysicalExamPane language currentDate measurements =
    let
        viewBodyTemperatureCell maybeBodyTemperature =
            maybeBodyTemperature
                |> Maybe.map
                    (\bodyTemperature_ ->
                        if bodyTemperature_ < 37.5 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "alert" ] [ text <| String.fromFloat bodyTemperature_ ++ " " ++ translate language Translate.CelsiusAbbrev ]
                    )
                |> Maybe.withDefault (td [] [])

        viewRespiratoryRateCell maybeRespiratoryRate =
            maybeRespiratoryRate
                |> Maybe.map
                    (\respiratoryRate_ ->
                        if respiratoryRate_ < 20 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "alert" ] [ text <| String.fromInt respiratoryRate_ ++ " " ++ translate language Translate.BpmUnit ]
                    )
                |> Maybe.withDefault (td [] [])

        bodyTemperature =
            measurements.vitals
                |> Maybe.map (Tuple.second >> .value >> .bodyTemperature)

        respiratoryRate =
            measurements.vitals
                |> Maybe.map (Tuple.second >> .value >> .respiratoryRate)

        values =
            [ ( currentDate, bodyTemperature, respiratoryRate ) ]

        tableHead =
            [ tr []
                [ th [] []
                , th [ class "uppercase" ]
                    [ text <| translate language Translate.Fever ]
                , th [ class "last" ]
                    [ text <| translate language Translate.Tachypnea ]
                ]
            ]

        tableBody =
            values
                |> List.map
                    (\( date, maybeBodyTemperature, maybeRespiratoryRate ) ->
                        tr []
                            [ td [ class "first" ] [ formatDDMMYY date |> text ]
                            , viewBodyTemperatureCell maybeBodyTemperature
                            , viewRespiratoryRateCell maybeRespiratoryRate
                            ]
                    )
    in
    div [ class "pane physical-exam" ]
        [ viewItemHeading language Translate.PhysicalExam "blue"
        , table
            [ class "ui celled table" ]
            [ thead [] tableHead
            , tbody [] tableBody
            ]
        ]


viewActionsTakenPane : Language -> NominalDate -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenPane language currentDate diagnosis measurements =
    let
        actions =
            []
    in
    div [ class "pane actions-taken" ]
        [ viewItemHeading language Translate.ActionsTaken "blue"
        , div [ class "pane-content" ] actions
        ]
