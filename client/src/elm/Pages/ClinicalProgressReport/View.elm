module Pages.ClinicalProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (PrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import EveryDict
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (unwrap)
import Pages.DemographicsReport.View exposing (viewHeader, viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.Utils exposing (generateEDDandEGA, generateGravida, generatePara)
import PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnosis
        , allObstetricDiagnosis
        , allRiskFactors
        , allTrimesters
        )
import PrenatalActivity.Utils
    exposing
        ( generateMedicalDiagnosisAlertData
        , generateObstetricDiagnosisAlertData
        , generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)
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


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate prenatalEncounterId db =
    let
        encounter =
            EveryDict.get prenatalEncounterId db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            EveryDict.get prenatalEncounterId db.prenatalMeasurements
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
                |> RemoteData.andMap (Success prenatalEncounterId)

        header =
            viewHeader language prenatalEncounterId Translate.ClinicalProgressReport

        content =
            viewWebData language (viewContent language currentDate) identity data
    in
    div [ class "page-clinical-progress-report" ] <|
        [ header
        , content
        ]


viewContent : Language -> NominalDate -> FetchedData -> Html Msg
viewContent language currentDate data =
    div [ class "ui unstackable items" ]
        [ viewHeaderPane language currentDate data.person data.measurements
        , viewRiskFactorsPane language currentDate data.measurements
        , viewMedicalDiagnosisPane language currentDate data.measurements
        , viewObstetricDiagnosisPane language currentDate data.measurements
        , viewPatientProgressPane language currentDate data.measurements
        ]


viewHeaderPane : Language -> NominalDate -> Person -> PrenatalMeasurements -> Html Msg
viewHeaderPane language currentDate mother measurements =
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
                    , generatePara value
                    )
                )
                obstetricHistoryValue

        viewLineItem class_ label value =
            p [ class class_ ]
                [ span [ class "label" ] [ text <| translate language label ++ ":" ]
                , span [ class "value" ] [ text value ]
                ]
    in
    div [ class "header-pane" ]
        [ div [ class "mother-details" ]
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "content middle" ]
                [ viewLineItem "mother-name" Translate.Name mother.name
                , showMaybe <|
                    Maybe.map
                        (\age ->
                            viewLineItem "age-wrapper" Translate.AgeWord (translate language <| Translate.YearsOld age)
                        )
                        (ageInYears currentDate mother)
                ]
            , div [ class "content right" ]
                [ viewLineItem "edd" Translate.Edd edd
                , viewLineItem "ega" Translate.Ega ega
                ]
            ]
        , div [ class "gravida-para" ]
            [ div [ class "gravida" ]
                [ div [ class "label" ] [ text <| translate language Translate.Gravida ]
                , div [ class "value" ] [ text gravida ]
                ]
            , div [ class "para" ]
                [ div [ class "label" ] [ text <| translate language Translate.Para ]
                , div [ class "para-breakdown" ]
                    [ div [ class "term" ]
                        [ div [] [ text <| String.slice 0 1 para ]
                        , div [ class "label small" ] [ text <| translate language Translate.Term ]
                        ]
                    , div [ class "pre-term" ]
                        [ div [] [ text <| String.slice 1 2 para ]
                        , div [ class "label small" ] [ text <| translate language Translate.PreTerm ]
                        ]
                    , div [ class "abortions" ]
                        [ div [] [ text <| String.slice 2 3 para ]
                        , div [ class "label small" ] [ text <| translate language Translate.Abortions ]
                        ]
                    , div [ class "live-children" ]
                        [ div [] [ text <| String.slice 3 4 para ]
                        , div [ class "label small" ] [ text <| translate language Translate.LiveChildren ]
                        ]
                    ]
                ]
            ]
        ]


viewRiskFactorsPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewRiskFactorsPane language currentDate measurements =
    let
        alerts =
            allRiskFactors
                |> List.filterMap (generateRiskFactorAlertData language currentDate measurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.RiskFactors ]
            ]
        , div [ class "pane-content" ] alerts
        ]


viewMedicalDiagnosisPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewMedicalDiagnosisPane language currentDate measurements =
    let
        alerts =
            allMedicalDiagnosis
                |> List.filterMap (generateMedicalDiagnosisAlertData language currentDate measurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "medical-diagnosis" ]
        [ viewItemHeading language Translate.MedicalDiagnosis "blue"
        , div [ class "pane-content" ] alerts
        ]


viewObstetricDiagnosisPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewObstetricDiagnosisPane language currentDate measurements =
    let
        alerts =
            allObstetricDiagnosis
                |> List.filterMap (generateObstetricDiagnosisAlertData language currentDate measurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "obstetric-diagnosis" ]
        [ viewItemHeading language Translate.ObstetricDiagnosis "blue"
        , div [ class "pane-content" ] alerts
        ]


viewPatientProgressPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewPatientProgressPane language currentDate measurements =
    let
        currentEncounterDate =
            currentDate

        -- Right now we have only the current encounter to display.
        encountersTrimestersData =
            [ ( currentEncounterDate
              , measurements.lastMenstrualPeriod
                    |> Maybe.map (Tuple.second >> .value >> .date)
              )
            ]
                |> List.map
                    (\( encounterDate, lmp ) ->
                        getEncounterTrimesterData encounterDate lmp
                    )

        countTrimesterEncounters trimester =
            encountersTrimestersData
                |> List.filter
                    (\t -> t == Just trimester)
                |> List.length

        encountersFirstTrimester =
            countTrimesterEncounters FirstTrimester

        encountersSecondTrimester =
            countTrimesterEncounters SecondTrimester

        encountersThirdTrimester =
            countTrimesterEncounters ThirdTrimester

        viewTrimesterTimeline trimester =
            let
                encounterIconWidth =
                    16

                ( dueDateIcon, extraClass ) =
                    if trimester == ThirdTrimester then
                        ( span [ class "due-date" ] [ img [ src "assets/images/icon-baby-due-date.png" ] [] ]
                        , " third"
                        )

                    else
                        ( emptyNode, "" )

                currentEncounterTrimester =
                    if encountersThirdTrimester > 0 then
                        ThirdTrimester

                    else if encountersSecondTrimester > 0 then
                        SecondTrimester

                    else
                        FirstTrimester

                periodWidth =
                    case trimester of
                        FirstTrimester ->
                            (180 - encounterIconWidth * encountersFirstTrimester) // (encountersFirstTrimester + 1)

                        SecondTrimester ->
                            (180 - encounterIconWidth * encountersSecondTrimester) // (encountersSecondTrimester + 1)

                        ThirdTrimester ->
                            (210 - encounterIconWidth * encountersThirdTrimester) // (encountersThirdTrimester + 1)

                trimesterPeriodsColors =
                    case trimester of
                        FirstTrimester ->
                            if currentEncounterTrimester == FirstTrimester then
                                List.repeat encountersFirstTrimester "blue" ++ [ "gray" ]

                            else
                                List.repeat (encountersFirstTrimester + 1) "blue"

                        SecondTrimester ->
                            if currentEncounterTrimester == SecondTrimester then
                                List.repeat encountersSecondTrimester "blue" ++ [ "gray" ]

                            else if currentEncounterTrimester == FirstTrimester then
                                List.repeat (encountersSecondTrimester + 1) "gray"

                            else
                                List.repeat (encountersSecondTrimester + 1) "blue"

                        ThirdTrimester ->
                            if currentEncounterTrimester == ThirdTrimester then
                                List.repeat encountersThirdTrimester "blue" ++ [ "gray" ]

                            else
                                List.repeat (encountersThirdTrimester + 1) "gray"
            in
            trimesterPeriodsColors
                |> List.map
                    (\color ->
                        p
                            [ class <| "period " ++ color
                            , style [ ( "width", toString periodWidth ++ "px" ) ]
                            ]
                            []
                    )
                |> List.intersperse
                    (span []
                        [ img
                            [ src "assets/images/icon-blue-circle.png"
                            , style [ ( "width", toString encounterIconWidth ++ "px" ) ]
                            ]
                            []
                        ]
                    )
                |> List.append [ dueDateIcon ]
                |> div [ class <| "trimester-timeline" ++ extraClass ]

        viewTrimesterVisits trimester =
            let
                ( expectedVisits, actualVisists, visitsLabel ) =
                    case trimester of
                        FirstTrimester ->
                            ( 1, encountersFirstTrimester, Translate.OneVisit )

                        SecondTrimester ->
                            ( 2, encountersSecondTrimester, Translate.TwoVisits )

                        ThirdTrimester ->
                            ( 5, encountersThirdTrimester, Translate.FiveVisits )

                actualVisists_ =
                    if actualVisists > expectedVisits then
                        expectedVisits

                    else
                        actualVisists

                missingVisits =
                    expectedVisits - actualVisists_

                visitsView =
                    List.repeat actualVisists_ "icon-checked-green-circle.png"
                        ++ List.repeat missingVisits "icon-gray-circle-small.png"
                        |> List.map (\icon -> img [ src <| "assets/images/" ++ icon ] [])
            in
            div [ class "trimester-visits" ]
                [ div [ class "label-trimester" ] [ text <| translate language <| Translate.PregnancyTrimester trimester ]
                , div [ class "details" ]
                    [ div [ class "label-visit" ] [ text <| translate language visitsLabel ]
                    , div [ class "visits" ] visitsView
                    ]
                ]
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ div [ class "caption timeline" ] [ text <| translate language Translate.ProgressTimeline ++ ":" ]
            , allTrimesters
                |> List.map viewTrimesterTimeline
                |> div [ class "timeline-section" ]
            , allTrimesters
                |> List.map viewTrimesterVisits
                |> div [ class "visits-section" ]
            ]
        ]
