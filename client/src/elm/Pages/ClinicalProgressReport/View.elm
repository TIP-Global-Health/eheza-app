module Pages.ClinicalProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (PrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Date
import Date.Extra exposing (Interval(Day))
import EveryDict
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (unwrap)
import Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.DemographicsReport.View exposing (viewHeader, viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Utils exposing (calculateBmi)
import Pages.PrenatalEncounter.Utils exposing (calculateEDDandEGADays, generateEDDandEGA, generateEGAWeeksDaysLabel, generateGravida, generatePara, getLmpMeasurement)
import Pages.Utils exposing (viewPhotoThumb)
import PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnosis
        , allObstetricalDiagnosis
        , allRiskFactors
        , allTrimesters
        )
import PrenatalActivity.Utils
    exposing
        ( generateMedicalDiagnosisAlertData
        , generateObstetricalDiagnosisAlertData
        , generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import RemoteData exposing (RemoteData(..), WebData)
import Round
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
    , participant : IndividualEncounterParticipant
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
                        EveryDict.get encounter.participant db.individualParticipants
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
        , viewObstetricalDiagnosisPane language currentDate data.measurements
        , viewPatientProgressPane language currentDate data.measurements
        ]


viewHeaderPane : Language -> NominalDate -> Person -> PrenatalMeasurements -> Html Msg
viewHeaderPane language currentDate mother measurements =
    let
        ( edd, ega ) =
            getLmpMeasurement measurements
                |> generateEDDandEGA language currentDate ( "--/--/----", "----" )

        obstetricHistoryValue =
            measurements.obstetricHistory
                |> Maybe.map (Tuple.second >> .value)

        ( gravida, para ) =
            unwrap
                ( "----", "----" )
                (\value ->
                    ( generateGravida value
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


viewObstetricalDiagnosisPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewObstetricalDiagnosisPane language currentDate measurements =
    let
        alerts =
            allObstetricalDiagnosis
                |> List.filterMap (generateObstetricalDiagnosisAlertData language currentDate measurements)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "obstetric-diagnosis" ]
        [ viewItemHeading language Translate.ObstetricalDiagnosis "blue"
        , div [ class "pane-content" ] alerts
        ]


viewPatientProgressPane : Language -> NominalDate -> PrenatalMeasurements -> Html Msg
viewPatientProgressPane language currentDate measurements =
    let
        allEncountersData =
            [ ( currentDate, measurements )
            ]

        allEncountersMeasurements =
            allEncountersData
                |> List.map Tuple.second

        encountersTrimestersData =
            allEncountersData
                |> List.map
                    (\( date, measurements ) ->
                        getLmpMeasurement measurements
                            |> getEncounterTrimesterData date
                    )

        countTrimesterEncounters trimester =
            encountersTrimestersData
                |> List.filter (\t -> t == Just trimester)
                |> List.length

        encountersFirstTrimester =
            countTrimesterEncounters FirstTrimester

        encountersSecondTrimester =
            countTrimesterEncounters SecondTrimester

        encountersThirdTrimester =
            countTrimesterEncounters ThirdTrimester

        fetalMovementsDetected =
            measurements.obstetricalExam
                |> Maybe.map
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .fetalMovement
                        in
                        value == True
                    )
                |> Maybe.withDefault False

        fetalHeartRateDetected =
            measurements.obstetricalExam
                |> Maybe.map
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .fetalHeartRate
                        in
                        value > 0
                    )
                |> Maybe.withDefault False

        ( eddLabel, fetalHeartRateLabel, fetalMovementsLabel ) =
            getLmpMeasurement measurements
                |> Maybe.map
                    (\lmpDate ->
                        let
                            ( eddDate_, diffDays ) =
                                calculateEDDandEGADays currentDate lmpDate

                            eddDate =
                                toLocalDateTime eddDate_ 12 0 0 0

                            egaWeeksDaysLabel =
                                generateEGAWeeksDaysLabel language diffDays
                        in
                        ( div [ class "due-date-label" ]
                            [ div [] [ text <| translate language Translate.DueDate ++ ":" ]
                            , div []
                                [ text <|
                                    (Date.day eddDate |> toString)
                                        ++ " "
                                        ++ translate language (Date.month eddDate |> Translate.ResolveMonth)
                                ]
                            ]
                        , if fetalHeartRateDetected then
                            div [ class "heart-rate-label" ]
                                [ span [] [ text <| translate language Translate.FetalHeartRate ++ ": " ]
                                , span [] [ text egaWeeksDaysLabel ]
                                ]

                          else
                            emptyNode
                        , if fetalMovementsDetected then
                            div [ class "movements-label" ]
                                [ span [] [ text <| translate language Translate.FetalMovement ++ ": " ]
                                , span [] [ text egaWeeksDaysLabel ]
                                ]

                          else
                            emptyNode
                        )
                    )
                |> Maybe.withDefault ( emptyNode, emptyNode, emptyNode )

        viewTrimesterTimeline trimester =
            let
                encounterIconWidth =
                    18

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

                fetalMovementsIcon =
                    span [ class "fetal-movements" ]
                        [ img
                            [ src "assets/images/icon-fetal-movement.png"
                            , style [ ( "height", "30px" ) ]
                            ]
                            []
                        ]

                fetalHeartRateIcon rightMargin =
                    span
                        [ class "fetal-heart-rate"
                        , style [ ( "margin-right", rightMargin ) ]
                        ]
                        [ img
                            [ src "assets/images/icon-fetal-heartrate.png"
                            , style [ ( "height", "30px" ) ]
                            ]
                            []
                        ]

                timelineIcons =
                    if fetalMovementsDetected && fetalHeartRateDetected then
                        div [ style [ ( "margin-left", "-25px" ), ( "width", "65px" ) ] ]
                            [ fetalHeartRateIcon "5px"
                            , fetalMovementsIcon
                            ]

                    else if fetalHeartRateDetected then
                        div [ style [ ( "margin-left", "-6px" ), ( "width", "35px" ) ] ] [ fetalHeartRateIcon "0" ]

                    else if fetalMovementsDetected then
                        div [ style [ ( "margin-left", "-2px" ), ( "width", "30px" ) ] ] [ fetalMovementsIcon ]

                    else
                        emptyNode

                dueDateInfo =
                    if trimester == ThirdTrimester then
                        div [ class "due-date-info" ]
                            [ span [ class "due-date-icon" ] [ img [ src "assets/images/icon-baby-due-date.png" ] [] ]
                            , eddLabel
                            ]

                    else
                        emptyNode
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
                    (span [ style [ ( "width", toString encounterIconWidth ++ "px" ) ] ]
                        [ img
                            [ src "assets/images/icon-blue-circle.png"
                            , style [ ( "width", toString encounterIconWidth ++ "px" ) ]
                            ]
                            []
                        , timelineIcons
                        ]
                    )
                |> List.append [ dueDateInfo ]
                |> div [ class "trimester-timeline" ]

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

        viewChartHeading transId =
            div [ class "chart-heading" ]
                [ img [ src <| "assets/images/icon-gray-circle-small.png" ] []
                , span [] [ text <| translate language transId ]
                ]

        egaBmiValues =
            allEncountersMeasurements
                |> List.filterMap
                    (\measurements ->
                        getLmpMeasurement measurements
                            |> Maybe.map
                                (\lmpDate ->
                                    let
                                        bmi =
                                            measurements.nutrition
                                                |> Maybe.map
                                                    (\measurement ->
                                                        let
                                                            height =
                                                                Tuple.second measurement
                                                                    |> .value
                                                                    |> .height
                                                                    |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)

                                                            weight =
                                                                Tuple.second measurement
                                                                    |> .value
                                                                    |> .weight
                                                                    |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                                        in
                                                        calculateBmi (Just height) (Just weight)
                                                            |> Maybe.withDefault 0
                                                    )
                                                |> Maybe.withDefault 0
                                    in
                                    ( diffDays lmpDate currentDate, bmi )
                                )
                    )

        egaFundalHeightValues =
            allEncountersMeasurements
                |> List.filterMap
                    (\measurements ->
                        getLmpMeasurement measurements
                            |> Maybe.map
                                (\lmpDate ->
                                    let
                                        fundalHeight =
                                            measurements.obstetricalExam
                                                |> Maybe.map
                                                    (\measurement ->
                                                        Tuple.second measurement
                                                            |> .value
                                                            |> .fundalHeight
                                                            |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                                    )
                                                |> Maybe.withDefault 0
                                    in
                                    ( diffDays lmpDate currentDate, fundalHeight )
                                )
                    )

        progressPhotos =
            allEncountersData
                |> List.filterMap
                    (\( date, measurements ) ->
                        measurements.prenatalPhoto
                            |> Maybe.map
                                (Tuple.second
                                    >> .value
                                    >> (\photoUrl ->
                                            let
                                                egaLabel =
                                                    getLmpMeasurement measurements
                                                        |> Maybe.map (\lmpDate -> diffDays lmpDate date |> generateEGAWeeksDaysLabel language)
                                                        |> Maybe.withDefault ""
                                            in
                                            div [ class "progress-photo" ]
                                                [ viewPhotoThumb photoUrl
                                                , div [ class "ega" ] [ text egaLabel ]
                                                ]
                                       )
                                )
                    )
                |> div [ class "photos-section" ]
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ div [ class "caption timeline" ] [ text <| translate language Translate.ProgressTimeline ++ ":" ]
            , div [ class "timeline-section" ]
                [ div [ class "indicators" ]
                    [ fetalHeartRateLabel
                    , fetalMovementsLabel
                    ]
                , allTrimesters
                    |> List.map viewTrimesterTimeline
                    |> div [ class "timeline" ]
                ]
            , allTrimesters
                |> List.map viewTrimesterVisits
                |> div [ class "visits-section" ]
            , div [ class "caption photos" ] [ text <| translate language Translate.ProgressPhotos ++ ":" ]
            , progressPhotos
            , div [ class "caption trends" ] [ text <| translate language Translate.ProgressTrends ++ ":" ]
            , div [ class "trends-section" ]
                [ viewMarkers
                , div [ class "bmi-info" ]
                    [ viewChartHeading Translate.BMI
                    , heightWeightBMITable language currentDate allEncountersMeasurements
                    , viewBMIForEGA language egaBmiValues
                    , illustrativePurposes language
                    ]
                , div [ class "fundal-height-info" ]
                    [ viewChartHeading Translate.FundalHeight
                    , fundalHeightTable language currentDate allEncountersMeasurements
                    , viewFundalHeightForEGA language egaFundalHeightValues
                    , illustrativePurposes language
                    ]
                ]
            ]
        ]


tableEgaHeading : Language -> NominalDate -> List PrenatalMeasurements -> Html any
tableEgaHeading language currentDate measurements =
    measurements
        |> List.map
            (getLmpMeasurement
                >> Maybe.map
                    (\lmpDate ->
                        diffDays lmpDate currentDate
                            |> generateEGAWeeksDaysLabel language
                            |> String.toLower
                            |> text
                            |> List.singleton
                    )
                >> Maybe.withDefault [ text "--" ]
                >> th
                    [ classList
                        [ ( "center", True )
                        , ( "bottom", True )
                        , ( "aligned", True )
                        , ( "ega-header", True )
                        ]
                    ]
            )
        |> (::)
            (th
                [ class "uppercase" ]
                [ text <| translate language Translate.Ega ]
            )
        |> tr []


heightWeightBMITable : Language -> NominalDate -> List PrenatalMeasurements -> Html any
heightWeightBMITable language currentDate allEncounetrsMeasurements =
    let
        cell language transId =
            td [ class "uppercase" ]
                [ text <| translate language transId ]
    in
    allEncounetrsMeasurements
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (.nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| toString height ++ translate language Translate.CentimeterShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Height)
                            |> tr []

                    weights =
                        groupOfSix
                            |> List.map
                                (.nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                            in
                                            [ text <| toString weight ++ translate language Translate.KilogramShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Weight)
                            |> tr []

                    bmis =
                        groupOfSix
                            |> List.map
                                (.nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)

                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)

                                                bmi =
                                                    calculateBmi (Just height) (Just weight)
                                                        |> Maybe.withDefault 0
                                                        |> Round.round 1
                                            in
                                            [ text bmi ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.BMI)
                            |> tr []
                in
                [ egas
                , heights
                , weights
                , bmis
                ]
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


fundalHeightTable : Language -> NominalDate -> List PrenatalMeasurements -> Html any
fundalHeightTable language currentDate allEncounetrsMeasurements =
    let
        cell language transId =
            td [ class "uppercase" ]
                [ text <| translate language transId ]
    in
    allEncounetrsMeasurements
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (.obstetricalExam
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .fundalHeight
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| toString height ++ translate language Translate.CentimeterShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.FundalHeight)
                            |> tr []
                in
                [ egas
                , heights
                ]
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "ui collapsing celled table" ]


illustrativePurposes : Language -> Html any
illustrativePurposes language =
    div [ class "illustrative-purposes" ] [ text <| translate language Translate.ForIllustrativePurposesOnly ]
