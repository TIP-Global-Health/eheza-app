module Pages.ClinicalProgressReport.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (PrenatalMeasurements, PrenatalTestExecutionNote(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnosis
        , allObstetricalDiagnosis
        , allRiskFactors
        , allTrimesters
        )
import Backend.PrenatalActivity.Utils
    exposing
        ( generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import Backend.PrenatalEncounter.Model exposing (ClinicalProgressReportInitiator(..), PrenatalEncounter)
import Date exposing (Interval(..))
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.ClinicalProgressReport.Model exposing (..)
import Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (LaboratoryTask(..))
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.Utils exposing (viewPhotoThumbFromPhotoUrl)
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (thumbnailImage)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> ClinicalProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        allowBackAction =
            initiator == InitiatorEncounterPage

        data =
            generateAssembledData id db

        header =
            viewHeader language id Translate.ClinicalProgressReport allowBackAction

        content =
            viewWebData language (viewContent language currentDate isChw initiator model) identity data
    in
    div [ class "page-clinical-progress-report" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalEncounterId -> TranslationId -> Bool -> Html Msg
viewHeader language prenatalEncounterId label allowBackAction =
    let
        backIcon =
            if allowBackAction then
                span
                    [ class "icon-back"
                    , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage prenatalEncounterId
                    ]
                    []

            else
                emptyNode
    in
    div
        [ class "ui basic segment head" ]
        [ backIcon
        , h1 [ class "ui header" ]
            [ text <| translate language label ]
        ]


viewContent : Language -> NominalDate -> Bool -> ClinicalProgressReportInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate isChw initiator model data =
    let
        derivedContent =
            case model.labResultsHistoryMode of
                Just mode ->
                    [ viewLabResultsHistoryPane language currentDate mode ]

                Nothing ->
                    let
                        firstEncounterMeasurements =
                            getFirstEncounterMeasurements isChw data

                        actions =
                            case initiator of
                                InitiatorEncounterPage ->
                                    emptyNode

                                InitiatorNewEncounter encounterId ->
                                    div [ class "actions" ]
                                        [ button
                                            [ class "ui fluid primary button"
                                            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage encounterId
                                            ]
                                            [ text <| translate language Translate.Reviewed ]
                                        ]
                    in
                    [ viewRiskFactorsPane language currentDate firstEncounterMeasurements
                    , viewMedicalDiagnosisPane language currentDate firstEncounterMeasurements
                    , viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements data
                    , viewPatientProgressPane language currentDate isChw data
                    , viewLabResultsPane language currentDate data
                    , actions
                    ]
    in
    div [ class "ui unstackable items" ] <|
        viewHeaderPane language currentDate data
            :: derivedContent


viewHeaderPane : Language -> NominalDate -> AssembledData -> Html Msg
viewHeaderPane language currentDate data =
    let
        mother =
            data.person

        ( edd, ega ) =
            data.globalLmpDate
                |> generateEDDandEGA language currentDate ( "--/--/----", "----" )

        obstetricHistoryValue =
            data.globalObstetricHistory

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


viewObstetricalDiagnosisPane : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> Html Msg
viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements data =
    let
        alerts =
            allObstetricalDiagnosis
                |> List.filterMap (generateObstetricalDiagnosisAlertData language currentDate isChw firstEncounterMeasurements data)
                |> List.map (\alert -> p [] [ text <| "- " ++ alert ])
    in
    div [ class "obstetric-diagnosis" ]
        [ viewItemHeading language Translate.ObstetricalDiagnosis "blue"
        , div [ class "pane-content" ] alerts
        ]


viewPatientProgressPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewPatientProgressPane language currentDate isChw data =
    let
        allMeasurementsWithDates =
            data.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, data.measurements ) ]
                   )

        allMeasurements =
            allMeasurementsWithDates
                |> List.map Tuple.second

        encountersTrimestersData =
            allMeasurementsWithDates
                |> List.map
                    (\( date, _ ) ->
                        ( date
                        , getEncounterTrimesterData date data.globalLmpDate
                        )
                    )

        getTrimesterEncounters trimester =
            encountersTrimestersData
                |> List.filter (\t -> Tuple.second t == Just trimester)
                |> List.map Tuple.first

        encountersFirstTrimester =
            getTrimesterEncounters FirstTrimester

        encountersFirstTrimesterCount =
            List.length encountersFirstTrimester

        encountersSecondTrimester =
            getTrimesterEncounters SecondTrimester

        encountersSecondTrimesterCount =
            List.length encountersSecondTrimester

        encountersThirdTrimester =
            getTrimesterEncounters ThirdTrimester

        encountersThirdTrimesterCount =
            List.length encountersThirdTrimester

        fetalMovementsDate =
            allMeasurementsWithDates
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalMovement >> (==) True)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        fetalHeartRateDate =
            allMeasurementsWithDates
                |> List.filter
                    (\( _, measurements ) ->
                        measurements.obstetricalExam
                            |> Maybe.map (Tuple.second >> .value >> .fetalHeartRate >> (<) 0)
                            |> Maybe.withDefault False
                    )
                |> List.head
                |> Maybe.map Tuple.first

        egaWeeksDaysLabel language_ encounterDate lmpDate =
            let
                diffInDays =
                    diffDays lmpDate encounterDate
            in
            generateEGAWeeksDaysLabel language_ diffInDays

        ( eddLabel, fetalHeartRateLabel, fetalMovementsLabel ) =
            data.globalLmpDate
                |> Maybe.map
                    (\lmpDate ->
                        let
                            eddDate =
                                calculateEDD lmpDate
                        in
                        ( div [ class "due-date-label" ]
                            [ div [] [ text <| translate language Translate.DueDate ++ ":" ]
                            , div []
                                [ text <|
                                    (Date.day eddDate |> String.fromInt)
                                        ++ " "
                                        ++ translate language (Translate.ResolveMonth False (Date.month eddDate))
                                ]
                            ]
                        , fetalHeartRateDate
                            |> Maybe.map
                                (\date ->
                                    div [ class "heart-rate-label" ]
                                        [ span [] [ text <| translate language Translate.FetalHeartRate ++ ": " ]
                                        , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode
                        , fetalMovementsDate
                            |> Maybe.map
                                (\date ->
                                    div [ class "movements-label" ]
                                        [ span [] [ text <| translate language Translate.FetalMovement ++ ": " ]
                                        , span [] [ egaWeeksDaysLabel language date lmpDate |> text ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode
                        )
                    )
                |> Maybe.withDefault ( emptyNode, emptyNode, emptyNode )

        viewTrimesterTimeline trimester =
            let
                encounterIconWidth =
                    18

                currentEncounterTrimester =
                    if encountersThirdTrimesterCount > 0 then
                        ThirdTrimester

                    else if encountersSecondTrimesterCount > 0 then
                        SecondTrimester

                    else
                        FirstTrimester

                periodWidth =
                    case trimester of
                        FirstTrimester ->
                            (180 - encounterIconWidth * encountersFirstTrimesterCount) // (encountersFirstTrimesterCount + 1)

                        SecondTrimester ->
                            (180 - encounterIconWidth * encountersSecondTrimesterCount) // (encountersSecondTrimesterCount + 1)

                        ThirdTrimester ->
                            (210 - encounterIconWidth * encountersThirdTrimesterCount) // (encountersThirdTrimesterCount + 1)

                ( trimesterPeriodsColors, trimesterEncountersDates ) =
                    case trimester of
                        FirstTrimester ->
                            ( if currentEncounterTrimester == FirstTrimester then
                                List.repeat encountersFirstTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersFirstTrimesterCount + 1) "blue"
                            , encountersFirstTrimester
                            )

                        SecondTrimester ->
                            ( if currentEncounterTrimester == SecondTrimester then
                                List.repeat encountersSecondTrimesterCount "blue" ++ [ "gray" ]

                              else if currentEncounterTrimester == FirstTrimester then
                                List.repeat (encountersSecondTrimesterCount + 1) "gray"

                              else
                                List.repeat (encountersSecondTrimesterCount + 1) "blue"
                            , encountersSecondTrimester
                            )

                        ThirdTrimester ->
                            ( if currentEncounterTrimester == ThirdTrimester then
                                List.repeat encountersThirdTrimesterCount "blue" ++ [ "gray" ]

                              else
                                List.repeat (encountersThirdTrimesterCount + 1) "gray"
                            , encountersThirdTrimester
                            )

                fetalMovementsIcon =
                    span [ class "fetal-movements" ]
                        [ img
                            [ src "assets/images/icon-fetal-movement.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                fetalHeartRateIcon rightMargin =
                    span
                        [ class "fetal-heart-rate"
                        , style "margin-right" rightMargin
                        ]
                        [ img
                            [ src "assets/images/icon-fetal-heartrate.png"
                            , style "height" "30px"
                            ]
                            []
                        ]

                dueDateInfo =
                    if trimester == ThirdTrimester then
                        div [ class "due-date-info" ]
                            [ span [ class "due-date-icon" ] [ img [ src "assets/images/icon-baby-due-date.png" ] [] ]
                            , eddLabel
                            ]

                    else
                        emptyNode

                trimesterPeriods =
                    trimesterPeriodsColors
                        |> List.map
                            (\color ->
                                p
                                    [ class <| "period " ++ color
                                    , style "width" (String.fromInt periodWidth ++ "px")
                                    ]
                                    []
                            )

                timelineIcons date =
                    if fetalMovementsDate == Just date && fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-25px", style "width" "65px" ]
                            [ fetalHeartRateIcon "5px"
                            , fetalMovementsIcon
                            ]

                    else if fetalHeartRateDate == Just date then
                        div [ style "margin-left" "-6px", style "width" "35px" ] [ fetalHeartRateIcon "0" ]

                    else if fetalMovementsDate == Just date then
                        div [ style "margin-left" "-2px", style "width" "30px" ] [ fetalMovementsIcon ]

                    else
                        emptyNode

                trimesterEncounters =
                    trimesterEncountersDates
                        |> List.map
                            (\date ->
                                span [ style "width" (String.fromInt encounterIconWidth ++ "px") ]
                                    [ img
                                        [ src "assets/images/icon-blue-circle.png"
                                        , style "width" (String.fromInt encounterIconWidth ++ "px")
                                        ]
                                        []
                                    , timelineIcons date
                                    ]
                            )
            in
            List.Extra.interweave trimesterPeriods trimesterEncounters
                |> List.append [ dueDateInfo ]
                |> div [ class "trimester-timeline" ]

        viewTrimesterVisits trimester =
            let
                ( expectedVisits, actualVisists, visitsLabel ) =
                    case trimester of
                        FirstTrimester ->
                            ( 1, encountersFirstTrimesterCount, Translate.OneVisit )

                        SecondTrimester ->
                            ( 2, encountersSecondTrimesterCount, Translate.TwoVisits )

                        ThirdTrimester ->
                            ( 5, encountersThirdTrimesterCount, Translate.FiveVisits )

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
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        data.globalLmpDate
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
                                    ( diffDays lmpDate date, bmi )
                                )
                    )

        egaFundalHeightValues =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        data.globalLmpDate
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
                                    ( diffDays lmpDate date, fundalHeight )
                                )
                    )

        progressPhotos =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date, measurements ) ->
                        measurements.prenatalPhoto
                            |> Maybe.map
                                (Tuple.second
                                    >> .value
                                    >> (\photoUrl ->
                                            let
                                                egaLabel =
                                                    data.globalLmpDate
                                                        |> Maybe.map (\lmpDate -> diffDays lmpDate date |> generateEGAWeeksDaysLabel language)
                                                        |> Maybe.withDefault ""
                                            in
                                            div [ class "progress-photo" ]
                                                [ viewPhotoThumbFromPhotoUrl photoUrl
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
                    , heightWeightBMITable language currentDate data.globalLmpDate allMeasurementsWithDates
                    , viewBMIForEGA language egaBmiValues
                    , illustrativePurposes language
                    ]
                , div [ class "fundal-height-info" ]
                    [ viewChartHeading Translate.FundalHeight
                    , fundalHeightTable language currentDate data.globalLmpDate allMeasurementsWithDates
                    , viewFundalHeightForEGA language egaFundalHeightValues
                    , illustrativePurposes language
                    ]
                ]
            ]
        ]


tableEgaHeading : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
tableEgaHeading language currentDate maybeLmpDate measurementsWithDates =
    measurementsWithDates
        |> List.map
            (\( date, measurements ) ->
                maybeLmpDate
                    |> Maybe.map
                        (\lmpDate ->
                            diffDays lmpDate date
                                |> generateEGAWeeksDaysLabel language
                                |> String.toLower
                                |> text
                                |> List.singleton
                        )
                    |> Maybe.withDefault [ text "--" ]
                    |> th
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


heightWeightBMITable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
heightWeightBMITable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .height
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| String.fromFloat height ++ translate language Translate.CentimeterShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Height)
                            |> tr []

                    weights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                weight =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .weight
                                                        |> (\(Backend.Measurement.Model.WeightInKg kg) -> kg)
                                            in
                                            [ text <| String.fromFloat weight ++ translate language Translate.KilogramShorthand ]
                                        )
                                    >> Maybe.withDefault [ text "--" ]
                                    >> td [ class "center aligned" ]
                                )
                            |> (::) (cell language Translate.Weight)
                            |> tr []

                    bmis =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .nutrition
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


fundalHeightTable : Language -> NominalDate -> Maybe NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Html any
fundalHeightTable language currentDate maybeLmpDate allMeasurementsWithDates =
    let
        cell language_ transId =
            td [ class "uppercase" ]
                [ text <| translate language_ transId ]
    in
    allMeasurementsWithDates
        |> greedyGroupsOf 6
        |> List.map
            (\groupOfSix ->
                let
                    egas =
                        tableEgaHeading language currentDate maybeLmpDate groupOfSix

                    heights =
                        groupOfSix
                            |> List.map
                                (Tuple.second
                                    >> .obstetricalExam
                                    >> Maybe.map
                                        (\measurement ->
                                            let
                                                height =
                                                    Tuple.second measurement
                                                        |> .value
                                                        |> .fundalHeight
                                                        |> (\(Backend.Measurement.Model.HeightInCm cm) -> cm)
                                            in
                                            [ text <| String.fromFloat height ++ translate language Translate.CentimeterShorthand ]
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


viewLabResultsPane : Language -> NominalDate -> AssembledData -> Html Msg
viewLabResultsPane language currentDate assembled =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "name" ] [ translateText language Translate.TestName ]
                , div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                ]

        groupHeading label =
            div [ class "group-heading" ]
                [ text <| translate language label ]

        measurementsWithLabResults =
            assembled.measurements
                :: List.map Tuple.second assembled.nursePreviousMeasurementsWithDates

        getTestResults getMeasurementFunc getResultFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                measurementsWithLabResults
                |> List.filterMap
                    (\value ->
                        if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        getStandardTestResults getMeasurementFunc =
            getTestResults getMeasurementFunc .testResult

        hivTestResults =
            getStandardTestResults .hivTest

        syphilisTestResults =
            getStandardTestResults .syphilisTest

        hepatitisBTestResults =
            getStandardTestResults .hepatitisBTest

        malariaTestResults =
            getStandardTestResults .malariaTest

        groupOneEntry label resultWithDate =
            let
                ( dateText, resultText ) =
                    Maybe.map
                        (\( date, result ) ->
                            ( formatDDMMYYYY date
                            , Maybe.map (Translate.PrenatalTestResult >> translate language) result
                            )
                        )
                        resultWithDate
                        |> Maybe.withDefault ( "", Just "" )
            in
            viewEntry label dateText resultText

        groupOneContent =
            [ groupOneEntry (Translate.PrenatalLaboratoryTaskLabel TaskHIVTest) (List.head hivTestResults)
            , groupOneEntry (Translate.PrenatalLaboratoryTaskLabel TaskSyphilisTest) (List.head syphilisTestResults)
            , groupOneEntry (Translate.PrenatalLaboratoryTaskLabel TaskHepatitisBTest) (List.head hepatitisBTestResults)
            , groupOneEntry (Translate.PrenatalLaboratoryTaskLabel TaskMalariaTest) (List.head malariaTestResults)
            ]

        urineDipstickTestResults =
            List.filterMap (.urineDipstickTest >> getMeasurementValueFunc)
                measurementsWithLabResults
                |> List.filterMap
                    (\value ->
                        if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, ( value.protein, value.ph, value.glucose ) ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        groupTwoContent =
            List.head urineDipstickTestResults
                |> Maybe.map
                    (\( date, ( protein, ph, glucose ) ) ->
                        let
                            proteinResult =
                                Maybe.map (Translate.PrenatalLaboratoryProteinValue >> translate language) protein

                            phResult =
                                Maybe.map (Translate.PrenatalLaboratoryPHValue >> translate language) ph

                            glucoseResult =
                                Maybe.map (Translate.PrenatalLaboratoryGlucoseValue >> translate language) glucose

                            testDate =
                                formatDDMMYYYY date
                        in
                        [ viewEntry Translate.PrenatalLaboratoryProteinLabel testDate proteinResult
                        , viewEntry Translate.PrenatalLaboratoryPHLabel testDate phResult
                        , viewEntry Translate.PrenatalLaboratoryGlucoseLabel testDate glucoseResult
                        ]
                    )
                |> Maybe.withDefault
                    [ viewEntry Translate.PrenatalLaboratoryProteinLabel "" (Just "")
                    , viewEntry Translate.PrenatalLaboratoryPHLabel "" (Just "")
                    , viewEntry Translate.PrenatalLaboratoryGlucoseLabel "" (Just "")
                    ]

        randomBloodSugarResults =
            getTestResults .randomBloodSugarTest .sugarCount

        hemoglobinResults =
            getTestResults .hemoglobinTest .hemoglobinCount

        bloodGpRsResults =
            List.filterMap (.bloodGpRsTest >> getMeasurementValueFunc)
                measurementsWithLabResults
                |> List.filterMap
                    (\value ->
                        if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, ( value.bloodGroup, value.rhesus ) ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        groupThreeContent =
            let
                randomBloodSugarEntry =
                    let
                        ( dateText, resultText ) =
                            Maybe.map
                                (\( date, result ) ->
                                    ( formatDDMMYYYY date
                                    , Maybe.map String.fromFloat result
                                    )
                                )
                                (List.head randomBloodSugarResults)
                                |> Maybe.withDefault ( "", Just "" )
                    in
                    viewEntry (Translate.PrenatalLaboratoryTaskLabel TaskRandomBloodSugarTest) dateText resultText

                hemoglobinEntry =
                    let
                        ( dateText, resultText ) =
                            Maybe.map
                                (\( date, result ) ->
                                    ( formatDDMMYYYY date
                                    , Maybe.map String.fromFloat result
                                    )
                                )
                                (List.head hemoglobinResults)
                                |> Maybe.withDefault ( "", Just "" )
                    in
                    viewEntry (Translate.PrenatalLaboratoryTaskLabel TaskHemoglobinTest) dateText resultText

                gpRsEntries =
                    List.head bloodGpRsResults
                        |> Maybe.map
                            (\( date, ( bloodGroup, rhesus ) ) ->
                                let
                                    bloodGroupResult =
                                        Maybe.map (Translate.PrenatalLaboratoryBloodGroup >> translate language) bloodGroup

                                    rhesusResult =
                                        Maybe.map (Translate.PrenatalLaboratoryRhesus >> translate language) rhesus

                                    testDate =
                                        formatDDMMYYYY date
                                in
                                [ viewEntry Translate.PrenatalLaboratoryBloodGroupLabel testDate bloodGroupResult
                                , viewEntry Translate.PrenatalLaboratoryRhesusLabel testDate rhesusResult
                                ]
                            )
                        |> Maybe.withDefault
                            [ viewEntry Translate.PrenatalLaboratoryBloodGroupLabel "" (Just "")
                            , viewEntry Translate.PrenatalLaboratoryRhesusLabel "" (Just "")
                            ]
            in
            [ randomBloodSugarEntry
            , hemoglobinEntry
            ]
                ++ gpRsEntries

        viewEntry label date maybeResult =
            let
                resultCell =
                    Maybe.map (\result -> text result) maybeResult
                        |> Maybe.withDefault (span [ class "pending" ] [ translateText language Translate.ResultsPending ])
            in
            div [ class "entry" ]
                [ div [ class "name" ] [ translateText language label ]
                , div [ class "date" ] [ text date ]
                , div [ class "result" ] [ resultCell ]
                ]
    in
    div [ class "lab-results" ] <|
        [ viewItemHeading language Translate.LabResults "blue"
        , div [ class "pane-content" ] [ heading ]
        , groupHeading Translate.GroupOne
        , div [ class "group-content" ]
            groupOneContent
        , groupHeading Translate.GroupTwo
        , div [ class "group-content" ]
            groupTwoContent
        , groupHeading Translate.GroupThree
        , div [ class "group-content" ]
            groupThreeContent
        ]


viewLabResultsHistoryPane : Language -> NominalDate -> LabResultsHistoryMode -> Html Msg
viewLabResultsHistoryPane language currentDate mode =
    text "viewLabResultsHistoryPane"
