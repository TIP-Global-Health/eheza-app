module Pages.ClinicalProgressReport.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (PrenatalLaboratoryTest(..), PrenatalMeasurements, PrenatalTestExecutionNote(..), PrenatalTestVariant(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, prenatalLabExpirationPeriod)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model exposing (PregnancyTrimester(..), PrenatalActivity(..), allMedicalDiagnosis, allObstetricalDiagnosis, allRiskFactors, allTrimesters)
import Backend.PrenatalActivity.Utils
    exposing
        ( generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..), PrenatalProgressReportInitiator(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Interval(..), Unit(..))
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, unwrap)
import Measurement.View exposing (viewActionTakenLabel)
import Pages.ClinicalProgressReport.Model exposing (..)
import Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Types exposing (LaboratoryTask(..))
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


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> PrenatalProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        data =
            generateAssembledData id db

        header =
            viewHeader language id initiator model

        content =
            viewWebData language (viewContent language currentDate isChw initiator model) identity data
    in
    div [ class "page-report clinical" ] <|
        [ header
        , content
        ]


viewHeader : Language -> PrenatalEncounterId -> PrenatalProgressReportInitiator -> Model -> Html Msg
viewHeader language id initiator model =
    let
        label =
            if isJust model.labResultsHistoryMode then
                Translate.LabHistory

            else
                Translate.AntenatalProgressReport

        backIcon =
            let
                iconForView goBackPage =
                    let
                        action =
                            if isJust model.labResultsHistoryMode then
                                SetLabResultsHistoryMode Nothing

                            else
                                SetActivePage <| UserPage goBackPage
                    in
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick action
                            ]
                            []
                        ]
            in
            case initiator of
                InitiatorEncounterPage prenatalEncounterId ->
                    iconForView (PrenatalEncounterPage prenatalEncounterId)

                InitiatorNewEncounter _ ->
                    emptyNode

                Backend.PrenatalEncounter.Model.InitiatorPatientRecord patientId ->
                    iconForView (PatientRecordPage InitiatorParticipantDirectory patientId)
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> Bool -> PrenatalProgressReportInitiator -> Model -> AssembledData -> Html Msg
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
                                InitiatorEncounterPage _ ->
                                    emptyNode

                                InitiatorNewEncounter encounterId ->
                                    div [ class "actions" ]
                                        [ button
                                            [ class "ui fluid primary button"
                                            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage encounterId
                                            ]
                                            [ text <| translate language Translate.Reviewed ]
                                        ]

                                Backend.PrenatalEncounter.Model.InitiatorPatientRecord _ ->
                                    emptyNode
                    in
                    [ viewRiskFactorsPane language currentDate firstEncounterMeasurements
                    , viewMedicalDiagnosisPane language currentDate firstEncounterMeasurements
                    , viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements data
                    , viewChwActivityPane language currentDate isChw data
                    , viewPatientProgressPane language currentDate isChw data
                    , viewLabResultsPane language currentDate data
                    , viewProgressPhotosPane language currentDate isChw data
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
        [ viewItemHeading language Translate.PatientInformation "blue"
        , div [ class "pane-content" ]
            [ div [ class "mother-details" ]
                [ div [ class "ui image" ]
                    [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
                , div [ class "content middle" ]
                    [ p [ class "mother-name" ] [ text mother.name ]
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


viewChwActivityPane : Language -> NominalDate -> Bool -> AssembledData -> Html msg
viewChwActivityPane language currentDate isChw data =
    let
        allMeasurementsWithDates =
            data.chwPreviousMeasurementsWithDates

        pregnancyDatingAction =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date_, _, measurements ) ->
                        Just measurements
                    )
                |> List.any (.lastMenstrualPeriod >> isJust)

        sentToHCActivity =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date_, _, measurements ) ->
                        Just measurements
                    )
                |> List.any (.sendToHC >> isJust)

        birthPlan =
            allMeasurementsWithDates
                |> List.filterMap
                    (\( date_, _, measurements ) ->
                        Just measurements
                    )
                |> List.any (.birthPlan >> isJust)

        activity =
            if sentToHCActivity || pregnancyDatingAction || birthPlan then
                [ li [ class "activity-red" ] [ text "Referred Health Center" ]
                , li [ class "activity-blue" ] [ text "Pregnancy Dating" ]
                ]

            else
                [ emptyNode ]

        activities =
            div [ class "entry" ]
                [ div [ style "color" "black", class "cell date" ] [ text <| formatDDMMYYYY currentDate ]
                , div [ class <| "cell activity" ] [ ul [] activity ]
                ]

        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ translateText language Translate.Date ]
                , div [ class "activity" ] [ translateText language Translate.Actions ]
                ]
    in
    div [ class "chw-activity" ] <|
        [ viewItemHeading language Translate.ChwActivity "blue"
        , div [ class "pane-content" ] [ heading ]
        , activities
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
                                lmpToEDDDate lmpDate
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

        groupOneContent =
            [ viewLabResultsEntry language currentDate (LabResultsHistoryHIV hivTestResults)
            , viewLabResultsEntry language currentDate (LabResultsHistorySyphilis syphilisTestResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryHepatitisB hepatitisBTestResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryMalaria malariaTestResults)
            ]

        urineDipstickTestValues =
            List.filterMap (.urineDipstickTest >> getMeasurementValueFunc)
                measurementsWithLabResults

        urineDipstickTestResults =
            List.filterMap
                (\value ->
                    if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                        Maybe.map (\executionDate -> ( executionDate, ( value.protein, value.ph, value.glucose ) ))
                            value.executionDate

                    else
                        Nothing
                )
                urineDipstickTestValues
                |> List.sortWith sortTuplesByDateDesc

        longUrineDipstickTestResults =
            List.filterMap
                (\value ->
                    if value.testVariant == Just VariantLongTest && List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                        Maybe.map (\executionDate -> ( executionDate, value ))
                            value.executionDate

                    else
                        Nothing
                )
                urineDipstickTestValues
                |> List.sortWith sortTuplesByDateDesc

        proteinResults =
            List.map (\( date, ( protein, _, _ ) ) -> ( date, protein )) urineDipstickTestResults

        phResults =
            List.map (\( date, ( _, ph, _ ) ) -> ( date, ph )) urineDipstickTestResults

        glucoseResults =
            List.map (\( date, ( _, _, glucose ) ) -> ( date, glucose )) urineDipstickTestResults

        leukocytesResults =
            List.map (\( date, value ) -> ( date, value.leukocytes )) longUrineDipstickTestResults

        nitriteResults =
            List.map (\( date, value ) -> ( date, value.nitrite )) longUrineDipstickTestResults

        urobilinogenResults =
            List.map (\( date, value ) -> ( date, value.urobilinogen )) longUrineDipstickTestResults

        haemoglobinResults =
            List.map (\( date, value ) -> ( date, value.haemoglobin )) longUrineDipstickTestResults

        specificGravityResults =
            List.map (\( date, value ) -> ( date, value.specificGravity )) longUrineDipstickTestResults

        ketoneResults =
            List.map (\( date, value ) -> ( date, value.ketone )) longUrineDipstickTestResults

        bilirubinResults =
            List.map (\( date, value ) -> ( date, value.bilirubin )) longUrineDipstickTestResults

        groupTwoContent =
            [ viewLabResultsEntry language currentDate (LabResultsHistoryProtein proteinResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryPH phResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryGlucose glucoseResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryLeukocytes leukocytesResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryNitrite nitriteResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryUrobilinogen urobilinogenResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryHaemoglobin haemoglobinResults)
            , viewLabResultsEntry language currentDate (LabResultsHistorySpecificGravity specificGravityResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryKetone ketoneResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryBilirubin bilirubinResults)
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

        bloodGroupResults =
            List.map (\( date, ( bloodGroup, _ ) ) -> ( date, bloodGroup )) bloodGpRsResults

        rhesusResults =
            List.map (\( date, ( _, rhesus ) ) -> ( date, rhesus )) bloodGpRsResults

        groupThreeContent =
            [ viewLabResultsEntry language currentDate (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryHemoglobin hemoglobinResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryBloodGroup bloodGroupResults)
            , viewLabResultsEntry language currentDate (LabResultsHistoryRhesus rhesusResults)
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


viewLabResultsEntry : Language -> NominalDate -> LabResultsHistoryMode -> Html Msg
viewLabResultsEntry language currentDate results =
    let
        config =
            case results of
                LabResultsHistoryHIV data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHIVTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalTestResult >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistorySyphilis data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskSyphilisTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalTestResult >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryHepatitisB data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHepatitisBTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalTestResult >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryMalaria data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskMalariaTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalTestResult >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryProtein data ->
                    { label = Translate.PrenatalLaboratoryProteinLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryProteinValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryPH data ->
                    { label = Translate.PrenatalLaboratoryPHLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryPHValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryGlucose data ->
                    { label = Translate.PrenatalLaboratoryGlucoseLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryGlucoseValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryLeukocytes data ->
                    { label = Translate.PrenatalLaboratoryGlucoseLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryLeukocytesValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryNitrite data ->
                    { label = Translate.PrenatalLaboratoryNitriteLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryNitriteValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryUrobilinogen data ->
                    { label = Translate.PrenatalLaboratoryUrobilinogenLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryHaemoglobin data ->
                    { label = Translate.PrenatalLaboratoryHaemoglobinLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistorySpecificGravity data ->
                    { label = Translate.PrenatalLaboratorySpecificGravityLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratorySpecificGravityValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryKetone data ->
                    { label = Translate.PrenatalLaboratoryKetoneLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryKetoneValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryBilirubin data ->
                    { label = Translate.PrenatalLaboratoryBilirubinLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryBilirubinValue >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryRandomBloodSugar data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskRandomBloodSugarTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map String.fromFloat
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryHemoglobin data ->
                    { label = Translate.PrenatalLaboratoryTaskLabel TaskHemoglobinTest
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map String.fromFloat
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryBloodGroup data ->
                    { label = Translate.PrenatalLaboratoryBloodGroupLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryBloodGroup >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

                LabResultsHistoryRhesus data ->
                    { label = Translate.PrenatalLaboratoryRhesusLabel
                    , recentResult = List.head data |> Maybe.andThen Tuple.second |> Maybe.map (Translate.PrenatalLaboratoryRhesus >> translate language)
                    , recentResultDate = List.head data |> Maybe.map Tuple.first
                    , totalResults = List.length data
                    }

        dateCell =
            Maybe.map (formatDDMMYYYY >> text) config.recentResultDate
                |> Maybe.withDefault (text "")

        resultCell =
            if config.totalResults == 0 then
                text ""

            else
                Maybe.map text config.recentResult
                    |> Maybe.withDefault (viewUncompetedResult language currentDate config.recentResultDate)

        historyResultsIcon =
            if config.totalResults > 1 then
                div
                    [ class "icon-forward"
                    , onClick <| SetLabResultsHistoryMode <| Just results
                    ]
                    []

            else
                emptyNode
    in
    div [ class "entry" ]
        [ div [ class "name" ] [ translateText language config.label ]
        , div [ class "date" ] [ dateCell ]
        , div [ class "result" ] [ resultCell ]
        , historyResultsIcon
        ]


viewUncompetedResult : Language -> NominalDate -> Maybe NominalDate -> Html any
viewUncompetedResult language currentDate resultDate =
    let
        transId =
            Maybe.map
                (\date ->
                    if Date.diff Days date currentDate > prenatalLabExpirationPeriod then
                        Translate.ResultsMissing

                    else
                        Translate.ResultsPending
                )
                resultDate
                |> Maybe.withDefault Translate.ResultsPending
    in
    span [ class "uncompleted" ] [ translateText language transId ]


viewLabResultsHistoryPane : Language -> NominalDate -> LabResultsHistoryMode -> Html Msg
viewLabResultsHistoryPane language currentDate mode =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                ]

        entries =
            case mode of
                LabResultsHistoryHIV data ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language)) data

                LabResultsHistorySyphilis data ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language)) data

                LabResultsHistoryHepatitisB data ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language)) data

                LabResultsHistoryMalaria data ->
                    List.map (viewEntry (Translate.PrenatalTestResult >> translate language)) data

                LabResultsHistoryProtein data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryProteinValue >> translate language)) data

                LabResultsHistoryPH data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryPHValue >> translate language)) data

                LabResultsHistoryGlucose data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryGlucoseValue >> translate language)) data

                LabResultsHistoryLeukocytes data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryLeukocytesValue >> translate language)) data

                LabResultsHistoryNitrite data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryNitriteValue >> translate language)) data

                LabResultsHistoryUrobilinogen data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language)) data

                LabResultsHistoryHaemoglobin data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language)) data

                LabResultsHistorySpecificGravity data ->
                    List.map (viewEntry (Translate.PrenatalLaboratorySpecificGravityValue >> translate language)) data

                LabResultsHistoryKetone data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryKetoneValue >> translate language)) data

                LabResultsHistoryBilirubin data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBilirubinValue >> translate language)) data

                LabResultsHistoryRandomBloodSugar data ->
                    List.map (viewEntry String.fromFloat) data

                LabResultsHistoryHemoglobin data ->
                    List.map (viewEntry String.fromFloat) data

                LabResultsHistoryBloodGroup data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBloodGroup >> translate language)) data

                LabResultsHistoryRhesus data ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryRhesus >> translate language)) data

        viewEntry resultToStringFunc ( date, maybeResult ) =
            let
                resultCell =
                    Maybe.map (resultToStringFunc >> text) maybeResult
                        |> Maybe.withDefault (viewUncompetedResult language currentDate (Just date))
            in
            div [ class "entry" ]
                [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                , div [ class "result" ] [ resultCell ]
                ]
    in
    div [ class "lab-results-history" ]
        [ viewItemHeading language (Translate.LabResultsHistoryModeLabel mode) "blue"
        , div [ class "pane-content" ] <|
            heading
                :: entries
        ]


viewProgressPhotosPane : Language -> NominalDate -> Bool -> AssembledData -> Html Msg
viewProgressPhotosPane language currentDate isChw data =
    let
        allMeasurementsWithDates =
            data.nursePreviousMeasurementsWithDates
                ++ (if isChw then
                        []

                    else
                        [ ( currentDate, data.measurements ) ]
                   )

        content =
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
    in
    div [ class "progress-photos" ]
        [ viewItemHeading language Translate.ProgressPhotos "blue"
        , div [ class "pane-content" ] content
        ]
