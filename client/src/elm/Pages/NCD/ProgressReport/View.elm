module Pages.NCD.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( MedicalCondition(..)
        , NCDFamilyHistorySign(..)
        , NCDSocialHistorySign(..)
        , TestExecutionNote(..)
        , TestVariant(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.NCD.Activity.Utils exposing (expectLaboratoryTask)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.NCD.ProgressReport.Svg exposing (viewBloodGlucoseByTime, viewBloodPressureByTime, viewMarkers)
import Pages.NCD.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Model exposing (..)
import Pages.Report.Utils exposing (..)
import Pages.Report.View exposing (..)
import Pages.Utils exposing (viewPersonDetailsExtended)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id initiator db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language initiator model

        content =
            viewWebData language (viewContent language currentDate initiator model) identity assembled

        -- @todo
        -- endEncounterDialog =
        --     if model.showEndEncounterDialog then
        --         Just <|
        --             viewEndEncounterDialog language
        --                 Translate.EndEncounterQuestion
        --                 Translate.OnceYouEndTheEncounter
        --                 (CloseEncounter id)
        --                 (SetEndEncounterDialogState False)
        --
        --     else
        --         Nothing
    in
    div [ class "page-report ncd" ] <|
        [ header
        , content

        -- @todo
        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> NCDProgressReportInitiator -> Model -> Html Msg
viewHeader language initiator model =
    let
        label =
            Maybe.map
                (\mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            Translate.LabResults

                        LabResultsHistory _ ->
                            Translate.LabHistory
                )
                model.labResultsMode
                |> Maybe.withDefault Translate.NCDProgressReport

        backIcon =
            let
                iconForView action =
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick action
                            ]
                            []
                        ]

                goBackActionByLabResultsState defaultAction =
                    Maybe.map
                        (\mode ->
                            let
                                backToCurrentMsg targetMode =
                                    SetLabResultsMode (Just (LabResultsCurrent targetMode))
                            in
                            case mode of
                                LabResultsCurrent currentMode ->
                                    case currentMode of
                                        LabResultsCurrentMain ->
                                            SetLabResultsMode Nothing

                                        LabResultsCurrentDipstickShort ->
                                            backToCurrentMsg LabResultsCurrentMain

                                        LabResultsCurrentDipstickLong ->
                                            backToCurrentMsg LabResultsCurrentMain

                                LabResultsHistory historyMode ->
                                    Maybe.withDefault LabResultsCurrentMain model.labResultsHistoryOrigin
                                        |> backToCurrentMsg
                        )
                        model.labResultsMode
                        |> Maybe.withDefault defaultAction
            in
            case initiator of
                InitiatorEncounterPage id ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| NCDEncounterPage id)

                InitiatorRecurrentEncounterPage id ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| NCDRecurrentEncounterPage id)

        goBackPage =
            case initiator of
                InitiatorEncounterPage id ->
                    NCDEncounterPage id

                InitiatorRecurrentEncounterPage id ->
                    NCDRecurrentEncounterPage id
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> NCDProgressReportInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate initiator model assembled =
    let
        derivedContent =
            case model.labResultsMode of
                Just mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            let
                                config =
                                    { hivPCR = False
                                    , syphilis = False
                                    , hepatitisB = False
                                    , malaria = False
                                    , hemoglobin = False
                                    , bloodGpRs = False
                                    , creatinine = True
                                    , liverFunction = True
                                    , pregnancy = expectLaboratoryTask currentDate assembled TaskPregnancyTest
                                    }
                            in
                            [ generateLabsResultsPaneData currentDate assembled
                                |> viewLabResultsPane language currentDate currentMode SetLabResultsMode config
                            ]

                        LabResultsHistory historyMode ->
                            [ viewLabResultsHistoryPane language currentDate historyMode ]

                Nothing ->
                    let
                        -- @todo
                        actions =
                            --     case initiator of
                            --         InitiatorEncounterPage _ ->
                            --             let
                            --                 ( completedActivities, pendingActivities ) =
                            --                     getAllActivities assembled
                            --                         |> List.filter (Pages.NCD.Activity.Utils.expectActivity currentDate assembled)
                            --                         |> List.partition (Pages.NCD.Activity.Utils.activityCompleted currentDate assembled)
                            --             in
                            --             viewActionButton language
                            --                 pendingActivities
                            --                 completedActivities
                            --                 (SetActivePage PinCodePage)
                            --                 SetEndEncounterDialogState
                            --                 assembled
                            --
                            --         InitiatorRecurrentEncounterPage _ ->
                            --             let
                            --                 ( completedActivities, pendingActivities ) =
                            --                     Pages.NCD.RecurrentEncounter.Utils.allActivities
                            --                         |> List.filter (Pages.NCD.RecurrentActivity.Utils.expectActivity currentDate assembled)
                            --                         |> List.partition (Pages.NCD.RecurrentActivity.Utils.activityCompleted currentDate assembled)
                            --
                            --                 allowEndEcounter =
                            --                     List.isEmpty pendingActivities
                            --             in
                            --             viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState
                            emptyNode
                    in
                    [ viewRiskFactorsPane language currentDate assembled
                    , viewMedicalDiagnosisPane language currentDate assembled
                    , viewPatientProgressPane language currentDate assembled
                    , viewLabsPane language currentDate SetLabResultsMode

                    -- @todo
                    -- , viewProgressPhotosPane language currentDate isChw assembled
                    -- , actions
                    ]
    in
    div [ class "ui unstackable items" ] <|
        viewPersonInfoPane language currentDate assembled.person
            :: derivedContent


viewPersonInfoPane : Language -> NominalDate -> Person -> Html any
viewPersonInfoPane language currentDate person =
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div [ class "patient-info" ] <|
            viewPersonDetailsExtended language currentDate person
        ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]


viewRiskFactorsPane : Language -> NominalDate -> AssembledData -> Html Msg
viewRiskFactorsPane language currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        content =
            List.map (Translate.NCDRiskFactor >> translate language >> text >> List.singleton >> li [])
                riskFactors
                |> ul []
                |> List.singleton

        riskFactors =
            List.map
                (\measurements ->
                    let
                        familyRisks =
                            getMeasurementValueFunc measurements.familyHistory
                                |> Maybe.map (.signs >> generateFamilyHistoryRiskFactors)
                                |> Maybe.withDefault []

                        socialRisks =
                            getMeasurementValueFunc measurements.socialHistory
                                |> Maybe.map (.signs >> generateSocialHistoryRiskFactors)
                                |> Maybe.withDefault []
                    in
                    familyRisks ++ socialRisks
                )
                allMeasurements
                |> List.concat
                |> EverySet.fromList
                |> EverySet.toList
    in
    div [ class "risk-factors" ]
        [ div [ class <| "pane-heading red" ]
            [ img [ src "assets/images/exclamation-white-outline.png" ] []
            , span [] [ text <| translate language Translate.RiskFactors ]
            ]
        , div [ class "pane-content" ] content
        ]


generateFamilyHistoryRiskFactors : EverySet NCDFamilyHistorySign -> List NCDRiskFactor
generateFamilyHistoryRiskFactors signs =
    List.filter
        (\riskFactor ->
            case riskFactor of
                RiskFactorHypertensionHistory ->
                    EverySet.member SignHypertensionHistory signs

                RiskFactorHearProblemHistory ->
                    EverySet.member SignHeartProblemHistory signs

                RiskFactorDiabetesHistory ->
                    EverySet.member SignDiabetesHistory signs

                _ ->
                    False
        )
        familyHistoryRiskFactors


generateSocialHistoryRiskFactors : EverySet NCDSocialHistorySign -> List NCDRiskFactor
generateSocialHistoryRiskFactors signs =
    List.filter
        (\riskFactor ->
            case riskFactor of
                RiskFactorSmokeCigarettes ->
                    EverySet.member SignSmokeCigarettes signs

                RiskFactorConsumeSalt ->
                    EverySet.member SignConsumeSalt signs

                _ ->
                    False
        )
        socialHistoryRiskFactors


familyHistoryRiskFactors : List NCDRiskFactor
familyHistoryRiskFactors =
    [ RiskFactorHypertensionHistory
    , RiskFactorHearProblemHistory
    , RiskFactorDiabetesHistory
    ]


socialHistoryRiskFactors : List NCDRiskFactor
socialHistoryRiskFactors =
    [ RiskFactorSmokeCigarettes
    , RiskFactorConsumeSalt
    ]


coMorbiditiesMedicalContitions : List MedicalCondition
coMorbiditiesMedicalContitions =
    [ MedicalConditionHIV
    , MedicalConditionDiabetes
    , MedicalConditionKidneyDisease
    , MedicalConditionPregnancy
    , MedicalConditionHypertension
    , MedicalConditionGestationalDiabetes
    , MedicalConditionPregnancyRelatedHypertension
    ]


viewMedicalDiagnosisPane : Language -> NominalDate -> AssembledData -> Html Msg
viewMedicalDiagnosisPane language currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        content =
            List.map (Translate.MedicalCondition >> translate language >> text >> List.singleton >> li [])
                coMorbidities
                |> ul []
                |> List.singleton

        coMorbidities =
            List.map
                (.coMorbidities
                    >> getMeasurementValueFunc
                    >> Maybe.map
                        (EverySet.toList
                            >> List.filter
                                (\mdecicalCondition ->
                                    List.member mdecicalCondition coMorbiditiesMedicalContitions
                                )
                        )
                    >> Maybe.withDefault []
                )
                allMeasurements
                |> List.concat
                |> EverySet.fromList
                |> EverySet.toList
    in
    div [ class "medical-diagnosis" ]
        [ viewItemHeading language Translate.MedicalDiagnosis "blue"
        , div [ class "pane-content" ] content
        ]


viewPatientProgressPane : Language -> NominalDate -> AssembledData -> Html Msg
viewPatientProgressPane language currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        sysMeasurements =
            List.map Tuple.first bloodPressure

        diaMeasurements =
            List.map Tuple.second bloodPressure

        bloodPressure =
            List.map
                (.vitals
                    >> getMeasurementValueFunc
                    >> Maybe.andThen
                        (\value ->
                            Maybe.map2 (\sys dia -> ( sys, dia ))
                                value.sys
                                value.dia
                        )
                )
                allMeasurements
                |> Maybe.Extra.values
                |> List.take 12
                |> List.reverse

        sugarCountMeasurements =
            List.map
                (.randomBloodSugarTest
                    >> getMeasurementValueFunc
                    >> Maybe.andThen .sugarCount
                )
                allMeasurements
                |> Maybe.Extra.values
                |> List.take 12
                |> List.reverse
    in
    div [ class "patient-progress" ]
        [ viewItemHeading language Translate.PatientProgress "blue"
        , div [ class "pane-content" ]
            [ viewMarkers
            , div [ class "blood-pressure" ]
                [ div [] [ text <| translate language Translate.BloodPressure ]
                , viewBloodPressureByTime language sysMeasurements diaMeasurements
                , div [] [ text <| translate language Translate.BloodGlucose ]
                , viewBloodGlucoseByTime language sugarCountMeasurements
                ]
            ]
        ]


generateLabsResultsPaneData :
    NominalDate
    -> AssembledData
    -> LabsResultsValues NCDEncounterId
generateLabsResultsPaneData currentDate assembled =
    let
        allMeasurements =
            assembled.measurements
                :: List.map .measurements assembled.previousEncountersData

        extractValues getMeasurementFunc =
            List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
                allMeasurements
    in
    { hiv = extractValues .hivTest
    , urineDipstick = extractValues .urineDipstickTest
    , randomBloodSugar = extractValues .randomBloodSugarTest
    , hivPCR = []
    , syphilis = []
    , hepatitisB = []
    , malaria = []
    , hemoglobin = []
    , bloodGpRs = []
    , creatinine = extractValues .creatinineTest
    , liverFunction = extractValues .liverFunctionTest
    , pregnancy = extractValues .pregnancyTest
    }
