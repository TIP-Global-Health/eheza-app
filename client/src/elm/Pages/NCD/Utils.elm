module Pages.NCD.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (diabetesBySugarCount, diabetesByUrineGlucose, getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
import Backend.NCDEncounter.Types exposing (..)
import Backend.NCDEncounter.Utils exposing (getNCDEncountersForParticipant)
import Backend.NutritionEncounter.Utils exposing (sortByStartDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.View exposing (viewActionTakenLabel, viewMultipleTreatmentWithDosage, viewTreatmentOptionWithDosage)
import Pages.NCD.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeToBoolTask
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewInstructionsLabel
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


generateAssembledData : NCDEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.ncdEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.ncdMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousEncountersData =
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousEncountersData (Just id) encounter_.participant db)
                |> Maybe.withDefault []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousEncountersData)


generatePreviousEncountersData : Maybe NCDEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List PreviousEncounterData
generatePreviousEncountersData currentEncounterId participantId db =
    getNCDEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- If the ID of current encounter was provided,
                -- we do not want to get its data.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.ncdMeasurements of
                        Just (Success measurements) ->
                            Just
                                { id = encounterId
                                , startDate = encounter.startDate
                                , diagnoses = encounter.diagnoses
                                , measurements = measurements
                                }

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortByStartDateDesc


generateNCDDiagnoses : NominalDate -> AssembledData -> EverySet NCDDiagnosis
generateNCDDiagnoses currentDate assembled =
    List.filter (matchNCDDiagnosis currentDate assembled) allNCDDiagnoses
        |> applyHypertensionDiagnosesLogic assembled
        |> filterDiagnosesOfDeterminedConditions assembled
        |> EverySet.fromList


applyHypertensionDiagnosesLogic : AssembledData -> List NCDDiagnosis -> List NCDDiagnosis
applyHypertensionDiagnosesLogic assembled diagnoses =
    let
        ( hypertension, others ) =
            List.partition
                (\diagnosis ->
                    List.member diagnosis
                        [ DiagnosisHypertensionStage1
                        , DiagnosisHypertensionStage2
                        , DiagnosisHypertensionStage3
                        ]
                )
                diagnoses

        currentHypertensionCondition =
            resolveCurrentHypertensionCondition assembled
    in
    -- If no hypertension criteria is met, check if we can lower hypertanstion stage.
    if List.isEmpty hypertension && bloodPressureSatisfiesCondition lowerHypertensionStageCondition assembled then
        Maybe.map
            (\current ->
                case current of
                    DiagnosisHypertensionStage1 ->
                        -- For the moment we remain at stage 1.
                        DiagnosisHypertensionStage1 :: others

                    DiagnosisHypertensionStage2 ->
                        DiagnosisHypertensionStage1 :: others

                    DiagnosisHypertensionStage3 ->
                        DiagnosisHypertensionStage2 :: others

                    _ ->
                        others
            )
            currentHypertensionCondition
            |> Maybe.withDefault diagnoses

    else
        Maybe.map2
            (\new current ->
                if ncdDiagnosisToNumber new > ncdDiagnosisToNumber current then
                    new :: others

                else
                    -- We already diagnosed patient with higher stage, so
                    -- we can drop current diagnosis.
                    others
            )
            (List.head hypertension)
            currentHypertensionCondition
            |> Maybe.withDefault diagnoses


ncdDiagnosisToNumber : NCDDiagnosis -> Int
ncdDiagnosisToNumber diagnosis =
    case diagnosis of
        DiagnosisHypertensionStage1 ->
            1

        DiagnosisHypertensionStage2 ->
            2

        DiagnosisHypertensionStage3 ->
            3

        _ ->
            0


filterDiagnosesOfDeterminedConditions : AssembledData -> List NCDDiagnosis -> List NCDDiagnosis
filterDiagnosesOfDeterminedConditions assembled =
    let
        diagnosesToFilter =
            diagnosesToFilterForDiabetes ++ diagnosesToFilterForRenalComplications

        diagnosesToFilterForDiabetes =
            if diagnosedPreviouslyWithDiabetes assembled then
                diabetesDiagnoses

            else
                []

        diagnosesToFilterForRenalComplications =
            if diagnosedPreviously DiagnosisRenalComplications assembled then
                [ DiagnosisRenalComplications ]

            else
                []
    in
    List.filter (\diagnosis -> not <| List.member diagnosis diagnosesToFilter)


matchNCDDiagnosis : NominalDate -> AssembledData -> NCDDiagnosis -> Bool
matchNCDDiagnosis currentDate assembled diagnosis =
    case diagnosis of
        DiagnosisHypertensionStage1 ->
            (not <| matchNCDDiagnosis currentDate assembled DiagnosisHypertensionStage2)
                && (not <| matchNCDDiagnosis currentDate assembled DiagnosisHypertensionStage3)
                && (reportedAnyOfCoMorbidities assembled [ MedicalConditionHypertension, MedicalConditionPregnancyRelatedHypertension ]
                        || bloodPressureSatisfiesCondition stage1BloodPressureCondition assembled
                   )

        DiagnosisHypertensionStage2 ->
            (not <| matchNCDDiagnosis currentDate assembled DiagnosisHypertensionStage3)
                && bloodPressureSatisfiesCondition stage2BloodPressureCondition assembled

        DiagnosisHypertensionStage3 ->
            bloodPressureSatisfiesCondition stage3BloodPressureCondition assembled

        DiagnosisDiabetesInitial ->
            reportedAnyOfCoMorbidities assembled [ MedicalConditionDiabetes, MedicalConditionGestationalDiabetes ]

        DiagnosisDiabetesRecurrent ->
            (not <| matchNCDDiagnosis currentDate assembled DiagnosisDiabetesInitial)
                && (getMeasurementValueFunc assembled.measurements.randomBloodSugarTest
                        |> Maybe.map
                            (\value ->
                                let
                                    bySugarCount =
                                        diabetesBySugarCount value

                                    byUrineGlucose =
                                        getMeasurementValueFunc assembled.measurements.urineDipstickTest
                                            |> Maybe.map diabetesByUrineGlucose
                                            |> Maybe.withDefault False
                                in
                                bySugarCount || byUrineGlucose
                            )
                        |> Maybe.withDefault False
                   )

        DiagnosisRenalComplications ->
            renalComplicationsByCreatine assembled
                || renalComplicationsByUrineProtein assembled

        NoNCDDiagnosis ->
            False


reportedAnyOfCoMorbidities : AssembledData -> List MedicalCondition -> Bool
reportedAnyOfCoMorbidities assembled medicalConditions =
    getMeasurementValueFunc assembled.measurements.coMorbidities
        |> Maybe.map
            (\value ->
                List.any (\medicalCondition -> EverySet.member medicalCondition value) medicalConditions
            )
        |> Maybe.withDefault False


bloodPressureSatisfiesCondition : (Float -> Float -> Bool) -> AssembledData -> Bool
bloodPressureSatisfiesCondition condition assembled =
    getMeasurementValueFunc assembled.measurements.vitals
        |> Maybe.andThen (\value -> Maybe.map2 condition value.sys value.dia)
        |> Maybe.withDefault False


stage1BloodPressureCondition : Float -> Float -> Bool
stage1BloodPressureCondition sys dia =
    (sys >= 140 && sys < 160) || (dia >= 90 && dia < 100)


stage2BloodPressureCondition : Float -> Float -> Bool
stage2BloodPressureCondition sys dia =
    (sys >= 160 && sys < 180) || (dia >= 100 && dia < 110)


stage3BloodPressureCondition : Float -> Float -> Bool
stage3BloodPressureCondition sys dia =
    (sys >= 180) || (dia >= 110)


lowerHypertensionStageCondition : Float -> Float -> Bool
lowerHypertensionStageCondition sys dia =
    sys < 100


renalComplicationsByCreatine : AssembledData -> Bool
renalComplicationsByCreatine assembled =
    getMeasurementValueFunc assembled.measurements.creatinineTest
        |> Maybe.andThen .creatinineResult
        |> Maybe.map (\creatinineResult -> creatinineResult > 1.3)
        |> Maybe.withDefault False


renalComplicationsByUrineProtein : AssembledData -> Bool
renalComplicationsByUrineProtein assembled =
    getMeasurementValueFunc assembled.measurements.urineDipstickTest
        |> Maybe.andThen .protein
        |> Maybe.map ((/=) Protein0)
        |> Maybe.withDefault False


allNCDDiagnoses : List NCDDiagnosis
allNCDDiagnoses =
    [ DiagnosisHypertensionStage1
    , DiagnosisHypertensionStage2
    , DiagnosisHypertensionStage3
    , DiagnosisDiabetesInitial
    , DiagnosisDiabetesRecurrent
    , DiagnosisRenalComplications
    , NoNCDDiagnosis
    ]


hypertensionDiagnoses : List NCDDiagnosis
hypertensionDiagnoses =
    [ DiagnosisHypertensionStage1
    , DiagnosisHypertensionStage2
    , DiagnosisHypertensionStage3
    ]


diabetesDiagnoses : List NCDDiagnosis
diabetesDiagnoses =
    [ DiagnosisDiabetesInitial, DiagnosisDiabetesRecurrent ]


resolvePreviousHypertensionCondition : AssembledData -> Maybe NCDDiagnosis
resolvePreviousHypertensionCondition =
    .previousEncountersData >> resolveHypertensionCondition


resolveCurrentHypertensionCondition : AssembledData -> Maybe NCDDiagnosis
resolveCurrentHypertensionCondition assembled =
    { id = assembled.id
    , startDate = assembled.encounter.startDate
    , diagnoses = assembled.encounter.diagnoses
    , measurements = assembled.measurements
    }
        :: assembled.previousEncountersData
        |> resolveHypertensionCondition


resolveHypertensionCondition : List PreviousEncounterData -> Maybe NCDDiagnosis
resolveHypertensionCondition encountersData =
    List.filterMap
        (\data ->
            if EverySet.member DiagnosisHypertensionStage3 data.diagnoses then
                Just DiagnosisHypertensionStage3

            else if EverySet.member DiagnosisHypertensionStage2 data.diagnoses then
                Just DiagnosisHypertensionStage2

            else if EverySet.member DiagnosisHypertensionStage1 data.diagnoses then
                Just DiagnosisHypertensionStage1

            else
                Nothing
        )
        encountersData
        |> List.head


diagnosedPreviouslyWithDiabetes : AssembledData -> Bool
diagnosedPreviouslyWithDiabetes =
    diagnosedPreviouslyAnyOf diabetesDiagnoses


diagnosed : NCDDiagnosis -> AssembledData -> Bool
diagnosed diagnosis assembled =
    diagnosedAnyOf [ diagnosis ] assembled


diagnosedAnyOf : List NCDDiagnosis -> AssembledData -> Bool
diagnosedAnyOf diagnoses assembled =
    List.any (\diagnosis -> EverySet.member diagnosis assembled.encounter.diagnoses) diagnoses


diagnosedPreviously : NCDDiagnosis -> AssembledData -> Bool
diagnosedPreviously diagnosis assembled =
    diagnosedPreviouslyAnyOf [ diagnosis ] assembled


diagnosedPreviouslyAnyOf : List NCDDiagnosis -> AssembledData -> Bool
diagnosedPreviouslyAnyOf diagnoses assembled =
    List.filter
        (\data ->
            List.any (\diagnosis -> EverySet.member diagnosis data.diagnoses) diagnoses
        )
        assembled.previousEncountersData
        |> List.isEmpty
        |> not


{-| Recommended Treatment activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used, and at least one of
them must be set.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
recommendedTreatmentMeasurementTaken : List RecommendedTreatmentSign -> NCDMeasurements -> Bool
recommendedTreatmentMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map
            (.recommendedTreatmentSigns
                >> Backend.Measurement.Utils.recommendedTreatmentMeasurementTaken allowedSigns
            )
        |> Maybe.withDefault False


generateRecommendedTreatmentSignsForHypertension : AssembledData -> List RecommendedTreatmentSign
generateRecommendedTreatmentSignsForHypertension assembled =
    if patientIsPregnant assembled then
        [ TreatmentMethyldopa2
        , NoTreatmentForHypertension
        ]

    else
        [ TreatmentHydrochlorothiazide
        , TreatmentAmlodipine
        , TreatmentNifedipine
        , TreatmentCaptopril
        , TreatmentLisinopril
        , TreatmentAtenlol
        , TreatmentMethyldopa2
        , NoTreatmentForHypertension
        ]


recommendedTreatmentSignsForDiabetes : List RecommendedTreatmentSign
recommendedTreatmentSignsForDiabetes =
    [ TreatmentMetformin1m1e
    , TreatmentGlipenclamide1m1e
    , TreatmentMetformin2m1e
    , TreatmentGlipenclamide2m1e
    , TreatmentMetformin2m2e
    , TreatmentGlipenclamide2m2e
    , TreatmentMetformin2m2eGlipenclamide1m1e
    , TreatmentGlipenclamide2m2eMetformin1m1e
    , NoTreatmentForDiabetes
    ]


referralFormWithDefault : ReferralForm -> Maybe ReferralValue -> ReferralForm
referralFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    resolveFromFacilitySignsValue sign =
                        EverySet.member sign value.referralSigns |> Just
                in
                { referToHospital = or form.referToHospital (resolveFromFacilitySignsValue ReferToHospital)
                , referralFormHospital = or form.referralFormHospital (resolveFromFacilitySignsValue ReferralFormHospital)
                , referToANCServices = or form.referToANCServices (resolveFromFacilitySignsValue ReferToANCServices)
                , referralFormANCServices = or form.referralFormANCServices (resolveFromFacilitySignsValue ReferralFormANCServices)
                , accompanyToANCServices = or form.accompanyToANCServices (resolveFromFacilitySignsValue AccompanyToANCServices)
                , nonReferralReasons = or form.nonReferralReasons value.nonReferralReasons
                }
            )


toReferralValueWithDefault : Maybe ReferralValue -> ReferralForm -> Maybe ReferralValue
toReferralValueWithDefault saved form =
    referralFormWithDefault form saved
        |> toReferralValue


toReferralValue : ReferralForm -> Maybe ReferralValue
toReferralValue form =
    let
        referralSigns =
            [ ifNullableTrue ReferToHospital form.referToHospital
            , ifNullableTrue ReferralFormHospital form.referralFormHospital
            , ifNullableTrue ReferToANCServices form.referToANCServices
            , ifNullableTrue ReferralFormANCServices form.referralFormANCServices
            , ifNullableTrue AccompanyToANCServices form.accompanyToANCServices
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoReferToFacilitySigns)
    in
    Maybe.map
        (\signs ->
            { referralSigns = signs
            , nonReferralReasons = form.nonReferralReasons
            }
        )
        referralSigns


toMedicationDistributionValueWithDefault :
    Maybe NCDMedicationDistributionValue
    -> MedicationDistributionForm
    -> Maybe NCDMedicationDistributionValue
toMedicationDistributionValueWithDefault saved form =
    medicationDistributionFormWithDefault form saved
        |> toMedicationDistributionValue


medicationDistributionFormWithDefault : MedicationDistributionForm -> Maybe NCDMedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { recommendedTreatmentSigns = or form.recommendedTreatmentSigns (EverySet.toList value.recommendedTreatmentSigns |> Just)
                , guidedToReturnInOneMonth = or form.guidedToReturnInOneMonth (EverySet.member ReturnInOneMonth value.guidanceSigns |> Just)
                }
            )


toMedicationDistributionValue : MedicationDistributionForm -> Maybe NCDMedicationDistributionValue
toMedicationDistributionValue form =
    let
        recommendedTreatmentSigns =
            Maybe.map EverySet.fromList form.recommendedTreatmentSigns

        guidanceSigns =
            [ ifNullableTrue ReturnInOneMonth form.guidedToReturnInOneMonth ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDGuidanceSigns)
    in
    Maybe.map NCDMedicationDistributionValue recommendedTreatmentSigns
        |> andMap guidanceSigns


resolveMedicationDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> NCDEncounterPhase
    -> AssembledData
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasks language currentDate phase assembled setRecommendedTreatmentSignMsg setMedicationDistributionBoolInputMsg form =
    let
        ( hypertensionInputs, hypertensionCompleted, hypertensionActive ) =
            if medicateForHypertension phase assembled then
                recommendedTreatmentForHypertensionInputAndTask language
                    currentDate
                    (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
                    assembled
                    form

            else
                emptySection

        recommendedTreatmentSignsForHypertension =
            generateRecommendedTreatmentSignsForHypertension assembled

        ( diabetesInputs, diabetesCompleted, diabetesActive ) =
            if medicateForDiabetes phase assembled then
                recommendedTreatmentForDiabetesInputAndTask language
                    currentDate
                    recommendedTreatmentSignsForDiabetes
                    (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForDiabetes)
                    assembled
                    form

            else
                emptySection

        returnInOneMonthInput =
            [ viewQuestionLabel language <| Translate.NCDGuidanceSignQuestion ReturnInOneMonth
            , viewBoolInput
                language
                form.guidedToReturnInOneMonth
                (setMedicationDistributionBoolInputMsg (\value form_ -> { form_ | guidedToReturnInOneMonth = Just value }))
                "return-in-one-month"
                Nothing
            ]

        emptySection =
            ( [], 0, 0 )
    in
    ( hypertensionInputs ++ diabetesInputs ++ returnInOneMonthInput
    , hypertensionCompleted + diabetesCompleted + taskCompleted form.guidedToReturnInOneMonth
    , hypertensionActive + diabetesActive + 1
    )


medicateForDiabetes : NCDEncounterPhase -> AssembledData -> Bool
medicateForDiabetes phase assembled =
    case phase of
        NCDEncounterPhaseInitial ->
            diagnosed DiagnosisDiabetesInitial assembled
                || diagnosedPreviouslyWithDiabetes assembled

        NCDEncounterPhaseRecurrent ->
            diagnosed DiagnosisDiabetesRecurrent assembled


medicateForHypertension : NCDEncounterPhase -> AssembledData -> Bool
medicateForHypertension phase assembled =
    case phase of
        NCDEncounterPhaseInitial ->
            resolveCurrentHypertensionCondition assembled |> isJust

        -- This can only happen on first diagnostics of stage 1 Hypertension (without
        -- any HYpertension history), and with no history of Diabetes or Renal
        -- Complications, and Diabetes at initial phase of the encounter.
        NCDEncounterPhaseRecurrent ->
            -- No Hypertension history.
            (not <| diagnosedPreviouslyAnyOf hypertensionDiagnoses assembled)
                -- No Diabetes or Renal Complication history.
                && (not <| diagnosedPreviouslyAnyOf (DiagnosisRenalComplications :: diabetesDiagnoses) assembled)
                && -- Diagnosed with Stage 1 Hypertension at curernt encounter.
                   diagnosed DiagnosisHypertensionStage1 assembled
                && -- Diagnosed with either Diabetes or Renal Complications on
                   -- recurrent phase of the encounter.
                   (diagnosed DiagnosisRenalComplications assembled
                        || diagnosed DiagnosisDiabetesRecurrent assembled
                   )


recommendedTreatmentForHypertensionInputAndTask :
    Language
    -> NominalDate
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
recommendedTreatmentForHypertensionInputAndTask language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                    >> List.head
                )
                form.recommendedTreatmentSigns

        recommendedTreatmentSignsForHypertension =
            generateRecommendedTreatmentSignsForHypertension assembled

        ( header, instructions ) =
            if patientIsPregnant assembled then
                ( Translate.HypertensionAndPregnantHeader
                , Translate.InstructionsChooseOneMedication
                )

            else
                let
                    renalComplicationsPresent =
                        diagnosed DiagnosisRenalComplications assembled
                            || diagnosedPreviously DiagnosisRenalComplications assembled
                in
                resolveCurrentHypertensionCondition assembled
                    |> Maybe.map
                        (\diagnosis ->
                            ( Translate.HypertensionStageAndRenalComplicationsHeader renalComplicationsPresent diagnosis
                            , if diagnosis == DiagnosisHypertensionStage1 then
                                Translate.InstructionsChooseOneMedication

                              else
                                Translate.InstructionsChooseTwoMedications
                            )
                        )
                    |> Maybe.withDefault ( Translate.EmptyString, Translate.EmptyString )
    in
    ( [ viewCustomLabel language header "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel
                "icon-pills"
                (text <| translate language instructions ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            recommendedTreatmentSignsForHypertension
            []
            currentValue
            setRecommendedTreatmentSignMsg
            (viewTreatmentOptionWithDosage language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentForDiabetesInputAndTask :
    Language
    -> NominalDate
    -> List RecommendedTreatmentSign
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
recommendedTreatmentForDiabetesInputAndTask language currentDate options setRecommendedTreatmentSignMsg assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForDiabetes)
                    >> List.head
                )
                form.recommendedTreatmentSigns

        header =
            -- We specify values at diganosis only if diagnosis was made as a result
            -- of lab test (which can happen only on recurrent phase of encounter).
            if diagnosed DiagnosisDiabetesRecurrent assembled then
                let
                    bySugarCount =
                        Maybe.map diabetesBySugarCount randomBloodSugarValue
                            |> Maybe.withDefault False

                    randomBloodSugarValue =
                        getMeasurementValueFunc assembled.measurements.randomBloodSugarTest
                in
                if bySugarCount then
                    Maybe.andThen
                        (\value ->
                            Maybe.map2
                                (\testPrerequisites sugarCount ->
                                    let
                                        fasting =
                                            EverySet.member PrerequisiteFastFor12h testPrerequisites
                                    in
                                    Translate.PatientGotDiabetesByGlucoseHeader fasting sugarCount
                                )
                                value.testPrerequisites
                                value.sugarCount
                        )
                        randomBloodSugarValue
                        |> Maybe.withDefault Translate.EmptyString

                else
                    getMeasurementValueFunc assembled.measurements.urineDipstickTest
                        |> Maybe.andThen .glucose
                        |> Maybe.map
                            (\glucose ->
                                Translate.PatientGotDiabetesByUrineDip (translate language <| Translate.PrenatalLaboratoryGlucoseValue glucose)
                            )
                        |> Maybe.withDefault Translate.EmptyString

            else
                Translate.PatientGotDiabetesHeader
    in
    ( [ viewCustomLabel language header "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel
                "icon-pills"
                (text <| translate language Translate.InstructionsChooseOneMedication ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            options
            []
            currentValue
            setRecommendedTreatmentSignMsg
            (viewTreatmentOptionForDiabetes language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


viewTreatmentOptionForDiabetes : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionForDiabetes language sign =
    case sign of
        TreatmentMetformin2m2eGlipenclamide1m1e ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentMetformin2m2e
                , TreatmentGlipenclamide1m1e
                ]

        TreatmentGlipenclamide2m2eMetformin1m1e ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentGlipenclamide2m2e
                , TreatmentMetformin1m1e
                ]

        _ ->
            viewTreatmentOptionWithDosage language sign


resolveReferralInputsAndTasks :
    Language
    -> NominalDate
    -> NCDEncounterPhase
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralInputsAndTasks language currentDate phase assembled setReferralBoolInputMsg setNonReferralReasonMsg form =
    let
        facility =
            if referForHypertension phase assembled && patientIsPregnant assembled then
                FacilityANCServices

            else
                FacilityHospital

        maybeConfig =
            case facility of
                FacilityHospital ->
                    Just
                        { header =
                            [ viewCustomLabel language Translate.ReferToHospitalForFurtherEvaluation "." "instructions" ]
                        , referralField = form.referToHospital
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToHospital = Just value
                                    , referralFormHospital = Nothing
                                }
                        , formField = form.referralFormHospital
                        , formUpdateFunc = \value form_ -> { form_ | referralFormHospital = Just value }
                        , accompanyConfig = Nothing
                        , reasonToSignFunc = NonReferralReasonHospital
                        }

                FacilityANCServices ->
                    Just
                        { header =
                            [ viewCustomLabel language Translate.NCDANCServicesInstructions "." "instructions" ]
                        , referralField = form.referToANCServices
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToANCServices = Just value
                                    , referralFormANCServices = Nothing
                                    , accompanyToANCServices = Nothing
                                }
                        , formField = form.referralFormANCServices
                        , formUpdateFunc = \value form_ -> { form_ | referralFormANCServices = Just value }
                        , accompanyConfig =
                            Just
                                ( form.accompanyToANCServices
                                , \value form_ -> { form_ | accompanyToANCServices = Just value }
                                )
                        , reasonToSignFunc = NonReferralReasonANCServices
                        }

                -- Other facilities are not in use at NCD.
                _ ->
                    Nothing
    in
    Maybe.map
        (\config ->
            let
                ( derivedSection, derivedTasks ) =
                    Maybe.map
                        (\referred ->
                            if referred then
                                let
                                    ( accompanySection, accompanyTasks ) =
                                        Maybe.map
                                            (\( field, updateFunc ) ->
                                                ( [ viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion facility
                                                  , viewBoolInput
                                                        language
                                                        field
                                                        (setReferralBoolInputMsg updateFunc)
                                                        "accompany-to-hc"
                                                        Nothing
                                                  ]
                                                , [ field ]
                                                )
                                            )
                                            config.accompanyConfig
                                            |> Maybe.withDefault ( [], [] )
                                in
                                ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
                                  , viewBoolInput
                                        language
                                        config.formField
                                        (setReferralBoolInputMsg config.formUpdateFunc)
                                        "hand-referral-form"
                                        Nothing
                                  ]
                                    ++ accompanySection
                                , [ config.formField ] ++ accompanyTasks
                                )

                            else
                                ( nonReferralReasonSection language facility config.reasonToSignFunc setNonReferralReasonMsg form
                                , [ maybeToBoolTask <| getCurrentReasonForNonReferralByForm config.reasonToSignFunc form ]
                                )
                        )
                        config.referralField
                        |> Maybe.withDefault ( [], [] )
            in
            ( config.header
                ++ [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                   , div [ class "instructions" ]
                        [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing
                        , viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" Nothing
                        ]
                   , viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion facility
                   , viewBoolInput
                        language
                        config.referralField
                        (setReferralBoolInputMsg config.referralUpdateFunc)
                        "referral"
                        Nothing
                   ]
                ++ derivedSection
                ++ [ div [ class "separator" ] [] ]
            , [ config.referralField ] ++ derivedTasks
            )
        )
        maybeConfig
        |> Maybe.withDefault ( [], [] )


referForHypertension : NCDEncounterPhase -> AssembledData -> Bool
referForHypertension phase assembled =
    case phase of
        NCDEncounterPhaseInitial ->
            resolveCurrentHypertensionCondition assembled
                |> Maybe.map
                    (\condition ->
                        (condition == DiagnosisHypertensionStage3)
                            || patientIsPregnant assembled
                    )
                |> Maybe.withDefault False

        NCDEncounterPhaseRecurrent ->
            False


patientIsPregnant : AssembledData -> Bool
patientIsPregnant assembled =
    getMeasurementValueFunc assembled.measurements.pregnancyTest
        |> Maybe.map
            (\value ->
                (value.executionNote == TestNoteKnownAsPositive)
                    || (value.testResult == Just TestPositive)
            )
        |> Maybe.withDefault False


referForDiabetes : NCDEncounterPhase -> AssembledData -> Bool
referForDiabetes phase assembled =
    (isJust <| resolveCurrentHypertensionCondition assembled)
        && (case phase of
                NCDEncounterPhaseInitial ->
                    diagnosedPreviouslyWithDiabetes assembled
                        || diagnosed DiagnosisDiabetesInitial assembled

                NCDEncounterPhaseRecurrent ->
                    diagnosed DiagnosisDiabetesRecurrent assembled
           )


referForRenalComplications : NCDEncounterPhase -> AssembledData -> Bool
referForRenalComplications phase assembled =
    (isJust <| resolveCurrentHypertensionCondition assembled)
        && (case phase of
                NCDEncounterPhaseInitial ->
                    diagnosedPreviously DiagnosisRenalComplications assembled

                NCDEncounterPhaseRecurrent ->
                    (not <| diagnosedPreviously DiagnosisRenalComplications assembled)
                        && diagnosed DiagnosisRenalComplications assembled
           )


nonReferralReasonSection :
    Language
    -> ReferralFacility
    -> (ReasonForNonReferral -> NonReferralSign)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> List (Html msg)
nonReferralReasonSection language facility reasonToSignFunc setNonReferralReasonMsg form =
    let
        currentValue =
            getCurrentReasonForNonReferralByForm reasonToSignFunc form

        options =
            if facility == FacilityHospital then
                [ ClientRefused
                , NoAmbulance
                , ClientUnableToAffordFees
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]

            else
                [ ClientRefused
                , ClientAlreadyInCare
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]
    in
    [ viewQuestionLabel language Translate.WhyNot
    , viewCheckBoxSelectInput language
        options
        []
        currentValue
        (setNonReferralReasonMsg currentValue facility)
        Translate.ReasonForNonReferral
    ]


getCurrentReasonForNonReferralByForm :
    (ReasonForNonReferral -> NonReferralSign)
    -> ReferralForm
    -> Maybe ReasonForNonReferral
getCurrentReasonForNonReferralByForm reasonToSignFunc form =
    getCurrentReasonForNonReferral reasonToSignFunc form.nonReferralReasons


{-| Referal to facility is completed when we mark that facility was referred to,
or, reason was set for not referring to that facility.
-}
referralToFacilityCompleted : AssembledData -> ReferralFacility -> Bool
referralToFacilityCompleted assembled facility =
    getMeasurementValueFunc assembled.measurements.referral
        |> Maybe.map
            (\value ->
                Backend.Measurement.Utils.referralToFacilityCompleted value.referralSigns value.nonReferralReasons facility
            )
        |> Maybe.withDefault False
