module Pages.NCD.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.NCDActivity.Utils exposing (allActivities)
import Backend.NCDEncounter.Types exposing (..)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , isTestResultValid
        , resolveLabTestDate
        , vitalsFormWithDefault
        )
import Measurement.View exposing (vitalsFormInputsAndTasks)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Types exposing (..)
import Pages.NCD.Model exposing (..)
import Pages.NCD.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewNumberInput
        , viewQuestionLabel
        )
import SyncManager.Model exposing (Site)
import Translate
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> NCDActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        DangerSigns ->
            True

        SymptomReview ->
            True

        Examination ->
            True

        FamilyPlanning ->
            isPersonAFertileWoman currentDate assembled.person

        MedicalHistory ->
            -- Only for first encounter.
            List.isEmpty assembled.previousEncountersData

        Laboratory ->
            True

        OutsideCare ->
            -- For subsequent encounters.
            not <| List.isEmpty assembled.previousEncountersData

        NextSteps ->
            mandatoryActivitiesForNextStepsCompleted currentDate assembled
                && (resolveNextStepsTasks currentDate assembled
                        |> List.isEmpty
                        |> not
                   )


activityCompleted : NominalDate -> AssembledData -> NCDActivity -> Bool
activityCompleted currentDate assembled activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate assembled activityToCheck
    in
    case activity of
        DangerSigns ->
            notExpected DangerSigns
                || isJust assembled.measurements.dangerSigns

        SymptomReview ->
            notExpected SymptomReview
                || isJust assembled.measurements.symptomReview

        Examination ->
            notExpected Examination
                || (isJust assembled.measurements.coreExam
                        && isJust assembled.measurements.vitals
                   )

        FamilyPlanning ->
            notExpected FamilyPlanning
                || isJust assembled.measurements.familyPlanning

        MedicalHistory ->
            notExpected MedicalHistory
                || (isJust assembled.measurements.coMorbidities
                        && isJust assembled.measurements.medicationHistory
                        && isJust assembled.measurements.socialHistory
                        && isJust assembled.measurements.familyHistory
                        && isJust assembled.measurements.outsideCare
                   )

        Laboratory ->
            notExpected Laboratory
                || List.all (laboratoryTaskCompleted currentDate assembled) laboratoryTasks

        OutsideCare ->
            notExpected OutsideCare
                || isJust assembled.measurements.outsideCare

        NextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.all (nextStepsTaskCompleted assembled)


resolveNextStepsTasks : NominalDate -> AssembledData -> List Pages.NCD.Activity.Types.NextStepsTask
resolveNextStepsTasks currentDate assembled =
    List.filter (expectNextStepsTask currentDate assembled)
        [ TaskHealthEducation, TaskMedicationDistribution, TaskReferral ]


expectNextStepsTask : NominalDate -> AssembledData -> Pages.NCD.Activity.Types.NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        TaskHealthEducation ->
            -- Diagnosed Stage 1 at current encounter.
            diagnosed DiagnosisHypertensionStage1 assembled
                -- Not diagnosed any Hypertension diagnoses at previous encounters.
                && (not <| diagnosedPreviouslyAnyOf hypertensionDiagnoses assembled.previousEncountersData)
                -- Not diagnosed any Diaberes / RenalComplications diagnoses at current or previous encounters.
                && (not <| diagnosedAnyOf [ DiagnosisRenalComplications, DiagnosisDiabetesInitial ] assembled)
                && (not <| diagnosedPreviouslyAnyOf (DiagnosisRenalComplications :: diabetesDiagnoses) assembled.previousEncountersData)
                && -- Pregnant women always get Methyldopa treatment, so, no health education is provided.
                   (not <| patientIsPregnant assembled.measurements)

        TaskMedicationDistribution ->
            medicateForDiabetes NCDEncounterPhaseInitial assembled
                || medicateForHypertension NCDEncounterPhaseInitial assembled

        TaskReferral ->
            referForDiabetes NCDEncounterPhaseInitial assembled
                || referForHypertension NCDEncounterPhaseInitial assembled
                || referForRenalComplications NCDEncounterPhaseInitial assembled


nextStepsTaskCompleted : AssembledData -> Pages.NCD.Activity.Types.NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        TaskHealthEducation ->
            isJust assembled.measurements.healthEducation

        TaskMedicationDistribution ->
            let
                hypertensionTreatmentCompleted =
                    if medicateForHypertension NCDEncounterPhaseInitial assembled then
                        let
                            recommendedTreatmentSignsForHypertension =
                                generateRecommendedTreatmentSignsForHypertension assembled
                        in
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True

                diabetesTreatmentCompleted =
                    if medicateForDiabetes NCDEncounterPhaseInitial assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForDiabetes assembled.measurements

                    else
                        True
            in
            hypertensionTreatmentCompleted && diabetesTreatmentCompleted

        TaskReferral ->
            isJust assembled.measurements.referral


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    -- All activities that will appear at current
    -- encounter are completed, besides Next Steps itself.
    EverySet.fromList allActivities
        |> EverySet.remove NextSteps
        |> EverySet.toList
        |> List.all (activityCompleted currentDate assembled)


resolvePreviousValue : AssembledData -> (NCDMeasurements -> Maybe ( id, NCDMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousEncountersData
        |> List.filterMap
            (.measurements
                >> measurementFunc
                >> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (NCDMeasurements -> Maybe ( id, NCDMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.previousEncountersData
        |> List.filterMap
            (.measurements
                >> measurementFunc
                >> Maybe.andThen (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


dangerSignsFormWithDefault : DangerSignsForm -> Maybe NCDDangerSignsValue -> DangerSignsForm
dangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toDangerSignsValueWithDefault : Maybe NCDDangerSignsValue -> DangerSignsForm -> Maybe NCDDangerSignsValue
toDangerSignsValueWithDefault saved form =
    dangerSignsFormWithDefault form saved
        |> toDangerSignsValue


toDangerSignsValue : DangerSignsForm -> Maybe NCDDangerSignsValue
toDangerSignsValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDDangerSigns) form.signs


symptomReviewFormWithDefault : SymptomReviewForm -> Maybe NCDSymptomReviewValue -> SymptomReviewForm
symptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { group1Symptoms = or form.group1Symptoms (EverySet.toList value.group1Symptoms |> Just)
                , group2Symptoms = or form.group2Symptoms (EverySet.toList value.group2Symptoms |> Just)
                , painSymptoms = or form.painSymptoms (EverySet.toList value.painSymptoms |> Just)
                }
            )


toSymptomReviewValueWithDefault : Maybe NCDSymptomReviewValue -> SymptomReviewForm -> Maybe NCDSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


toSymptomReviewValue : SymptomReviewForm -> Maybe NCDSymptomReviewValue
toSymptomReviewValue form =
    let
        group1Symptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDGroup1Symptoms) form.group1Symptoms

        group2Symptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDGroup2Symptoms) form.group2Symptoms

        painSymptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoNCDPainSymptoms) form.painSymptoms
    in
    Maybe.map NCDSymptomReviewValue group1Symptoms
        |> andMap group2Symptoms
        |> andMap painSymptoms


examinationTasksCompletedFromTotal : NominalDate -> AssembledData -> ExaminationData -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal currentDate assembled data task =
    case task of
        TaskVitals ->
            let
                formConfig =
                    generateVitalsFormConfig assembled

                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> vitalsFormInputsAndTasks English currentDate formConfig
            in
            resolveTasksCompletedFromTotal tasks

        TaskCoreExam ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.coreExam
                        |> corePhysicalExamFormWithDefault data.coreExamForm

                extremitiesTaskCompleted =
                    if isJust form.hands && isJust form.legs then
                        1

                    else
                        0
            in
            ( extremitiesTaskCompleted
                + taskCompleted form.neck
                + taskCompleted form.lungs
                + taskCompleted form.abdomen
                + taskCompleted form.heart
                + ([ form.brittleHair
                   , form.paleConjuctiva
                   ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 7
            )


generateVitalsFormConfig : AssembledData -> VitalsFormConfig Msg
generateVitalsFormConfig assembled =
    { setIntInputMsg = SetVitalsIntInput
    , setFloatInputMsg = SetVitalsFloatInput
    , sysBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .sys
    , diaBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .dia
    , heartRatePreviousValue =
        resolvePreviousMaybeValue assembled .vitals .heartRate
            |> Maybe.map toFloat
    , respiratoryRatePreviousValue =
        resolvePreviousValue assembled .vitals .respiratoryRate
            |> Maybe.map toFloat
    , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
    , birthDate = assembled.person.birthDate
    , formClass = "vitals"
    , mode = VitalsFormFull
    , invokationModule = InvokationModuleNCD
    }


coMorbiditiesFormWithDefault : CoMorbiditiesForm -> Maybe NCDCoMorbiditiesValue -> CoMorbiditiesForm
coMorbiditiesFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { conditions = or form.conditions (EverySet.toList value |> Just) }
            )


toCoMorbiditiesValueWithDefault : Maybe NCDCoMorbiditiesValue -> CoMorbiditiesForm -> Maybe NCDCoMorbiditiesValue
toCoMorbiditiesValueWithDefault saved form =
    coMorbiditiesFormWithDefault form saved
        |> toCoMorbiditiesValue


toCoMorbiditiesValue : CoMorbiditiesForm -> Maybe NCDCoMorbiditiesValue
toCoMorbiditiesValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicalConditions) form.conditions


familyHistoryFormWithDefault : FamilyHistoryForm -> Maybe NCDFamilyHistoryValue -> FamilyHistoryForm
familyHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { hypertensionInFamily = or form.hypertensionInFamily (EverySet.member SignHypertensionHistory value.signs |> Just)
                , heartProblemInFamily = or form.heartProblemInFamily (EverySet.member SignHeartProblemHistory value.signs |> Just)
                , diabetesInFamily = or form.diabetesInFamily (EverySet.member SignDiabetesHistory value.signs |> Just)
                , hypertensionPredecessors =
                    maybeValueConsideringIsDirtyField form.hypertensionPredecessorsDirty
                        form.hypertensionPredecessors
                        (Maybe.map EverySet.toList value.hypertensionPredecessors)
                , hypertensionPredecessorsDirty = form.hypertensionPredecessorsDirty
                , heartProblemPredecessors =
                    maybeValueConsideringIsDirtyField form.heartProblemPredecessorsDirty
                        form.heartProblemPredecessors
                        (Maybe.map EverySet.toList value.heartProblemPredecessors)
                , heartProblemPredecessorsDirty = form.heartProblemPredecessorsDirty
                , diabetesPredecessors =
                    maybeValueConsideringIsDirtyField form.diabetesPredecessorsDirty
                        form.diabetesPredecessors
                        (Maybe.map EverySet.toList value.diabetesPredecessors)
                , diabetesPredecessorsDirty = form.diabetesPredecessorsDirty
                }
            )


toFamilyHistoryValueWithDefault : Maybe NCDFamilyHistoryValue -> FamilyHistoryForm -> Maybe NCDFamilyHistoryValue
toFamilyHistoryValueWithDefault saved form =
    familyHistoryFormWithDefault form saved
        |> toFamilyHistoryValue


toFamilyHistoryValue : FamilyHistoryForm -> Maybe NCDFamilyHistoryValue
toFamilyHistoryValue form =
    let
        maybeSigns =
            [ Maybe.map (ifTrue SignHypertensionHistory) form.hypertensionInFamily
            , Maybe.map (ifTrue SignHeartProblemHistory) form.heartProblemInFamily
            , Maybe.map (ifTrue SignDiabetesHistory) form.diabetesInFamily
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDFamilyHistorySigns)
    in
    Maybe.map
        (\signs ->
            { signs = signs
            , hypertensionPredecessors = Maybe.map EverySet.fromList form.hypertensionPredecessors
            , heartProblemPredecessors = Maybe.map EverySet.fromList form.heartProblemPredecessors
            , diabetesPredecessors = Maybe.map EverySet.fromList form.diabetesPredecessors
            }
        )
        maybeSigns


medicationHistoryFormWithDefault : MedicationHistoryForm -> Maybe NCDMedicationHistoryValue -> MedicationHistoryForm
medicationHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { medicationsCausingHypertension = or form.medicationsCausingHypertension (EverySet.toList value.medicationsCausingHypertension |> Just)
                , medicationsTreatingHypertension = or form.medicationsTreatingHypertension (EverySet.toList value.medicationsTreatingHypertension |> Just)
                , medicationsTreatingDiabetes = or form.medicationsTreatingDiabetes (EverySet.toList value.medicationsTreatingDiabetes |> Just)
                }
            )


toMedicationHistoryValueWithDefault : Maybe NCDMedicationHistoryValue -> MedicationHistoryForm -> Maybe NCDMedicationHistoryValue
toMedicationHistoryValueWithDefault saved form =
    medicationHistoryFormWithDefault form saved
        |> toMedicationHistoryValue


toMedicationHistoryValue : MedicationHistoryForm -> Maybe NCDMedicationHistoryValue
toMedicationHistoryValue form =
    let
        medicationsCausingHypertension =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationCausingHypertension) form.medicationsCausingHypertension

        medicationsTreatingHypertension =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationTreatingHypertension) form.medicationsTreatingHypertension

        medicationsTreatingDiabetes =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationTreatingDiabetes) form.medicationsTreatingDiabetes
    in
    Maybe.map NCDMedicationHistoryValue medicationsCausingHypertension
        |> andMap medicationsTreatingHypertension
        |> andMap medicationsTreatingDiabetes


socialHistoryFormWithDefault : SocialHistoryForm -> Maybe NCDSocialHistoryValue -> SocialHistoryForm
socialHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { alcohol = or form.alcohol (EverySet.member SignDrinkAlcohol value.signs |> Just)
                , cigarettes = or form.cigarettes (EverySet.member SignSmokeCigarettes value.signs |> Just)
                , salt = or form.salt (EverySet.member SignConsumeSalt value.signs |> Just)
                , difficult4Times = or form.difficult4Times (EverySet.member SignDifficult4TimesAYear value.signs |> Just)
                , helpAtHome = or form.helpAtHome (EverySet.member SignHelpWithTreatmentAtHome value.signs |> Just)
                , foodGroup = or form.foodGroup (Just value.foodGroup)
                , beveragesPerWeek = maybeValueConsideringIsDirtyField form.beveragesPerWeekDirty form.beveragesPerWeek value.beveragesPerWeek
                , beveragesPerWeekDirty = form.beveragesPerWeekDirty
                , cigarettesPerWeek = maybeValueConsideringIsDirtyField form.cigarettesPerWeekDirty form.cigarettesPerWeek value.cigarettesPerWeek
                , cigarettesPerWeekDirty = form.cigarettesPerWeekDirty
                }
            )


toSocialHistoryValueWithDefault : Maybe NCDSocialHistoryValue -> SocialHistoryForm -> Maybe NCDSocialHistoryValue
toSocialHistoryValueWithDefault saved form =
    socialHistoryFormWithDefault form saved
        |> toSocialHistoryValue


toSocialHistoryValue : SocialHistoryForm -> Maybe NCDSocialHistoryValue
toSocialHistoryValue form =
    let
        maybeSigns =
            [ Maybe.map (ifTrue SignDrinkAlcohol) form.alcohol
            , Maybe.map (ifTrue SignSmokeCigarettes) form.cigarettes
            , Maybe.map (ifTrue SignConsumeSalt) form.salt
            , Maybe.map (ifTrue SignDifficult4TimesAYear) form.salt
            , Maybe.map (ifTrue SignHelpWithTreatmentAtHome) form.helpAtHome
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDSocialHistorySigns)
    in
    Maybe.map2
        (\signs foodGroup ->
            { signs = signs
            , foodGroup = foodGroup
            , beveragesPerWeek = form.beveragesPerWeek
            , cigarettesPerWeek = form.cigarettesPerWeek
            }
        )
        maybeSigns
        form.foodGroup


medicalHistoryTasksCompletedFromTotal : NominalDate -> Site -> AssembledData -> MedicalHistoryData -> MedicalHistoryTask -> ( Int, Int )
medicalHistoryTasksCompletedFromTotal currentDate site assembled data task =
    let
        ( _, tasks ) =
            case task of
                TaskCoMorbidities ->
                    getMeasurementValueFunc assembled.measurements.coMorbidities
                        |> coMorbiditiesFormWithDefault data.coMorbiditiesForm
                        |> coMorbiditiesFormInputsAndTasks English currentDate

                TaskMedicationHistory ->
                    getMeasurementValueFunc assembled.measurements.medicationHistory
                        |> medicationHistoryFormWithDefault data.medicationHistoryForm
                        |> medicationHistoryFormInputsAndTasks English currentDate

                TaskSocialHistory ->
                    getMeasurementValueFunc assembled.measurements.socialHistory
                        |> socialHistoryFormWithDefault data.socialHistoryForm
                        |> socialHistoryFormInputsAndTasks English currentDate site

                TaskFamilyHistory ->
                    getMeasurementValueFunc assembled.measurements.familyHistory
                        |> familyHistoryFormWithDefault data.familyHistoryForm
                        |> familyHistoryFormInputsAndTasks English currentDate

                TaskOutsideCare ->
                    -- This is not in use, because OutsideCare task got
                    -- special treatment at viewMedicalHistoryContent().
                    ( [], [] )
    in
    resolveTasksCompletedFromTotal tasks


coMorbiditiesFormInputsAndTasks : Language -> NominalDate -> CoMorbiditiesForm -> ( List (Html Msg), List (Maybe Bool) )
coMorbiditiesFormInputsAndTasks language currentDate form =
    ( [ viewQuestionLabel language Translate.MedicalConditionQuestion
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ MedicalConditionHIV
            , MedicalConditionDiabetes
            , MedicalConditionKidneyDisease
            , MedicalConditionPregnancy
            , MedicalConditionHypertension
            , MedicalConditionGestationalDiabetes
            , MedicalConditionPregnancyRelatedHypertension
            ]
            []
            (Maybe.withDefault [] form.conditions)
            (Just NoMedicalConditions)
            SetMedicalCondition
            Translate.MedicalCondition
      ]
    , [ maybeToBoolTask form.conditions ]
    )


medicationHistoryFormInputsAndTasks : Language -> NominalDate -> MedicationHistoryForm -> ( List (Html Msg), List (Maybe Bool) )
medicationHistoryFormInputsAndTasks language currentDate form =
    ( [ viewQuestionLabel language Translate.MedicationCausingHypertensionQuestion
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ MedicationOestrogens
            , MedicationSteroids
            , MedicationAmitriptyline
            ]
            [ MedicationIbuprofen
            , NoMedicationCausingHypertension
            ]
            (Maybe.withDefault [] form.medicationsCausingHypertension)
            Nothing
            SetMedicationCausingHypertension
            Translate.MedicationCausingHypertension
      , viewQuestionLabel language Translate.MedicationTreatingHypertensionQuestion
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ MedicationAceInhibitors
            , MedicationARBs
            , MedicationHCTZ
            , MedicationCalciumChannelBlockers
            ]
            [ MedicationMethyldopa
            , MedicationBetaBlockers
            , MedicationHydralazine
            , NoMedicationTreatingHypertension
            ]
            (Maybe.withDefault [] form.medicationsTreatingHypertension)
            Nothing
            SetMedicationTreatingHypertension
            Translate.MedicationTreatingHypertension
      , viewQuestionLabel language Translate.MedicationTreatingDiabetesQuestion
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ MedicationMetformin
            , MedicationGlibenclamide
            ]
            [ MedicationInsulin, NoMedicationTreatingDiabetes ]
            (Maybe.withDefault [] form.medicationsTreatingDiabetes)
            Nothing
            SetMedicationTreatingDiabetes
            Translate.MedicationTreatingDiabetes
      ]
    , [ maybeToBoolTask form.medicationsCausingHypertension
      , maybeToBoolTask form.medicationsTreatingHypertension
      , maybeToBoolTask form.medicationsTreatingDiabetes
      ]
    )


socialHistoryFormInputsAndTasks : Language -> NominalDate -> Site -> SocialHistoryForm -> ( List (Html Msg), List (Maybe Bool) )
socialHistoryFormInputsAndTasks language currentDate site form =
    let
        ( alcoholInputs, alcoholTasks ) =
            let
                ( derivedInput, derivedTask ) =
                    if form.alcohol == Just True then
                        ( [ viewQuestionLabel language Translate.HowManyPerWeek
                          , viewNumberInput language
                                form.beveragesPerWeek
                                (SetSocialHistoryIntInput
                                    (\value form_ ->
                                        { form_ | beveragesPerWeek = value, beveragesPerWeekDirty = True }
                                    )
                                )
                                "beverages"
                          ]
                        , [ maybeToBoolTask form.beveragesPerWeek ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion site SignDrinkAlcohol
              , viewBoolInput language
                    form.alcohol
                    (SetSocialHistoryBoolInput
                        (\value form_ ->
                            { form_ | alcohol = Just value, beveragesPerWeek = Nothing, beveragesPerWeekDirty = True }
                        )
                    )
                    "alcohol"
                    Nothing
              ]
                ++ derivedInput
            , form.alcohol :: derivedTask
            )

        ( cigarettesInputs, cigarettesTasks ) =
            let
                ( derivedInput, derivedTask ) =
                    if form.cigarettes == Just True then
                        ( [ viewQuestionLabel language Translate.HowManyPerWeek
                          , viewNumberInput language
                                form.cigarettesPerWeek
                                (SetSocialHistoryIntInput
                                    (\value form_ ->
                                        { form_ | cigarettesPerWeek = value, cigarettesPerWeekDirty = True }
                                    )
                                )
                                "cigarettes"
                          ]
                        , [ maybeToBoolTask form.cigarettesPerWeek ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion site SignSmokeCigarettes
              , viewBoolInput language
                    form.cigarettes
                    (SetSocialHistoryBoolInput
                        (\value form_ ->
                            { form_ | cigarettes = Just value, cigarettesPerWeek = Nothing, cigarettesPerWeekDirty = True }
                        )
                    )
                    "cigarettes"
                    Nothing
              ]
                ++ derivedInput
            , form.cigarettes :: derivedTask
            )
    in
    ( alcoholInputs
        ++ cigarettesInputs
        ++ [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion site SignConsumeSalt
           , viewBoolInput language
                form.salt
                (SetSocialHistoryBoolInput
                    (\value form_ ->
                        { form_ | salt = Just value }
                    )
                )
                "salt"
                Nothing
           , viewQuestionLabel language Translate.NCDSocialHistoryFoodQuestion
           , viewCustomLabel language Translate.NCDSocialHistoryFoodQuestionInstructions "." "helper"
           , viewCheckBoxSelectInput language
                [ FoodGroupVegetables, FoodGroupCarbohydrates, FoodGroupProtein ]
                []
                form.foodGroup
                SetFoodGroup
                Translate.FoodGroup
           , viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion site SignDifficult4TimesAYear
           , viewBoolInput language
                form.difficult4Times
                (SetSocialHistoryBoolInput
                    (\value form_ ->
                        { form_ | difficult4Times = Just value }
                    )
                )
                "difficult-4-times"
                Nothing
           , viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion site SignHelpWithTreatmentAtHome
           , viewBoolInput language
                form.helpAtHome
                (SetSocialHistoryBoolInput
                    (\value form_ ->
                        { form_ | helpAtHome = Just value }
                    )
                )
                "help-at-home"
                Nothing
           ]
    , alcoholTasks
        ++ cigarettesTasks
        ++ [ form.salt, form.difficult4Times, form.helpAtHome, maybeToBoolTask form.foodGroup ]
    )


familyHistoryFormInputsAndTasks : Language -> NominalDate -> FamilyHistoryForm -> ( List (Html Msg), List (Maybe Bool) )
familyHistoryFormInputsAndTasks language currentDate form =
    let
        ( hypertensionInputs, hypertensionTasks ) =
            let
                ( derivedInput, derivedTask ) =
                    if form.hypertensionInFamily == Just True then
                        ( viewPredecessorInput form.hypertensionPredecessors SetHypertensionPredecessor
                        , [ maybeToBoolTask form.hypertensionPredecessors ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.NCDFamilyHistorySignQuestion SignHypertensionHistory
              , viewBoolInput language
                    form.hypertensionInFamily
                    (SetFamilyHistoryBoolInput
                        (\value form_ ->
                            { form_ | hypertensionInFamily = Just value, hypertensionPredecessors = Nothing, hypertensionPredecessorsDirty = True }
                        )
                    )
                    "hypertension-in-family"
                    Nothing
              ]
                ++ derivedInput
            , form.hypertensionInFamily :: derivedTask
            )

        ( heartProblemInputs, heartProblemTasks ) =
            let
                ( derivedInput, derivedTask ) =
                    if form.heartProblemInFamily == Just True then
                        ( viewPredecessorInput form.heartProblemPredecessors SetHeartProblemPredecessor
                        , [ maybeToBoolTask form.heartProblemPredecessors ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.NCDFamilyHistorySignQuestion SignHeartProblemHistory
              , viewBoolInput language
                    form.heartProblemInFamily
                    (SetFamilyHistoryBoolInput
                        (\value form_ ->
                            { form_ | heartProblemInFamily = Just value, heartProblemPredecessors = Nothing, heartProblemPredecessorsDirty = True }
                        )
                    )
                    "heartProblem-in-family"
                    Nothing
              ]
                ++ derivedInput
            , form.heartProblemInFamily :: derivedTask
            )

        ( diabetesInputs, diabetesTasks ) =
            let
                ( derivedInput, derivedTask ) =
                    if form.diabetesInFamily == Just True then
                        ( viewPredecessorInput form.diabetesPredecessors SetDiabetesPredecessor
                        , [ maybeToBoolTask form.diabetesPredecessors ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.NCDFamilyHistorySignQuestion SignDiabetesHistory
              , viewBoolInput language
                    form.diabetesInFamily
                    (SetFamilyHistoryBoolInput
                        (\value form_ ->
                            { form_ | diabetesInFamily = Just value, diabetesPredecessors = Nothing, diabetesPredecessorsDirty = True }
                        )
                    )
                    "diabetes-in-family"
                    Nothing
              ]
                ++ derivedInput
            , form.diabetesInFamily :: derivedTask
            )

        viewPredecessorInput formField setMsg =
            [ viewQuestionLabel language Translate.WhoInFamilyHasCondition
            , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
            , viewCheckBoxMultipleSelectInput language
                [ PredecessorMother
                , PredecessorFather
                , PredecessorGrandMother
                , PredecessorGrandFather
                ]
                []
                (Maybe.withDefault [] formField)
                Nothing
                setMsg
                Translate.Predecessor
            ]
    in
    ( hypertensionInputs ++ heartProblemInputs ++ diabetesInputs
    , hypertensionTasks ++ heartProblemTasks ++ diabetesTasks
    )


outsideCareDiagnosesLeftColumn : List MedicalCondition
outsideCareDiagnosesLeftColumn =
    [ MedicalConditionNeuropathy
    , MedicalConditionHIV
    , MedicalConditionRentalComplications
    , MedicalConditionMalaria
    , MedicalConditionTuberculosis
    , MedicalConditionHypertension
    ]


outsideCareDiagnosesRightColumn : List MedicalCondition
outsideCareDiagnosesRightColumn =
    [ MedicalConditionHepatitisB
    , MedicalConditionSyphilis
    , MedicalConditionEyeComplications
    , MedicalConditionAnemia
    , MedicalConditionPregnancy
    , MedicalConditionDiabetes
    ]


expectLaboratoryTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryTask currentDate assembled task =
    let
        testsDates =
            generatePreviousLaboratoryTestsDatesDict currentDate assembled

        initialTestRequired test =
            Dict.get test testsDates
                |> Maybe.map List.isEmpty
                |> Maybe.withDefault True

        recurrentTestRequired period test =
            Dict.get test testsDates
                |> Maybe.andThen
                    (List.sortWith Date.compare
                        >> List.reverse
                        >> List.head
                        >> Maybe.map (\lastTestDate -> diffMonths lastTestDate currentDate >= period)
                    )
                |> Maybe.withDefault True
    in
    case task of
        TaskRandomBloodSugarTest ->
            True

        TaskUrineDipstickTest ->
            recurrentTestRequired 12 TaskUrineDipstickTest

        TaskHIVTest ->
            let
                notKnownAsPositive =
                    List.filter
                        (.measurements
                            >> .hivTest
                            >> getMeasurementValueFunc
                            >> Maybe.map (.executionNote >> (==) TestNoteKnownAsPositive)
                            >> Maybe.withDefault False
                        )
                        assembled.previousEncountersData
                        |> List.isEmpty
            in
            notKnownAsPositive && initialTestRequired TaskHIVTest

        TaskPregnancyTest ->
            isPersonAFertileWoman currentDate assembled.person
                && initialTestRequired TaskPregnancyTest

        TaskCreatinineTest ->
            recurrentTestRequired 12 TaskCreatinineTest

        TaskLiverFunctionTest ->
            recurrentTestRequired 12 TaskLiverFunctionTest

        TaskLipidPanelTest ->
            recurrentTestRequired 12 TaskLiverFunctionTest

        TaskHbA1cTest ->
            recurrentTestRequired 6 TaskHbA1cTest

        -- Others are not in use at NCD.
        _ ->
            False


generatePreviousLaboratoryTestsDatesDict : NominalDate -> AssembledData -> Dict LaboratoryTask (List NominalDate)
generatePreviousLaboratoryTestsDatesDict currentDate assembled =
    let
        generateTestDates getMeasurementFunc resultsExistFunc resultsValidFunc =
            List.filterMap
                (.measurements
                    >> getMeasurementFunc
                    >> resolveLabTestDate currentDate resultsExistFunc resultsValidFunc
                )
                assembled.previousEncountersData
    in
    [ ( TaskRandomBloodSugarTest, generateTestDates .randomBloodSugarTest (.sugarCount >> isJust) (always True) )
    , ( TaskUrineDipstickTest, generateTestDates .urineDipstickTest (.protein >> isJust) (always True) )
    , ( TaskHIVTest, generateTestDates .hivTest (always True) isTestResultValid )
    , ( TaskPregnancyTest, generateTestDates .pregnancyTest (.testResult >> isJust) isTestResultValid )
    , ( TaskCreatinineTest, generateTestDates .creatinineTest (.creatinineResult >> isJust) (always True) )
    , ( TaskLiverFunctionTest, generateTestDates .liverFunctionTest (.altResult >> isJust) (always True) )
    , ( TaskHbA1cTest, generateTestDates .hba1cTest (.hba1cResult >> isJust) (always True) )
    ]
        |> Dict.fromList


laboratoryTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryTaskCompleted currentDate assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectLaboratoryTask currentDate assembled
    in
    case task of
        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || isJust measurements.randomBloodSugarTest

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || isJust measurements.urineDipstickTest

        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || isJust measurements.hivTest

        TaskPregnancyTest ->
            (not <| taskExpected TaskPregnancyTest) || isJust measurements.pregnancyTest

        TaskCreatinineTest ->
            (not <| taskExpected TaskCreatinineTest) || isJust measurements.creatinineTest

        TaskLiverFunctionTest ->
            (not <| taskExpected TaskLiverFunctionTest) || isJust measurements.liverFunctionTest

        TaskLipidPanelTest ->
            (not <| taskExpected TaskLipidPanelTest) || isJust measurements.lipidPanelTest

        TaskHbA1cTest ->
            (not <| taskExpected TaskHbA1cTest) || isJust measurements.hba1cTest

        -- Others are not in use at NCD.
        _ ->
            False


laboratoryTasks : List LaboratoryTask
laboratoryTasks =
    [ TaskRandomBloodSugarTest
    , TaskCreatinineTest
    , TaskUrineDipstickTest
    , TaskHIVTest
    , TaskPregnancyTest
    , TaskLiverFunctionTest
    , TaskLipidPanelTest
    , TaskHbA1cTest
    ]


toHealthEducationValueWithDefault : Maybe NCDHealthEducationValue -> Pages.NCD.Activity.Model.HealthEducationForm -> Maybe NCDHealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue saved


healthEducationFormWithDefault :
    Pages.NCD.Activity.Model.HealthEducationForm
    -> Maybe NCDHealthEducationValue
    -> Pages.NCD.Activity.Model.HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { hypertension = or form.hypertension (EverySet.member EducationHypertension value |> Just) }
            )


toHealthEducationValue : Maybe NCDHealthEducationValue -> Pages.NCD.Activity.Model.HealthEducationForm -> Maybe NCDHealthEducationValue
toHealthEducationValue saved form =
    [ ifNullableTrue EducationHypertension form.hypertension ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDHealthEducationSigns)


nextStepsTasksCompletedFromTotal :
    NominalDate
    -> AssembledData
    -> NextStepsData
    -> Pages.NCD.Activity.Types.NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate assembled data task =
    case task of
        TaskHealthEducation ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInputsAndTasks English currentDate
            in
            resolveTasksCompletedFromTotal tasks

        TaskMedicationDistribution ->
            let
                ( _, completed, total ) =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> resolveMedicationDistributionInputsAndTasks English
                            currentDate
                            NCDEncounterPhaseInitial
                            assembled
                            SetRecommendedTreatmentSignSingle
                            SetRecommendedTreatmentSignMultiple
                            SetMedicationDistributionBoolInput
            in
            ( completed, total )

        TaskReferral ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.referral
                        |> referralFormWithDefault data.referralForm
                        |> resolveReferralInputsAndTasks English
                            currentDate
                            NCDEncounterPhaseInitial
                            assembled
                            SetReferralBoolInput
                            SetFacilityNonReferralReason
            in
            resolveTasksCompletedFromTotal tasks


healthEducationFormInputsAndTasks :
    Language
    -> NominalDate
    -> Pages.NCD.Activity.Model.HealthEducationForm
    -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language currentDate form =
    ( [ viewCustomLabel language Translate.NCDHealthEducationHeader "" "label header"
      , viewCustomLabel language Translate.NCDHealthEducationInstructions "." "label paragraph"
      , viewQuestionLabel language Translate.NCDHealthEducationQuestion
      , viewBoolInput
            language
            form.hypertension
            (SetHealthEducationBoolInput (\value form_ -> { form_ | hypertension = Just value }))
            "hypertension"
            Nothing
      ]
    , [ form.hypertension ]
    )
