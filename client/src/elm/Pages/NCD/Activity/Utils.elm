module Pages.NCD.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NCDEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (corePhysicalExamFormWithDefault, vitalsFormWithDefault)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Types exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewNumberInput
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
expectActivity currentDate assembled db activity =
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
            True

        Laboratory ->
            True

        NextSteps ->
            -- @todo
            False


activityCompleted : NominalDate -> AssembledData -> ModelIndexedDb -> NCDActivity -> Bool
activityCompleted currentDate assembled db activity =
    case activity of
        DangerSigns ->
            isJust assembled.measurements.dangerSigns

        SymptomReview ->
            isJust assembled.measurements.symptomReview

        Examination ->
            isJust assembled.measurements.coreExam
                && isJust assembled.measurements.vitals

        FamilyPlanning ->
            (not <| expectActivity currentDate assembled db FamilyPlanning)
                || isJust assembled.measurements.familyPlanning

        MedicalHistory ->
            isJust assembled.measurements.coMorbidities
                && isJust assembled.measurements.medicationHistory
                && isJust assembled.measurements.socialHistory
                && isJust assembled.measurements.familyHistory
                && isJust assembled.measurements.outsideCare

        Laboratory ->
            List.all (laboratoryTaskCompleted currentDate assembled) laboratoryTasks

        -- @todo
        NextSteps ->
            False


resolvePreviousValue : AssembledData -> (NCDMeasurements -> Maybe ( id, NCDMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( _, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (NCDMeasurements -> Maybe ( id, NCDMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( _, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.andThen (Tuple.second >> .value >> valueFunc)
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
            Maybe.map
                (\birthDate ->
                    let
                        form =
                            assembled.measurements.vitals
                                |> getMeasurementValueFunc
                                |> vitalsFormWithDefault data.vitalsForm

                        ageYears =
                            Gizra.NominalDate.diffYears birthDate currentDate

                        ( ageDependentInputsCompleted, ageDependentInputsActive ) =
                            if ageYears < 12 then
                                ( 0, 0 )

                            else
                                ( taskCompleted form.sysBloodPressure
                                    + taskCompleted form.diaBloodPressure
                                , 2
                                )
                    in
                    ( taskCompleted form.heartRate
                        + taskCompleted form.respiratoryRate
                        + taskCompleted form.bodyTemperature
                        + ageDependentInputsCompleted
                    , 3 + ageDependentInputsActive
                    )
                )
                assembled.person.birthDate
                |> Maybe.withDefault ( 0, 0 )

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


medicalHistoryTasksCompletedFromTotal : NominalDate -> AssembledData -> MedicalHistoryData -> MedicalHistoryTask -> ( Int, Int )
medicalHistoryTasksCompletedFromTotal currentDate assembled data task =
    case task of
        TaskCoMorbidities ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.coMorbidities
                        |> coMorbiditiesFormWithDefault data.coMorbiditiesForm
            in
            ( taskCompleted form.conditions, 1 )

        TaskMedicationHistory ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medicationHistory
                        |> medicationHistoryFormWithDefault data.medicationHistoryForm
            in
            ( taskCompleted form.medicationsCausingHypertension
                + taskCompleted form.medicationsTreatingHypertension
                + taskCompleted form.medicationsTreatingDiabetes
            , 3
            )

        TaskSocialHistory ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.socialHistory
                        |> socialHistoryFormWithDefault data.socialHistoryForm
                        |> socialHistoryFormInputsAndTasks English currentDate
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )

        TaskFamilyHistory ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.familyHistory
                        |> familyHistoryFormWithDefault data.familyHistoryForm
                        |> familyHistoryFormInputsAndTasks English currentDate
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )

        TaskOutsideCare ->
            -- This is not in use, because OutsideCare task got
            -- special treatment at viewMedicalHistoryContent().
            ( 0, 0 )


socialHistoryFormInputsAndTasks : Language -> NominalDate -> SocialHistoryForm -> ( List (Html Msg), List (Maybe Bool) )
socialHistoryFormInputsAndTasks language currentDate form =
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
            ( [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion SignDrinkAlcohol
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
            ( [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion SignSmokeCigarettes
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
        ++ [ viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion SignConsumeSalt
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
           , viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion SignDifficult4TimesAYear
           , viewBoolInput language
                form.difficult4Times
                (SetSocialHistoryBoolInput
                    (\value form_ ->
                        { form_ | difficult4Times = Just value }
                    )
                )
                "difficult-4-times"
                Nothing
           , viewQuestionLabel language <| Translate.NCDSocialHistorySignQuestion SignHelpWithTreatmentAtHome
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
        ++ [ form.salt, form.difficult4Times, form.helpAtHome ]
        ++ [ maybeToBoolTask form.foodGroup ]
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
    case task of
        TaskRandomBloodSugarTest ->
            True

        TaskUrineDipstickTest ->
            True

        TaskHIVTest ->
            True

        TaskPregnancyTest ->
            isPersonAFertileWoman currentDate assembled.person

        TaskCreatinineTest ->
            True

        TaskLiverFunctionTest ->
            True

        -- Others are not in use at NCD.
        _ ->
            False


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
    ]
