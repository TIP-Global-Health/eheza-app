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
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (corePhysicalExamFormWithDefault, vitalsFormWithDefault)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Types exposing (..)
import Pages.NCD.Encounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, ifTrue, maybeValueConsideringIsDirtyField, taskCompleted)
import RemoteData exposing (RemoteData(..))


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

        -- @todo
        _ ->
            True


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

        -- @todo
        _ ->
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
                        form.hypertensionPredecessors
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
                { medicationCausingHypertension = or form.medicationCausingHypertension (EverySet.toList value.medicationCausingHypertension |> Just)
                , medicationTreatingHypertension = or form.medicationTreatingHypertension (EverySet.toList value.medicationTreatingHypertension |> Just)
                , medicationTreatingDiabetes = or form.medicationTreatingDiabetes (EverySet.toList value.medicationTreatingDiabetes |> Just)
                }
            )


toMedicationHistoryValueWithDefault : Maybe NCDMedicationHistoryValue -> MedicationHistoryForm -> Maybe NCDMedicationHistoryValue
toMedicationHistoryValueWithDefault saved form =
    medicationHistoryFormWithDefault form saved
        |> toMedicationHistoryValue


toMedicationHistoryValue : MedicationHistoryForm -> Maybe NCDMedicationHistoryValue
toMedicationHistoryValue form =
    let
        medicationCausingHypertension =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationCausingHypertension) form.medicationCausingHypertension

        medicationTreatingHypertension =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationTreatingHypertension) form.medicationTreatingHypertension

        medicationTreatingDiabetes =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoMedicationTreatingDiabetes) form.medicationTreatingDiabetes
    in
    Maybe.map NCDMedicationHistoryValue medicationCausingHypertension
        |> andMap medicationTreatingHypertension
        |> andMap medicationTreatingDiabetes


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
            ( 0, 1 )

        TaskSocialHistory ->
            ( 0, 1 )

        TaskFamilyHistory ->
            ( 0, 1 )

        TaskOutsideCare ->
            ( 0, 1 )
