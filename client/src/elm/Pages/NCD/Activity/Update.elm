module Pages.NCD.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , FamilyPlanningSign(..)
        , HandsCPESign(..)
        , LegsCPESign(..)
        , LungsCPESign(..)
        , MedicalCondition(..)
        , MedicationCausingHypertension(..)
        , MedicationTreatingDiabetes(..)
        , MedicationTreatingHypertension(..)
        , NCDDangerSign(..)
        , NCDGroup1Symptom(..)
        , NCDGroup2Symptom(..)
        , NCDPainSymptom(..)
        , NeckCPESign(..)
        , OutsideCareMedication(..)
        , Predecessor(..)
        , TestExecutionNote(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, nonReferralReasonToSign, testResultFromString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , familyPlanningFormWithDefault
        , outsideCareFormWithDefault
        , toCorePhysicalExamValueWithDefault
        , toCreatinineTestValueWithEmptyResults
        , toFamilyPlanningValueWithDefault
        , toHIVTestValueWithDefault
        , toHbA1cTestValueWithDefault
        , toLipidPanelTestValueWithEmptyResults
        , toLiverFunctionTestValueWithEmptyResults
        , toNonRDTValueWithDefault
        , toOutsideCareValueWithDefault
        , toPregnancyTestValueWithDefault
        , toRandomBloodSugarTestValueWithDefault
        , toUrineDipstickTestValueWithDefault
        , toVitalsValueWithDefault
        )
import Pages.NCD.Activity.Model exposing (Model, Msg(..))
import Pages.NCD.Activity.Utils exposing (coMorbiditiesFormWithDefault, dangerSignsFormWithDefault, familyHistoryFormWithDefault, medicationHistoryFormWithDefault, symptomReviewFormWithDefault, toCoMorbiditiesValueWithDefault, toDangerSignsValueWithDefault, toFamilyHistoryValueWithDefault, toHealthEducationValueWithDefault, toMedicationHistoryValueWithDefault, toSocialHistoryValueWithDefault, toSymptomReviewValueWithDefault)
import Pages.NCD.Utils exposing (medicationDistributionFormWithDefault, referralFormWithDefault, toMedicationDistributionValueWithDefault, toReferralValueWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (saveMeasurementMsgs, setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> NCDEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        symptomReviewForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.symptomReview
                        >> getMeasurementValueFunc
                        >> symptomReviewFormWithDefault model.symptomReviewData.form
                    )
                |> Maybe.withDefault model.symptomReviewData.form

        coreExamForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.coreExam
                        >> getMeasurementValueFunc
                        >> corePhysicalExamFormWithDefault model.examinationData.coreExamForm
                    )
                |> Maybe.withDefault model.examinationData.coreExamForm

        medicationHistoryForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicationHistory
                        >> getMeasurementValueFunc
                        >> medicationHistoryFormWithDefault model.medicalHistoryData.medicationHistoryForm
                    )
                |> Maybe.withDefault model.medicalHistoryData.medicationHistoryForm

        familyHistoryForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.familyHistory
                        >> getMeasurementValueFunc
                        >> familyHistoryFormWithDefault model.medicalHistoryData.familyHistoryForm
                    )
                |> Maybe.withDefault model.medicalHistoryData.familyHistoryForm

        outsideCareForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.outsideCare
                        >> getMeasurementValueFunc
                        >> outsideCareFormWithDefault model.medicalHistoryData.outsideCareForm
                    )
                |> Maybe.withDefault model.medicalHistoryData.outsideCareForm

        medicationDistributionForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicationDistribution
                        >> getMeasurementValueFunc
                        >> medicationDistributionFormWithDefault model.nextStepsData.medicationDistributionForm
                    )
                |> Maybe.withDefault model.nextStepsData.medicationDistributionForm

        generateExaminationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveExaminationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]

        generateMedicalHistoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicalHistoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]

        generateLaboratoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveLaboratoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]

        toIndexedDbMsg =
            Backend.Model.MsgNCDEncounter id >> App.Model.MsgIndexedDb
    in
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDangerSign sign ->
            let
                form =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.dangerSigns
                                >> getMeasurementValueFunc
                                >> dangerSignsFormWithDefault model.dangerSignsData.form
                            )
                        |> Maybe.withDefault model.dangerSignsData.form

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoNCDDangerSigns
                        sign
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDangerSigns personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.dangerSignsData.form
                        |> toDangerSignsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NCDEncounter.Model.SaveDangerSigns personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetGroup1Symptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .group1Symptoms
                        (\symptoms -> { symptomReviewForm | group1Symptoms = symptoms })
                        NoNCDGroup1Symptoms
                        sign
                        symptomReviewForm

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetGroup2Symptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .group2Symptoms
                        (\symptoms -> { symptomReviewForm | group2Symptoms = symptoms })
                        NoNCDGroup2Symptoms
                        sign
                        symptomReviewForm

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetPainSymptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .painSymptoms
                        (\symptoms -> { symptomReviewForm | painSymptoms = symptoms })
                        NoNCDPainSymptoms
                        sign
                        symptomReviewForm

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.symptomReviewData.form
                        |> toSymptomReviewValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NCDEncounter.Model.SaveSymptomReview personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetFamilyPlanningSign sign ->
            let
                form =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.familyPlanning
                                >> getMeasurementValueFunc
                                >> familyPlanningFormWithDefault model.familyPlanningData.form
                            )
                        |> Maybe.withDefault model.familyPlanningData.form

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoFamilyPlanning
                        sign
                        form

                updatedData =
                    model.familyPlanningData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | familyPlanningData = updatedData }
            , Cmd.none
            , []
            )

        SaveFamilyPlanning personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.familyPlanningData.form
                        |> toFamilyPlanningValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NCDEncounter.Model.SaveFamilyPlanning personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveExaminationTask task ->
            let
                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsIntInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc (String.toInt value) model.examinationData.vitalsForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsFloatInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc (String.toFloat value) model.examinationData.vitalsForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVitalsValueWithDefault
                model.examinationData.vitalsForm
                saved
                (Backend.NCDEncounter.Model.SaveVitals personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateExaminationMsgs nextTask)

        SetCoreExamBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.examinationData.coreExamForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamHeart value ->
            let
                updatedForm =
                    { coreExamForm | heart = Just value }

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamNeck sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .neck
                        (\signs -> { form | neck = signs })
                        NormalNeck
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamLungs sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .lungs
                        (\signs -> { form | lungs = signs })
                        NormalLungs
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamAbdomen sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .abdomen
                        (\signs -> { form | abdomen = signs })
                        NormalAbdomen
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamHands sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .hands
                        (\signs -> { form | hands = signs })
                        NormalHands
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamLegs sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .legs
                        (\signs -> { form | legs = signs })
                        NormalLegs
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveCoreExam personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toCorePhysicalExamValueWithDefault
                model.examinationData.coreExamForm
                saved
                (Backend.NCDEncounter.Model.SaveCoreExam personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateExaminationMsgs nextTask)

        SetActiveMedicalHistoryTask task ->
            let
                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicalCondition condition ->
            let
                form =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.coMorbidities
                                >> getMeasurementValueFunc
                                >> coMorbiditiesFormWithDefault model.medicalHistoryData.coMorbiditiesForm
                            )
                        |> Maybe.withDefault model.medicalHistoryData.coMorbiditiesForm

                updatedForm =
                    setMultiSelectInputValue .conditions
                        (\conditions -> { form | conditions = conditions })
                        NoMedicalConditions
                        condition
                        form

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | coMorbiditiesForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveCoMorbidities personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toCoMorbiditiesValueWithDefault
                model.medicalHistoryData.coMorbiditiesForm
                saved
                (Backend.NCDEncounter.Model.SaveCoMorbidities personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateMedicalHistoryMsgs nextTask)

        SetMedicationCausingHypertension medication ->
            let
                updatedForm =
                    setMultiSelectInputValue .medicationsCausingHypertension
                        (\medications -> { medicationHistoryForm | medicationsCausingHypertension = medications })
                        NoMedicationCausingHypertension
                        medication
                        medicationHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | medicationHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationTreatingHypertension medication ->
            let
                updatedForm =
                    setMultiSelectInputValue .medicationsTreatingHypertension
                        (\medications -> { medicationHistoryForm | medicationsTreatingHypertension = medications })
                        NoMedicationTreatingHypertension
                        medication
                        medicationHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | medicationHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationTreatingDiabetes medication ->
            let
                updatedForm =
                    setMultiSelectInputValue .medicationsTreatingDiabetes
                        (\medications -> { medicationHistoryForm | medicationsTreatingDiabetes = medications })
                        NoMedicationTreatingDiabetes
                        medication
                        medicationHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | medicationHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationHistory personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toMedicationHistoryValueWithDefault
                model.medicalHistoryData.medicationHistoryForm
                saved
                (Backend.NCDEncounter.Model.SaveMedicationHistory personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateMedicalHistoryMsgs nextTask)

        SetSocialHistoryBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.medicalHistoryData.socialHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | socialHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetSocialHistoryIntInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc (String.toInt value) model.medicalHistoryData.socialHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | socialHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetFoodGroup value ->
            let
                updatedForm =
                    model.medicalHistoryData.socialHistoryForm
                        |> (\form -> { form | foodGroup = Just value })

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | socialHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveSocialHistory personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toSocialHistoryValueWithDefault
                model.medicalHistoryData.socialHistoryForm
                saved
                (Backend.NCDEncounter.Model.SaveSocialHistory personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateMedicalHistoryMsgs nextTask)

        SetFamilyHistoryBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.medicalHistoryData.familyHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | familyHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHypertensionPredecessor predecessor ->
            let
                updatedForm =
                    setMultiSelectInputValue .hypertensionPredecessors
                        (\hypertensionPredecessors ->
                            { familyHistoryForm
                                | hypertensionPredecessors = hypertensionPredecessors
                                , hypertensionPredecessorsDirty = True
                            }
                        )
                        NoPredecessors
                        predecessor
                        familyHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | familyHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHeartProblemPredecessor predecessor ->
            let
                updatedForm =
                    setMultiSelectInputValue .heartProblemPredecessors
                        (\heartProblemPredecessors ->
                            { familyHistoryForm
                                | heartProblemPredecessors = heartProblemPredecessors
                                , heartProblemPredecessorsDirty = True
                            }
                        )
                        NoPredecessors
                        predecessor
                        familyHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | familyHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetDiabetesPredecessor predecessor ->
            let
                updatedForm =
                    setMultiSelectInputValue .diabetesPredecessors
                        (\diabetesPredecessors ->
                            { familyHistoryForm
                                | diabetesPredecessors = diabetesPredecessors
                                , diabetesPredecessorsDirty = True
                            }
                        )
                        NoPredecessors
                        predecessor
                        familyHistoryForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | familyHistoryForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveFamilyHistory personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toFamilyHistoryValueWithDefault
                model.medicalHistoryData.familyHistoryForm
                saved
                (Backend.NCDEncounter.Model.SaveFamilyHistory personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateMedicalHistoryMsgs nextTask)

        SetOutsideCareStep step ->
            let
                updatedForm =
                    model.medicalHistoryData.outsideCareForm
                        |> (\form -> { form | step = step })

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareSignBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.medicalHistoryData.outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareDiagnosis diagnosis ->
            let
                updatedForm =
                    setMultiSelectInputValue .diagnoses
                        (\value -> { outsideCareForm | diagnoses = value })
                        MedicalConditionOther
                        diagnosis
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareMalariaMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .malariaMedications
                        (\medication -> { outsideCareForm | malariaMedications = medication })
                        NoOutsideCareMedicationForMalaria
                        value
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareHypertensionMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .hypertensionMedications
                        (\medication -> { outsideCareForm | hypertensionMedications = medication })
                        NoOutsideCareMedicationForHypertension
                        value
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareSyphilisMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .syphilisMedications
                        (\medication -> { outsideCareForm | syphilisMedications = medication })
                        NoOutsideCareMedicationForSyphilis
                        value
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareAnemiaMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .anemiaMedications
                        (\medication -> { outsideCareForm | anemiaMedications = medication })
                        NoOutsideCareMedicationForAnemia
                        value
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareHIVMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .hivMedications
                        (\medication -> { outsideCareForm | hivMedications = medication })
                        NoOutsideCareMedicationForHIV
                        value
                        outsideCareForm

                updatedData =
                    model.medicalHistoryData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | medicalHistoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveOutsideCare personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs (toOutsideCareValueWithDefault NoMedicalConditions)
                model.medicalHistoryData.outsideCareForm
                saved
                (Backend.NCDEncounter.Model.SaveOutsideCare personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateMedicalHistoryMsgs nextTask)

        SetActiveLaboratoryTask task ->
            let
                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestResult value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form
                        | testResult = testResultFromString value
                        , hivProgramHC = Nothing
                        , hivProgramHCDirty = True
                        , partnerHIVPositive = Nothing
                        , partnerHIVPositiveDirty = True
                        , partnerTakingARV = Nothing
                        , partnerTakingARVDirty = True
                        , partnerSurpressedViralLoad = Nothing
                        , partnerSurpressedViralLoadDirty = True
                    }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.hivTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHIVTest personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHIVTestValueWithDefault
                model.laboratoryData.hivTestForm
                saved
                (Backend.NCDEncounter.Model.SaveHIVTest personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateLaboratoryMsgs nextTask)

        SetUrineDipstickTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestVariant value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | testVariant = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveUrineDipstickTest personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toUrineDipstickTestValueWithDefault
                model.laboratoryData.urineDipstickTestForm
                saved
                (Backend.NCDEncounter.Model.SaveUrineDipstickTest personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateLaboratoryMsgs nextTask)

        SetRandomBloodSugarTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarResult value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value, sugarCountDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        PreSaveRandomBloodSugarTest personId saved nextTask ->
            let
                form =
                    getMeasurementValueFunc saved
                        |> Measurement.Utils.randomBloodSugarFormWithDefault model.laboratoryData.randomBloodSugarTestForm

                outOfRange =
                    Measurement.Utils.bloodGlucoseOutOfRange form.sugarCount

                extraMsgs =
                    if List.isEmpty outOfRange then
                        [ SaveRandomBloodSugarTest personId saved nextTask ]

                    else
                        [ SetMeasurementOutOfRangePopupState outOfRange ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetMeasurementOutOfRangePopupState state ->
            ( { model | measurementOutOfRangePopupState = state }, Cmd.none, [] )

        SaveRandomBloodSugarTest personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toRandomBloodSugarTestValueWithDefault
                model.laboratoryData.randomBloodSugarTestForm
                saved
                (Backend.NCDEncounter.Model.SaveRandomBloodSugarTest personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateLaboratoryMsgs nextTask)

        SetPregnancyTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.pregnancyTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPregnancyTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.pregnancyTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPregnancyTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.pregnancyTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPregnancyTestResult value ->
            let
                form =
                    model.laboratoryData.pregnancyTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPregnancyTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.pregnancyTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SavePregnancyTest personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toPregnancyTestValueWithDefault
                model.laboratoryData.pregnancyTestForm
                saved
                (Backend.NCDEncounter.Model.SavePregnancyTest personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateLaboratoryMsgs nextTask)

        SetCreatinineTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.creatinineTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetCreatinineTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.creatinineTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetCreatinineTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.creatinineTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetCreatinineTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.creatinineTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | creatinineTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveCreatinineTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.creatinineTestForm
                        |> toNonRDTValueWithDefault measurement toCreatinineTestValueWithEmptyResults
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveCreatinineTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetLiverFunctionTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.liverFunctionTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLiverFunctionTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.liverFunctionTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLiverFunctionTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.liverFunctionTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLiverFunctionTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.liverFunctionTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | liverFunctionTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveLiverFunctionTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.liverFunctionTestForm
                        |> toNonRDTValueWithDefault measurement toLiverFunctionTestValueWithEmptyResults
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveLiverFunctionTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetLipidPanelTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.lipidPanelTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLipidPanelTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.lipidPanelTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLipidPanelTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.lipidPanelTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLipidPanelTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.lipidPanelTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | lipidPanelTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveLipidPanelTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.lipidPanelTestForm
                        |> toNonRDTValueWithDefault measurement toLipidPanelTestValueWithEmptyResults
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveLipidPanelTest personId measurementId
                                >> Backend.Model.MsgNCDEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs

        SetHbA1cTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hba1cTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hba1cTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHbA1cTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.hba1cTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hba1cTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHbA1cTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.hba1cTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                -- The date is set when date selectore is closed by pressing
                -- 'Save' button. Once it happens, we get here, and examine the
                -- date that was entered, and set execution note according to
                -- time that has passed since.
                executionNote =
                    Maybe.map
                        (\selectedDate ->
                            if Date.diff Months selectedDate currentDate >= 6 then
                                TestNoteToBeDoneAtHospital

                            else
                                TestNoteRunPreviously
                        )
                        defaultSelection

                updatedForm =
                    { form
                        | dateSelectorPopupState = state
                        , executionDate = defaultSelection
                        , executionNote = executionNote
                    }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hba1cTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHbA1cTestResult value ->
            let
                form =
                    model.laboratoryData.hba1cTestForm

                updatedForm =
                    { form | hba1cResult = String.toFloat value, hba1cResultDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hba1cTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHbA1cTest personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHbA1cTestValueWithDefault
                model.laboratoryData.hba1cTestForm
                saved
                (Backend.NCDEncounter.Model.SaveHbA1cTest personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateLaboratoryMsgs nextTask)

        SetActiveNextStepsTask task ->
            let
                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHealthEducationBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.healthEducationForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | healthEducationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHealthEducation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHealthEducationValueWithDefault
                model.nextStepsData.healthEducationForm
                saved
                (Backend.NCDEncounter.Model.SaveHealthEducation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateNextStepsMsgs nextTask)

        SetRecommendedTreatmentSignSingle allowedSigns sign ->
            let
                updatedSigns =
                    -- Since we may have values from recurrent phase of encounter, we make
                    -- sure to preserve them, before setting new value at inital phase.
                    Maybe.map
                        (\signs ->
                            List.filter (\sign_ -> not <| List.member sign_ allowedSigns) signs
                                |> List.append [ sign ]
                        )
                        medicationDistributionForm.recommendedTreatmentSigns
                        |> Maybe.withDefault [ sign ]

                updatedForm =
                    { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendedTreatmentSignMultiple allowedSigns noneValue sign ->
            let
                ( currentSigns, preservedSigns ) =
                    Maybe.map
                        (\signs ->
                            List.partition (\sign_ -> List.member sign_ allowedSigns) signs
                        )
                        medicationDistributionForm.recommendedTreatmentSigns
                        |> Maybe.withDefault ( [], [] )

                form =
                    { signs = Just currentSigns }

                formAfterUpdate =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        noneValue
                        sign
                        form

                updatedSigns =
                    Maybe.withDefault [] formAfterUpdate.signs
                        |> List.append preservedSigns

                updatedForm =
                    { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationDistributionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.medicationDistributionForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationDistribution personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toMedicationDistributionValueWithDefault
                model.nextStepsData.medicationDistributionForm
                saved
                (Backend.NCDEncounter.Model.SaveMedicationDistribution personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateNextStepsMsgs nextTask)

        SetReferralBoolInput updateFunc value ->
            let
                updatedForm =
                    updateFunc value model.nextStepsData.referralForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetFacilityNonReferralReason currentValue facility reason ->
            let
                referralForm =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.referral
                                >> getMeasurementValueFunc
                                >> referralFormWithDefault model.nextStepsData.referralForm
                            )
                        |> Maybe.withDefault model.nextStepsData.referralForm

                updatedValue =
                    nonReferralReasonToSign facility reason

                updatedNonReferralReasons =
                    Maybe.map
                        (\nonReferralReasons ->
                            case currentValue of
                                Just value ->
                                    EverySet.remove (nonReferralReasonToSign facility value) nonReferralReasons
                                        |> EverySet.insert updatedValue

                                Nothing ->
                                    EverySet.insert updatedValue nonReferralReasons
                        )
                        referralForm.nonReferralReasons
                        |> Maybe.withDefault (EverySet.singleton updatedValue)

                updatedForm =
                    { referralForm | nonReferralReasons = Just updatedNonReferralReasons }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveReferral personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toReferralValueWithDefault
                model.nextStepsData.referralForm
                saved
                (Backend.NCDEncounter.Model.SaveReferral personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate id db) (generateNextStepsMsgs nextTask)
