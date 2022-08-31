module Pages.NCD.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , AdministrationNote(..)
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
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , familyPlanningFormWithDefault
        , toCorePhysicalExamValueWithDefault
        , toFamilyPlanningValueWithDefault
        , toVitalsValueWithDefault
        )
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMultiSelectInputValue)
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

        generateExaminationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveExaminationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]

        generateMedicalHistoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicalHistoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NCDEncounterPage id ]
    in
    case msg of
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toVitalsValueWithDefault measurement model.examinationData.vitalsForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveVitals personId measurementId
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toCorePhysicalExamValueWithDefault measurement model.examinationData.coreExamForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveCoreExam personId measurementId
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicalHistoryMsgs nextTask

                appMsgs =
                    toCoMorbiditiesValueWithDefault measurement model.medicalHistoryData.coMorbiditiesForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveCoMorbidities personId measurementId
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

        SetMedicationCausingHypertension medication ->
            let
                updatedForm =
                    setMultiSelectInputValue .medicationCausingHypertension
                        (\medications -> { medicationHistoryForm | medicationCausingHypertension = medications })
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
                    setMultiSelectInputValue .medicationTreatingHypertension
                        (\medications -> { medicationHistoryForm | medicationTreatingHypertension = medications })
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
                    setMultiSelectInputValue .medicationTreatingDiabetes
                        (\medications -> { medicationHistoryForm | medicationTreatingDiabetes = medications })
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicalHistoryMsgs nextTask

                appMsgs =
                    toMedicationHistoryValueWithDefault measurement model.medicalHistoryData.medicationHistoryForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveMedicationHistory personId measurementId
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

        SaveSocialHistory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicalHistoryMsgs nextTask

                appMsgs =
                    toSocialHistoryValueWithDefault measurement model.medicalHistoryData.socialHistoryForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveSocialHistory personId measurementId
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

        SaveFamilyHistory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicalHistoryMsgs nextTask

                appMsgs =
                    toFamilyHistoryValueWithDefault measurement model.medicalHistoryData.familyHistoryForm
                        |> Maybe.map
                            (Backend.NCDEncounter.Model.SaveFamilyHistory personId measurementId
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

        SaveOutsideCare personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicalHistoryMsgs nextTask

                appMsgs =
                    -- @todo
                    -- toOutsideCareValueWithDefault measurement model.medicalHistoryData.outsideCareForm
                    --     |> Maybe.map
                    --         (Backend.NCDEncounter.Model.SaveOutsideCare personId measurementId
                    --             >> Backend.Model.MsgNCDEncounter id
                    --             >> App.Model.MsgIndexedDb
                    --             >> List.singleton
                    --         )
                    --     |> Maybe.withDefault []
                    []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) extraMsgs
