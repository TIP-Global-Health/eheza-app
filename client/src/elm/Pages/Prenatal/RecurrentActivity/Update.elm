module Pages.Prenatal.RecurrentActivity.Update exposing (update, updateLabsHistory)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (IllnessSymptom(..), PhaseRecorded(..), ViralLoadStatus(..))
import Backend.Measurement.Utils exposing (bilirubinValueFromString, bloodGroupFromString, bloodSmearResultFromString, getMeasurementValueFunc, glucoseValueFromString, haemoglobinValueFromString, ketoneValueFromString, leukocytesValueFromString, nitriteValueFromString, nonReferralReasonToSign, phValueFromString, proteinValueFromString, rhesusFromString, testResultFromString, urobilinogenValueFromString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..))
import EverySet
import Gizra.Update exposing (sequenceExtra)
import Measurement.Utils
    exposing
        ( syphilisResultFormWithDefault
        , toBloodGpRsResultValueWithDefault
        , toHIVPCRResultValueWithDefault
        , toHIVResultValueWithDefault
        , toHemoglobinResultValueWithDefault
        , toHepatitisBResultValueWithDefault
        , toMalariaResultValueWithDefault
        , toPartnerHIVResultValueWithDefault
        , toRandomBloodSugarResultValueWithDefault
        , toSyphilisResultValueWithDefault
        , toUrineDipstickResultValueWithDefault
        , toVitalsValueWithDefault
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.RecurrentActivity.Model exposing (LabResultsData, Model, Msg(..))
import Pages.Prenatal.RecurrentActivity.Utils exposing (toHealthEducationValueWithDefault)
import Pages.Prenatal.Utils exposing (medicationDistributionFormWithDefaultRecurrentPhase, referralFormWithDefault, toMalariaPreventionValueWithDefault, toMedicationDistributionValueWithDefaultRecurrentPhase, toPrenatalReferralValueWithDefault)
import Pages.Utils exposing (nonAdministrationReasonToSign, setMultiSelectInputValue)
import RemoteData


update : PrenatalEncounterId -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id isLabTech db msg model =
    let
        medicationDistributionForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicationDistribution
                        >> getMeasurementValueFunc
                        >> medicationDistributionFormWithDefaultRecurrentPhase model.nextStepsData.medicationDistributionForm
                    )
                |> Maybe.withDefault model.nextStepsData.medicationDistributionForm

        generateLabResultsMsgs nextTask =
            let
                defaultAction =
                    if isLabTech then
                        SetActivePage <|
                            UserPage <|
                                ClinicalProgressReportPage (InitiatorRecurrentEncounterPage id) id

                    else
                        SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id
            in
            Maybe.map (\task -> [ SetActiveLabResultsTask task ]) nextTask
                |> Maybe.withDefault [ defaultAction ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id ]
    in
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState value ->
            ( { model | showAlertsDialog = value }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

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

        SaveVitals personId saved ->
            let
                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.examinationData.vitalsForm
                        |> toVitalsValueWithDefault measurement
                        |> Maybe.map
                            (\value ->
                                let
                                    measurementId =
                                        Maybe.map Tuple.first saved
                                in
                                [ Backend.PrenatalEncounter.Model.SaveVitals personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id
                                ]
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveLabResultsTask task ->
            let
                updatedData =
                    model.labResultsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.syphilisTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestExecutionNote value ->
            let
                form =
                    model.labResultsData.syphilisTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestResult value ->
            let
                form =
                    model.labResultsData.syphilisTestForm

                updatedForm =
                    { form | testResult = testResultFromString value, symptoms = Nothing, symptomsDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetIllnessSymptom symptom ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.syphilisTest
                                >> getMeasurementValueFunc
                                >> syphilisResultFormWithDefault model.labResultsData.syphilisTestForm
                            )
                        |> Maybe.withDefault model.labResultsData.syphilisTestForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\symptoms -> { form | symptoms = symptoms, symptomsDirty = True })
                        NoIllnessSymptoms
                        symptom
                        form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSyphilisResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toSyphilisResultValueWithDefault isLabTech measurement model.labResultsData.syphilisTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSyphilisTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetHepatitisBTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.hepatitisBTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHepatitisBTestExecutionNote value ->
            let
                form =
                    model.labResultsData.hepatitisBTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHepatitisBTestResult value ->
            let
                form =
                    model.labResultsData.hepatitisBTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHepatitisBResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toHepatitisBResultValueWithDefault measurement model.labResultsData.hepatitisBTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHepatitisBTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetBloodGpRsTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.bloodGpRsTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodGpRsTestExecutionNote value ->
            let
                form =
                    model.labResultsData.bloodGpRsTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodGroup value ->
            let
                form =
                    model.labResultsData.bloodGpRsTestForm

                updatedForm =
                    { form | bloodGroup = bloodGroupFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetRhesus value ->
            let
                form =
                    model.labResultsData.bloodGpRsTestForm

                updatedForm =
                    { form | rhesus = rhesusFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveBloodGpRsResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toBloodGpRsResultValueWithDefault measurement model.labResultsData.bloodGpRsTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveBloodGpRsTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetUrineDipstickTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestExecutionNote value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetProtein value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | protein = proteinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetPH value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | ph = phValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetGlucose value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | glucose = glucoseValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetLeukocytes value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | leukocytes = leukocytesValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetNitrite value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | nitrite = nitriteValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetUrobilinogen value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | urobilinogen = urobilinogenValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHaemoglobin value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | haemoglobin = haemoglobinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetKetone value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | ketone = ketoneValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBilirubin value ->
            let
                form =
                    model.labResultsData.urineDipstickTestForm

                updatedForm =
                    { form | bilirubin = bilirubinValueFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveUrineDipstickResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toUrineDipstickResultValueWithDefault measurement model.labResultsData.urineDipstickTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveUrineDipstickTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetHemoglobinTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.hemoglobinTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHemoglobinTestExecutionNote value ->
            let
                form =
                    model.labResultsData.hemoglobinTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHemoglobin value ->
            let
                form =
                    model.labResultsData.hemoglobinTestForm

                updatedForm =
                    { form | hemoglobinCount = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHemoglobinResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toHemoglobinResultValueWithDefault measurement model.labResultsData.hemoglobinTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHemoglobinTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetRandomBloodSugarTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.randomBloodSugarTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestExecutionNote value ->
            let
                form =
                    model.labResultsData.randomBloodSugarTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugar value ->
            let
                form =
                    model.labResultsData.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveRandomBloodSugarResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toRandomBloodSugarResultValueWithDefault measurement model.labResultsData.randomBloodSugarTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveRandomBloodSugarTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetHIVPCRTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.hivPCRTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVPCRTestExecutionNote value ->
            let
                form =
                    model.labResultsData.hivPCRTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVViralLoadUndetectable undetectable ->
            let
                form =
                    model.labResultsData.hivPCRTestForm

                status =
                    if undetectable then
                        ViralLoadUndetectable

                    else
                        ViralLoadDetectable

                updatedForm =
                    { form | hivViralLoadStatus = Just status }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVViralLoad value ->
            let
                form =
                    model.labResultsData.hivPCRTestForm

                updatedForm =
                    { form | hivViralLoad = String.toFloat value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHIVPCRResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    toHIVPCRResultValueWithDefault measurement model.labResultsData.hivPCRTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHIVPCRTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetHIVTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.hivTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestExecutionNote value ->
            let
                form =
                    model.labResultsData.hivTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestResult value ->
            let
                form =
                    model.labResultsData.hivTestForm

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
                    model.labResultsData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHIVResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    model.labResultsData.hivTestForm
                        |> toHIVResultValueWithDefault isLabTech measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHIVTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetPartnerHIVTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.partnerHIVTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetPartnerHIVTestExecutionNote value ->
            let
                form =
                    model.labResultsData.partnerHIVTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetPartnerHIVTestResult value ->
            let
                form =
                    model.labResultsData.partnerHIVTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SavePartnerHIVResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    model.labResultsData.partnerHIVTestForm
                        |> toPartnerHIVResultValueWithDefault isLabTech measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SavePartnerHIVTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetMalariaTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.labResultsData.malariaTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetMalariaTestExecutionNote value ->
            let
                form =
                    model.labResultsData.malariaTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetMalariaTestResult value ->
            let
                form =
                    model.labResultsData.malariaTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodSmearResult value ->
            let
                form =
                    model.labResultsData.malariaTestForm

                updatedForm =
                    { form | bloodSmearResult = bloodSmearResultFromString value }

                updatedData =
                    model.labResultsData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | labResultsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaResult personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLabResultsMsgs nextTask

                appMsgs =
                    model.labResultsData.malariaTestForm
                        |> toMalariaResultValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMalariaTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

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
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.sendToHC
                                >> getMeasurementValueFunc
                                >> referralFormWithDefault model.nextStepsData.referralForm
                            )
                        |> Maybe.withDefault model.nextStepsData.referralForm

                updatedValue =
                    nonReferralReasonToSign facility reason

                updatedFacilityNonReferralReasons =
                    Maybe.map
                        (\facilityNonReferralReasons ->
                            case currentValue of
                                Just value ->
                                    EverySet.remove (nonReferralReasonToSign facility value) facilityNonReferralReasons
                                        |> EverySet.insert updatedValue

                                Nothing ->
                                    EverySet.insert updatedValue facilityNonReferralReasons
                        )
                        referralForm.facilityNonReferralReasons
                        |> Maybe.withDefault (EverySet.singleton updatedValue)

                updatedForm =
                    { referralForm | facilityNonReferralReasons = Just updatedFacilityNonReferralReasons }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSendToHC personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    model.nextStepsData.referralForm
                        |> toPrenatalReferralValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSendToHC personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

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

        SetMedicationDistributionAdministrationNote currentValue medication reason ->
            let
                updatedValue =
                    nonAdministrationReasonToSign medication reason

                updatedNonAdministrationSigns =
                    medicationDistributionForm.nonAdministrationSigns
                        |> Maybe.map
                            (\nonAdministrationSigns ->
                                case currentValue of
                                    Just value ->
                                        EverySet.remove (nonAdministrationReasonToSign medication value) nonAdministrationSigns
                                            |> EverySet.insert updatedValue

                                    Nothing ->
                                        EverySet.insert updatedValue nonAdministrationSigns
                            )
                        |> Maybe.withDefault (EverySet.singleton updatedValue)

                updatedForm =
                    { medicationDistributionForm | nonAdministrationSigns = Just updatedNonAdministrationSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendedTreatmentSign allowedSigns sign ->
            let
                updatedSigns =
                    -- Since we may have values from inital phase of encounter, we make
                    -- sure to preserve them, before setting new value at recurrent phase.
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

        SaveMedicationDistribution personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    model.nextStepsData.medicationDistributionForm
                        |> toMedicationDistributionValueWithDefaultRecurrentPhase measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMedicationDistribution personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    model.nextStepsData.healthEducationForm
                        |> toHealthEducationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update id isLabTech db) extraMsgs

        SetMalariaPreventionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.malariaPreventionData.form

                updatedData =
                    model.malariaPreventionData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | malariaPreventionData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaPrevention personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.malariaPreventionData.form
                        |> toMalariaPreventionValueWithDefault PhaseRecurrent measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMalariaPrevention personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
                        |> List.append [ App.Model.SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id ]
            in
            ( model
            , Cmd.none
            , appMsgs
            )



-- LAB HISTORY


updateLabsHistory :
    PrenatalEncounterId
    -> PrenatalEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Msg
    -> LabResultsData
    -> ( LabResultsData, Cmd Msg, List App.Model.Msg )
updateLabsHistory originEncounterId labEncounterId isLabTech db msg data =
    let
        extraMsgs =
            [ SetActivePage <| UserPage <| PrenatalActivityPage originEncounterId Backend.PrenatalActivity.Model.Laboratory ]
    in
    case msg of
        SetActivePage page ->
            ( data
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSyphilisTestResult value ->
            let
                form =
                    data.syphilisTestForm

                updatedForm =
                    { form | testResult = testResultFromString value, symptoms = Nothing, symptomsDirty = True }
            in
            ( { data | syphilisTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetIllnessSymptom symptom ->
            let
                form =
                    Dict.get labEncounterId db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.syphilisTest
                                >> getMeasurementValueFunc
                                >> syphilisResultFormWithDefault data.syphilisTestForm
                            )
                        |> Maybe.withDefault data.syphilisTestForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\symptoms -> { form | symptoms = symptoms, symptomsDirty = True })
                        NoIllnessSymptoms
                        symptom
                        form
            in
            ( { data | syphilisTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveSyphilisResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    data.syphilisTestForm

                appMsgs =
                    toSyphilisResultValueWithDefault isLabTech measurement { form | originatingEncounter = Just originEncounterId }
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSyphilisTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetHepatitisBTestResult value ->
            let
                form =
                    data.hepatitisBTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }
            in
            ( { data | hepatitisBTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHepatitisBResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    data.hepatitisBTestForm

                appMsgs =
                    toHepatitisBResultValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHepatitisBTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetBloodGroup value ->
            let
                form =
                    data.bloodGpRsTestForm

                updatedForm =
                    { form | bloodGroup = bloodGroupFromString value }
            in
            ( { data | bloodGpRsTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetRhesus value ->
            let
                form =
                    data.bloodGpRsTestForm

                updatedForm =
                    { form | rhesus = rhesusFromString value }
            in
            ( { data | bloodGpRsTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveBloodGpRsResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    data.bloodGpRsTestForm

                appMsgs =
                    toBloodGpRsResultValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveBloodGpRsTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetProtein value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | protein = proteinValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetPH value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | ph = phValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetGlucose value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | glucose = glucoseValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetLeukocytes value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | leukocytes = leukocytesValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetNitrite value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | nitrite = nitriteValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetUrobilinogen value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | urobilinogen = urobilinogenValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetHaemoglobin value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | haemoglobin = haemoglobinValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetKetone value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | ketone = ketoneValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBilirubin value ->
            let
                form =
                    data.urineDipstickTestForm

                updatedForm =
                    { form | bilirubin = bilirubinValueFromString value }
            in
            ( { data | urineDipstickTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveUrineDipstickResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toUrineDipstickResultValueWithDefault measurement data.urineDipstickTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveUrineDipstickTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetHemoglobin value ->
            let
                form =
                    data.hemoglobinTestForm

                updatedForm =
                    { form | hemoglobinCount = String.toFloat value }
            in
            ( { data | hemoglobinTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHemoglobinResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toHemoglobinResultValueWithDefault measurement data.hemoglobinTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHemoglobinTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetRandomBloodSugar value ->
            let
                form =
                    data.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value }
            in
            ( { data | randomBloodSugarTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveRandomBloodSugarResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    data.randomBloodSugarTestForm

                appMsgs =
                    toRandomBloodSugarResultValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveRandomBloodSugarTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        SetHIVViralLoadUndetectable undetectable ->
            let
                form =
                    data.hivPCRTestForm

                status =
                    if undetectable then
                        ViralLoadUndetectable

                    else
                        ViralLoadDetectable

                updatedForm =
                    { form | hivViralLoadStatus = Just status }
            in
            ( { data | hivPCRTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SetHIVViralLoad value ->
            let
                form =
                    data.hivPCRTestForm

                updatedForm =
                    { form | hivViralLoad = String.toFloat value }
            in
            ( { data | hivPCRTestForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveHIVPCRResult personId saved _ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toHIVPCRResultValueWithDefault measurement data.hivPCRTestForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHIVPCRTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter labEncounterId
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( data
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateLabsHistory originEncounterId labEncounterId isLabTech db) extraMsgs

        -- Other messages are not related to Labs History, and will not be sent.
        _ ->
            ( data, Cmd.none, [] )
