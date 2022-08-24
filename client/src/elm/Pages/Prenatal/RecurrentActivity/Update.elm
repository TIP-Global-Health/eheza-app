module Pages.Prenatal.RecurrentActivity.Update exposing (update, updateLabsHistory)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (IllnessSymptom(..), ViralLoadStatus(..))
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (toSendToHCValueWithDefault, toVitalsValueWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (nonAdministrationReasonToSign)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.RecurrentActivity.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Utils exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils exposing (setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)


updateLabsHistory :
    Language
    -> NominalDate
    -> PrenatalEncounterId
    -> PrenatalEncounterId
    -> ModelIndexedDb
    -> Msg
    -> LabResultsData
    -> ( LabResultsData, Cmd Msg, List App.Model.Msg )
updateLabsHistory language currentDate originEncounterId labEncounterId db msg data =
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
                    { form | testResult = prenatalTestResultFromString value, symptoms = Nothing, symptomsDirty = True }
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
                    toSyphilisResultValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

        SetHepatitisBTestResult value ->
            let
                form =
                    data.hepatitisBTestForm

                updatedForm =
                    { form | testResult = prenatalTestResultFromString value }
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
                    toHepatitisBValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

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
                    toPrenatalBloodGpRsResultsValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

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
                    toPrenatalUrineDipstickResultsValueWithDefault measurement data.urineDipstickTestForm
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

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
                    toPrenatalHemoglobinResultsValueWithDefault measurement data.hemoglobinTestForm
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

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
                    toPrenatalRandomBloodSugarResultsValueWithDefault measurement { form | originatingEncounter = Just originEncounterId }
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

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
                    toPrenatalHIVPCRResultsValueWithDefault measurement data.hivPCRTestForm
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
                |> sequenceExtra (updateLabsHistory language currentDate originEncounterId labEncounterId db) extraMsgs

        -- Other messages are not related to Labs History, and will not be sent.
        _ ->
            ( data, Cmd.none, [] )


update : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update language currentDate id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )

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
            Maybe.map (\task -> [ SetActiveLabResultsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault
                    [ SetActivePage <|
                        UserPage <|
                            ClinicalProgressReportPage (Backend.PrenatalEncounter.Model.InitiatorRecurrentEncounterPage id) id
                    ]
    in
    case msg of
        NoOp ->
            noChange

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState value ->
            ( { model | showAlertsDialog = value }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        ViewWarningPopupForNonUrgentDiagnoses ->
            let
                nonUrgentDiagnoses =
                    Dict.get id db.prenatalEncounters
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map (.diagnoses >> EverySet.toList >> filterNonUrgentDiagnoses)
                        |> Maybe.withDefault []

                extraMsgs =
                    if List.isEmpty nonUrgentDiagnoses then
                        []

                    else
                        let
                            message =
                                List.map (Translate.PrenatalDiagnosisNonUrgentMessage >> translate language) nonUrgentDiagnoses
                                    |> String.join ", "
                        in
                        [ SetWarningPopupState <| Just ( message, "" ) ]
            in
            sequenceExtra (update language currentDate id db) extraMsgs noChange

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
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.examinationData.vitalsForm
                        |> toVitalsValueWithDefault measurement
                        |> Maybe.map
                            (\value ->
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

        SetSyphilisTestResult value ->
            let
                form =
                    model.labResultsData.syphilisTestForm

                updatedForm =
                    { form | testResult = prenatalTestResultFromString value, symptoms = Nothing, symptomsDirty = True }

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
                    toSyphilisResultValueWithDefault measurement model.labResultsData.syphilisTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

        SetHepatitisBTestResult value ->
            let
                form =
                    model.labResultsData.hepatitisBTestForm

                updatedForm =
                    { form | testResult = prenatalTestResultFromString value }

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
                    toHepatitisBValueWithDefault measurement model.labResultsData.hepatitisBTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toPrenatalBloodGpRsResultsValueWithDefault measurement model.labResultsData.bloodGpRsTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toPrenatalUrineDipstickResultsValueWithDefault measurement model.labResultsData.urineDipstickTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toPrenatalHemoglobinResultsValueWithDefault measurement model.labResultsData.hemoglobinTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toPrenatalRandomBloodSugarResultsValueWithDefault measurement model.labResultsData.randomBloodSugarTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    toPrenatalHIVPCRResultsValueWithDefault measurement model.labResultsData.hivPCRTestForm
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                |> sequenceExtra (update language currentDate id db) extraMsgs
