module Pages.HIV.Activity.Update exposing (update)

import App.Model
import App.Utils exposing (focusOnCalendarMsg)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (HIVOutcome(..))
import Backend.Measurement.Model exposing (AdverseEvent(..), HIVPrescribedMedication(..), HIVSymptom(..), TestResult(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, testResultFromString)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils
    exposing
        ( ongoingTreatmentReviewFormWithDefault
        , toFollowUpValueWithDefault
        , toOngoingTreatmentReviewValueWithDefault
        , toSendToHCValueWithDefault
        )
import Pages.HIV.Activity.Model exposing (..)
import Pages.HIV.Activity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMultiSelectInputValue)
import RemoteData


update : NominalDate -> HIVEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        resolveFormWithDefaults getMeasurementFunc formWithDefaultsFunc form =
            Dict.get id db.hivMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> formWithDefaultsFunc form
                    )
                |> Maybe.withDefault form

        diagnosticsForm =
            resolveFormWithDefaults .diagnostics diagnosticsFormWithDefault model.diagnosticsData.form

        medicationForm =
            resolveFormWithDefaults .medication prescribedMedicationFormWithDefault model.medicationData.prescribedMedicationForm

        treatmentReviewForm =
            resolveFormWithDefaults .treatmentReview ongoingTreatmentReviewFormWithDefault model.medicationData.treatmentReviewForm

        generateMedicationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| HIVEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| HIVEncounterPage id ]
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDiagnosticsBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value diagnosticsForm

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        ConfirmPositiveResultDate date confirmed ->
            let
                updatedForm =
                    { diagnosticsForm
                        | resultDateCorrect = Just confirmed
                        , positiveResultDate = Nothing
                        , positiveResultDateDirty = True
                        , positiveResultDateEstimated = Nothing
                        , positiveResultDateEstimatedDirty = True
                    }

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })

                setPositiveResultDateMsg =
                    if confirmed then
                        [ SetPositiveResultDate date ]

                    else
                        []
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate id db) setPositiveResultDateMsg

        SetPositiveResultDate value ->
            let
                updatedForm =
                    { diagnosticsForm
                        | positiveResultDate = Just value
                        , positiveResultDateDirty = True
                        , positiveResultDateEstimated = Nothing
                        , positiveResultDateEstimatedDirty = True
                    }

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestResult value ->
            let
                updatedForm =
                    let
                        testResult =
                            testResultFromString value
                    in
                    if testResult == Just TestPositive then
                        { diagnosticsForm
                            | testResult = testResult
                            , testResultDirty = True
                            , positiveResultDate = Just currentDate
                            , positiveResultDateDirty = True
                            , positiveResultDateEstimated = Just False
                            , positiveResultDateEstimatedDirty = True
                        }

                    else
                        { diagnosticsForm
                            | testResult = testResult
                            , testResultDirty = True
                            , positiveResultDate = Nothing
                            , positiveResultDateDirty = True
                            , positiveResultDateEstimated = Nothing
                            , positiveResultDateEstimatedDirty = True
                        }

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SetEndEncounterDialogState showDialog mRevertAnswerFunc ->
            let
                updatedForm =
                    Maybe.map (\revertAnswerFunc -> revertAnswerFunc diagnosticsForm) mRevertAnswerFunc
                        |> Maybe.withDefault diagnosticsForm

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm, showEndEncounterDialog = showDialog })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SetDateSelectorState state ->
            let
                updatedForm =
                    { diagnosticsForm | dateSelectorPopupState = state }

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , [ focusOnCalendarMsg ]
            )

        SaveDiagnostics personId particpantId positiveResultRecorded saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.diagnosticsData.form
                        |> toDiagnosticsValueWithDefault positiveResultRecorded measurement
                        |> unwrap
                            []
                            (\value ->
                                let
                                    saveMsg =
                                        Backend.HIVEncounter.Model.SaveDiagnostics personId measurementId value
                                            |> Backend.Model.MsgHIVEncounter id
                                            |> App.Model.MsgIndexedDb

                                    additionalMsgs =
                                        if
                                            not positiveResultRecorded
                                                && (diagnosticsForm.resultPositive == Just False)
                                                && (diagnosticsForm.testResult /= Just TestPositive)
                                        then
                                            [ Backend.IndividualEncounterParticipant.Model.CloseHIVSession HIVOutcomeNotDiagnosed
                                                |> Backend.Model.MsgIndividualEncounterParticipant particpantId
                                                |> App.Model.MsgIndexedDb
                                            , App.Model.SetActivePage PinCodePage
                                            ]

                                        else
                                            [ App.Model.SetActivePage <| UserPage <| HIVEncounterPage id ]
                                in
                                saveMsg :: additionalMsgs
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveMedicationTask task ->
            let
                updatedData =
                    model.medicationData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetPrescribedMedicationsNotChanged value ->
            let
                updatedForm =
                    { medicationForm | medicationsNotChanged = Just value, medications = Nothing }

                updatedData =
                    model.medicationData
                        |> (\data -> { data | prescribedMedicationForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetPrescribedMedication medication ->
            let
                form =
                    medicationForm

                updatedForm =
                    setMultiSelectInputValue .medications
                        (\medications -> { form | medications = medications })
                        NoHIVPrescribedMedications
                        medication
                        form

                updatedData =
                    model.medicationData
                        |> (\data -> { data | prescribedMedicationForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SavePrescribedMedication personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    toPrescribedMedicationValueWithDefault measurement model.medicationData.prescribedMedicationForm
                        |> Maybe.map
                            (Backend.HIVEncounter.Model.SavePrescribedMedication personId measurementId
                                >> Backend.Model.MsgHIVEncounter id
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

        SetTreatmentReviewBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value treatmentReviewForm
                    in
                    model.medicationData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotTaking value ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    { form | reasonForNotTaking = Just value, reasonForNotTakingDirty = True }

                updatedData =
                    model.medicationData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetTotalMissedDoses value ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    { form | totalMissedDoses = String.toInt value, totalMissedDosesDirty = True }

                updatedData =
                    model.medicationData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetAdverseEvent event ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    setMultiSelectInputValue .adverseEvents
                        (\events -> { form | adverseEvents = events, adverseEventsDirty = True })
                        NoAdverseEvent
                        event
                        form

                updatedData =
                    model.medicationData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveTreatmentReview personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    toOngoingTreatmentReviewValueWithDefault measurement model.medicationData.treatmentReviewForm
                        |> Maybe.map
                            (Backend.HIVEncounter.Model.SaveTreatmentReview personId measurementId
                                >> Backend.Model.MsgHIVEncounter id
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

        SetSymptom symptom ->
            let
                symptomReviewForm =
                    resolveFormWithDefaults .symptomReview symptomReviewFormWithDefault model.symptomReviewData.form

                form =
                    symptomReviewForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\symptoms -> { form | symptoms = symptoms, symptomsDirty = True })
                        NoHIVSymptoms
                        symptom
                        form

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
                                [ Backend.HIVEncounter.Model.SaveSymptomReview personId measurementId value
                                    |> Backend.Model.MsgHIVEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| HIVEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toHealthEducationValueWithDefault measurement model.nextStepsData.healthEducationForm
                        |> Maybe.map
                            (Backend.HIVEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgHIVEncounter id
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

        SetFollowUpOption option ->
            let
                form =
                    model.nextStepsData.followUpForm

                updatedForm =
                    { form | option = Just option }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | followUpForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveFollowUp personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toFollowUpValueWithDefault measurement model.nextStepsData.followUpForm
                        |> Maybe.map
                            (Backend.HIVEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgHIVEncounter id
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

        SetReferToHealthCenter value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | referToHealthCenter = Just value, reasonForNotSendingToHC = Nothing }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHandReferralForm value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | handReferralForm = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNonReferral value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | reasonForNotSendingToHC = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveReferral personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toSendToHCValueWithDefault measurement model.nextStepsData.sendToHCForm
                        |> Maybe.map
                            (Backend.HIVEncounter.Model.SaveReferral personId measurementId
                                >> Backend.Model.MsgHIVEncounter id
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
