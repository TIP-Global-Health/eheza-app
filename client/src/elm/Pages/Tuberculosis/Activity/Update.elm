module Pages.Tuberculosis.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (TuberculosisOutcome(..))
import Backend.Measurement.Model exposing (AdverseEvent(..), TuberculosisPrescribedMedication(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisEncounter.Model
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (ongoingTreatmentReviewFormWithDefault, toFollowUpValueWithDefault, toOngoingTreatmentReviewValueWithDefault, toSendToHCValueWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (Model, Msg(..))
import Pages.Tuberculosis.Activity.Utils exposing (diagnosticsFormWithDefault, dotFormWithDefault, prescribedMedicationFormWithDefault, toDOTValueWithDefault, toDiagnosticsValueWithDefault, toHealthEducationValueWithDefault, toPrescribedMedicationValueWithDefault, toSymptomReviewValueWithDefault)
import Pages.Utils exposing (saveMeasurementMsgs, setMultiSelectInputValue)
import RemoteData


update : TuberculosisEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id db msg model =
    let
        resolveFormWithDefaults getMeasurementFunc formWithDefaultsFunc form =
            Dict.get id db.tuberculosisMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> formWithDefaultsFunc form
                    )
                |> Maybe.withDefault form

        diagnosticsForm =
            resolveFormWithDefaults .diagnostics diagnosticsFormWithDefault model.diagnosticsData.form

        dotForm =
            resolveFormWithDefaults .dot dotFormWithDefault model.medicationData.dotForm

        treatmentReviewForm =
            resolveFormWithDefaults .treatmentReview ongoingTreatmentReviewFormWithDefault model.medicationData.treatmentReviewForm

        medicationForm =
            resolveFormWithDefaults .medication prescribedMedicationFormWithDefault model.medicationData.prescribedMedicationForm

        generateMedicationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| TuberculosisEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| TuberculosisEncounterPage id ]

        toIndexedDbMsg =
            Backend.Model.MsgTuberculosisEncounter id >> App.Model.MsgIndexedDb
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

        SetEndEncounterDialogState showDialog ->
            let
                updatedForm =
                    if not showDialog then
                        { diagnosticsForm | diagnosed = Nothing }

                    else
                        diagnosticsForm

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm, showEndEncounterDialog = showDialog })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDiagnostics personId particpantId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.diagnosticsData.form
                        |> toDiagnosticsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                let
                                    saveMsg =
                                        Backend.TuberculosisEncounter.Model.SaveDiagnostics personId measurementId value
                                            |> Backend.Model.MsgTuberculosisEncounter id
                                            |> App.Model.MsgIndexedDb

                                    additionalMsgs =
                                        if diagnosticsForm.diagnosed == Just False then
                                            [ Backend.IndividualEncounterParticipant.Model.CloseTuberculosisSession TuberculosisOutcomeNotDiagnosed
                                                |> Backend.Model.MsgIndividualEncounterParticipant particpantId
                                                |> App.Model.MsgIndexedDb
                                            , App.Model.SetActivePage PinCodePage
                                            ]

                                        else
                                            [ App.Model.SetActivePage <| UserPage <| TuberculosisEncounterPage id ]
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
                updatedForm =
                    setMultiSelectInputValue .medications
                        (\medications -> { medicationForm | medications = medications })
                        NoTuberculosisPrescribedMedications
                        medication
                        medicationForm

                updatedData =
                    model.medicationData
                        |> (\data -> { data | prescribedMedicationForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SavePrescribedMedication personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toPrescribedMedicationValueWithDefault
                model.medicationData.prescribedMedicationForm
                saved
                (Backend.TuberculosisEncounter.Model.SavePrescribedMedication personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateMedicationMsgs nextTask)

        SetDOTBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value dotForm

                updatedData =
                    model.medicationData
                        |> (\data -> { data | dotForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonNotProvidedToday value ->
            let
                form =
                    dotForm

                updatedForm =
                    { form | reasonNotProvidedToday = Just value, reasonNotProvidedTodayDirty = True }

                updatedData =
                    model.medicationData
                        |> (\data -> { data | dotForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonMedicationsNotDistributed value ->
            let
                form =
                    dotForm

                updatedForm =
                    { form | reasonNotDistributedMedications = Just value, reasonNotDistributedMedicationsDirty = True }

                updatedData =
                    model.medicationData
                        |> (\data -> { data | dotForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveDOT personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toDOTValueWithDefault
                model.medicationData.dotForm
                saved
                (Backend.TuberculosisEncounter.Model.SaveDOT personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateMedicationMsgs nextTask)

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
            ( model
            , Cmd.none
            , saveMeasurementMsgs toOngoingTreatmentReviewValueWithDefault
                model.medicationData.treatmentReviewForm
                saved
                (Backend.TuberculosisEncounter.Model.SaveTreatmentReview personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateMedicationMsgs nextTask)

        SetSymptomReviewBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.symptomReviewData.form

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
                                [ Backend.TuberculosisEncounter.Model.SaveSymptomReview personId measurementId value
                                    |> Backend.Model.MsgTuberculosisEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| TuberculosisEncounterPage id
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
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHealthEducationValueWithDefault
                model.nextStepsData.healthEducationForm
                saved
                (Backend.TuberculosisEncounter.Model.SaveHealthEducation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateNextStepsMsgs nextTask)

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
            ( model
            , Cmd.none
            , saveMeasurementMsgs toFollowUpValueWithDefault
                model.nextStepsData.followUpForm
                saved
                (Backend.TuberculosisEncounter.Model.SaveFollowUp personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateNextStepsMsgs nextTask)

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
            ( model
            , Cmd.none
            , saveMeasurementMsgs toSendToHCValueWithDefault
                model.nextStepsData.sendToHCForm
                saved
                (Backend.TuberculosisEncounter.Model.SaveReferral personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update id db) (generateNextStepsMsgs nextTask)
