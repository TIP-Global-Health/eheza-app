module Pages.Tuberculosis.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (TuberculosisOutcome(..))
import Backend.Measurement.Model exposing (AdverseEvent(..), TuberculosisPrescribedMedication(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisEncounter.Model
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (ongoingTreatmentReviewFormWithDefault, toFollowUpValueWithDefault, toOngoingTreatmentReviewValueWithDefault, toSendToHCValueWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Activity.Utils exposing (..)
import Pages.Utils exposing (setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> TuberculosisEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
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

        medicationForm =
            resolveFormWithDefaults .medication prescribedMedicationFormWithDefault model.medicationData.prescribedMedicationForm

        dotForm =
            resolveFormWithDefaults .dot dotFormWithDefault model.medicationData.dotForm

        treatmentReviewForm =
            resolveFormWithDefaults .treatmentReview ongoingTreatmentReviewFormWithDefault model.medicationData.treatmentReviewForm

        generateMedicationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| TuberculosisEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| TuberculosisEncounterPage id ]
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
                form =
                    medicationForm

                updatedForm =
                    setMultiSelectInputValue .medications
                        (\medications -> { form | medications = medications })
                        NoTuberculosisPrescribedMedications
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
                            (Backend.TuberculosisEncounter.Model.SavePrescribedMedication personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    toDOTValueWithDefault measurement model.medicationData.dotForm
                        |> Maybe.map
                            (Backend.TuberculosisEncounter.Model.SaveDOT personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
                            (Backend.TuberculosisEncounter.Model.SaveTreatmentReview personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
                            (Backend.TuberculosisEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
                            (Backend.TuberculosisEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
                            (Backend.TuberculosisEncounter.Model.SaveReferral personId measurementId
                                >> Backend.Model.MsgTuberculosisEncounter id
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
