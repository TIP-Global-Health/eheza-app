module Pages.Tuberculosis.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (AdverseEvent(..))
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
        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| TuberculosisEncounterPage id ]

        resolveFormWithDefaults getMeasurementFunc formWithDefaultsFunc form =
            Dict.get id db.tuberculosisMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> formWithDefaultsFunc form
                    )
                |> Maybe.withDefault form

        treatmentReviewForm =
            resolveFormWithDefaults .treatmentReview ongoingTreatmentReviewFormWithDefault model.medicationData.treatmentReviewForm
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
                    formUpdateFunc value model.diagnosticsData.form

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDiagnostics personId saved ->
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
                                [ Backend.TuberculosisEncounter.Model.SaveDiagnostics personId measurementId value
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

        SaveTreatmentReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.medicationData.treatmentReviewForm
                        |> toOngoingTreatmentReviewValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.TuberculosisEncounter.Model.SaveTreatmentReview personId measurementId value
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
