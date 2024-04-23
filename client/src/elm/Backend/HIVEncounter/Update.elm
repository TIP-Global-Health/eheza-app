module Backend.HIVEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> HIVEncounterId
    -> Maybe HIVEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseHIVEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeHIVEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull hivEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedHIVEncounter)
                        , []
                        )
                    )

        HandleClosedHIVEncounter data ->
            ( { model | closeHIVEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDiagnostics personId valueId value ->
            ( { model | saveDiagnostics = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivDiagnosticsEndpoint HandleSavedDiagnostics
            , []
            )

        HandleSavedDiagnostics data ->
            ( { model | saveDiagnostics = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePrescribedMedication personId valueId value ->
            ( { model | savePrescribedMedication = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivMedicationEndpoint HandleSavedPrescribedMedication
            , []
            )

        HandleSavedPrescribedMedication data ->
            ( { model | savePrescribedMedication = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTreatmentReview personId valueId value ->
            ( { model | saveTreatmentReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivTreatmentReviewEndpoint HandleSavedTreatmentReview
            , []
            )

        HandleSavedTreatmentReview data ->
            ( { model | saveTreatmentReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomReview personId valueId value ->
            ( { model | saveSymptomReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivSymptomReviewEndpoint HandleSavedSymptomReview
            , []
            )

        HandleSavedSymptomReview data ->
            ( { model | saveSymptomReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveReferral personId valueId value ->
            ( { model | saveReferral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivReferralEndpoint HandleSavedReferral
            , []
            )

        HandleSavedReferral data ->
            ( { model | saveReferral = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value hivFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
