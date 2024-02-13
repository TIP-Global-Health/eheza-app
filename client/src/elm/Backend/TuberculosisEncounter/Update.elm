module Backend.TuberculosisEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.TuberculosisEncounter.Model exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> TuberculosisEncounterId
    -> Maybe TuberculosisEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseTuberculosisEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none, [] )
                    (\encounter ->
                        ( { model | closeTuberculosisEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull tuberculosisEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleClosedTuberculosisEncounter)
                        , []
                        )
                    )

        HandleClosedTuberculosisEncounter data ->
            ( { model | closeTuberculosisEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDiagnostics personId valueId value ->
            ( { model | saveDiagnostics = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisDiagnosticsEndpoint HandleSavedDiagnostics
            , []
            )

        HandleSavedDiagnostics data ->
            ( { model | saveDiagnostics = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomReview personId valueId value ->
            ( { model | saveSymptomReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisSymptomReviewEndpoint HandleSavedSymptomReview
            , []
            )

        HandleSavedSymptomReview data ->
            ( { model | saveSymptomReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveReferral personId valueId value ->
            ( { model | saveReferral = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisReferralEndpoint HandleSavedReferral
            , []
            )

        HandleSavedReferral data ->
            ( { model | saveReferral = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePrescribedMedication personId valueId value ->
            ( { model | savePrescribedMedication = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisMedicationEndpoint HandleSavedPrescribedMedication
            , []
            )

        HandleSavedPrescribedMedication data ->
            ( { model | savePrescribedMedication = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDOT personId valueId value ->
            ( { model | saveDOT = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisDOTEndpoint HandleSavedDOT
            , []
            )

        HandleSavedDOT data ->
            ( { model | saveDOT = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTreatmentReview personId valueId value ->
            ( { model | saveTreatmentReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value tuberculosisTreatmentReviewEndpoint HandleSavedTreatmentReview
            , []
            )

        HandleSavedTreatmentReview data ->
            ( { model | saveTreatmentReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )
