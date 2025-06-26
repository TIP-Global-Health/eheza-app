module Backend.HIVEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HIVEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyHIVEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> HIVEncounter
emptyHIVEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , deleted = False
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeHIVEncounter : WebData ()
    , saveDiagnostics : WebData ()
    , savePrescribedMedication : WebData ()
    , saveTreatmentReview : WebData ()
    , saveSymptomReview : WebData ()
    , saveReferral : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeHIVEncounter = NotAsked
    , saveDiagnostics = NotAsked
    , savePrescribedMedication = NotAsked
    , saveTreatmentReview = NotAsked
    , saveSymptomReview = NotAsked
    , saveReferral = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    }


type Msg
    = CloseHIVEncounter
    | HandleClosedHIVEncounter (WebData ())
    | SaveDiagnostics PersonId (Maybe HIVDiagnosticsId) HIVDiagnosticsValue
    | HandleSavedDiagnostics (WebData ())
    | SavePrescribedMedication PersonId (Maybe HIVMedicationId) HIVMedicationValue
    | HandleSavedPrescribedMedication (WebData ())
    | SaveTreatmentReview PersonId (Maybe HIVTreatmentReviewId) TreatmentOngoingValue
    | HandleSavedTreatmentReview (WebData ())
    | SaveSymptomReview PersonId (Maybe HIVSymptomReviewId) HIVSymptomReviewValue
    | HandleSavedSymptomReview (WebData ())
    | SaveReferral PersonId (Maybe HIVReferralId) SendToHCValue
    | HandleSavedReferral (WebData ())
    | SaveHealthEducation PersonId (Maybe HIVHealthEducationId) HIVHealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe HIVFollowUpId) FollowUpValue
    | HandleSavedFollowUp (WebData ())
