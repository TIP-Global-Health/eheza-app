module Backend.AcuteIllnessEncounter.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias AcuteIllnessEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , sequenceNumber : Int
    , encounterType : AcuteIllnessEncounterType
    , diagnosis : AcuteIllnessDiagnosis
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyAcuteIllnessEncounter : IndividualEncounterParticipantId -> NominalDate -> Int -> AcuteIllnessEncounterType -> Maybe HealthCenterId -> AcuteIllnessEncounter
emptyAcuteIllnessEncounter participant startDate sequenceNumber encounterType shard =
    { participant = participant
    , startDate = startDate
    , sequenceNumber = sequenceNumber
    , endDate = Nothing
    , encounterType = encounterType
    , diagnosis = NoAcuteIllnessDiagnosis
    , deleted = False
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateAcuteIllnessEncounter : WebData ()
    , saveSymptomsGeneral : WebData ()
    , saveSymptomsRespiratory : WebData ()
    , saveSymptomsGI : WebData ()
    , saveVitals : WebData ()
    , saveAcuteFindings : WebData ()
    , saveMalariaTesting : WebData ()
    , saveCovidTesting : WebData ()
    , saveSendToHC : WebData ()
    , saveMedicationDistribution : WebData ()
    , saveTravelHistory : WebData ()
    , saveExposure : WebData ()
    , saveIsolation : WebData ()
    , saveHCContact : WebData ()
    , saveCall114 : WebData ()
    , saveTreatmentReview : WebData ()
    , saveMuac : WebData ()
    , saveTreatmentOngoing : WebData ()
    , saveDangerSigns : WebData ()
    , saveNutrition : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    , saveCoreExam : WebData ()
    , saveContactsTracing : WebData ()
    , saveTraceContact : Dict PersonId (WebData ())
    }


emptyModel : Model
emptyModel =
    { updateAcuteIllnessEncounter = NotAsked
    , saveSymptomsGeneral = NotAsked
    , saveSymptomsRespiratory = NotAsked
    , saveSymptomsGI = NotAsked
    , saveVitals = NotAsked
    , saveAcuteFindings = NotAsked
    , saveMalariaTesting = NotAsked
    , saveCovidTesting = NotAsked
    , saveSendToHC = NotAsked
    , saveMedicationDistribution = NotAsked
    , saveTravelHistory = NotAsked
    , saveExposure = NotAsked
    , saveIsolation = NotAsked
    , saveHCContact = NotAsked
    , saveCall114 = NotAsked
    , saveTreatmentReview = NotAsked
    , saveMuac = NotAsked
    , saveTreatmentOngoing = NotAsked
    , saveDangerSigns = NotAsked
    , saveNutrition = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    , saveCoreExam = NotAsked
    , saveContactsTracing = NotAsked
    , saveTraceContact = Dict.empty
    }


type Msg
    = CloseAcuteIllnessEncounter
    | SetAcuteIllnessDiagnosis AcuteIllnessDiagnosis
    | HandleUpdatedAcuteIllnessEncounter (WebData ())
    | SaveSymptomsGeneral PersonId (Maybe SymptomsGeneralId) (Dict SymptomsGeneralSign Int)
    | HandleSavedSymptomsGeneral (WebData ())
    | SaveSymptomsRespiratory PersonId (Maybe SymptomsRespiratoryId) (Dict SymptomsRespiratorySign Int)
    | HandleSavedSymptomsRespiratory (WebData ())
    | SaveSymptomsGI PersonId (Maybe SymptomsGIId) SymptomsGIValue
    | HandleSavedSymptomsGI (WebData ())
    | SaveVitals PersonId (Maybe AcuteIllnessVitalsId) VitalsValue
    | HandleSavedVitals (WebData ())
    | SaveAcuteFindings PersonId (Maybe AcuteFindingsId) AcuteFindingsValue
    | HandleSavedAcuteFindings (WebData ())
    | SaveMalariaTesting PersonId (Maybe MalariaTestingId) RapidTestResult
    | HandleSavedMalariaTesting (WebData ())
    | SaveCovidTesting PersonId (Maybe CovidTestingId) CovidTestingValue
    | HandleSavedCovidTesting (WebData ())
    | SaveSendToHC PersonId (Maybe SendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveMedicationDistribution PersonId (Maybe MedicationDistributionId) MedicationDistributionValue
    | HandleSavedMedicationDistribution (WebData ())
    | SaveTravelHistory PersonId (Maybe TravelHistoryId) (EverySet TravelHistorySign)
    | HandleSavedTravelHistory (WebData ())
    | SaveExposure PersonId (Maybe ExposureId) (EverySet ExposureSign)
    | HandleSavedExposure (WebData ())
    | SaveIsolation PersonId (Maybe IsolationId) IsolationValue
    | HandleSavedIsolation (WebData ())
    | SaveHCContact PersonId (Maybe HCContactId) HCContactValue
    | HandleSavedHCContact (WebData ())
    | SaveCall114 PersonId (Maybe Call114Id) Call114Value
    | HandleSavedCall114 (WebData ())
    | SaveTreatmentReview PersonId (Maybe TreatmentReviewId) (EverySet TreatmentReviewSign)
    | HandleSavedTreatmentReview (WebData ())
    | SaveMuac PersonId (Maybe AcuteIllnessMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveTreatmentOngoing PersonId (Maybe TreatmentOngoingId) TreatmentOngoingValue
    | HandleSavedTreatmentOngoing (WebData ())
    | SaveDangerSigns PersonId (Maybe AcuteIllnessDangerSignsId) (EverySet AcuteIllnessDangerSign)
    | HandleSavedDangerSigns (WebData ())
    | SaveNutrition PersonId (Maybe AcuteIllnessNutritionId) (EverySet ChildNutritionSign)
    | HandleSavedNutrition (WebData ())
    | SaveHealthEducation PersonId (Maybe HealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe AcuteIllnessFollowUpId) AcuteIllnessFollowUpValue
    | HandleSavedFollowUp (WebData ())
    | SaveCoreExam PersonId (Maybe AcuteIllnessCoreExamId) AcuteIllnessCoreExamValue
    | HandleSavedCoreExam (WebData ())
    | SaveContactsTracing PersonId (Maybe AcuteIllnessContactsTracingId) (List ContactTraceItem)
    | HandleSavedContactsTracing PersonId (List ContactTraceItem) (WebData ())
    | SaveTraceContact PersonId (Maybe AcuteIllnessTraceContactId) ContactTraceItem
    | HandleSavedTraceContact PersonId (WebData ())
