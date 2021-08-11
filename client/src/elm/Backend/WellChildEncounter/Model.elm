module Backend.WellChildEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias WellChildEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : WellChildEncounterType
    , encounterNote : EncounterNote
    , encounterWarnings : EverySet EncounterWarning
    , shard : Maybe HealthCenterId
    }


emptyWellChildEncounter : IndividualEncounterParticipantId -> NominalDate -> WellChildEncounterType -> Maybe HealthCenterId -> WellChildEncounter
emptyWellChildEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , encounterNote = NoEncounterNotes
    , encounterWarnings = EverySet.singleton NoEncounterWarnings
    , shard = shard
    }


type WellChildEncounterType
    = NewbornExam
      -- This encounter will occur if immunisation at
      -- birth was not completed.
    | PediatricCareBirthTo6Weeks
    | PediatricCare6Weeks
    | PediatricCare10Weeks
    | PediatricCare14Weeks
    | PediatricCare6Months
    | PediatricCare9Months
    | PediatricCare12Months
    | PediatricCare15Months
    | PediatricCare18Months
      -- From age of 2 years, every 6 months.
    | PediatricCareRecurrent


type EncounterNote
    = NoteTriggeredAcuteIllnessEncounter
    | NoEncounterNotes


type
    EncounterWarning
    -- ECD related warnings.
    = WarningECDMilestoneBehind
    | WarningECDMilestoneReferToSpecialist
      -- We use this option when ECD activity was completed,
      -- and no warnings were generated.
    | NoECDMilstoneWarning
      -- Head Circumference related warnings.
    | WarningHeadCircumferenceMicrocephaly
    | WarningHeadCircumferenceMacrocephaly
      -- We use this option when Head Circumference activity was
      -- completed, and no warnings were generated.
    | NoHeadCircumferenceWarning
      -- This option is set when neither ECD nor
      -- Head Circumference activity were completed.
    | NoEncounterWarnings


ecdMilestoneWarnings : List EncounterWarning
ecdMilestoneWarnings =
    [ WarningECDMilestoneBehind, WarningECDMilestoneReferToSpecialist, NoECDMilstoneWarning ]


headCircumferenceWarnings : List EncounterWarning
headCircumferenceWarnings =
    [ WarningHeadCircumferenceMicrocephaly, WarningHeadCircumferenceMacrocephaly, NoECDMilstoneWarning ]


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { editWellChildEncounter : WebData ()
    , savePregnancySummary : WebData ()
    , saveSymptomsReview : WebData ()
    , saveVitals : WebData ()
    , saveHeight : WebData ()
    , saveHeadCircumference : WebData ()
    , saveMuac : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    , saveWeight : WebData ()
    , saveContributingFactors : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    , saveSendToHC : WebData ()
    , saveImmunisation : WebData ()
    , saveECD : WebData ()
    , saveAlbendazole : WebData ()
    , saveMebendezole : WebData ()
    , saveVitaminA : WebData ()
    , saveNextVisit : WebData ()
    , saveVaccinationHistory : WebData ()
    }


emptyModel : Model
emptyModel =
    { editWellChildEncounter = NotAsked
    , savePregnancySummary = NotAsked
    , saveSymptomsReview = NotAsked
    , saveVitals = NotAsked
    , saveHeight = NotAsked
    , saveHeadCircumference = NotAsked
    , saveMuac = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    , saveWeight = NotAsked
    , saveContributingFactors = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    , saveSendToHC = NotAsked
    , saveImmunisation = NotAsked
    , saveECD = NotAsked
    , saveAlbendazole = NotAsked
    , saveMebendezole = NotAsked
    , saveVitaminA = NotAsked
    , saveNextVisit = NotAsked
    , saveVaccinationHistory = NotAsked
    }


type Msg
    = CloseWellChildEncounter
    | HandleWellChildEncounterEdited (WebData ())
    | SetWellChildEncounterNote EncounterNote
    | SetWellChildEncounterWarning EncounterWarning
    | SavePregnancySummary PersonId (Maybe WellChildPregnancySummaryId) PregnancySummaryValue
    | HandleSavedPregnancySummary (WebData ())
    | SaveSymptomsReview PersonId (Maybe WellChildSymptomsReviewId) (EverySet WellChildSymptom)
    | HandleSavedSymptomsReview (WebData ())
    | SaveVitals PersonId (Maybe WellChildVitalsId) BasicVitalsValue
    | HandleSavedVitals (WebData ())
    | SaveHeight PersonId (Maybe WellChildHeightId) HeightInCm
    | SaveHeadCircumference PersonId (Maybe WellChildHeadCircumferenceId) HeadCircumferenceValue
    | HandleSavedHeadCircumference (WebData ())
    | HandleSavedHeight (WebData ())
    | SaveMuac PersonId (Maybe WellChildMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveNutrition PersonId (Maybe WellChildNutritionId) (EverySet ChildNutritionSign)
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe WellChildPhotoId) PhotoUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe WellChildWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
    | SaveContributingFactors PersonId (Maybe WellChildContributingFactorsId) (EverySet ContributingFactorsSign)
    | HandleSavedContributingFactors (WebData ())
    | SaveHealthEducation PersonId (Maybe WellChildHealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe WellChildFollowUpId) FollowUpValue
    | HandleSavedFollowUp (WebData ())
    | SaveSendToHC PersonId (Maybe WellChildSendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveECD PersonId (Maybe WellChildECDId) (EverySet ECDSign)
    | SaveImmunisation PersonId (Maybe WellChildImmunisationId) ImmunisationValue
    | HandleSavedImmunisation (WebData ())
    | HandleSavedECD (WebData ())
    | SaveAlbendazole PersonId (Maybe WellChildAlbendazoleId) AdministrationNote
    | HandleSavedAlbendazole (WebData ())
    | SaveMebendezole PersonId (Maybe WellChildMebendezoleId) AdministrationNote
    | HandleSavedMebendezole (WebData ())
    | SaveVitaminA PersonId (Maybe WellChildVitaminAId) AdministrationNote
    | HandleSavedVitaminA (WebData ())
    | SaveNextVisit PersonId (Maybe WellChildNextVisitId) NextVisitValue
    | HandleSavedNextVisit (WebData ())
    | SaveVaccinationHistory PersonId (Maybe WellChildVaccinationHistoryId) VaccinationHistoryValue
    | HandleSavedVaccinationHistory (WebData ())
