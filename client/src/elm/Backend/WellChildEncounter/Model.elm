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
    , skippedForms : EverySet SkippedForm
    , shard : Maybe HealthCenterId
    }


emptyWellChildEncounter :
    IndividualEncounterParticipantId
    -> NominalDate
    -> WellChildEncounterType
    -> Maybe HealthCenterId
    -> WellChildEncounter
emptyWellChildEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , encounterNote = NoEncounterNotes
    , encounterWarnings = EverySet.singleton NoEncounterWarnings
    , skippedForms = EverySet.empty
    , shard = shard
    }


type WellChildEncounterType
    = PediatricCare
    | PediatricCareChw
    | NewbornExam


type PediatricCareMilestone
    = Milestone6Weeks
    | Milestone14Weeks
    | Milestone6Months
    | Milestone9Months
    | Milestone12Months
    | Milestone15Months
    | Milestone18Months
    | Milestone2Years
    | Milestone3Years
    | Milestone4Years


pediatricCareMilestones : List PediatricCareMilestone
pediatricCareMilestones =
    [ Milestone6Weeks
    , Milestone14Weeks
    , Milestone6Months
    , Milestone9Months
    , Milestone12Months
    , Milestone15Months
    , Milestone18Months
    , Milestone2Years
    , Milestone3Years
    , Milestone4Years
    ]


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
    [ WarningHeadCircumferenceMicrocephaly, WarningHeadCircumferenceMacrocephaly, NoHeadCircumferenceWarning ]


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
    , saveBCGImmunisation : WebData ()
    , saveDTPImmunisation : WebData ()
    , saveDTPStandaloneImmunisation : WebData ()
    , saveHPVImmunisation : WebData ()
    , saveIPVImmunisation : WebData ()
    , saveMRImmunisation : WebData ()
    , saveOPVImmunisation : WebData ()
    , savePCV13Immunisation : WebData ()
    , saveRotarixImmunisation : WebData ()
    , saveECD : WebData ()
    , saveAlbendazole : WebData ()
    , saveMebendezole : WebData ()
    , saveVitaminA : WebData ()
    , saveNextVisit : WebData ()
    , saveNCDA : WebData ()
    , saveFeeding : WebData ()
    , saveHygiene : WebData ()
    , saveFoodSecurity : WebData ()
    , saveCaring : WebData ()
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
    , saveBCGImmunisation = NotAsked
    , saveDTPImmunisation = NotAsked
    , saveDTPStandaloneImmunisation = NotAsked
    , saveHPVImmunisation = NotAsked
    , saveIPVImmunisation = NotAsked
    , saveMRImmunisation = NotAsked
    , saveOPVImmunisation = NotAsked
    , savePCV13Immunisation = NotAsked
    , saveRotarixImmunisation = NotAsked
    , saveECD = NotAsked
    , saveAlbendazole = NotAsked
    , saveMebendezole = NotAsked
    , saveVitaminA = NotAsked
    , saveNextVisit = NotAsked
    , saveNCDA = NotAsked
    , saveFeeding = NotAsked
    , saveHygiene = NotAsked
    , saveFoodSecurity = NotAsked
    , saveCaring = NotAsked
    }


type Msg
    = CloseWellChildEncounter
    | AddSkippedForm SkippedForm
    | RemoveSkippedForm SkippedForm
    | HandleUpdatedWellChildEncounter (WebData ())
    | SetWellChildEncounterNote EncounterNote
    | SetWellChildEncounterWarning EncounterWarning
    | SavePregnancySummary PersonId (Maybe WellChildPregnancySummaryId) PregnancySummaryValue
    | HandleSavedPregnancySummary (WebData ())
    | SaveSymptomsReview PersonId (Maybe WellChildSymptomsReviewId) (EverySet WellChildSymptom)
    | HandleSavedSymptomsReview (WebData ())
    | SaveVitals PersonId (Maybe WellChildVitalsId) VitalsValue
    | HandleSavedVitals (WebData ())
    | SaveHeight PersonId (Maybe WellChildHeightId) HeightInCm
    | SaveHeadCircumference PersonId (Maybe WellChildHeadCircumferenceId) HeadCircumferenceValue
    | HandleSavedHeadCircumference (WebData ())
    | HandleSavedHeight (WebData ())
    | SaveMuac PersonId (Maybe WellChildMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveNutrition PersonId (Maybe WellChildNutritionId) NutritionValue
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe WellChildPhotoId) ImageUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe WellChildWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
    | SaveContributingFactors PersonId (Maybe WellChildContributingFactorsId) (EverySet ContributingFactorsSign)
    | HandleSavedContributingFactors (WebData ())
    | SaveHealthEducation PersonId (Maybe WellChildHealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe WellChildFollowUpId) NutritionFollowUpValue
    | HandleSavedFollowUp (WebData ())
    | SaveSendToHC PersonId (Maybe WellChildSendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveECD PersonId (Maybe WellChildECDId) (EverySet ECDSign)
    | HandleSavedECD (WebData ())
    | SaveBCGImmunisation PersonId (Maybe WellChildBCGImmunisationId) VaccinationValue
    | HandleSavedBCGImmunisation (WebData ())
    | SaveDTPImmunisation PersonId (Maybe WellChildDTPImmunisationId) VaccinationValue
    | HandleSavedDTPImmunisation (WebData ())
    | SaveDTPStandaloneImmunisation PersonId (Maybe WellChildDTPStandaloneImmunisationId) VaccinationValue
    | HandleSavedDTPStandaloneImmunisation (WebData ())
    | SaveHPVImmunisation PersonId (Maybe WellChildHPVImmunisationId) VaccinationValue
    | HandleSavedHPVImmunisation (WebData ())
    | SaveIPVImmunisation PersonId (Maybe WellChildIPVImmunisationId) VaccinationValue
    | HandleSavedIPVImmunisation (WebData ())
    | SaveMRImmunisation PersonId (Maybe WellChildMRImmunisationId) VaccinationValue
    | HandleSavedMRImmunisation (WebData ())
    | SaveOPVImmunisation PersonId (Maybe WellChildOPVImmunisationId) VaccinationValue
    | HandleSavedOPVImmunisation (WebData ())
    | SavePCV13Immunisation PersonId (Maybe WellChildPCV13ImmunisationId) VaccinationValue
    | HandleSavedPCV13Immunisation (WebData ())
    | SaveRotarixImmunisation PersonId (Maybe WellChildRotarixImmunisationId) VaccinationValue
    | HandleSavedRotarixImmunisation (WebData ())
    | SaveAlbendazole PersonId (Maybe WellChildAlbendazoleId) AdministrationNote
    | HandleSavedAlbendazole (WebData ())
    | SaveMebendezole PersonId (Maybe WellChildMebendezoleId) AdministrationNote
    | HandleSavedMebendezole (WebData ())
    | SaveVitaminA PersonId (Maybe WellChildVitaminAId) AdministrationNote
    | HandleSavedVitaminA (WebData ())
    | SaveNextVisit PersonId (Maybe WellChildNextVisitId) NextVisitValue
    | HandleSavedNextVisit (WebData ())
    | SaveNCDA PersonId (Maybe WellChildNCDAId) NCDAValue
    | HandleSavedNCDA (WebData ())
    | SaveFeeding PersonId (Maybe WellChildFeedingId) NutritionFeedingValue
    | HandleSavedFeeding (WebData ())
    | SaveHygiene PersonId (Maybe WellChildHygieneId) NutritionHygieneValue
    | HandleSavedHygiene (WebData ())
    | SaveFoodSecurity PersonId (Maybe WellChildFoodSecurityId) NutritionFoodSecurityValue
    | HandleSavedFoodSecurity (WebData ())
    | SaveCaring PersonId (Maybe WellChildCaringId) NutritionCaringValue
    | HandleSavedCaring (WebData ())
