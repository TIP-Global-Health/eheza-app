module Backend.Entities exposing (..)

{-|


## Why are all the ID types here?

It's nice to have type-safe IDs for backend entities, but it tends
to lead to circular imports if you put the ID types in the "usual"
place alongside the data-type itself.

One typical case where the circular references arise is where a "child"
entity has a reference to its "parent". So, for instance:

  - the various measurements have a `sessionId` to refer to the session the
    measurement was taken in.

  - but the `OfflineSession` also has a `DictList` of all its measurements

Now, you could imagine avoiding this in one way or another. For instance, you
could imagine not storing the `sessionId` in the measurement, but instead
tracking it separately. But that would be awkward in its own way -- that is, it
would be awkward if `Measurement.Model` couldn't refer to the `SessionId`,
since each of the measurements really does have one -- we get it from the
backend, and send it to the backend.

So, it seems simpler to just have one ID type here for each of our backend
entities.

The way this is implemented is inspired by
[johneshf/elm-tagged](http://package.elm-lang.org/packages/joneshf/elm-tagged/latest).
You might want to start with something like:

    type ChildId
        = ChildId Int

    type ChildNutritionId
        = ChildNutritionId Int

But then the type-checker doesn't actually know that these two types are related
in some way -- for instance, that both are IDs. For instance, to extract the
`Int` you have to do two different things.

What we do instead is have a "unityfing" `EntityId` type (from `Restful.Endpoint`),
which takes what we call a "phantom" type variable -- a type variable that
isn't actually used for any data. This gives us all the type-safety we need at
compile time, but lets us have a single way of actually getting the `Int` when
we need it.

-}

import Restful.Endpoint exposing (EntityUuid)



{-
    The rest of this are the phantom types for each entity we're using.
    This would benefit from some kind of code-generation step, since you
    could generate the code below easily from a list of base types.

    We create the type aliases so that we can just say

        ChildId

    most of the time, rather than the more verbose

        EntityUuid ChildId

    There are some possibly-attractive variations on this.

    - In a way, it would be nice to do something like Yesod does, with
      a `Key Child` and `Value Child` ... that is, it would be nice if
     `Child` itself would be the phantom type, rather than `ChildIdType`.
     But then our circular import problem would be much worse -- we'd have
     to keep all the actual definitions of the various entity records in
     a single file, which doesn't seem desirable. (It is, in effect, what
     Yesod does, seeing as you define all the entities in one file, typically).

   - Another alternative would be to also explicitly tie the `Child` type in
     `Backend.Child.Model` to the phantom type here, so that we have a kind of
     association between the `ChildId` type and the `Child` type ... that
     is, both refer to the same phantom type. That is probably desirable,
     since it helps enforce that we're using the right kind of key with
     the right kind of value -- I'll play with that at some point and see
     how it can be made to work.

-}


type alias BreastExamId =
    EntityUuid BreastExamUuidType


type BreastExamUuidType
    = BreastExamUuidType


type alias BirthPlanId =
    EntityUuid BirthPlanUuidType


type BirthPlanUuidType
    = BirthPlanUuidType


type alias CorePhysicalExamId =
    EntityUuid CorePhysicalExamUuidType


type CorePhysicalExamUuidType
    = CorePhysicalExamUuidType


type alias DangerSignsId =
    EntityUuid DangerSignsUuidType


type DangerSignsUuidType
    = DangerSignsUuidType


type alias LastMenstrualPeriodId =
    EntityUuid LastMenstrualPeriodUuidType


type LastMenstrualPeriodUuidType
    = LastMenstrualPeriodUuidType


type alias MedicalHistoryId =
    EntityUuid MedicalHistoryUuidType


type MedicalHistoryUuidType
    = MedicalHistoryUuidType


type alias MedicationId =
    EntityUuid MedicationUuidType


type MedicationUuidType
    = MedicationUuidType


type alias ObstetricalExamId =
    EntityUuid ObstetricalExamUuidType


type ObstetricalExamUuidType
    = ObstetricalExamUuidType


type alias ObstetricHistoryId =
    EntityUuid ObstetricHistoryUuidType


type ObstetricHistoryUuidType
    = ObstetricHistoryUuidType


type alias ObstetricHistoryStep2Id =
    EntityUuid ObstetricHistoryStep2UuidType


type ObstetricHistoryStep2UuidType
    = ObstetricHistoryStep2UuidType


type alias PrenatalFamilyPlanningId =
    EntityUuid PrenatalFamilyPlanningUuidType


type PrenatalFamilyPlanningUuidType
    = PrenatalFamilyPlanningUuidType


type alias PrenatalNutritionId =
    EntityUuid PrenatalNutritionUuidType


type PrenatalNutritionUuidType
    = PrenatalNutritionUuidType


type alias MalariaPreventionId =
    EntityUuid MalariaPreventionUuidType


type MalariaPreventionUuidType
    = MalariaPreventionUuidType


type alias SocialHistoryId =
    EntityUuid SocialHistoryUuidType


type SocialHistoryUuidType
    = SocialHistoryUuidType


type alias VitalsId =
    EntityUuid VitalsUuidType


type VitalsUuidType
    = VitalsUuidType


type alias CatchmentAreaId =
    EntityUuid CatchmentAreaUuidType


type CatchmentAreaUuidType
    = CatchmentAreaUuidType


type alias ChildFbfId =
    EntityUuid ChildFbfUuidType


type ChildFbfUuidType
    = ChildFbfUuidType


type alias ChildNutritionId =
    EntityUuid ChildNutritionUuidType


type ChildNutritionUuidType
    = ChildNutritionUuidType


type alias ClinicId =
    EntityUuid ClinicUuidType


type ClinicUuidType
    = ClinicUuidType


type alias CounselingScheduleId =
    EntityUuid CounselingScheduleUuidType


type CounselingScheduleUuidType
    = CounselingScheduleUuidType


type alias CounselingSessionId =
    EntityUuid CounselingSessionUuidType


type CounselingSessionUuidType
    = CounselingSessionUuidType


type alias CounselingTopicId =
    EntityUuid CounselingTopicUuidType


type CounselingTopicUuidType
    = CounselingTopicUuidType


type alias AttendanceId =
    EntityUuid AttendanceUuidType


type AttendanceUuidType
    = AttendanceUuidType


type alias FamilyPlanningId =
    EntityUuid FamilyPlanningUuidType


type FamilyPlanningUuidType
    = FamilyPlanningUuidType


type alias HealthCenterId =
    EntityUuid HealthCenterUuidType


type HealthCenterUuidType
    = HealthCenterUuidType


type alias HeightId =
    EntityUuid HeightUuidType


type HeightUuidType
    = HeightUuidType


type alias LactationId =
    EntityUuid LactationUuidType


type LactationUuidType
    = LactationUuidType


type alias MotherFbfId =
    EntityUuid MotherFbfUuidType


type MotherFbfUuidType
    = MotherFbfUuidType


type alias MuacId =
    EntityUuid MuacUuidType


type MuacUuidType
    = MuacUuidType


type alias NurseId =
    EntityUuid NurseUuidType


type NurseUuidType
    = NurseUuidType


type alias ParticipantConsentId =
    EntityUuid ParticipantConsentUuidType


type ParticipantConsentUuidType
    = ParticipantConsentUuidType


type alias ParticipantFormId =
    EntityUuid ParticipantFormUuidType


type ParticipantFormUuidType
    = ParticipantFormUuidType


type alias PersonId =
    EntityUuid PersonUuidType


type PersonUuidType
    = PersonUuidType


type alias PhotoId =
    EntityUuid PhotoUuidType


type PhotoUuidType
    = PhotoUuidType


type alias IndividualEncounterParticipantId =
    EntityUuid IndividualEncounterParticipantIdType


type IndividualEncounterParticipantIdType
    = IndividualEncounterParticipantIdType


type alias PrenatalEncounterId =
    EntityUuid PrenatalEncounterIdType


type PrenatalEncounterIdType
    = PrenatalEncounterIdType


type alias PrenatalPhotoId =
    EntityUuid PrenatalPhotoUuidType


type PrenatalPhotoUuidType
    = PrenatalPhotoUuidType


type alias PmtctParticipantId =
    EntityUuid PmtctParticipantUuidType


type PmtctParticipantUuidType
    = PmtctParticipantUuidType


type alias RelationshipId =
    EntityUuid RelationshipUuidType


type RelationshipUuidType
    = RelationshipUuidType


type alias SessionId =
    EntityUuid SessionUuidType


type SessionUuidType
    = SessionUuidType


type alias VillageId =
    EntityUuid VillageUuidType


type VillageUuidType
    = VillageUuidType


type alias WeightId =
    EntityUuid WeightUuidType


type WeightUuidType
    = WeightUuidType


type alias NutritionEncounterId =
    EntityUuid NutritionEncounterUuidType


type NutritionEncounterUuidType
    = NutritionEncounterUuidType


type alias NutritionMuacId =
    EntityUuid NutritionMuacUuidType


type NutritionMuacUuidType
    = NutritionMuacUuidType


type alias NutritionHeightId =
    EntityUuid NutritionHeightUuidType


type NutritionHeightUuidType
    = NutritionHeightUuidType


type alias NutritionNutritionId =
    EntityUuid NutritionNutritionUuidType


type NutritionNutritionUuidType
    = NutritionNutritionUuidType


type alias NutritionPhotoId =
    EntityUuid NutritionPhotoUuidType


type NutritionPhotoUuidType
    = NutritionPhotoUuidType


type alias NutritionWeightId =
    EntityUuid NutritionWeightUuidType


type NutritionWeightUuidType
    = NutritionWeightUuidType


type alias AcuteIllnessEncounterId =
    EntityUuid AcuteIllnessEncounterUuidType


type AcuteIllnessEncounterUuidType
    = AcuteIllnessEncounterUuidType


type alias SymptomsGeneralId =
    EntityUuid SymptomsGeneralUuidType


type SymptomsGeneralUuidType
    = SymptomsGeneralUuidType


type alias SymptomsRespiratoryId =
    EntityUuid SymptomsRespiratoryUuidType


type SymptomsRespiratoryUuidType
    = SymptomsRespiratoryUuidType


type alias SymptomsGIId =
    EntityUuid SymptomsGIUuidType


type SymptomsGIUuidType
    = SymptomsGIUuidType


type alias AcuteIllnessVitalsId =
    EntityUuid AcuteIllnessVitalsUuidType


type AcuteIllnessVitalsUuidType
    = AcuteIllnessVitalsUuidType


type alias MalariaTestingId =
    EntityUuid MalariaTestingUuidType


type MalariaTestingUuidType
    = MalariaTestingUuidType


type alias TravelHistoryId =
    EntityUuid TravelHistoryUuidType


type TravelHistoryUuidType
    = TravelHistoryUuidType


type alias TreatmentReviewId =
    EntityUuid TreatmentReviewUuidType


type TreatmentReviewUuidType
    = TreatmentReviewUuidType


type alias ExposureId =
    EntityUuid ExposureUuidType


type ExposureUuidType
    = ExposureUuidType


type alias IsolationId =
    EntityUuid IsolationUuidType


type IsolationUuidType
    = IsolationUuidType


type alias HCContactId =
    EntityUuid HCContactUuidType


type HCContactUuidType
    = HCContactUuidType


type alias Call114Id =
    EntityUuid Call114UuidType


type Call114UuidType
    = Call114UuidType


type alias AcuteFindingsId =
    EntityUuid AcuteFindingsUuidType


type AcuteFindingsUuidType
    = AcuteFindingsUuidType


type alias SendToHCId =
    EntityUuid SendToHCIdUuidType


type SendToHCIdUuidType
    = SendToHCIdUuidType


type alias MedicationDistributionId =
    EntityUuid MedicationDistributionIdUuidType


type MedicationDistributionIdUuidType
    = MedicationDistributionIdUuidType


type alias AcuteIllnessMuacId =
    EntityUuid AcuteIllnessMuacUuidType


type AcuteIllnessMuacUuidType
    = AcuteIllnessMuacUuidType


type alias TreatmentOngoingId =
    EntityUuid TreatmentOngoingUuidType


type TreatmentOngoingUuidType
    = TreatmentOngoingUuidType


type alias AcuteIllnessDangerSignsId =
    EntityUuid AcuteIllnessDangerSignsUuidType


type AcuteIllnessDangerSignsUuidType
    = AcuteIllnessDangerSignsUuidType


type alias AcuteIllnessNutritionId =
    EntityUuid AcuteIllnessNutritionUuidType


type AcuteIllnessNutritionUuidType
    = AcuteIllnessNutritionUuidType


type alias HealthEducationId =
    EntityUuid HealthEducationUuidType


type HealthEducationUuidType
    = HealthEducationUuidType


type alias NutritionSendToHCId =
    EntityUuid NutritionSendToHCIdUuidType


type NutritionSendToHCIdUuidType
    = NutritionSendToHCIdUuidType


type alias NutritionHealthEducationId =
    EntityUuid NutritionHealthEducationUuidType


type NutritionHealthEducationUuidType
    = NutritionHealthEducationUuidType


type alias NutritionContributingFactorsId =
    EntityUuid NutritionContributingFactorsIdUuidType


type NutritionContributingFactorsIdUuidType
    = NutritionContributingFactorsIdUuidType


type alias NutritionFollowUpId =
    EntityUuid NutritionFollowUpIdUuidType


type NutritionFollowUpIdUuidType
    = NutritionFollowUpIdUuidType


type alias GroupSendToHCId =
    EntityUuid GroupSendToHCIdUuidType


type GroupSendToHCIdUuidType
    = GroupSendToHCIdUuidType


type alias GroupHealthEducationId =
    EntityUuid GroupHealthEducationUuidType


type GroupHealthEducationUuidType
    = GroupHealthEducationUuidType


type alias ContributingFactorsId =
    EntityUuid ContributingFactorsIdUuidType


type ContributingFactorsIdUuidType
    = ContributingFactorsIdUuidType


type alias FollowUpId =
    EntityUuid FollowUpIdUuidType


type FollowUpIdUuidType
    = FollowUpIdUuidType


type alias HomeVisitEncounterId =
    EntityUuid HomeVisitEncounterUuidType


type HomeVisitEncounterUuidType
    = HomeVisitEncounterUuidType


type alias NutritionFeedingId =
    EntityUuid NutritionFeedingIdUuidType


type NutritionFeedingIdUuidType
    = NutritionFeedingIdUuidType


type alias NutritionHygieneId =
    EntityUuid NutritionHygieneIdUuidType


type NutritionHygieneIdUuidType
    = NutritionHygieneIdUuidType


type alias NutritionFoodSecurityId =
    EntityUuid NutritionFoodSecurityIdUuidType


type NutritionFoodSecurityIdUuidType
    = NutritionFoodSecurityIdUuidType


type alias NutritionCaringId =
    EntityUuid NutritionCaringIdUuidType


type NutritionCaringIdUuidType
    = NutritionCaringIdUuidType


type alias AcuteIllnessFollowUpId =
    EntityUuid AcuteIllnessFollowUpIdUuidType


type AcuteIllnessFollowUpIdUuidType
    = AcuteIllnessFollowUpIdUuidType


type alias PregnancyTestId =
    EntityUuid PregnancyTestUuidType


type PregnancyTestUuidType
    = PregnancyTestUuidType


type alias PrenatalHealthEducationId =
    EntityUuid PrenatalHealthEducationUuidType


type PrenatalHealthEducationUuidType
    = PrenatalHealthEducationUuidType


type alias PrenatalFollowUpId =
    EntityUuid PrenatalFollowUpUuidType


type PrenatalFollowUpUuidType
    = PrenatalFollowUpUuidType


type alias PrenatalSendToHCId =
    EntityUuid PrenatalSendToHcUuidType


type PrenatalSendToHcUuidType
    = PrenatalSendToHcUuidType


type alias PrenatalAppointmentConfirmationId =
    EntityUuid PrenatalAppointmentConfirmationUuidType


type PrenatalAppointmentConfirmationUuidType
    = PrenatalAppointmentConfirmationUuidType


type alias WellChildEncounterId =
    EntityUuid WellChildEncounterUuidType


type WellChildEncounterUuidType
    = WellChildEncounterUuidType


type alias WellChildECDId =
    EntityUuid WellChildECDUuidType


type WellChildECDUuidType
    = WellChildECDUuidType


type alias WellChildNutritionId =
    EntityUuid WellChildNutritionUuidType


type WellChildNutritionUuidType
    = WellChildNutritionUuidType


type alias WellChildMuacId =
    EntityUuid WellChildMuacUuidType


type WellChildMuacUuidType
    = WellChildMuacUuidType


type alias WellChildHeightId =
    EntityUuid WellChildHeightUuidType


type WellChildHeightUuidType
    = WellChildHeightUuidType


type alias WellChildPhotoId =
    EntityUuid WellChildPhotoUuidType


type WellChildPhotoUuidType
    = WellChildPhotoUuidType


type alias WellChildWeightId =
    EntityUuid WellChildWeightUuidType


type WellChildWeightUuidType
    = WellChildWeightUuidType


type alias WellChildSendToHCId =
    EntityUuid WellChildSendToHCIdUuidType


type WellChildSendToHCIdUuidType
    = WellChildSendToHCIdUuidType


type alias WellChildHealthEducationId =
    EntityUuid WellChildHealthEducationUuidType


type WellChildHealthEducationUuidType
    = WellChildHealthEducationUuidType


type alias WellChildContributingFactorsId =
    EntityUuid WellChildContributingFactorsIdUuidType


type WellChildContributingFactorsIdUuidType
    = WellChildContributingFactorsIdUuidType


type alias WellChildFollowUpId =
    EntityUuid WellChildFollowUpIdUuidType


type WellChildFollowUpIdUuidType
    = WellChildFollowUpIdUuidType


type alias WellChildHeadCircumferenceId =
    EntityUuid WellChildHeadCircumferenceUuidType


type WellChildHeadCircumferenceUuidType
    = WellChildHeadCircumferenceUuidType


type alias WellChildSymptomsReviewId =
    EntityUuid WellChildSymptomsReviewUuidType


type WellChildSymptomsReviewUuidType
    = WellChildSymptomsReviewUuidType


type alias WellChildVitalsId =
    EntityUuid WellChildVitalsUuidType


type WellChildVitalsUuidType
    = WellChildVitalsUuidType


type alias WellChildMebendezoleId =
    EntityUuid WellChildMebendezoleUuidType


type WellChildMebendezoleUuidType
    = WellChildMebendezoleUuidType


type alias WellChildPregnancySummaryId =
    EntityUuid WellChildPregnancySummaryUuidType


type WellChildPregnancySummaryUuidType
    = WellChildPregnancySummaryUuidType


type alias WellChildVitaminAId =
    EntityUuid WellChildVitaminAUuidType


type WellChildVitaminAUuidType
    = WellChildVitaminAUuidType


type alias WellChildAlbendazoleId =
    EntityUuid WellChildAlbendazoleUuidType


type WellChildAlbendazoleUuidType
    = WellChildAlbendazoleUuidType


type alias WellChildNextVisitId =
    EntityUuid WellChildNextVisitUuidType


type WellChildNextVisitUuidType
    = WellChildNextVisitUuidType


type alias WellChildBCGImmunisationId =
    EntityUuid WellChildBCGImmunisationUuidType


type WellChildBCGImmunisationUuidType
    = WellChildBCGImmunisationUuidType


type alias WellChildDTPImmunisationId =
    EntityUuid WellChildDTPImmunisationUuidType


type WellChildDTPImmunisationUuidType
    = WellChildDTPImmunisationUuidType


type alias WellChildDTPStandaloneImmunisationId =
    EntityUuid WellChildDTPStandaloneImmunisationUuidType


type WellChildDTPStandaloneImmunisationUuidType
    = WellChildDTPStandaloneImmunisationUuidType


type alias WellChildHPVImmunisationId =
    EntityUuid WellChildHPVImmunisationUuidType


type WellChildHPVImmunisationUuidType
    = WellChildHPVImmunisationUuidType


type alias WellChildIPVImmunisationId =
    EntityUuid WellChildIPVImmunisationUuidType


type WellChildIPVImmunisationUuidType
    = WellChildIPVImmunisationUuidType


type alias WellChildMRImmunisationId =
    EntityUuid WellChildMRImmunisationUuidType


type WellChildMRImmunisationUuidType
    = WellChildMRImmunisationUuidType


type alias WellChildOPVImmunisationId =
    EntityUuid WellChildOPVImmunisationUuidType


type WellChildOPVImmunisationUuidType
    = WellChildOPVImmunisationUuidType


type alias WellChildPCV13ImmunisationId =
    EntityUuid WellChildPCV13ImmunisationUuidType


type WellChildPCV13ImmunisationUuidType
    = WellChildPCV13ImmunisationUuidType


type alias WellChildRotarixImmunisationId =
    EntityUuid WellChildRotarixImmunisationUuidType


type WellChildRotarixImmunisationUuidType
    = WellChildRotarixImmunisationUuidType


type alias AcuteIllnessCoreExamId =
    EntityUuid AcuteIllnessCoreExamUuidType


type AcuteIllnessCoreExamUuidType
    = AcuteIllnessCoreExamUuidType


type alias CovidTestingId =
    EntityUuid CovidTestingUuidType


type CovidTestingUuidType
    = CovidTestingUuidType


type alias AcuteIllnessContactsTracingId =
    EntityUuid AcuteIllnessContactsTracingUuidType


type AcuteIllnessContactsTracingUuidType
    = AcuteIllnessContactsTracingUuidType


type alias AcuteIllnessTraceContactId =
    EntityUuid AcuteIllnessTraceContactUuidType


type AcuteIllnessTraceContactUuidType
    = AcuteIllnessTraceContactUuidType


type alias PrenatalBloodGpRsTestId =
    EntityUuid PrenatalBloodGpRsTestUuidType


type PrenatalBloodGpRsTestUuidType
    = PrenatalBloodGpRsTestUuidType


type alias PrenatalHemoglobinTestId =
    EntityUuid PrenatalHemoglobinTestUuidType


type PrenatalHemoglobinTestUuidType
    = PrenatalHemoglobinTestUuidType


type alias PrenatalHepatitisBTestId =
    EntityUuid PrenatalHepatitisBTestUuidType


type PrenatalHepatitisBTestUuidType
    = PrenatalHepatitisBTestUuidType


type alias PrenatalHIVTestId =
    EntityUuid PrenatalHIVTestUuidType


type PrenatalHIVTestUuidType
    = PrenatalHIVTestUuidType


type alias PrenatalMalariaTestId =
    EntityUuid PrenatalMalariaTestUuidType


type PrenatalMalariaTestUuidType
    = PrenatalMalariaTestUuidType


type alias PrenatalRandomBloodSugarTestId =
    EntityUuid PrenatalRandomBloodSugarTestUuidType


type PrenatalRandomBloodSugarTestUuidType
    = PrenatalRandomBloodSugarTestUuidType


type alias PrenatalSyphilisTestId =
    EntityUuid PrenatalSyphilisTestUuidType


type PrenatalSyphilisTestUuidType
    = PrenatalSyphilisTestUuidType


type alias PrenatalUrineDipstickTestId =
    EntityUuid PrenatalUrineDipstickTestUuidType


type PrenatalUrineDipstickTestUuidType
    = PrenatalUrineDipstickTestUuidType


type alias PrenatalLabsResultsId =
    EntityUuid PrenatalLabsResultsUuidType


type PrenatalLabsResultsUuidType
    = PrenatalLabsResultsUuidType


type alias PrenatalMedicationDistributionId =
    EntityUuid PrenatalMedicationDistributionUuidType


type PrenatalMedicationDistributionUuidType
    = PrenatalMedicationDistributionUuidType


type alias PrenatalSymptomReviewId =
    EntityUuid PrenatalSymptomReviewUuidType


type PrenatalSymptomReviewUuidType
    = PrenatalSymptomReviewUuidType


type alias PrenatalOutsideCareId =
    EntityUuid PrenatalOutsideCareUuidType


type PrenatalOutsideCareUuidType
    = PrenatalOutsideCareUuidType


type alias PrenatalHIVPCRTestId =
    EntityUuid PrenatalHIVPCRTestUuidType


type PrenatalHIVPCRTestUuidType
    = PrenatalHIVPCRTestUuidType


type alias PrenatalMentalHealthId =
    EntityUuid PrenatalMentalHealthUuidType


type PrenatalMentalHealthUuidType
    = PrenatalMentalHealthUuidType


type alias PrenatalTetanusImmunisationId =
    EntityUuid PrenatalTetanusImmunisationUuidType


type PrenatalTetanusImmunisationUuidType
    = PrenatalTetanusImmunisationUuidType


type alias PrenatalBreastfeedingId =
    EntityUuid PrenatalBreastfeedingUuidType


type PrenatalBreastfeedingUuidType
    = PrenatalBreastfeedingUuidType


type alias PrenatalGUExamId =
    EntityUuid PrenatalGUExamUuidType


type PrenatalGUExamUuidType
    = PrenatalGUExamUuidType


type alias PrenatalSpecialityCareId =
    EntityUuid PrenatalSpecialityCareUuidType


type PrenatalSpecialityCareUuidType
    = PrenatalSpecialityCareUuidType


type alias PrenatalCalciumId =
    EntityUuid PrenatalCalciumUuidType


type PrenatalCalciumUuidType
    = PrenatalCalciumUuidType


type alias PrenatalFolateId =
    EntityUuid PrenatalFolateUuidType


type PrenatalFolateUuidType
    = PrenatalFolateUuidType


type alias PrenatalIronId =
    EntityUuid PrenatalIronUuidType


type PrenatalIronUuidType
    = PrenatalIronUuidType


type alias PrenatalMMSId =
    EntityUuid PrenatalMMSUuidType


type PrenatalMMSUuidType
    = PrenatalMMSUuidType


type alias PrenatalMebendazoleId =
    EntityUuid PrenatalMebendazoleUuidType


type PrenatalMebendazoleUuidType
    = PrenatalMebendazoleUuidType


type alias NCDEncounterId =
    EntityUuid NCDEncounterUuidType


type NCDEncounterUuidType
    = NCDEncounterUuidType


type alias NCDCoMorbiditiesId =
    EntityUuid NCDCoMorbiditiesUuidType


type NCDCoMorbiditiesUuidType
    = NCDCoMorbiditiesUuidType


type alias NCDCoreExamId =
    EntityUuid NCDCoreExamUuidType


type NCDCoreExamUuidType
    = NCDCoreExamUuidType


type alias NCDCreatinineTestId =
    EntityUuid NCDCreatinineTestUuidType


type NCDCreatinineTestUuidType
    = NCDCreatinineTestUuidType


type alias NCDDangerSignsId =
    EntityUuid NCDDangerSignsUuidType


type NCDDangerSignsUuidType
    = NCDDangerSignsUuidType


type alias NCDFamilyHistoryId =
    EntityUuid NCDFamilyHistoryUuidType


type NCDFamilyHistoryUuidType
    = NCDFamilyHistoryUuidType


type alias NCDFamilyPlanningId =
    EntityUuid NCDFamilyPlanningUuidType


type NCDFamilyPlanningUuidType
    = NCDFamilyPlanningUuidType


type alias NCDHealthEducationId =
    EntityUuid NCDHealthEducationUuidType


type NCDHealthEducationUuidType
    = NCDHealthEducationUuidType


type alias NCDHIVTestId =
    EntityUuid NCDHIVTestUuidType


type NCDHIVTestUuidType
    = NCDHIVTestUuidType


type alias NCDLabsResultsId =
    EntityUuid NCDLabsResultsUuidType


type NCDLabsResultsUuidType
    = NCDLabsResultsUuidType


type alias NCDLiverFunctionTestId =
    EntityUuid NCDLiverFunctionTestUuidType


type NCDLiverFunctionTestUuidType
    = NCDLiverFunctionTestUuidType


type alias NCDMedicationDistributionId =
    EntityUuid NCDMedicationDistributionUuidType


type NCDMedicationDistributionUuidType
    = NCDMedicationDistributionUuidType


type alias NCDMedicationHistoryId =
    EntityUuid NCDMedicationHistoryUuidType


type NCDMedicationHistoryUuidType
    = NCDMedicationHistoryUuidType


type alias NCDOutsideCareId =
    EntityUuid NCDOutsideCareUuidType


type NCDOutsideCareUuidType
    = NCDOutsideCareUuidType


type alias NCDPregnancyTestId =
    EntityUuid NCDPregnancyTestUuidType


type NCDPregnancyTestUuidType
    = NCDPregnancyTestUuidType


type alias NCDRandomBloodSugarTestId =
    EntityUuid NCDRandomBloodSugarTestUuidType


type NCDRandomBloodSugarTestUuidType
    = NCDRandomBloodSugarTestUuidType


type alias NCDReferralId =
    EntityUuid NCDReferralUuidType


type NCDReferralUuidType
    = NCDReferralUuidType


type alias NCDSocialHistoryId =
    EntityUuid NCDSocialHistoryUuidType


type NCDSocialHistoryUuidType
    = NCDSocialHistoryUuidType


type alias NCDSymptomReviewId =
    EntityUuid NCDSymptomReviewUuidType


type NCDSymptomReviewUuidType
    = NCDSymptomReviewUuidType


type alias NCDUrineDipstickTestId =
    EntityUuid NCDUrineDipstickTestUuidType


type NCDUrineDipstickTestUuidType
    = NCDUrineDipstickTestUuidType


type alias NCDVitalsId =
    EntityUuid NCDVitalsUuidType


type NCDVitalsUuidType
    = NCDVitalsUuidType


type alias GroupNCDAId =
    EntityUuid GroupNCDAUuidType


type GroupNCDAUuidType
    = GroupNCDAUuidType


type alias NutritionNCDAId =
    EntityUuid NutritionNCDAUuidType


type NutritionNCDAUuidType
    = NutritionNCDAUuidType


type alias WellChildNCDAId =
    EntityUuid WellChildNCDAUuidType


type WellChildNCDAUuidType
    = WellChildNCDAUuidType


type alias NCDLipidPanelTestId =
    EntityUuid NCDLipidPanelTestUuidType


type NCDLipidPanelTestUuidType
    = NCDLipidPanelTestUuidType


type alias NCDHbA1cTestId =
    EntityUuid NCDHbA1cTestUuidType


type NCDHbA1cTestUuidType
    = NCDHbA1cTestUuidType


type alias ResilienceSurveyId =
    EntityUuid ResilienceSurveyUuidType


type ResilienceSurveyUuidType
    = ResilienceSurveyUuidType


type alias ResilienceMessageId =
    String


type alias PrenatalPartnerHIVTestId =
    EntityUuid PrenatalPartnerHIVTestUuidType


type PrenatalPartnerHIVTestUuidType
    = PrenatalPartnerHIVTestUuidType


type alias StockUpdateId =
    EntityUuid StockUpdateUuidType


type StockUpdateUuidType
    = StockUpdateUuidType


type alias ChildScoreboardEncounterId =
    EntityUuid ChildScoreboardEncounterUuidType


type ChildScoreboardEncounterUuidType
    = ChildScoreboardEncounterUuidType


type alias ChildScoreboardNCDAId =
    EntityUuid ChildScoreboardNCDAUuidType


type ChildScoreboardNCDAUuidType
    = ChildScoreboardNCDAUuidType


type alias ChildScoreboardBCGImmunisationId =
    EntityUuid ChildScoreboardBCGImmunisationUuidType


type ChildScoreboardBCGImmunisationUuidType
    = ChildScoreboardBCGImmunisationUuidType


type alias ChildScoreboardDTPImmunisationId =
    EntityUuid ChildScoreboardDTPImmunisationUuidType


type ChildScoreboardDTPImmunisationUuidType
    = ChildScoreboardDTPImmunisationUuidType


type alias ChildScoreboardDTPStandaloneImmunisationId =
    EntityUuid ChildScoreboardDTPStandaloneImmunisationUuidType


type ChildScoreboardDTPStandaloneImmunisationUuidType
    = ChildScoreboardDTPStandaloneImmunisationUuidType


type alias ChildScoreboardIPVImmunisationId =
    EntityUuid ChildScoreboardIPVImmunisationUuidType


type ChildScoreboardIPVImmunisationUuidType
    = ChildScoreboardIPVImmunisationUuidType


type alias ChildScoreboardMRImmunisationId =
    EntityUuid ChildScoreboardMRImmunisationUuidType


type ChildScoreboardMRImmunisationUuidType
    = ChildScoreboardMRImmunisationUuidType


type alias ChildScoreboardOPVImmunisationId =
    EntityUuid ChildScoreboardOPVImmunisationUuidType


type ChildScoreboardOPVImmunisationUuidType
    = ChildScoreboardOPVImmunisationUuidType


type alias ChildScoreboardPCV13ImmunisationId =
    EntityUuid ChildScoreboardPCV13ImmunisationUuidType


type ChildScoreboardPCV13ImmunisationUuidType
    = ChildScoreboardPCV13ImmunisationUuidType


type alias ChildScoreboardRotarixImmunisationId =
    EntityUuid ChildScoreboardRotarixImmunisationUuidType


type ChildScoreboardRotarixImmunisationUuidType
    = ChildScoreboardRotarixImmunisationUuidType


type alias WellChildFeedingId =
    EntityUuid WellChildFeedingIdUuidType


type WellChildFeedingIdUuidType
    = WellChildFeedingIdUuidType


type alias WellChildHygieneId =
    EntityUuid WellChildHygieneIdUuidType


type WellChildHygieneIdUuidType
    = WellChildHygieneIdUuidType


type alias WellChildFoodSecurityId =
    EntityUuid WellChildFoodSecurityIdUuidType


type WellChildFoodSecurityIdUuidType
    = WellChildFoodSecurityIdUuidType


type alias WellChildCaringId =
    EntityUuid WellChildCaringIdUuidType


type WellChildCaringIdUuidType
    = WellChildCaringIdUuidType


type alias TuberculosisEncounterId =
    EntityUuid TuberculosisEncounterUuidType


type TuberculosisEncounterUuidType
    = TuberculosisEncounterUuidType


type alias TuberculosisDOTId =
    EntityUuid TuberculosisDOTUuidType


type TuberculosisDOTUuidType
    = TuberculosisDOTUuidType


type alias TuberculosisDiagnosticsId =
    EntityUuid TuberculosisDiagnosticsUuidType


type TuberculosisDiagnosticsUuidType
    = TuberculosisDiagnosticsUuidType


type alias TuberculosisFollowUpId =
    EntityUuid TuberculosisFollowUpUuidType


type TuberculosisFollowUpUuidType
    = TuberculosisFollowUpUuidType


type alias TuberculosisHealthEducationId =
    EntityUuid TuberculosisHealthEducationUuidType


type TuberculosisHealthEducationUuidType
    = TuberculosisHealthEducationUuidType


type alias TuberculosisMedicationId =
    EntityUuid TuberculosisMedicationUuidType


type TuberculosisMedicationUuidType
    = TuberculosisMedicationUuidType


type alias TuberculosisReferralId =
    EntityUuid TuberculosisReferralUuidType


type TuberculosisReferralUuidType
    = TuberculosisReferralUuidType


type alias TuberculosisSymptomReviewId =
    EntityUuid TuberculosisSymptomReviewUuidType


type TuberculosisSymptomReviewUuidType
    = TuberculosisSymptomReviewUuidType


type alias TuberculosisTreatmentReviewId =
    EntityUuid TuberculosisTreatmentReviewUuidType


type TuberculosisTreatmentReviewUuidType
    = TuberculosisTreatmentReviewUuidType


type alias EducationSessionId =
    EntityUuid EducationSessionUuidType


type EducationSessionUuidType
    = EducationSessionUuidType


type alias HIVEncounterId =
    EntityUuid HIVEncounterUuidType


type HIVEncounterUuidType
    = HIVEncounterUuidType


type alias HIVDiagnosticsId =
    EntityUuid HIVDiagnosticsUuidType


type HIVDiagnosticsUuidType
    = HIVDiagnosticsUuidType


type alias HIVFollowUpId =
    EntityUuid HIVFollowUpUuidType


type HIVFollowUpUuidType
    = HIVFollowUpUuidType


type alias HIVHealthEducationId =
    EntityUuid HIVHealthEducationUuidType


type HIVHealthEducationUuidType
    = HIVHealthEducationUuidType


type alias HIVMedicationId =
    EntityUuid HIVMedicationUuidType


type HIVMedicationUuidType
    = HIVMedicationUuidType


type alias HIVReferralId =
    EntityUuid HIVReferralUuidType


type HIVReferralUuidType
    = HIVReferralUuidType


type alias HIVSymptomReviewId =
    EntityUuid HIVSymptomReviewUuidType


type HIVSymptomReviewUuidType
    = HIVSymptomReviewUuidType


type alias HIVTreatmentReviewId =
    EntityUuid HIVTreatmentReviewUuidType


type HIVTreatmentReviewUuidType
    = HIVTreatmentReviewUuidType
