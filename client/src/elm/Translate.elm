module Translate exposing
    ( ChartPhrase(..)
    , Dashboard(..)
    , Language
    , LoginPhrase(..)
    , TranslationId(..)
    , ValidationError(..)
    , translate
    , translateText
    )

{-| This module has just the translations ... for types and
general utilities, see `Translate.Model` and `Translate.Utils`.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..), IndividualEncounterType(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (..)
import Backend.NCDActivity.Model exposing (NCDActivity, NCDRecurrentActivity)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.Nurse.Model exposing (ResilienceRole(..))
import Backend.NutritionActivity.Model exposing (NutritionActivity)
import Backend.Person.Model
    exposing
        ( EducationLevel(..)
        , HIVStatus(..)
        , MaritalStatus(..)
        , ModeOfDelivery(..)
        , Ubudehe(..)
        , VaginalDelivery(..)
        )
import Backend.PrenatalActivity.Model
    exposing
        ( HighRiskFactor(..)
        , HighSeverityAlert
        , MedicalDiagnosis(..)
        , ObstetricalDiagnosis(..)
        , PregnancyTrimester(..)
        , PrenatalActivity(..)
        , PrenatalRecurrentActivity(..)
        , RecurringHighSeverityAlert
        , RiskFactor(..)
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.ResilienceMessage.Model exposing (ResilienceCategory(..))
import Backend.ResilienceSurvey.Model
    exposing
        ( ResilienceSurveyQuestion(..)
        , ResilienceSurveyQuestionOption(..)
        )
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), PediatricCareMilestone(..))
import Components.ReportToWhatsAppDialog.Model
    exposing
        ( ReportComponentAntenatal(..)
        , ReportComponentNCD(..)
        , ReportComponentWellChild(..)
        )
import Form.Error exposing (ErrorValue(..))
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (Html, text)
import Http
import Measurement.Model
    exposing
        ( FloatInputConstraints
        , GroupOfFoods(..)
        , LaboratoryTask(..)
        , NCDAStep(..)
        , NextStepsTask(..)
        )
import Pages.AcuteIllness.Activity.Types
    exposing
        ( AILaboratoryTask(..)
        , DangerSignsTask(..)
        , ExposureTask(..)
        , NextStepsTask(..)
        , OngoingTreatmentTask(..)
        , PhysicalExamTask(..)
        , PriorTreatmentTask(..)
        , SymptomReliefType(..)
        , SymptomsTask(..)
        )
import Pages.AcuteIllness.ProgressReport.Model exposing (AcuteIllnessStatus(..))
import Pages.Attendance.Model exposing (InitialResultsDisplay(..))
import Pages.Dashboard.Model as Dashboard
    exposing
        ( BeneficiariesTableLabels(..)
        , DashboardFilter(..)
        , DashboardSubFilter(..)
        , FeverCause(..)
        , FilterPeriod
        , FilterProgramType(..)
        )
import Pages.GlobalCaseManagement.Model exposing (CaseManagementFilter(..), FollowUpDueOption(..), LabsEntryState(..))
import Pages.MessagingCenter.Model exposing (MessagingTab(..))
import Pages.NCD.Activity.Types exposing (ExaminationTask(..), MedicalHistoryTask(..))
import Pages.NCD.ProgressReport.Model exposing (NCDRiskFactor(..))
import Pages.NCD.RecurrentActivity.Types
import Pages.Page exposing (..)
import Pages.PatientRecord.Model exposing (PatientRecordFilter(..))
import Pages.PinCode.Model exposing (MainMenuActivity(..), ResilienceReminderType(..))
import Pages.Prenatal.Activity.Types
    exposing
        ( EarlyMastitisOrEngorgmentReliefMethod(..)
        , ExaminationTask(..)
        , HeartburnReliefMethod(..)
        , HistoryTask(..)
        , LegCrampsReliefMethod(..)
        , TreatmentReviewTask(..)
        )
import Pages.Prenatal.Model exposing (HypertensionTreatementUpdateOption(..))
import Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..))
import Pages.Prenatal.RecurrentActivity.Types
import Pages.Report.Model
    exposing
        ( LabResultsCurrentMode(..)
        , LabResultsHistoryMode(..)
        , PaneEntryStatus(..)
        , RandomBloodSugarResult(..)
        , ReportTab(..)
        )
import Pages.StockManagement.Model exposing (CorrectionEntryType(..), StockManagementMenu(..))
import Pages.TraceContact.Model exposing (NoContactReason(..))
import Pages.WellChild.Activity.Types
    exposing
        ( HomeVisitTask(..)
        , NextStepsTask(..)
        , NutritionAssessmentTask(..)
        , VaccinationStatus(..)
        )
import Pages.WellChild.Encounter.Model exposing (ECDPopupType(..), WarningPopupType(..))
import Pages.WellChild.ProgressReport.Model
    exposing
        ( ECDStatus(..)
        , NCDAANCNewbornItem(..)
        , NCDAFillTheBlanksItem(..)
        , NCDAInfrastructureEnvironmentWashItem
        , NCDANutritionBehaviorItem(..)
        , NCDATargetedInterventionsItem(..)
        , NCDAUniversalInterventionsItem(..)
        )
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model exposing (Site(..))
import Time exposing (Month(..))
import Translate.Model exposing (TranslationSet)
import Translate.Utils exposing (..)
import ZScore.Model exposing (ChartAgeRange(..))


{-| We re-export this one for convenience, so you don't have to import
`Translate.Model` in simple cases. That is, you can do this, which will be
enough for most "view" modules:

    import Translate exposing (translate, Language)

Note that importing `Language` from here gives you only the type, not the
constructors. For more complex cases, where you need `English` and
`Kinyarwanda` as well, you have to do this instead:

    import Translate.Model exposing (Language(..))

-}
type alias Language =
    Translate.Model.Language


translate : Language -> TranslationId -> String
translate lang trans =
    selectLanguage lang (translationSet trans)


translateText : Language -> TranslationId -> Html msg
translateText lang trans =
    translate lang trans
        |> text


type LoginPhrase
    = ForgotPassword1
    | ForgotPassword2
    | LoggedInAs
    | LoginToSyncHealthCenters
    | PinCode
    | PinCodeRejected
    | SignIn
    | SignOut
    | YouMustLoginBefore


type ChartPhrase
    = AgeCompletedMonthsYears
    | AgeWeeks
    | ChartAgeRange ChartAgeRange
    | HeadCircumferenceCm
    | HeadCircumferenceForAge Gender
    | HeightForAge Gender
    | LengthCm
    | LengthForAge Gender
    | Months
    | WeightForAge Gender
    | WeightForLength Gender
    | WeightKg


type ValidationError
    = DigitsOnly
    | InvalidBirthDate
    | InvalidBirthDateForAdult
    | InvalidBirthDateForChild
    | InvalidHmisNumber
    | LengthError Int
    | RequiredField
    | UnknownProvince
    | UnknownDistrict
    | UnknownSector
    | UnknownCell
    | UnknownVillage
    | DecoderError String


type Dashboard
    = AcuteIllnessDiagnosed
    | BeneficiariesLabel
    | BeneficiariesTableColumnLabel BeneficiariesTableLabels
    | BeneficiariesTableLabel
    | BoysFilterLabel
    | CallsTo114
    | CaseManagementFirstWordHelper
    | CaseManagementHelper
    | CaseManagementLabel
    | CompletedProgramLabel
    | CommunityLevelCases
    | ComplicatedMalariaReferredToHC
    | ComplicatedGIInfectionsReferredToHc
    | CurrentPregnancies
    | DiagnosisUndetermined
    | DiagnosedCases
    | FamilyPlanningLabel
    | FamilyPlanningOutOfWomen { total : Int, useFamilyPlanning : Int }
    | FeversByCause
    | FeverCause FeverCause
    | FeverOfUnknownOrigin
    | Filter DashboardFilter
    | FilterProgramType FilterProgramType
    | Filters
    | GirlsFilterLabel
    | GoodNutritionLabel
    | HomeDeliveries
    | HealthFacilityDeliveries
    | HealthCenterReferrals
    | IncidenceOf
    | LastUpdated
    | MissedSessionsLabel
    | Moderate
    | ModeratelyMalnourished
    | MothersInANC
    | NewBeneficiaries
    | NewbornsInCare
    | NewCasesLabel
    | NewCasesPerMonth
    | NewPregnancy
    | NoDataForPeriod
    | PatientsManagedAtHome
    | PatientCurrentlyUnderCare
    | PercentageLabel FilterPeriod
    | PeriodFilter FilterPeriod
    | ProgramType
    | ResolvedCases
    | Severe
    | SeverelyMalnourished
    | StatisticsFirstWordHelper
    | StatisticsHelper
    | SubFilter DashboardSubFilter
    | TotalBeneficiaries
    | TotalMalnourished
    | TotalEncountersLabel
    | TotalAssessment
    | UncomplicatedMalariaByChws
    | UncomplicatedMalariaInPregnancyReferredToHc
    | UncomplicatedGIInfectionByCHWS
    | UseFamilyPlanning
    | Within4MonthsOfDueDate
    | WithDangerSigns


type TranslationId
    = Abdomen
    | AbdomenCPESign AbdomenCPESign
    | Abnormal
    | Abortions
    | Accept
    | AccompaniedByPartner
    | AccompanyToFacilityQuestion ReferralFacility
    | Actions
    | ActionsTaken
    | ActionsToTake
    | AcuteFindingsGeneralSign AcuteFindingsGeneralSign
    | AcuteFindingsRespiratorySign AcuteFindingsRespiratorySign
    | AcuteIllnessAdverseEvent AdverseEvent
    | AcuteIllnessAdverseEventKindsQuestion
    | AcuteIllnessDangerSign AcuteIllnessDangerSign
    | AcuteIllnessDiagnosis AcuteIllnessDiagnosis
    | AcuteIllnessDiagnosisWarning AcuteIllnessDiagnosis
    | AcuteIllnessExisting
    | AcuteIllnessHistory
    | AcuteIllnessLowRiskCaseHelper
    | AcuteIllnessNew
    | AcuteIllnessOutcome AcuteIllnessOutcome
    | AcuteIllnessOutcomeLabel
    | AcuteIllnessStatus AcuteIllnessStatus
    | ActiveDiagnosis
    | Activities
    | ActivitiesCompleted Int
    | ActivitiesHelp Activity
    | ActivitiesLabel Activity
    | ActivitiesTitle Activity
    | Activity
    | ActivitityTitleAchi
    | ActivitiesToComplete Int
    | ActivitityLabelAchi
    | ActivePage Page
    | AcuteIllnessActivityTitle AcuteIllnessActivity
    | AddChild
    | AddContact
    | AddedToPatientRecordOn
    | AddFamilyMember
    | AddFamilyMemberFor String
    | AddNewParticipant
    | AddParentOrCaregiver
    | AddToGroup
    | Administer
    | AdministerAlbendazoleHelper
    | AdministerAzithromycinHelper
    | AdministerCeftriaxoneHelper
    | AdministerMebendezoleHelper
    | AdministerMetronidazoleHelper
    | AdministerPrenatalMebendezoleHelper
    | AdministerFolicAcidHelper
    | AdministerHIVARVHelper
    | AdministerIronHelper
    | AdministerParacetamolHelper
    | AdministerVitaminAHelperPrenatal
    | AdministerVitaminAHelperWellChild
    | Administered
    | AdministeredMedicationQuestion
    | AdministeredOneOfAboveMedicinesQuestion
    | AddressInformation
    | AdverseEventSinglePlural Int
    | AfterEachLiquidStool
    | AgeWord
    | Age Int Int
    | AgeDays Int
    | AgeMonthsWithoutDay Int
    | AgeSingleBoth Int Int
    | AgeSingleMonth Int Int
    | AgeSingleMonthWithoutDay Int
    | AgeSingleDayWithMonth Int Int
    | AgeSingleDayWithoutMonth Int
    | AlertChwToFollowUp
    | AgeOneYearOld
    | AgeOneYearAndOneMonth
    | AgeOneYearWithMonths Int
    | AgeYearsWithSingleMonth Int Int
    | AgeYearsAndMonths Int Int
    | AILaboratoryTask AILaboratoryTask
    | All
    | AllowedValuesRangeHelper FloatInputConstraints
    | AmbulancArrivalPeriodQuestion
    | ANCEncountersNotRecordedQuestion
    | ANCIndicateVisitsMonthsPhrase
    | ANCNewborn
    | And
    | AndSentence
    | AntenatalCare
    | AntenatalProgressReport
    | AntenatalVisistsHistory
    | AppName
    | AppointmentConfirmation
    | AppointmentConfirmationInstrunction
    | AreYouSure
    | Assessment
    | Asthma
    | At
    | Attendance
    | AvoidingGuidanceReason AvoidingGuidanceReason
    | Baby
    | BabyDiedOnDayOfBirthPreviousDelivery
    | Back
    | BackendError
    | Balance
    | BatchNumberAbbrev
    | BreastfeedingSignQuestion BreastfeedingSign
    | BeatsPerMinuteUnitLabel
    | BeginNewEncounter
    | BirthDefect BirthDefect
    | BirthDefectLabel
    | BirthDefectsPresentQuestion
    | BirthDefectsSelectionLabel
    | BloodGlucose
    | BloodPressure
    | BloodPressureDiaLabel
    | BloodPressureSysLabel
    | BloodSmearQuestion
    | BloodSmearLabel
    | BloodSmearResult BloodSmearResult
    | BMI
    | BMIHelper
    | BodyTemperature
    | BornUnderweight
    | BoughtClothesQuestion
    | BowedLegs
    | BpmUnit Int
    | BreathsPerMinuteUnitLabel
    | BreastExam
    | BreastExamSign BreastExamSign
    | BreastExamDischargeQuestion
    | BreastExamDischargeType DischargeType
    | BreastExamQuestion
    | BrittleHair
    | ByMouthDaylyForXDays Int
    | ByMouthTwiceADayForXDays Int
    | ByMouthThreeTimesADayForXDays Int
    | Call114
    | Called114Question
    | Cancel
    | CandidiasisRecommendedTreatmentHeader
    | CandidiasisRecommendedTreatmentHelper
    | CannotStartEncounterLabel
    | CardiacDisease
    | CaregiverAccompanyQuestion
    | CaregiverMessage
    | Caring
    | CaseManagement
    | CaseManagementFilterLabel CaseManagementFilter
    | CaseManagementPaneHeader CaseManagementFilter
    | Celsius
    | CelsiusAbbrev
    | Cell
    | ChartPhrase ChartPhrase
    | CheckAllThatApply
    | CheckIn
    | Child0to5
    | Child6to24
    | ChildCleanQuestion
    | ChildHasMalnutritionPhrase
    | ChildHmisNumber
    | ChildIdentification
    | ChildNutritionSignLabel ChildNutritionSign
    | ChildName
    | Children
    | ChildrenNames
    | ChildrenNationalId
    | ChildScoreboardActivityTitle ChildScoreboardActivity
    | ChildScorecard
    | ChooseOne
    | CHWAction CHWAction
    | ChwActivity
    | Clear
    | ClickTheCheckMark
    | ClinicType ClinicType
    | Clinical
    | ClinicalProgressReport
    | CloseAcuteIllnessLabel
    | CloseAndContinue
    | Colline
    | CollineSub
    | ColorAlertIndication ColorAlertIndication
    | ColorGreen
    | ColorRed
    | ColorYellow
    | Commune
    | CompleteFacilityReferralForm ReferralFacility
    | Contacted114
    | ContactedHC
    | ContactedHCQuestion
    | ContactedRecommendedSiteQuestion
    | ContactInitiatedQuestion
    | ContactName
    | ContactsTracingCompleteDetails
    | ContactsTracingHelper
    | ContactWithCOVID19SymptomsHelper
    | ContactWithCOVID19SymptomsQuestion
    | Continued
    | ContributingFactor ContributingFactorsSign
    | ContributingFactorsQuestion
    | ConvulsionsAndUnconsciousPreviousDelivery
    | ConvulsionsPreviousDelivery
    | CSectionScar CSectionScar
    | Dashboard Dashboard
    | Group
    | Groups
    | Close
    | Closed
    | ConditionImproving Bool
    | ConditionImprovingQuestion
    | ContactExposure
    | ContactInformation
    | Continue
    | CounselingTopic CounselingTopic
    | CounselorReviewed
    | CovidContactTracing
    | CovidTestingInstructions
    | CounselorSignature
    | CSectionInPreviousDelivery
    | CSectionReason
    | CSectionReasons CSectionReason
    | CreateRelationship
    | ChwDashboardLabel
    | CurrentlyPregnant
    | CurrentlyPregnantQuestion
    | CurrentStock
    | DangerSign DangerSign
    | DangerSignsLabelForNurse
    | Date
    | DateConcludedEstimatedQuestion
    | DateOfContact
    | DatePregnancyConcluded
    | DashboardLabel
    | DateReceived
    | DateOfBirth
    | DayAbbrev
    | DaySinglePlural Int
    | DaysAbbrev
    | DaysPresent
    | DaysSinglePlural Int
    | Delete
    | DeliveryComplication DeliveryComplication
    | DeliveryComplicationsPresentQuestion
    | DeliveryComplicationsSelectionLabel
    | DeliveryLocation
    | DemographicInformation
    | DemographicsReport
    | Details
    | DetectableViralLoad
    | Device
    | DeviceNotAuthorized
    | DeviceStatus
    | Diabetes
    | DiagnosedAtAnotherFacilityPrefix
    | DiagnosedAtAnotherFacilitySuffix
    | DiagnosedByOutsideCare
    | DiagnosedOn
    | Diagnosis
    | DiagnosisDate
    | DifferenceBetweenDueAndDeliveryDates
    | Disabled
    | DistributionNotice DistributionNotice
    | District
    | DOB
    | DropzoneDefaultMessage
    | DueDate
    | DueTo
    | EarlyMastitisOrEngorgmentReliefMethod EarlyMastitisOrEngorgmentReliefMethod
    | EarlyChildhoodDevelopment
    | ECDSignQuestion ECDSign
    | ECDStatus ECDStatus
    | Edd
    | EddHeader
    | Edema
    | Ega
    | EgaHeader
    | EgaWeeks
    | ElevatedRespiratoryRate
    | EmergencyReferralHelperReferToHC
    | EmergencyReferralHelperReferToHospitalForImmediateDelivery
    | EmergencyReferralHelperReferToHospitalImmediately
    | EmergencyReferralHelperReferToMaternityWard
    | EmergencyReferralHelperReferToEmergencyObstetricCareServices
    | DangerSignsTask DangerSignsTask
    | EmptyString
    | EncounterDate
    | EncounterTypePageLabel ChwDashboardPage
    | EncounterTypeFollowUpQuestion IndividualEncounterType
    | EncounterWarningForDiagnosisPane EncounterWarning String
    | EndEncounter
    | EndEncounterQuestion
    | EndGroupEncounter
    | EnrolNewborn
    | EnrolNewbornHelper Bool
    | EnrollToProgramAction
    | EnrollToProgramQuestion
    | EnterPairingCode
    | EntryStatusAntenatal PaneEntryStatus
    | EntryStatusDiagnosis PaneEntryStatus
    | EPDSPreformedOn
    | EpisiotomyOrPerinealTearQuestion
    | EpisiotomyOrPerinealTearHealingQuestion
    | ErrorCheckLocalConfig
    | ErrorConfigurationError
    | Estimated
    | ExaminationTask ExaminationTask
    | ExaminationTaskRecurrent Pages.Prenatal.RecurrentActivity.Types.ExaminationTask
    | ExpirityDate
    | ExposureTask ExposureTask
    | Extremities
    | Eyes
    | Facility
    | FamilyInformation
    | FamilyMembers
    | FamilyPlanningCurentlyQuestion
    | FamilyPlanningInFutureQuestion
    | FamilyPlanningSignLabel FamilyPlanningSign
    | FamilyUbudehe
    | FatherOrChiefId
    | FatherOrChiefName
    | FavoriteToggle Bool
    | FbfDistribution ClinicType
    | Feeding
    | FetalHeartRate
    | FetalMovement
    | FetalPresentationLabel
    | FetalPresentation FetalPresentation
    | FillTheBlanks
    | FilterByName
    | Finish
    | FirstName
    | FiveVisits
    | FoodGroup FoodGroup
    | FoodSecurity
    | FollowPostpartumProtocols
    | FollowUpWithPatientIn
    | FollowUpWithPatientOn
    | FollowUpByChwLabel
    | FollowUpLabel
    | FollowUpWithMotherLabel
    | FollowUpOption FollowUpOption
    | FollowUpDueOption FollowUpDueOption
    | ForIllustrativePurposesOnly
    | FormError (ErrorValue ValidationError)
    | FormField String
    | FundalHeight
    | FundalPalpableQuestion
    | FundalPalpableWarning
    | Gender Gender
    | GenderLabel
    | GestationalDiabetesPreviousPregnancy
    | Glass String
    | GoHome
    | GotResultsPreviouslyQuestion
    | GroupAssessment
    | Grams
    | Gravida
    | GroupOfFoods GroupOfFoods
    | Growth
    | HalfOfDosage String
    | HandedReferralFormQuestion
    | HandPallor
    | Hands
    | HandsCPESign HandsCPESign
    | HbA1c
    | HbA1cPercentage
    | HbA1cMostRecentTestResultInstruction
    | HCRecommendation HCRecommendation
    | HCResponseQuestion
    | HCResponsePeriodQuestion
    | HeadCircumferenceHelper
    | HeadCircumferenceNotTakenLabel
    | HeadHair
    | HealthCenter
    | HealthCenterDetermined
    | HealthEducationNotProvided
    | HealthEducationProvided
    | HealthEducationProvidedQuestion
    | HealthInsuranceQuestion
    | Heart
    | HeartburnReliefMethod HeartburnReliefMethod
    | HeartburnRecommendedTreatmentHeader
    | HeartburnRecommendedTreatmentHelper
    | HeartMurmur
    | HeartCPESign HeartCPESign
    | HeartRate
    | Height
    | High
    | HighRiskCase
    | HighRiskCaseHelper
    | HighRiskFactor HighRiskFactor
    | HighRiskFactors
    | HighSeverityAlert HighSeverityAlert
    | HighSeverityAlerts
    | History
    | HistoryTask HistoryTask
    | HIV
    | HIVPCRResult HIVPCRResult
    | HIVStatus HIVStatus
    | HIVStatusLabel
    | HIVTreatmentSign HIVTreatmentSign
    | Home
    | HomeVisitActivityTitle HomeVisitActivity
    | HowManyDoses
    | HaveAnyOfTheFollowingQuestion
    | HttpError Http.Error
    | HomeVisit
    | HoursSinglePlural Int
    | HowManyPerWeek
    | Hygiene
    | Hypertension
    | HypertensionAndPregnantHeader
    | HypertensionBeforePregnancy
    | HypertensionRecommendedTreatmentHeader Bool
    | HypertensionRecommendedTreatmentHelper
    | HypertensionRecommendedTreatmentUpdateHeader Bool
    | HypertensionRecommendedTreatmentUpdateBPLabel
    | HypertensionRecommendedTreatmentUpdateCurrentTreatment
    | HypertensionRecommendedTreatmentUpdateNewTreatment HypertensionTreatementUpdateOption
    | HypertensionRecommendedTreatmentUpdateNoCurrentTreatment
    | HypertensionRecommendedTreatmentUpdateStartTreatment
    | HypertensionStageAndRenalComplicationsHeader Bool NCDDiagnosis
    | IdentityPopupHeader
    | IdentityPopupInstructions
    | IdleWaitingForSync
    | Ignore
    | IllnessSymptom IllnessSymptom
    | Immunisation
    | ImmunizationHistory
    | IncompleteCervixPreviousPregnancy
    | IndexPatient
    | IndividualEncounter
    | IndividualEncounterFirstVisit IndividualEncounterType
    | IndividualEncounterLabel IndividualEncounterType Bool
    | IndividualEncounterSelectVisit IndividualEncounterType Bool
    | IndividualEncounterSubsequentVisit IndividualEncounterType
    | IndividualEncounterType IndividualEncounterType Bool
    | InfrastructureEnvironment
    | InfrastructureEnvironmentWash
    | InitialResultsDisplay InitialResultsDisplay
    | IntractableVomiting Bool
    | IntractableVomitingQuestion
    | InstructionsChooseOneMedication
    | InstructionsChooseTwoMedications
    | IsCurrentlyBreastfeeding
    | IsolatedAtHome
    | IsThisYouQuestion
    | Issued
    | IssuedTo
    | KilogramShorthand
    | KnownAsPositiveQuestion LaboratoryTask
    | KnownPositive
    | KnownPositiveHepatitisB
    | KnownPositiveHIV
    | LabelOnePregnancyEpisodeOpen
    | LabelSeenHealthcareProviderForPregnancy
    | LabelDocumentPregnancyOutcome
    | Lab
    | LabHistory
    | LaboratoryBloodGroupLabel
    | LaboratoryBloodGroupTestResult
    | LaboratoryBloodGroup BloodGroup
    | LaboratoryRhesusLabel
    | LaboratoryRhesusTestResult
    | LaboratoryRhesus Rhesus
    | LaboratoryProteinLabel
    | LaboratoryProteinTestResult
    | LaboratoryProteinValue ProteinValue
    | LaboratoryPHLabel
    | LaboratoryPHTestResult
    | LaboratoryPHValue PHValue
    | LaboratoryGlucoseLabel
    | LaboratoryGlucoseTestResult
    | LaboratoryGlucoseValue GlucoseValue
    | LaboratoryLeukocytesLabel
    | LaboratoryLeukocytesTestResult
    | LaboratoryLeukocytesValue LeukocytesValue
    | LaboratoryNitriteLabel
    | LaboratoryNitriteTestResult
    | LaboratoryNitriteValue NitriteValue
    | LaboratoryUrobilinogenLabel
    | LaboratoryUrobilinogenTestResult
    | LaboratoryUrobilinogenValue UrobilinogenValue
    | LaboratoryHaemoglobinLabel
    | LaboratoryHaemoglobinTestResult
    | LaboratoryHaemoglobinValue HaemoglobinValue
    | LaboratoryKetoneLabel
    | LaboratoryKetoneTestResult
    | LaboratoryKetoneValue KetoneValue
    | LaboratoryBilirubinLabel
    | LaboratoryBilirubinTestResult
    | LaboratoryBilirubinValue BilirubinValue
    | LaboratoryHemoglobinTestResult
    | LaboratoryRandomBloodSugarTestResult
    | LaboratoryHIVPCRTestResult
    | LaboratoryHIVPCRViralLoadStatusQuestion
    | LaboratoryCreatinineLabel
    | LaboratoryBUNLabel
    | LaboratoryALTLabel
    | LaboratoryASTLabel
    | LaboratoryPregnancyLabel
    | LaboratoryTest LaboratoryTest
    | LabsEntryState LabsEntryState
    | LabsHistoryCompletedQuestion
    | LaboratoryCreatinineCreatinineResult
    | LaboratoryCreatinineBUNResult
    | LaboratoryLipidPanelUnitOfMeasurementQuestion
    | LaboratoryLipidPanelTotalCholesterolLabel
    | LaboratoryLipidPanelLDLCholesterolLabel
    | LaboratoryLipidPanelHDLCholesterolLabel
    | LaboratoryLipidPanelTriglyceridesLabel
    | LaboratoryLiverFunctionAltResult
    | LaboratoryLiverFunctionAstResult
    | LaboratoryTask LaboratoryTask
    | LaboratoryTaskLabel LaboratoryTask
    | LaboratoryTaskDate LaboratoryTask
    | LaboratoryTaskResult LaboratoryTask
    | LaboratoryTaskResultsHelper
    | LabResults
    | LabResultsHistoryModeLabel LabResultsHistoryMode
    | LabResultsNormalRange LabResultsHistoryMode
    | LabResultsPaneHeader LabResultsCurrentMode
    | LastChecked
    | LastContacted
    | LastSuccesfulContactLabel
    | LeaveEncounter
    | Left
    | LegCrampsReliefMethod LegCrampsReliefMethod
    | LegLeft
    | LegRight
    | Legs
    | LegsCPESign LegsCPESign
    | LevelOfEducationLabel
    | LevelOfEducation EducationLevel
    | LevelOfEducationForResilience EducationLevel
    | LipidPanel
    | LiveChildren
    | LmpDateConfirmationLabel
    | LmpDateConfirmationQuestion
    | LmpDateConfidentHeader
    | LmpDateNotConfidentQuestion
    | LmpDateNotConfidentReason LmpDateNotConfidentReason
    | LmpDateNotConfidentReasonforReport LmpDateNotConfidentReason
    | LmpDateHeader
    | LmpLabel
    | LmpRangeHeader
    | LmpRange Pages.Prenatal.Activity.Types.LmpRange
    | LoggedInAsPhrase
    | Location
    | LoginPhrase LoginPhrase
    | Low
    | LowRiskCase
    | Lungs
    | LungsCPESign LungsCPESign
    | MainIncomeSource MainIncomeSource
    | MainIncomeSourceQuestion
    | MainMenuActivity MainMenuActivity
    | MainWaterSource MainWaterSource
    | MainWaterPreparationOption WaterPreparationOption
    | MainWaterSourceQuestion
    | MainWaterPreparationQuestion
    | MalariaRapidDiagnosticTest
    | MalariaRecommendedTreatmentHeader
    | MalariaRecommendedTreatmentHelper
    | MalariaWithGIComplications
    | RapidTestResult RapidTestResult
    | MalnutritionWithComplications
    | MaritalStatusLabel
    | MaritalStatus MaritalStatus
    | MastitisRecommendedTreatmentHeader Bool
    | MastitisRecommendedTreatmentHelper
    | MeasurementNotTaken
    | MedicationCausingHypertension MedicationCausingHypertension
    | MedicationCausingHypertensionQuestion
    | MedicalCondition MedicalCondition
    | MedicalConditionQuestion
    | MedicationDistribution
    | MedicationTreatingDiabetes MedicationTreatingDiabetes
    | MedicationTreatingDiabetesQuestion
    | MedicationTreatingHypertension MedicationTreatingHypertension
    | MedicationTreatingHypertensionQuestion
    | MedicalDiagnosis
    | MedicalDiagnosisAlert MedicalDiagnosis
    | MedicationCausesSideEffectsQuestion
    | MedicationDistributionHelperAnemia
    | MedicationDistributionHelperDiscordantPartnership
    | MedicationDistributionHelperDiscordantPartnershipNoARVs
    | MedicationDistributionHelperEarlyMastitisOrEngorgment
    | MedicationDistributionHelperHIV
    | MedicationDistributionHelperMebendazole
    | MedicationDistributionHelperGonorrhea
    | MedicationDistributionHelperTrichomonasOrBacterialVaginosis
    | MedicationDistributionHelperVitaminA
    | MedicationDistributionNoticeGonorrhea
    | MedicationDistributionNoticeGonorrheaPartnerMed1
    | MedicationDistributionNoticeGonorrheaPartnerMed2
    | MedicationDistributionSign MedicationDistributionSign
    | MedicationDoxycycline
    | MedicationDosesMissedQuestion
    | MedicationForFeverPast6Hours
    | MedicationHelpedEnding Bool
    | MedicationFeelBetterAfterTakingQuestion
    | MedicationForMalariaToday
    | MedicationForMalariaPastMonth
    | MedicalFormHelper
    | MedicationForFeverPast6HoursQuestion
    | MedicationForMalariaTodayQuestion
    | MedicationForMalariaWithinPastMonthQuestion
    | MedicationHelpedQuestion
    | MedicationTaken
    | MedicationTakenAsPrescribedQuestion
    | MentalHealthHistory
    | MemoryQuota { totalJSHeapSize : Int, usedJSHeapSize : Int, jsHeapSizeLimit : Int }
    | MessagingTab MessagingTab
    | MMHGUnit
    | Minutes Int
    | MinutesAgo Int
    | MissedDosesOfMedicatgion Int
    | ModeOfDelivery ModeOfDelivery
    | ModeOfDeliveryLabel
    | ModeratePreeclampsia
    | Month
    | MonthAbbrev
    | MonthlySurveyScoreInterpretation Int
    | MonthSinglePlural Int
    | MonthsOfStock
    | MotherId
    | MotherName String
    | MotherNameLabel
    | MTDIn
    | MTDOut
    | MUAC
    | MyRelatedBy MyRelatedBy
    | MyRelatedByQuestion MyRelatedBy
    | Name
    | NationalIdNumber
    | NCDAANCVisitsCounseling
    | NCDABirthweightQuestion
    | NCDADiarrheaPopupMessage
    | NCDAMealFrequency6to9
    | NCDAMealFrequency9to12
    | NCDAMealFrequency12to24
    | NCDASignCounseling NCDASign
    | NCDASignHelperHeader NCDASign
    | NCDASignQuestion NCDASign
    | NCDAUpdateVaccineRecordMessage
    | NCDActivityTitle NCDActivity
    | NCDANCServicesInstructions
    | NCDAANCNewbornItemLabel NCDAANCNewbornItem
    | NCDAInfrastructureEnvironmentWashItemLabel NCDAInfrastructureEnvironmentWashItem
    | NCDANutritionBehaviorItemLabel NCDANutritionBehaviorItem
    | NCDATargetedInterventionsItemLabel NCDATargetedInterventionsItem
    | NCDAUniversalInterventionsItemLabel NCDAUniversalInterventionsItem
    | NCDAFillTheBlanksItemLabel NCDAFillTheBlanksItem
    | NCDANoANVCVisitsOnRecord
    | NCDANumberImmunizationAppointmentLabel (Maybe NominalDate)
    | NCDAStep NCDAStep
    | NCDDangerSign NCDDangerSign
    | NCDDiagnosisForProgressReport Bool Bool NCDDiagnosis
    | NCDExaminationTask Pages.NCD.Activity.Types.ExaminationTask
    | NCDFamilyHistorySignQuestion NCDFamilyHistorySign
    | NCDGuidanceSignQuestion NCDGuidanceSign
    | NCDHealthEducationHeader
    | NCDHealthEducationInstructions
    | NCDHealthEducationQuestion
    | NCDLabsCaseManagementEntryTypeResults
    | NCDMedicalHistoryTask Pages.NCD.Activity.Types.MedicalHistoryTask
    | NCDNextStepsTask Pages.NCD.Activity.Types.NextStepsTask
    | NCDGroup1Symptom NCDGroup1Symptom
    | NCDGroup2Symptom NCDGroup2Symptom
    | NCDPainSymptom NCDPainSymptom
    | NCDProgressReport
    | NCDRecurrentActivitiesTitle NCDRecurrentActivity
    | NCDRecurrentNextStepsTask Pages.NCD.RecurrentActivity.Types.NextStepsTask
    | NCDRiskFactor NCDRiskFactor
    | NCDSocialHistoryFoodQuestion
    | NCDSocialHistoryFoodQuestionInstructions
    | NCDSocialHistorySignQuestion Site NCDSocialHistorySign
    | Neck
    | NeckCPESign NeckCPESign
    | NegativeLabel
    | Never
    | NextAppointment
    | NextDue
    | NextImmunisationVisit Bool
    | NextPediatricVisit Bool
    | NextSteps
    | NextStepsTask Bool Pages.AcuteIllness.Activity.Types.NextStepsTask
    | No
    | NoActivitiesCompleted
    | NoActivitiesCompletedForThisParticipant
    | NoActivitiesPending
    | NoActivitiesPendingForThisParticipant
    | NoContactReason NoContactReason
    | NoMatchesFound
    | NormalRange
    | NotApplicable
    | NoTreatmentAdministered
    | NoTreatmentRecorded
    | NutritionSigns
    | ReasonForNonReferral ReasonForNonReferral
    | AdministrationNote AdministrationNote
    | AdministrationNoteForPrenatalImmunisation AdministrationNote
    | AdministrationNoteForWellChildImmunisation AdministrationNote
    | NoParticipantsPending
    | NoParticipantsPendingForThisActivity
    | NoParticipantsCompleted
    | NoParticipantsCompletedForThisActivity
    | NoReferralRecorded
    | Normal
    | NoChildrenRegisteredInTheSystem
    | NotAvailable
    | NotFollowingRecommendationQuestion
    | NotTaken
    | NumberOfAbortions
    | NumberOfChildrenUnder5
    | NumberOfCSections
    | NumberOfLiveChildren
    | NumberOfStillbirthsAtTerm
    | NumberOfStillbirthsPreTerm
    | NutritionActivityHelper NutritionActivity
    | NutritionActivityTitle NutritionActivity
    | NutritionAssessment NutritionAssessment
    | NutritionAssessmentLabel
    | NutritionAssessmentTask NutritionAssessmentTask
    | NutritionBehavior
    | NutritionCaringOption CaringOption
    | NutritionFeedingSignQuestion NutritionFeedingSign
    | NutritionFoodSecuritySignQuestion NutritionFoodSecuritySign
    | NutritionHelper
    | NutritionHygieneSignQuestion NutritionHygieneSign
    | NutritionNextStepsTask Measurement.Model.NextStepsTask
    | NitritionSigns
    | NutritionSupplementType NutritionSupplementType
    | Observations
    | ObstetricalDiagnosis
    | ObstetricalDiagnosisAlert ObstetricalDiagnosis
    | OK
    | On
    | OneVisit
    | OnceYouEndTheEncounter
    | OnceYouEndYourGroupEncounter
    | OngoingTreatmentTask OngoingTreatmentTask
    | OnlySickChild
    | Or
    | OutsideCareLabel
    | PackagesPerMonth
    | Page
    | PageNotFoundMsg
    | PaleConjuctiva
    | Pallor
    | Para
    | ParacetamolPrescriptionForAdult
    | ParentsAliveAndHealthyQuestion
    | PartialPlacentaPreviousDelivery
    | Participants
    | ParticipantReviewed
    | ParticipantSignature
    | ParticipantDemographicInformation
    | PartnerReceivedHivCounseling
    | PastDiagnosisReportReason
    | PatientDiagnosedWithLabel
    | PatientExhibitAnyFindings
    | PatientExhibitAnyRespiratoryFindings
    | PatientGotAnyDangerSigns
    | PatientGotAnySymptoms
    | PatientGotPainAnywhewre
    | PatientGotDiabetesByGlucoseHeader Bool Float
    | PatientGotDiabetesByUrineDip String
    | PatientGotDiabetesHeader
    | PatientProgress
    | PatientRecord
    | PatientInformation
    | PatientIsolatedQuestion Bool
    | PatientNotYetSeenAtHCLabel
    | PatientRecordFilter PatientRecordFilter
    | PauseEncounter
    | PatientShowsNoSignsOfCovid
    | Patients
    | PediatricCareMilestone PediatricCareMilestone
    | PediatricVisit
    | People
    | Percentage
    | PersistentStorage Bool
    | Person
    | PersonHasBeenSaved
    | PertinentSymptoms
    | PhotosTransferStatus
    | PhysicalExam
    | PhysicalExamTask PhysicalExamTask
    | PlaceholderEnterHeight
    | PlaceholderEnterMUAC
    | PlaceholderEnterParticipantName
    | PlaceholderEnterWeight
    | PlaceholderSearchContactName
    | PleaseCall
    | PleaseContact
    | PleaseSync
    | PointOfCare
    | PostpartumEncounter
    | PostpartumHealingProblem PostpartumHealingProblem
    | PostpartumHealingProblemQuestion
    | PostpartumChildDangerSign PostpartumChildDangerSign
    | PostpartumMotherDangerSign PostpartumMotherDangerSign
    | Predecessor Predecessor
    | PreeclampsiaPreviousPregnancy
    | Pregnancy
    | PregnancyConclusion
    | PregnancyStart
    | PregnancySummarySignQuestion PregnancySummarySign
    | PregnancyTestResult PregnancyTestResult
    | PregnancyTrimester PregnancyTrimester
    | PregnancyUrineTest
    | PrenatalActivityTitle PrenatalActivity
    | PrenatalRecurrentActivitiesTitle PrenatalRecurrentActivity
    | PrenatalAssesment PrenatalAssesment
    | PrenatalDiagnosis PrenatalDiagnosis
    | PrenatalDiagnosisForProgressReport PrenatalDiagnosis
    | PrenatalDiagnosisNonUrgentMessage PrenatalDiagnosis
    | PrenatalEncounterType PrenatalEncounterType
    | PrenatalFlankPainSign PrenatalFlankPainSign
    | PrenatalHealthEducationAppropriateProvided
    | PrenatalHealthEducationSignsDiagnosis Bool String PrenatalHealthEducationSign
    | PrenatalHealthEducationLabel PrenatalHealthEducationSign
    | PrenatalHealthEducationQuestion Bool PrenatalHealthEducationSign
    | PrenatalHealthEducationDiabetesInform
    | PrenatalHealthEducationHivDetectableViralLoadInform
    | PrenatalHealthEducationNauseaAndVomitingAdvise
    | PrenatalHealthEducationNauseaAndVomitingInform
    | PrenatalHealthEducationLegCrampsInform
    | PrenatalHealthEducationLowBackPainInform
    | PrenatalHealthEducationConstipationInform
    | PrenatalHealthEducationHeartburnInform
    | PrenatalHealthEducationVaricoseVeinsInform
    | PrenatalHealthEducationLegPainRednessInform
    | PrenatalHealthEducationPelvicPainInform
    | PrenatalHealthEducationSaferSexInform
    | PrenatalHealthEducationEarlyMastitisOrEngorgmentInform
    | PrenatalHealthEducationMentalHealthInform
    | PrenatalARVProgramInstructions Bool
    | PrenatalHIVSignQuestion PrenatalHIVSign
    | PrenatalImmunisationTask Pages.Prenatal.Activity.Types.ImmunisationTask
    | PrenatalImmunisationDescription PrenatalVaccineType
    | PrenatalImmunisationHeader PrenatalVaccineType
    | PrenatalImmunizationHistory PrenatalVaccineType
    | PrenatalLabsCaseManagementEntryTypeResults
    | PrenatalLabsCaseManagementEntryTypeVitals
    | PrenatalMentalHealthQuestion PrenatalMentalHealthQuestion
    | PrenatalMentalHealthOptionForQuestion PrenatalMentalHealthQuestion PrenatalMentalHealthQuestionOption
    | PrenatalMentalHealthSpecialistHelper
    | PrenatalMentalHealthSpecialistQuestion
    | PrenatalMentalHealthWarningPopupMessage
    | PrenatalMentalHealthWarningPopupInstructions
    | PrenatalNCDProgramHeaderPrefix
    | PrenatalNCDProgramHeaderSuffix
    | PrenatalNCDProgramInstructions
    | PrenatalNextStepsTask Bool Pages.Prenatal.Activity.Types.NextStepsTask
    | OutsideCareSignQuestion OutsideCareSign
    | OutsideCareMedicationLabel OutsideCareMedication
    | OutsideCareMedicationDosage OutsideCareMedication
    | PrenatalPhotoHelper
    | PrenatalRecurrentNextStepsTask Pages.Prenatal.RecurrentActivity.Types.NextStepsTask
    | PrenatalSymptom PrenatalSymptom
    | PrenatalSymptomQuestion PrenatalSymptomQuestion
    | PrenatalSymptomQuestionsHeader
    | TestExecutionNote TestExecutionNote
    | TestResult TestResult
    | PrenatalUltrasoundHeader
    | PrenatalUltrasoundInstructions
    | PrenatalVaccineLabel PrenatalVaccineType
    | PreTerm
    | PregnancyConcludedLabel
    | PregnancyOutcomeLabel
    | PregnancyOutcome PregnancyOutcome
    | PreviousCSectionScar
    | PreviousDelivery
    | PreviousDeliveryPeriods PreviousDeliveryPeriod
    | PreviousFloatMeasurement Float
    | PreviousMeasurementNotFound
    | PriorTreatmentTask PriorTreatmentTask
    | Programs
    | ProgressPhotos
    | ProgressReport
    | ProgressReports
    | ProgressTimeline
    | ProgressTrends
    | ProvideHealthEducationAndInstructToIsolate
    | PreTermPregnancy
    | PriorDiagnosis
    | ProvideHealthEducation
    | ProvideHealthEducationShort
    | ProvidedHealthEducationAction
    | ProvidedPreventionEducationQuestion
    | ProvidedPreventionEducationQuestionShort
    | ProvidedSymtomReliefGuidanceQuestion
    | Province
    | RandomBloodSugarResultNormalRange RandomBloodSugarResult
    | Read
    | ReadToggle Bool
    | ReasonForNotBreastfeeding BreastfeedingSign
    | ReasonForNotIsolating ReasonForNotIsolating
    | ReasonForNotTaking ReasonForNotTaking
    | ReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | Received
    | ReceivedFolicAcid
    | ReceivedFrom
    | ReceivedIronFolicAcid
    | ReceivedMebendazole
    | ReceivedMosquitoNet
    | ReceivedVitaminA
    | ReceiveOption ReceiveOption
    | Recommendation114 Recommendation114
    | RecommendationSite RecommendationSite
    | Recommended
    | RecommendedButNotGivenDueTo
    | RecommendedSymptomRelief
    | RecommendedTreatmentSignDosage RecommendedTreatmentSign
    | RecommendedTreatmentSignLabel RecommendedTreatmentSign
    | RecommendedTreatmentSignLabelForProgressReport RecommendedTreatmentSign
    | RecordAcuteIllnessOutcome
    | RecordPregnancyOutcome
    | RectalHemorrhoids
    | RecurringHighSeverityAlert RecurringHighSeverityAlert
    | ReferredPatientToFacilityQuestion ReferralFacility
    | ReferredToFacility ReferralFacility
    | ReferredToFacilityNot ReferralFacility
    | ReferredToFacilityPostpartum ReferralFacility
    | ReferToHospitalForFurtherEvaluation
    | ReferToHospitalForTesting
    | ReferToProgramAction
    | ReferToProgramQuestion
    | RegisterContactHelper
    | RegisterParticipantHelper
    | RegisterNewContact
    | RegisterNewParticipant
    | RegistratingHealthCenter
    | RemainingForDownloadLabel
    | RemainingForUploadLabel
    | RemindMe
    | RemindMePhrase
    | RenalDisease
    | ReportAge String
    | ReportComponentAntenatal ReportComponentAntenatal
    | ReportComponentNCD ReportComponentNCD
    | ReportComponentWellChild ReportComponentWellChild
    | ReportDOB String
    | ReportResultsOfContactsSearch Int
    | ReportResultsOfParticipantsSearch Int
    | ReportTab ReportTab
    | Reports
    | ReportCompleted { pending : Int, completed : Int }
    | ResilienceCategory ResilienceCategory
    | ResilienceMessage
    | ResilienceMessageIntroduction1Title
    | ResilienceMessageIntroduction1Paragraph1 String
    | ResilienceMessageIntroduction1Paragraph2
    | ResilienceMessageIntroduction1Paragraph3
    | ResilienceMessageIntroduction2Title
    | ResilienceMessageIntroduction2Paragraph1
    | ResilienceMessageIntroduction2Paragraph2
    | ResilienceMessageIntroduction2Bullet1
    | ResilienceMessageIntroduction2Bullet2
    | ResilienceMessageIntroduction2Bullet3
    | ResilienceMessageIntroduction2Bullet4
    | ResilienceMessageIntroduction2Bullet5
    | ResilienceMessageIntroduction3Title
    | ResilienceMessageIntroduction3Paragraph1
    | ResilienceMessageIntroduction3Paragraph2
    | ResilienceMessageIntroduction3Paragraph3
    | ResilienceMessageIntroduction4Title
    | ResilienceMessageIntroduction4Paragraph1
    | ResilienceMessageIntroduction4Paragraph2
    | ResilienceMessageIntroduction4Bullet1
    | ResilienceMessageIntroduction4Bullet2
    | ResilienceMessageIntroduction4Bullet3
    | ResilienceMessageIntroduction4Paragraph3
    | ResilienceMessageIntroduction5Title
    | ResilienceMessageIntroduction5Paragraph1
    | ResilienceMessageIntroduction6Title
    | ResilienceMessageIntroduction6Paragraph1
    | ResilienceMessageIntroduction7Title
    | ResilienceMessageIntroduction7Paragraph1
    | ResilienceMessageIntroduction7Paragraph2
    | ResilienceMessageIntroduction8Title
    | ResilienceMessageIntroduction8Paragraph1
    | ResilienceMessageIntroduction8Paragraph2
    | ResilienceMessageIntroduction8Paragraph3
    | ResilienceMessageGrowth1Title
    | ResilienceMessageGrowth1Paragraph1
    | ResilienceMessageGrowth1Paragraph2
    | ResilienceMessageGrowth2Title
    | ResilienceMessageGrowth2Paragraph1
    | ResilienceMessageGrowth2Paragraph2
    | ResilienceMessageGrowth2Paragraph3
    | ResilienceMessageGrowth3Title
    | ResilienceMessageGrowth3Paragraph1
    | ResilienceMessageGrowth3Paragraph2
    | ResilienceMessageGrowth4Title
    | ResilienceMessageGrowth4Paragraph1
    | ResilienceMessageGrowth4Paragraph2
    | ResilienceMessageStressManagement1Title
    | ResilienceMessageStressManagement1Paragraph1
    | ResilienceMessageStressManagement1Paragraph2
    | ResilienceMessageStressManagement1Paragraph3
    | ResilienceMessageStressManagement2Title
    | ResilienceMessageStressManagement2Paragraph1
    | ResilienceMessageStressManagement2Paragraph2
    | ResilienceMessageStressManagement2Paragraph3
    | ResilienceMessageStressManagement2Paragraph4
    | ResilienceMessageStressManagement2Bullet1
    | ResilienceMessageStressManagement2Bullet2
    | ResilienceMessageStressManagement2Bullet3
    | ResilienceMessageStressManagement3Title
    | ResilienceMessageStressManagement3Paragraph1
    | ResilienceMessageStressManagement3Paragraph2
    | ResilienceMessageStressManagement4Title
    | ResilienceMessageStressManagement4Paragraph1
    | ResilienceMessageStressManagement4Paragraph2
    | ResilienceMessageStressManagement4Paragraph3
    | ResilienceMessageStressManagement5Title String
    | ResilienceMessageStressManagement5Paragraph1
    | ResilienceMessageStressManagement5Paragraph2
    | ResilienceMessageStressManagement5Paragraph3
    | ResilienceMessageStressManagement6Title
    | ResilienceMessageStressManagement6Paragraph1
    | ResilienceMessageStressManagement6Paragraph2
    | ResilienceMessageStressManagement7Title
    | ResilienceMessageStressManagement7Paragraph1
    | ResilienceMessageStressManagement7Paragraph2
    | ResilienceMessageStressManagement7Bullet1
    | ResilienceMessageStressManagement7Bullet2
    | ResilienceMessageStressManagement7Bullet3
    | ResilienceMessageMindfulness1Title
    | ResilienceMessageMindfulness1Paragraph1
    | ResilienceMessageMindfulness1Paragraph2
    | ResilienceMessageMindfulness2Title
    | ResilienceMessageMindfulness2Paragraph1
    | ResilienceMessageMindfulness2Paragraph2
    | ResilienceMessageMindfulness2Paragraph3
    | ResilienceMessageMindfulness3Title
    | ResilienceMessageMindfulness3Paragraph1
    | ResilienceMessageMindfulness3Paragraph2
    | ResilienceMessageMindfulness3Paragraph3
    | ResilienceMessageMindfulness3Paragraph4
    | ResilienceMessageMindfulness4Title
    | ResilienceMessageMindfulness4Paragraph1
    | ResilienceMessageMindfulness4Paragraph2
    | ResilienceMessageMindfulness4Paragraph3
    | ResilienceMessageMindfulness5Title
    | ResilienceMessageMindfulness5Paragraph1
    | ResilienceMessageMindfulness5Paragraph2
    | ResilienceMessageMindfulness5Paragraph3
    | ResilienceMessageMindfulness5Bullet1
    | ResilienceMessageMindfulness5Bullet2
    | ResilienceMessageMindfulness6Title
    | ResilienceMessageMindfulness6Paragraph1
    | ResilienceMessageMindfulness6Paragraph2
    | ResilienceMessageMindfulness6Paragraph3
    | ResilienceMessageMindfulness6Paragraph4
    | ResilienceMessageMindfulness6Bullet1
    | ResilienceMessageMindfulness6Bullet2
    | ResilienceMessageMindfulness6Bullet3
    | ResilienceMessageConnecting1Title
    | ResilienceMessageConnecting1Paragraph1
    | ResilienceMessageConnecting1Paragraph2
    | ResilienceMessageConnecting1Paragraph3
    | ResilienceMessageConnecting2Title
    | ResilienceMessageConnecting2Paragraph1
    | ResilienceMessageConnecting2Paragraph2
    | ResilienceMessageConnecting2Paragraph3
    | ResilienceMessageConnecting3Title
    | ResilienceMessageConnecting3Paragraph1
    | ResilienceMessageConnecting3Paragraph2
    | ResilienceMessageConnecting3Paragraph3
    | ResilienceMessageConnecting4Title
    | ResilienceMessageConnecting4Paragraph1
    | ResilienceMessageConnecting4Paragraph2
    | ResilienceMessageConnecting4Paragraph3
    | ResilienceMessageConnecting5Title
    | ResilienceMessageConnecting5Paragraph1
    | ResilienceMessageConnecting5Paragraph2
    | ResilienceMessageConnecting6Title
    | ResilienceMessageConnecting6Paragraph1
    | ResilienceMessageConnecting6Paragraph2
    | ResilienceMessageConnecting6Paragraph3
    | ResilienceMessageSelfCare1Title
    | ResilienceMessageSelfCare1Paragraph1
    | ResilienceMessageSelfCare1Paragraph2
    | ResilienceMessageSelfCare1Paragraph3
    | ResilienceMessageSelfCare1Paragraph4
    | ResilienceMessageSelfCare1Bullet1
    | ResilienceMessageSelfCare1Bullet2
    | ResilienceMessageSelfCare1Bullet3
    | ResilienceMessageSelfCare2Title
    | ResilienceMessageSelfCare2Paragraph1
    | ResilienceMessageSelfCare2Paragraph2
    | ResilienceMessageSelfCare2Paragraph3
    | ResilienceMessageSelfCare3Title
    | ResilienceMessageSelfCare3Paragraph1
    | ResilienceMessageSelfCare3Paragraph2
    | ResilienceMessageSelfCare3Paragraph3
    | ResilienceMessageEndOfFirstMonthTitle
    | ResilienceMessageEndOfFirstMonthParagraph1
    | ResilienceMessageEndOfFirstMonthParagraph2
    | ResilienceMessageEndOfFirstMonthParagraph3
    | ResilienceMessageEndOfFirstMonthBullet1
    | ResilienceMessageEndOfFirstMonthBullet2
    | ResilienceMessageEndOfFirstMonthBullet3
    | ResilienceMessageEndOfFirstMonthBullet4
    | ResilienceMessageEndOfSecondMonthTitle
    | ResilienceMessageEndOfSecondMonthParagraph1
    | ResilienceMessageEndOfSecondMonthParagraph2
    | ResilienceMessageEndOfSecondMonthParagraph3
    | ResilienceMessageEndOfSecondMonthBullet1
    | ResilienceMessageEndOfSecondMonthBullet2
    | ResilienceMessageEndOfSecondMonthBullet3
    | ResilienceMessageEndOfSecondMonthBullet4
    | ResilienceMessageEndOfSecondMonthBullet5
    | ResilienceMonthlySurveyQuestion ResilienceSurveyQuestion
    | ResilienceKickOffBirthDateQuestion
    | ResilienceKickOffEducationLevelQuestion
    | ResilienceKickOffGenderQuestion
    | ResilienceKickOffMaritalStatusQuestion
    | ResilienceKickOffRoleQuestion
    | ResilienceKickOffUbudeheQuestion
    | ResilienceRole ResilienceRole
    | ResilienceNotificationHeader String
    | ResilienceNotificationNumberOfUnread Int
    | ResilienceNotificationReadNowQuestion
    | ResilienceReminderHeader String ResilienceReminderType
    | ResilienceReminderParagraph1 ResilienceReminderType
    | ResilienceReminderParagraph2 ResilienceReminderType
    | ResilienceReminderFooter ResilienceReminderType
    | ResilienceSurveyQuestionOption ResilienceSurveyQuestionOption
    | ResolveMonth Bool Month
    | ResolveMonthYY Month Int Bool
    | RespiratoryDistress
    | RespiratoryRate
    | ResponsePeriod ResponsePeriod
    | Result
    | ResultOfContacting114 Recommendation114
    | ResultOfContactingRecommendedSite RecommendationSite
    | ResultsMissing
    | ResultsPending
    | ReviewCaseWith144Respondent
    | Reviewed
    | ReviewPriorDiagnosis
    | RhNegative
    | Right
    | RiskFactorAlert RiskFactor
    | RiskFactors
    | SachetsPerDayHelper Float Float
    | SachetsPerDayQuestion
    | Save
    | SaveAndNext
    | SaveAndRecordOutcome
    | SavedMoneyQuestion
    | SaveError
    | ScheduleFollowUp
    | SearchEhezaForExistingParticipants
    | SearchExistingParticipants
    | SearchHelper
    | SearchHelperFamilyMember
    | SecondName
    | Sector
    | SeeDosageScheduleByWeight
    | SeeLabResults
    | SeeMore
    | SelectAllDiagnoses
    | SelectAllSigns
    | SelectDangerSigns
    | SelectDate
    | SelectedFamilyPlanningMethod
    | SelectIllnessSymptoms
    | SelectPostpartumChildDangerSigns
    | SelectPostpartumMotherDangerSigns
    | SelectedProgram
    | SelectedVillage
    | SelectEncounterType
    | SelectExistingAcuteIllness
    | SelectExistingAcuteIllnessToRecordOutcome
    | SelectProgram
    | SelectYourGroup
    | SelectYourHealthCenter
    | SelectYourVillage
    | SelectedHCDownloading
    | SelectedHCNotSynced
    | SelectedHCSyncing
    | Send
    | ReportToWhatsApp
    | ReportToWhatsAppComponentsSelectionHeader Components.ReportToWhatsAppDialog.Model.ReportType
    | ReportToWhatsAppConfirmationBeforeExecutingHeader
    | ReportToWhatsAppConfirmationBeforeExecutingInstructions
    | ReportToWhatsAppConfirmationBeforeExecutingQuestion
    | ReportToWhatsAppConsentQuestion
    | ReportToWhatsAppExecutionResultFailure
    | ReportToWhatsAppExecutionResultSomethingWentWrong
    | ReportToWhatsAppExecutionResultSuccess
    | ReportToWhatsAppNoticeOfNonRespobsibility
    | ReportToWhatsAppPhoneInputHeader
    | ReportToWhatsAppPhoneVerificationHeader
    | ReportToWhatsAppPhoneVerificationQuestion
    | ReportToWhatsAppPhoneUpdateAtProfileQuestionPrefix
    | ReportToWhatsAppPhoneUpdateAtProfileQuestionSuffix
    | ReportToWhatsAppPhoneUpdateConfirmationMessasge
    | ServiceWorkerActive
    | ServiceWorkerCurrent
    | ServiceWorkerCheckForUpdates
    | ServiceWorkerInstalling
    | ServiceWorkerInstalled
    | ServiceWorkerSkipWaiting
    | ServiceWorkerRestarting
    | ServiceWorkerActivating
    | ServiceWorkerActivated
    | ServiceWorkerRedundant
    | ServiceWorkerInactive
    | ServiceWorkerRegNotAsked
    | ServiceWorkerRegLoading
    | ServiceWorkerRegErr
    | ServiceWorkerRegSuccess
    | ServiceWorkerStatus
    | SevereAcuteMalnutrition
    | SevereHemorrhagingPreviousDelivery
    | Shared
    | Signature
    | SignOnDoorPostedQuestion
    | SpecialityCareHeaderPrefix
    | SpecialityCareHeaderSuffix
    | SpecialityCareSignQuestion SpecialityCareSign
    | StillbornPreviousDelivery
    | StockCorrectionReason StockCorrectionReason
    | StockManagement
    | StockManagementMenu StockManagementMenu
    | StockManagementBatchNumberQuestion
    | StockManagementCorrectionTypeLabel
    | StockManagementCorrectionEntryType CorrectionEntryType
    | StockManagementCorrectionReasonLabel
    | StockManagementDateExpiresQuestion
    | StockManagementEnterSignatureLabel
    | StockManagementQuantityAddedQuestion
    | StockManagementQuantityCorrectionLabel
    | StockManagementSelectDateLabel
    | StockManagementSupplierQuestion
    | StockSupplier StockSupplier
    | StockSupplierAbbrev StockSupplier
    | StuntingLevelLabel
    | StuntingLevel StuntingLevel
    | SubsequentEncounter
    | SubsequentEncounterReferral AcuteIllnessEncounterType
    | SuccessiveAbortions
    | SuccessivePrematureDeliveries
    | SuspectedCovid19CaseAlert
    | SuspectedCovid19CaseAlertHelper
    | SuspectedCovid19CaseIsolate
    | SuspectedCovid19CaseContactHC
    | SuspectedCovid19CasePerformRapidTest
    | SuspectedCovid19CaseReferToHCForTesting
    | SymptomRelief SymptomReliefType
    | Symptoms
    | SymptomsGeneralSign SymptomsGeneralSign
    | SymptomsGISign SymptomsGISign
    | SymptomsGISignAbbrev SymptomsGISign
    | SymptomsRespiratorySign SymptomsRespiratorySign
    | SymptomsTask SymptomsTask
    | SyphilisRecommendedTreatmentHeader
    | SyphilisRecommendedTreatmentHelper
    | SyphilisRecommendedTreatmentInstructions
    | SyphilisRecommendedTreatmentWarning
    | GroupEncounterClosed
    | GroupEncounterClosed2 SessionId
    | GroupEncounterLoading
    | GroupEncounterUnauthorized
    | GroupEncounterUnauthorized2
    | SendPatientToFacility ReferralFacility
    | StartEncounter
    | StrartNewAcuteIllnessHelper
    | StartDate
    | EndDate
    | StartingStock
    | StartSyncing
    | StatusLabel
    | StopSyncing
    | StorageQuota { usage : Int, quota : Int }
    | SubmitPairingCode
    | Success
    | SyncGeneral
    | TabletSinglePlural String
    | TakingMedicationAsPrescribed Bool
    | TasksCompleted Int Int
    | TargetedInterventions
    | TelephoneNumber
    | Term
    | TermPregnancy
    | TestDate
    | TestName
    | TestPerformedQuestion
    | TestPerformedTodayQuestion
    | TestPrerequisiteQuestion TestPrerequisite
    | TestResultQuestion
    | TestResultsQuestion
    | TestVariantUrineDipstickQuestion
    | ThisGroupHasNoMothers
    | Time
    | To
    | ToThePatient
    | TransportationPlanQuestion
    | TraveledToCOVID19CountryQuestion
    | TravelHistory
    | TreatedWith
    | TreatedWithNot
    | Treatment
    | TreatmentDetailsAnemia
    | TreatmentDetailsHIV Bool Bool
    | TreatmentDetailsHypertension Bool RecommendedTreatmentSign
    | TreatmentDetailsMalaria RecommendedTreatmentSign
    | TreatmentDetailsSyphilis RecommendedTreatmentSign
    | TreatmentReviewQuestionAdverseEvents
    | TreatmentReviewQuestionAdverseEventsHospitalization
    | TreatmentReviewQuestionMedicationByPMTCT
    | TreatmentReviewQuestionMissedDoses
    | TreatmentReviewQuestionStillTakingForHIV
    | TreatmentReviewQuestionStillTaking
    | TreatmentReviewTask Bool TreatmentReviewTask
    | TreatmentReviewWarningPopupMessage
    | TreatmentReviewWarningPopupInstructions
    | TrySyncing
    | TuberculosisPast
    | TuberculosisPresent
    | TuberculosisInstructions
    | TuberculosisInstructionsFollowed
    | TuberculosisWarning
    | TwoVisits
    | Type
    | UbudeheLabel
    | UbudeheNumber Ubudehe
    | UndeterminedDiagnoses
    | UndeterminedDiagnosisMessage
    | UnitCopiesPerMM3
    | UnitGramsPerDeciliter
    | UnitInternationalUnitsPerLiter
    | UnitCentimeter
    | UnitMilliGramsPerDeciliter
    | UnitMillimeter
    | UnitMillimolesPerLiter
    | UnitOfMeasurement UnitOfMeasurement
    | UniversalInterventions
    | Update
    | UpdateError
    | Uploading
    | UrineDipstickTestLabel TestVariant
    | UrineDipstickTestVariant TestVariant
    | UrinaryTractInfectionRecommendedTreatmentHeader
    | UrinaryTractInfectionRecommendedTreatmentHelper
    | UrinaryTractInfectionRecommendedTreatmentInstructions
    | UterineMyoma
    | VaccinationStatus VaccinationStatus
    | VaccinationNoDosesAdministered
    | VaccineDoseAdministeredPreviouslyPrenatalQuestion String
    | VaccineDoseAdministeredPreviouslyWellChildQuestion String
    | VaccineDoseAdministeredTodayPrenatalQuestion String
    | VaccineDoseAdministeredTodayWellChildQuestion String
    | VaccineType Site VaccineType
    | VaginalExamination
    | VaginalExamSign VaginalExamSign
    | ValidationErrors
    | Version
    | View
    | ViewProgressReport
    | Village
    | VitaminAWarningPopupMessage
    | WaitForVitalsRecheckHelper
    | WaitForLabsResultsHelper
    | WaitInstructions
    | Warning
    | WasFbfDistirbuted Activity
    | WeekSinglePlural Int
    | Weight
    | WelcomeUser String
    | Wellbeing
    | WellChildActivityTitle WellChildActivity
    | WellChildDangerSignsTask Pages.WellChild.Activity.Types.DangerSignsTask
    | WellChildEncounterPopup WarningPopupType
    | WellChildECDMilestoneForDiagnosisPane PediatricCareMilestone
    | WellChildMacrocephalyWarning
    | WellChildMicrocephalyWarning
    | WellChildImmunisationDescription Site WellChildVaccineType
    | WellChildImmunisationDosage Site WellChildVaccineType
    | WellChildImmunisationHeader WellChildVaccineType
    | WellChildImmunizationHistory Site WellChildVaccineType
    | WellChildHomeVisitTask Pages.WellChild.Activity.Types.HomeVisitTask
    | WellChildImmunisationTask Site Measurement.Model.ImmunisationTask
    | WellChildMedicationTask Pages.WellChild.Activity.Types.MedicationTask
    | WellChildNextStepsTask Bool Pages.WellChild.Activity.Types.NextStepsTask
    | WellChildSymptom WellChildSymptom
    | WellChildVaccineLabel Site WellChildVaccineType
    | WhatDoYouWantToDo
    | WhatType
    | WhatWasTheirResponse
    | WhoCaresForTheChildDuringTheDay
    | WhoInFamilyHasCondition
    | WhyNot
    | WrittenProtocolsFollowed
    | Year
    | YearsOld Int
    | Yes
    | Zone
    | ZScoreHeadCircumferenceForAge
    | ZScoreHeightForAge
    | ZScoreMuacForAge
    | ZScoreWeightForAge
    | ZScoreWeightForHeight


translationSet : TranslationId -> TranslationSet String
translationSet trans =
    case trans of
        Abdomen ->
            { english = "Abdomen"
            , kinyarwanda = Just "Inda"
            , kirundi = Just "Inda"
            }

        AbdomenCPESign option ->
            case option of
                Hepatomegaly ->
                    { english = "Hepatomegaly"
                    , kinyarwanda = Just "Kubyimba umwijima"
                    , kirundi = Just "Ivyimba ry'igitigu"
                    }

                Splenomegaly ->
                    { english = "Splenomegaly"
                    , kinyarwanda = Just "Kubyimba urwangashya"
                    , kirundi = Just "Ingwara y'indugwe"
                    }

                TPRightUpper ->
                    { english = "Tender to Palpation right upper"
                    , kinyarwanda = Just "Igice cyo hejuru iburyo kirababara  iyo ugikanze"
                    , kirundi = Just "Bitera bija mu kudidagizwa mu kuryo amasubiza hejuru"
                    }

                TPRightLower ->
                    { english = "Tender to Palpation right lower"
                    , kinyarwanda = Just "Igice cyo hasi iburyo kirababara  iyo ugikanze"
                    , kirundi = Just "Bitera bija mu kudidagizwa mu kuryo amasubiza hasi"
                    }

                TPLeftUpper ->
                    { english = "Tender to Palpation left upper"
                    , kinyarwanda = Just "Igice cyo hejuru ibumoso kirababara  iyo ugikanze"
                    , kirundi = Just "Bitera bija mu kudidagizwa mu kubamfu amasubiza hejuru"
                    }

                TPLeftLower ->
                    { english = "Tender to Palpation left lower"
                    , kinyarwanda = Just "Igice cyo hasi ibumoso kirababara  iyo ugikanze"
                    , kirundi = Just "Bitera bija mu kudidagizwa mu kubamfu amasubiza hasi"
                    }

                Hernia ->
                    { english = "Hernia"
                    , kinyarwanda = Just "Urugingo ruyobera cg rwinjira mu rundi"
                    , kirundi = Just "Isoho ry'agace k'umubiri w'indani hama kakavyimbira hanze"
                    }

                NormalAbdomen ->
                    translationSet Normal

        Abnormal ->
            { english = "Abnormal"
            , kinyarwanda = Nothing
            , kirundi = Just "Bidasanzwe"
            }

        Abortions ->
            { english = "Abortions"
            , kinyarwanda = Just "Inda yavuyemo"
            , kirundi = Just "Ikugwamwo ry'imbanyi/inda"
            }

        Accept ->
            { english = "Accept"
            , kinyarwanda = Nothing
            , kirundi = Just "Kwemera"
            }

        AccompaniedByPartner ->
            { english = "Was the patient accompanied by partner during the assessment"
            , kinyarwanda = Just "Umubyeyi yaherekejwe n'umugabo we mu gihe yaje kwipimisha"
            , kirundi = Just "Mbega umuvyeyi yaherekejwe n'umufasha wiwe mu gihe co gusuzumwa"
            }

        AccompanyToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Will you accompany the patient to the health center"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku kigonderabuzima"
                    , kirundi = Just "Uzoherekeza umugwayi kw'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Will you accompany the patient to the hospital"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku bitaro"
                    , kirundi = Just "Uzoherekeza umugwayi ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Will you accompany the patient to mental health specialist"
                    , kinyarwanda = Just "Uzaherekeza umurwayi ku muganaga winzobere k'ubuzima bwo mu mutwe"
                    , kirundi = Just "Uzoherekeza umugwayi kuraba umuhinga w'ingwara zo mu mutwe"
                    }

                FacilityARVProgram ->
                    { english = "Will you accompany the patient to ARV services"
                    , kinyarwanda = Just "Uraherekeza umubyei muri erivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Just "Uzoherekeza umugwayi mu gisata kiraba ivy'ama ARVs"
                    }

                FacilityNCDProgram ->
                    { english = "Will you accompany the patient to NCD services"
                    , kinyarwanda = Just "Uzaherekeza umurwayi muri serivisi y'indwara zitandura"
                    , kirundi = Just "Uzoherekeza umugwayi mu gisata kiraba ingwara zitandukira"
                    }

                FacilityANCServices ->
                    { english = "Will you accompany the patient to ANC services"
                    , kinyarwanda = Just "Uzaherekeza umubyeyi muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Just "Uzoherekeza umugwayi mu gisata kiraba ingwara zitandukira"
                    }

                FacilityUltrasound ->
                    { english = "Will you accompany the patient to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Uzoherekeza umugwayi mw'Iradiyo"
                    }

        Actions ->
            { english = "Actions"
            , kinyarwanda = Just "Ibikorwa"
            , kirundi = Just "Ibikorwa"
            }

        ActionsTaken ->
            { english = "Actions Taken"
            , kinyarwanda = Just "Ibyakozwe"
            , kirundi = Just "Ibikorwa vyakozwe"
            }

        ActionsToTake ->
            { english = "Actions To Take"
            , kinyarwanda = Just "Ibigomba gukorwa"
            , kirundi = Just "Ibikorwa vyokorwa"
            }

        AcuteFindingsGeneralSign sign ->
            case sign of
                LethargicOrUnconscious ->
                    { english = "Lethargic Or Unconscious"
                    , kinyarwanda = Just "Yahwereye cyangwa yataye ubwenge"
                    , kirundi = Just "Itiro rirenze canke ukuraba canke uguta ubwenge"
                    }

                AcuteFindingsPoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Just "Ugukwega kutakwiye"
                    }

                SunkenEyes ->
                    { english = "Sunken Eyes"
                    , kinyarwanda = Just "Amaso yahenengeye"
                    , kirundi = Just "Amaso yacokeye"
                    }

                PoorSkinTurgor ->
                    { english = "Poor Skin Turgor"
                    , kinyarwanda = Just "Uruhu rwumye"
                    , kirundi = Just "Uruhu rukene"
                    }

                Jaundice ->
                    { english = "Jaundice"
                    , kinyarwanda = Just "Umuhondo/umubiri wahindutse umuhondo"
                    , kirundi = Just "Ingwara y'umuhondo"
                    }

                NoAcuteFindingsGeneralSigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        AcuteFindingsRespiratorySign sign ->
            case sign of
                Stridor ->
                    { english = "Stridor"
                    , kinyarwanda = Just "Guhumeka ajwigira"
                    , kirundi = Just "Uguhema ujwigira mu gihe uri maso"
                    }

                NasalFlaring ->
                    { english = "Nasal Flaring"
                    , kinyarwanda = Just "Amazuru abyina igihe ahumeka"
                    , kirundi = Just "Ukwaguka kw'amazuru mu gihe uriko urahema"
                    }

                SevereWheezing ->
                    { english = "Severe Wheezing"
                    , kinyarwanda = Just "Guhumeka nabi cyane ajwigira"
                    , kirundi = Just "Uguhema nabi bikaze"
                    }

                SubCostalRetractions ->
                    { english = "Sub-Costal Retractions"
                    , kinyarwanda = Just "Icyena mu mbavu"
                    , kirundi = Just "Ugukuraho ibiri munsi y'imbavu"
                    }

                NoAcuteFindingsRespiratorySigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        AcuteIllnessAdverseEvent event ->
            case event of
                AdverseEventRashOrItching ->
                    { english = "Rash or Itching"
                    , kinyarwanda = Just "Kwishima cyangwa gusesa uduheri (turyaryata)"
                    , kirundi = Just "Amaherehere canke kwiyagaza"
                    }

                AdverseEventFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                AdverseEventDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    , kirundi = Just "Uguhitwa"
                    }

                AdverseEventVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    , kirundi = Just "Ukudahwa"
                    }

                AdverseEventFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "umunaniro"
                    , kirundi = Just "Uburuhe"
                    }

                AdverseEventOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoAdverseEvent ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        AcuteIllnessAdverseEventKindsQuestion ->
            { english = "What kind of adverse events"
            , kinyarwanda = Just "Ni ibihe bintu wabonye bidasanzwe (bitewe n'imiti wafashe)"
            , kirundi = Just "Ni ibihe bintu bibi vyabaye"
            }

        AcuteIllnessDangerSign sign ->
            case sign of
                DangerSignConditionNotImproving ->
                    { english = "Condition not improving"
                    , kinyarwanda = Just "Yanyoye imiti ariko ntiyoroherwa"
                    , kirundi = Just "Ingene bimeze, nta terambera"
                    }

                DangerSignUnableDrinkSuck ->
                    { english = "Unable to Drink/Suck"
                    , kinyarwanda = Just "Ntashoboye kunywa/konka"
                    , kirundi = Just "Ntibishoboka kunywa/konka"
                    }

                DangerSignVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    , kirundi = Just "Ukudahwa"
                    }

                DangerSignConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Just "Ukujugumira"
                    }

                DangerSignLethargyUnconsciousness ->
                    { english = "Lethargy or Unconsciousness"
                    , kinyarwanda = Just "Yahwereye cyangwa ntiyumva"
                    , kirundi = Just "Itiro rirenze canke ukuraba/uguta ubwenge"
                    }

                DangerSignRespiratoryDistress ->
                    { english = "Respiratory Distress"
                    , kinyarwanda = Just "Ahumeka bimugoye"
                    , kirundi = Just "Ingorane yo guhema"
                    }

                DangerSignSpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    , kirundi = Just "Ukuva amaraso aho nyene"
                    }

                DangerSignBloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    , kirundi = Just "Ugucibwamwo/uguhitwa bivanze n'amaraso"
                    }

                DangerSignNewSkinRash ->
                    { english = "New Skin Rash"
                    , kinyarwanda = Just "Yasheshe uduheri dushya"
                    , kirundi = Just "Uduherehere dushasha k'uruhu/urukoba"
                    }

                NoAcuteIllnessDangerSign ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        AcuteIllnessDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Just "Hiketswe umugera wa COVID-19"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Just "COVID-19 ikaze"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Just "Virisi ya Korona - 19 n'ibimenyetso vy'umusonga"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Just "Korona (COVID-19) isanzwe"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Complicated Malaria"
                    , kinyarwanda = Just "Malariya y'igikatu"
                    , kirundi = Just "Malariya ikomeye"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Gastrointestinal Infection with Complications"
                    , kinyarwanda = Just "Indwara yo mu nda ikabije"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito hamwe n'ingorane zijanye nazo"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Gastrointestinal Infection without Complications"
                    , kinyarwanda = Just "Indwara yo mu nda yoroheje"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito ata ngorane zijanye nazo"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Ibicurane n'inkorora byoroheje"
                    , kirundi = Just "Imbeho hamwe n'inkorora biswnzwe"
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Acute Respiratory Infection with Complications"
                    , kinyarwanda = Just "Indwara y'ubuhumekero ikabije"
                    , kirundi = Just "Ingwara yo guhema nabi ibabaje/uguhema nabi bibabaje hamwe n'ingorane bijanye"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Umusonga woroheje"
                    , kirundi = Just "Hiketswe ingwara y'umusonga igoye"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Just "Ubushuhe bitazwi iyo bwazananye"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Just "Ntibimenyekana - Isuzuma ryinshi rirakenewe"
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Nta Gupima/gusuzuma"
                    }

        AcuteIllnessDiagnosisWarning diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19 case"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Just "Hiketswe ingwara ya Korona-19 (COVID-19)"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Just "COVID-19 ikaze"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Just "Virisi ya Korona - 19 n'ibimenyetso vy'umusonga"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Just "Korona (COVID-19) isanzwe"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Malaria with Complications"
                    , kinyarwanda = Just "Afite Malariya y'igikatu"
                    , kirundi = Just "Malariya kumwe n'ingorane zikomeye zayo"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    , kirundi = Just "Malariya itagira ingorane zikomeye"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    , kirundi = Just "Malariya itagira ingorane zikomeye"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Suspected Gastrointestinal Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda ikabije"
                    , kirundi = Just "Hiketswe ingwara yo mu mara/mu nda (hamwe n'ingorane)"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Suspected Gastrointestinal Infection (without Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda yoroheje"
                    , kirundi = Just "Hiketswe ingwara yo mu mara/mu nda (ata ngorane)"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Inkorora n'ibicurane byoroheje "
                    , kirundi = Just "Imbeho hamwe n'inkorora biswnzwe"
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Suspected Acute Respiratory Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara y'ubuhumekero ikabije"
                    , kirundi = Just "Hiketswe ukwandura ingwara yo guhema ikaze/ikomeye (hamwe n'ingorane)"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Suspected Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Aracyekwaho umusonga woroheje"
                    , kirundi = Just "Hiketswe ingwara ingwara y'umusonga igoye"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Just "Ubushuhe bitazwi iyo bwazananye"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Just "Ntibimenyekana - Isuzuma ryinshi rirakenewe"
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Nta Gupima/gusuzuma"
                    }

        AcuteIllnessExisting ->
            { english = "Existing Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Just "Hariho ungwara ikaze/ikomeye"
            }

        AcuteIllnessHistory ->
            { english = "Acute Illness History"
            , kinyarwanda = Just "Amakuru ku ndwara ifatiyeho"
            , kirundi = Just "Akahise k'ingwara ibabaza cane/ikaze"
            }

        AcuteIllnessLowRiskCaseHelper ->
            { english = "This patient is a low risk case and should be sent home to be monitored by a CHW"
            , kinyarwanda = Just "Uyu murwayi nta byago afite byo kuba yaranduye Covid-19, agomba koherezwa mu rugo agakurikiranwa n'umujyanama w'ubuzima"
            , kirundi = Just "Uyu mugwayi afise ingorane/ikibazo kiri hasi/kidakabije c'ingwara, arategerezwa kurungikwa mu hira hama abaremeshakiyago bamukurikirane"
            }

        AcuteIllnessNew ->
            { english = "New Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho nshyashya"
            , kirundi = Just "Ingwara ikaze/ikomeye nshasha"
            }

        AcuteIllnessOutcomeLabel ->
            { english = "Acute Illness Outcome"
            , kinyarwanda = Just "Iherezo ry'indwara ifatiyeho\n"
            , kirundi = Just "Inkurikizi/ingaruka z'ingwara ibabaza cane/ikaze"
            }

        AcuteIllnessStatus status ->
            case status of
                AcuteIllnessBegan ->
                    { english = "Began"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Yatanguye"
                    }

                AcuteIllnessUpdated ->
                    { english = "Updated"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Vyagiye ku gihe"
                    }

                AcuteIllnessResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Cakemutse"
                    }

        ActiveDiagnosis ->
            { english = "Active Diagnosis"
            , kinyarwanda = Just "Uburwayi Bwasuzumwe"
            , kirundi = Just "Igisuzumo ciza/gusuzuma neza"
            }

        AcuteIllnessOutcome outcome ->
            case outcome of
                OutcomeIllnessResolved ->
                    { english = "Illness Resolved"
                    , kinyarwanda = Just "Indwara Yarakize"
                    , kirundi = Just "Ingwara yatorewe inyishu"
                    }

                OutcomeLostToFollowUp ->
                    { english = "Lost to Follow Up"
                    , kinyarwanda = Just "Umurwayi yaburiwe irengero"
                    , kirundi = Just "Yarabuzwe mu gihe co gukurikiranwa"
                    }

                OutcomeMovedOutsideCA ->
                    { english = "Moved outside the catchment area"
                    , kinyarwanda = Just "Umurwayi yimukiye ahandi"
                    , kirundi = Just "Bimuriwe/yimuriwe/yagiye hanze y'akarere k'ibikorwa/k'ubuvuzi"
                    }

                OutcomePatientDied ->
                    { english = "Patient Died"
                    , kinyarwanda = Just "Umurwayi yarapfuye"
                    , kirundi = Just "Umugwayi yapfuye"
                    }

                Backend.IndividualEncounterParticipant.Model.OutcomeReferredToHC ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    , kirundi = Just "Yarungitswe kw'ivuriro"
                    }

                OutcomeOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

        AddChild ->
            { english = "Add Child"
            , kinyarwanda = Just "Ongeraho umwana"
            , kirundi = Just "Kongerako umwana"
            }

        AddContact ->
            { english = "Add Contact"
            , kinyarwanda = Just "Ongeraho uwo bahuye"
            , kirundi = Just "Kongerako aho yotorwa/ingene yotorwa"
            }

        AddedToPatientRecordOn ->
            { english = "Added to patient record on"
            , kinyarwanda = Just "Yongewe ku makuru y'umurwayi kuwa"
            , kirundi = Just "Ongerako aho abagwayi bamaze kwandikwa"
            }

        AddFamilyMember ->
            { english = "Add Family Member"
            , kinyarwanda = Nothing
            , kirundi = Just "Kongerako umwe mu bagize umuryango"
            }

        AddFamilyMemberFor name ->
            { english = "Add Family Member for " ++ name
            , kinyarwanda = Nothing
            , kirundi = Just <| "Kongerako umwe mu bagize umuryango wa " ++ name
            }

        AddNewParticipant ->
            { english = "Add new participant"
            , kinyarwanda = Just "Ongeramo Umugenerwabikorwa musha"
            , kirundi = Just "Kongerako uwitavye mushasha"
            }

        AddParentOrCaregiver ->
            { english = "Add Parent or Caregiver"
            , kinyarwanda = Just "Ongeraho umubyeyi cyangwa umurezi"
            , kirundi = Just "Kongerako umuvyeyi canke umurezi"
            }

        AddToGroup ->
            { english = "Add to Group..."
            , kinyarwanda = Just "Ongeraho itsinda..."
            , kirundi = Just "Kongerako mu murwi..."
            }

        Administer ->
            { english = "Administer"
            , kinyarwanda = Just "Tanga umuti"
            , kirundi = Just "Kuyobora"
            }

        AdministerAzithromycinHelper ->
            { english = "By mouth 1x"
            , kinyarwanda = Just "Inshuro imwe mu kanwa"
            , kirundi = Just "Kumira incuro 1"
            }

        AdministerCeftriaxoneHelper ->
            { english = "IM once"
            , kinyarwanda = Just "Urushinge mu mikaya inshuro imwe"
            , kirundi = Just "Gucisha umuti mu mutsi rimwe"
            }

        AdministerMebendezoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            , kirundi = Just "Ha umwana ikinini 1 co kumira"
            }

        AdministerMetronidazoleHelper ->
            { english = "By mouth twice a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini inshuro ebyiri ku munsi mu minsi irindwi"
            , kirundi = Nothing
            }

        AdministerAlbendazoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            , kirundi = Just "Ha umwana ikinini 1 co kumira"
            }

        AdministerPrenatalMebendezoleHelper ->
            { english = "1 dose once a day for one day"
            , kinyarwanda = Just "Ikinini kimwe inshuro imwe ku munsi mu munsi umwe"
            , kirundi = Just "Urugero rw'umuti rumwe ku munsi mu munsi umwe"
            }

        AdministerFolicAcidHelper ->
            { english = "Take daily for 3 months"
            , kinyarwanda = Just "Fata imiti buri munsi mu gihe cy'amexi 3"
            , kirundi = Just "Fata iminsi yose mu kiringo c'amezi 3"
            }

        AdministerHIVARVHelper ->
            { english = "Take 1x a day by mouth"
            , kinyarwanda = Just "Fata ikinini 1 ku munsi mu kanwa"
            , kirundi = Just "Ugufata ikinini 1, ukimize, 1 ku munsi"
            }

        AdministerIronHelper ->
            { english = "Take 1 60 mg tabs 2x a day x 3 months"
            , kinyarwanda = Just "Fata mg 1 60 inshuro 2 ku munsi mu mezi atatu"
            , kirundi = Just "Ugufata ikinini 1 ca 60 mg 2 k'umunsi mu mezi 3"
            }

        AdministerParacetamolHelper ->
            { english = "Take 1 tablet by mouth 3 times a day for 5 days"
            , kinyarwanda = Just "Fata ikinini 1 mu kanwa inshuro 3 ku munsi mu minsi 5"
            , kirundi = Just "Ugufata ikinini 1, ukimize, 3 ku munsi mu minsi 5"
            }

        AdministerVitaminAHelperPrenatal ->
            { english = "Vitamin A is given once"
            , kinyarwanda = Just "Vitamine A itangwa inshuro 1"
            , kirundi = Just "Vitamine A yatanzwe rimwe"
            }

        AdministerVitaminAHelperWellChild ->
            { english = "Put the correct number of drops directly into the mouth of the child"
            , kinyarwanda = Just "Shyira mu kanwa k'umwana ibitonyanga bigenwe"
            , kirundi = Just "Shira igitigiri gikwiye aho nyene mu kanwa k'umwana"
            }

        Administered ->
            { english = "Administered"
            , kinyarwanda = Just "Umuti watanzwe"
            , kirundi = Just "Gutwagwa"
            }

        AdministeredMedicationQuestion ->
            { english = "Have you administered"
            , kinyarwanda = Just "Watanze umuti"
            , kirundi = Just "Woba warigeze utanga umuti"
            }

        AdministeredOneOfAboveMedicinesQuestion ->
            { english = "Have you administered one of the above medicines to the patient"
            , kinyarwanda = Just "Waba wahaye umurwyayi umwe mu miti yavuzwe haruguru"
            , kirundi = Just "Woba warigeze utanga umwe mu miti iraho hejuru k'umugwayi"
            }

        AddressInformation ->
            { english = "Address Information"
            , kinyarwanda = Just "Aho atuye/Aho abarizwa"
            , kirundi = Just "Amakuru (y'imvaho) yaho aba"
            }

        AfterEachLiquidStool ->
            { english = "after each liquid stool"
            , kinyarwanda = Just "buri uko amaze kwituma ibyoroshye"
            , kirundi = Just "Inyuma ya buri mwanda mukuru w'amazi"
            }

        AgeWord ->
            { english = "Age"
            , kinyarwanda = Just "Imyaka"
            , kirundi = Just "Imyaka"
            }

        Activities ->
            { english = "Activities"
            , kinyarwanda = Just "Ibikorwa"
            , kirundi = Just "Ibikorwa"
            }

        ActivitiesCompleted count ->
            { english = "Completed (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibyarangiye (" ++ String.fromInt count ++ ")"
            , kirundi = Just <| "Ivyarangiye (" ++ String.fromInt count ++ ")"
            }

        ActivitiesHelp activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                    , kinyarwanda = Just "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                    , kirundi = Just "Umuvyeyi wese arategerezwa kubazwa buri kwezi uburyo akoresha kugira ashire mu ngiro umugambi wo kuvyara k'urugero. Nimba umuvyeyi ashaka kuvyara k'urugero, murungike kw'ivuriro."
                    }

                MotherActivity Lactation ->
                    { english = "Ideally a mother exclusively breastfeeds her infant for at least 6 months. Every mother should be asked about how she is feeding her infant each month."
                    , kinyarwanda = Just "Ni byiza ko umubyeyi yonsa umwana we byibuze amezi 6 nta kindi amuvangiye. Buri mubyeyi agomba kubazwa uko agaburira umwana we buri kwezi."
                    , kirundi = Just "N'ivyiza cane ko umuvyeyi yonsa umwana ni miburiburi ikiringo c'amezi 6. Urategerezwa kubaza umuvyeyi wese ingene agaburira/afungurira umwana wiwe buri kwezi."
                    }

                MotherActivity MotherFbf ->
                    { english = "If a mother is breastfeeding, she should receive FBF every month. If she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    , kirundi = Just "Mugihe umuvyeyi yonsa, arategerezwa kuronka FSA \"(FBF)\" buri kwezi. Nimba ataronse igipimo ca muhariwe, andika urugero yaronse hama uhitemwo impamvu"
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Please review the following forms with the participant."
                    , kinyarwanda = Nothing
                    , kirundi = Just "Raba neza/suzuma impapuro hamwe nuwitavye."
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "Every child should receive FBF every month. If he/she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umwana wese ategerezwa kuronka FSA  (\"FBF\") buri kwezi. Nimba ataronse igipimo cagenywe, andika igipimo yaronse hama uhitemwo impamvu."
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    , kirundi = Just "Saba umuvyeyi afatire umutwe w'umwana wiwe ku mpera z'urubahu gw'ipimiro. Egereza akanyerezo ku gitsintsiri c'umwana hama ukwege ukuguru rimwe na rimwe."
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    , kirundi = Just "Urabe neza ko wapimye hagati na hagati hejuru y'ukuboko k'umwana."
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    , kirundi = Just "Sigurira umuvyeyi ingene yoraba ibimenyetso vyo gufungura nabi k'umwana wiwe."
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    , kirundi = Just "Fata ifoto y'umwana wese kuri buri isuzuma ry'amagara. Amafoto ategerezwa kwerekana umubiri wose wa buri mwana"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    , kirundi = Just "Tumbereza neza umunzane imbere yo gupima ibiro vya mbere vy'uruyoya/umwana mutoya canke akivuka. Shira umwana mu gisipi/igishipi/ igikoresho kimufata mu gikiriza kandi ata mpuzu yambaye."
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Just "Ivyazanye intererano"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Just "Kurikirana"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Just "Rungika kw'ivuriro"
                    }

                ChildActivity Activity.Model.NCDA ->
                    translationSet ChildScorecard

        ActivitiesLabel activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Which, if any, of the following methods do you use?"
                    , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                    , kirundi = Just "Ni ubuhe, nimba buhari, m'uburyo bukurikira ukoresha?"
                    }

                MotherActivity Lactation ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    , kirundi = Just "Injiza/andika igitigiri ca CSB++ hamwe na FBF catanzwe aha hepfo"
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms:"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Amafishi"
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    , kirundi = Just "Injiza/andika igitigiri ca CSB++ hamwe na FBF catanzwe aha hepfo"
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height:"
                    , kinyarwanda = Just "Uburebure:"
                    , kirundi = Just "Uburebure:"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC:"
                    , kinyarwanda = Just "Ikizigira cy'akaboko:"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi:"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Select all signs that are present:"
                    , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                    , kirundi = Just "Hitamwo ibimenyetso vyose bihari:"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo:"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Just "Ifoto:"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight:"
                    , kinyarwanda = Just "Ibiro:"
                    , kirundi = Just "uburemere:"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors:"
                    , kinyarwanda = Just "Impamvu zateye uburwayi:"
                    , kirundi = Just "Ivyazanye intererano"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up:"
                    , kinyarwanda = Just "Gukurikirana umurwayi:"
                    , kirundi = Just "Kurikirana:"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education:"
                    , kinyarwanda = Just "Inyigisho ku buzima:"
                    , kirundi = Just "Inyigisho z'amagara:"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center:"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima:"
                    , kirundi = Just "Rungika kw'ivuriro:"
                    }

                ChildActivity Activity.Model.NCDA ->
                    { english = "Child Scorecard:"
                    , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana:"
                    , kirundi = Just "Ikarata y'ikurikiranwa ry'umwana"
                    }

        ActivitiesTitle activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro?"
                    , kirundi = Just "Kuvyara k'urugero"
                    }

                MotherActivity Lactation ->
                    { english = "Lactation"
                    , kinyarwanda = Just "Konsa"
                    , kirundi = Just "Konsa"
                    }

                MotherActivity MotherFbf ->
                    { english = "FBF Mother"
                    , kinyarwanda = Just "FBF y'umubyeyi"
                    , kirundi = Just "FBF y'umuvyeyi"
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Counseling"
                   , kinyarwanda = Just "Ubujyanama"
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "FBF Child"
                    , kinyarwanda = Just "FBF y'umwana"
                    , kirundi = Just "FBF y'umwana"
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Just "Uburebure"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Just "Ugufungura"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Just "Uburemere"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Just "Ivyazanye intererano"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Just "Kurikirana"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Just "Rungika kw'ivuriro"
                    }

                ChildActivity Activity.Model.NCDA ->
                    translationSet ChildScorecard

        Activity ->
            { english = "Activity"
            , kinyarwanda = Just "Igikorwa"
            , kirundi = Nothing
            }

        ActivitityTitleAchi ->
            { english = "Aheza Child"
            , kinyarwanda = Just "Aheza igenewe umwana"
            , kirundi = Nothing
            }

        ActivitiesToComplete count ->
            { english = "To Do (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibisabwa gukora (" ++ String.fromInt count ++ ")"
            , kirundi = Just <| "Gukora (" ++ String.fromInt count ++ ")"
            }

        ActivitityLabelAchi ->
            { english = "Enter the amount of Aheza distributed below."
            , kinyarwanda = Just "Uzuza hano ingano ya Aheza utanze"
            , kirundi = Just "Andika igitigiri c'ivya Aheza watanze aha hepfo."
            }

        ActivePage page ->
            translateActivePage page

        AcuteIllnessActivityTitle activity ->
            case activity of
                AcuteIllnessSymptoms ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kongera kureba ibimenyetso"
                    , kirundi = Just "Isubiramwo ry'ikimenyetso"
                    }

                AcuteIllnessPhysicalExam ->
                    { english = "Physical Exam"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Just "Igipimo c'umubiri"
                    }

                AcuteIllnessPriorTreatment ->
                    { english = "Prior Treatment History"
                    , kinyarwanda = Just "Amakuru ku miti yafashe"
                    , kirundi = Just "Imbere yo kuvura akahise"
                    }

                AcuteIllnessLaboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Just "Icumba c'ipimiro/"
                    }

                AcuteIllnessExposure ->
                    { english = "Exposure / Travel History"
                    , kinyarwanda = Just "Afite ibyago byo kwandura/amakuru ku ngendo yakoze"
                    , kirundi = Just "Kumenyesha/Akahise k'urugendo"
                    }

                AcuteIllnessNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

                AcuteIllnessOngoingTreatment ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Just "Isubiramwo ry'imiti"
                    }

                AcuteIllnessDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    , kirundi = Just "Ibimenyetso vy'akaga"
                    }

        AdverseEventSinglePlural val ->
            if val == 1 then
                { english = "Adverse event"
                , kinyarwanda = Just "Ikintu kidasanzwe (bitewe n'imiti wafashe)"
                , kirundi = Just "Icyabaye bibabaje"
                }

            else
                { english = "Adverse events"
                , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                , kirundi = Just "Ivyabaye bibabaje"
                }

        Age months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " iminsi"
            , kirundi = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " iminsi"
            }

        AgeDays days ->
            { english = String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt days ++ " Iminsi"
            , kirundi = Just <| String.fromInt days ++ " Iminsi"
            }

        AgeMonthsWithoutDay months ->
            { english = String.fromInt months ++ " months"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi"
            , kirundi = Just <| String.fromInt months ++ " Amezi"
            }

        AgeSingleBoth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Umunsi"
            , kirundi = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Umunsi"
            }

        AgeSingleMonth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Iminsi"
            , kirundi = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Iminsi"
            }

        AgeSingleDayWithMonth months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " Umunsi"
            , kirundi = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " Umunsi"
            }

        AgeSingleDayWithoutMonth days ->
            { english = String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt days ++ " Umunsi"
            , kirundi = Just <| String.fromInt days ++ " Umunsi"
            }

        AlertChwToFollowUp ->
            { english = "Alert CHW to follow up with patient"
            , kinyarwanda = Just "Menyesha umujyanama w'ubuzima gukurikirana umurwayi"
            , kirundi = Just "Menyesha Abaremeshakiyago kugira bakurikirane umugwayi"
            }

        AgeOneYearOld ->
            { english = "One year old"
            , kinyarwanda = Just "Umwaka umwe"
            , kirundi = Just "Umwaka umwe"
            }

        AgeOneYearAndOneMonth ->
            { english = "One year and one month"
            , kinyarwanda = Just "Umwaka n'ukwezi kumwe"
            , kirundi = Just "Umwaka umwe n'ukwezi kumwe"
            }

        AgeOneYearWithMonths months ->
            { english = "One year and " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| "Umwaka n'amezi " ++ String.fromInt months
            , kirundi = Just <| "Umwaka n'amezi " ++ String.fromInt months
            }

        AgeYearsWithSingleMonth years month ->
            { english = String.fromInt years ++ " years " ++ String.fromInt month ++ " month"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt years ++ " Ukwezi " ++ String.fromInt month
            , kirundi = Just <| "Imyaka " ++ String.fromInt years ++ " Ukwezi " ++ String.fromInt month
            }

        AgeYearsAndMonths years months ->
            { english = String.fromInt years ++ " years " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt years ++ " Amezi " ++ String.fromInt months
            , kirundi = Just <| "Imyaka " ++ String.fromInt years ++ " Amezi " ++ String.fromInt months
            }

        AILaboratoryTask task ->
            case task of
                LaboratoryMalariaTesting ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                LaboratoryCovidTesting ->
                    { english = "Covid Rapid Test"
                    , kinyarwanda = Just "Ikizamini cya Covid-19 cyihuse"
                    , kirundi = Just "Igipimo c'ihuta ca Korona"
                    }

        And ->
            { english = "and"
            , kinyarwanda = Just "na"
            , kirundi = Just "na"
            }

        AndSentence ->
            { english = "and"
            , kinyarwanda = Just "maze"
            , kirundi = Nothing
            }

        AntenatalCare ->
            { english = "Antenatal Care"
            , kinyarwanda = Just "Isuzuma ku mugore utwite"
            , kirundi = Just "Kwitaho imbanyi imbere yo kuvyara/"
            }

        AntenatalProgressReport ->
            { english = "Antenatal Progress Report"
            , kinyarwanda = Nothing
            , kirundi = Just "Icegeranyo c'iterambere ry'imbanyi imbere yo kuvyara"
            }

        AntenatalVisistsHistory ->
            { english = "Antenatal Visits History"
            , kinyarwanda = Just "Amakuru ku isurwa ry'umugore utwite"
            , kirundi = Just "Akahise ko kugenderwa imbere yo kuvyara"
            }

        AmbulancArrivalPeriodQuestion ->
            { english = "How long did it take the ambulance to arrive"
            , kinyarwanda = Just "Bitwara igihe kingana gute ngo imbangukiragutabara ihagere"
            , kirundi = Just "Mbega Rusehabaniha (ambiranse) yafashe umwanya ungana gute kuhashisha"
            }

        ANCEncountersNotRecordedQuestion ->
            { english = "Were there any ANC encounters that are not recorded above"
            , kinyarwanda = Just "Haba hari ipimwa ry'inda ryakozwe bakaba batarabyanditse"
            , kirundi = Nothing
            }

        ANCIndicateVisitsMonthsPhrase ->
            { english = "Indicate the months of pregnancy in which a visit occured"
            , kinyarwanda = Just "Hitamo amezi y'inda isuzuma ryakoreweho"
            , kirundi = Nothing
            }

        ANCNewborn ->
            { english = "Antenatal Care & Newborn"
            , kinyarwanda = Just "Kwita k’umugore utwite n’uruhinja"
            , kirundi = Nothing
            }

        AgeSingleMonthWithoutDay month ->
            { english = String.fromInt month ++ " month"
            , kinyarwanda = Just <| String.fromInt month ++ " Ukwezi"
            , kirundi = Nothing
            }

        AppName ->
            { english = "E-Heza System"
            , kinyarwanda = Just "E-heza sisiteme"
            , kirundi = Just "Ubuhinga bwa E-Heza"
            }

        AppointmentConfirmation ->
            { english = "Appointment Confirmation"
            , kinyarwanda = Just "Kwemeza itariki yo kugaruka"
            , kirundi = Just "Kwemeza umubonano"
            }

        AppointmentConfirmationInstrunction ->
            { english = "The patient should visit the health center on the following date"
            , kinyarwanda = Just "Umubyeyi agomba kujya ku kigo nderabuzima ku itariki ikurikira"
            , kirundi = Just "Umugwayi ategerezwa kuja k'ivuriro kw'itarike ikurikira"
            }

        All ->
            { english = "All"
            , kinyarwanda = Just "Uburwayi bwose"
            , kirundi = Just "Vyose"
            }

        AllowedValuesRangeHelper constraints ->
            { english = "Allowed values are between " ++ String.fromFloat constraints.minVal ++ " and " ++ String.fromFloat constraints.maxVal ++ "."
            , kinyarwanda = Just <| "Imibare yemewe iri hagati ya " ++ String.fromFloat constraints.minVal ++ " na " ++ String.fromFloat constraints.maxVal ++ "."
            , kirundi = Just <| "ibiharuro vyemewe biri hagati ya " ++ String.fromFloat constraints.minVal ++ " na " ++ String.fromFloat constraints.maxVal ++ "."
            }

        AreYouSure ->
            { english = "Are you sure?"
            , kinyarwanda = Just "Urabyizeye?"
            , kirundi = Just "Uravyizeye?"
            }

        Assessment ->
            { english = "Assessment"
            , kinyarwanda = Just "Ipimwa"
            , kirundi = Just "Isuzuma"
            }

        Asthma ->
            { english = "Asthma"
            , kinyarwanda = Just "Asthma (Agahema)"
            , kirundi = Just "Asima"
            }

        At ->
            { english = "at"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Attendance ->
            { english = "Attendance"
            , kinyarwanda = Just "Ubwitabire"
            , kirundi = Just "Ukwitaba"
            }

        AvoidingGuidanceReason value ->
            case value of
                AvoidingGuidanceHypertensionLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Just "Ntabiriyo m'ububiko"
                    }

                AvoidingGuidanceHypertensionKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    , kirundi = Just "Ihindagurika ry'umuiri rizwi"
                    }

                AvoidingGuidanceHypertensionPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Just "Umugwayi yaranse"
                    }

                AvoidingGuidanceHypertensionPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Just "Umugwayi ntashobora kuronka"
                    }

                AvoidingGuidanceHypertensionReinforceAdherence ->
                    { english = "Reinforce adherence of existing dosage"
                    , kinyarwanda = Just "Shimangira umwigisha akamaro ko kubahiriza gufata imiti asanganwe"
                    , kirundi = Just "Gushimangira kubahiriza idoze/igipimo c'imiti ihari"
                    }

                AvoidingGuidanceHypertensionOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

        Baby ->
            { english = "Baby"
            , kinyarwanda = Just "Umwana"
            , kirundi = Just "Uruyoya"
            }

        BabyDiedOnDayOfBirthPreviousDelivery ->
            { english = "Live Birth but the baby died the same day in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
            , kirundi = Just "Umwana yavutse akomeye ariko umwana yaciye apfa uwo munsi nyene avuka mu gihe c'ukwibaruka guheruka"
            }

        Back ->
            { english = "Back"
            , kinyarwanda = Nothing
            , kirundi = Just "Inyuma"
            }

        BackendError ->
            { english = "Error contacting backend"
            , kinyarwanda = Just "Seriveri yerekanye amakosa akurikira"
            , kirundi = Just "Ikosa ryo kuvugana n'abinyuma"
            }

        Balance ->
            { english = "Balance"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BatchNumberAbbrev ->
            { english = "Batch #"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BreastfeedingSignQuestion sign ->
            case sign of
                IsBreastfeeding ->
                    { english = "Are you breastfeeding"
                    , kinyarwanda = Just "Waba wonsa"
                    , kirundi = Just "Uronsa"
                    }

                BreastPain ->
                    { english = "Are you experiencing breast pain"
                    , kinyarwanda = Just "Waba ubabara amabere"
                    , kirundi = Just "Mbega wumva ubabara amabere"
                    }

                BreastRedness ->
                    { english = "Are you experiencing breast redness"
                    , kinyarwanda = Just "Amabere yawe yaba atukuye"
                    , kirundi = Just "Mbega ubona amabere atukura"
                    }

                EnoughMilk ->
                    { english = "Do you have enough milk for your baby to breastfeed at least 8 times per day"
                    , kinyarwanda = Just "Waba ufite amashereka ahagije yo konsa umwana wawe nibura inshuro 8 kumunsi"
                    , kirundi = Just "Mbega urafise amaberebere akwiye kugira n'imiburiburi umwana ashobore konka incuro munani (8) ku munsi"
                    }

                LatchingWell ->
                    { english = "Is the baby latching well"
                    , kinyarwanda = Just "Umwana aronka neza"
                    , kirundi = Just "Mbega umwana aronka neza"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        BeatsPerMinuteUnitLabel ->
            { english = "Beats per minute"
            , kinyarwanda = Just "Inshuro umutima utera ku munota"
            , kirundi = Just "Ugukubita k'umunota"
            }

        BeginNewEncounter ->
            { english = "Begin a New Encounter"
            , kinyarwanda = Just "Tangira igikorwa gishya"
            , kirundi = Just "Ugutangura kubonana bushasha"
            }

        BirthDefect defect ->
            case defect of
                DefectBirthInjury ->
                    { english = "Birth Injury"
                    , kinyarwanda = Just "Impanuka zo mu kuvuka"
                    , kirundi = Just "Ingorane zabaye igihe c'ivuka"
                    }

                DefectCleftLipWithCleftPalate ->
                    { english = "Cleft Lip with Cleft Palate"
                    , kinyarwanda = Just "Ibibari k'umunwa n'urusenge rw'akanwa"
                    , kirundi = Just "Umunwa wapasutse hamwe n'ubujana"
                    }

                DefectCleftPalate ->
                    { english = "Cleft Palate"
                    , kinyarwanda = Just "Ibibari ku rusenge rw'akanwa"
                    , kirundi = Just "Ubujana"
                    }

                DefectClubFoot ->
                    { english = "ClubFoot"
                    , kinyarwanda = Just "Ibirenge bitameze neza"
                    , kirundi = Just "Ubumuga bw'ikirenge"
                    }

                DefectMacrocephaly ->
                    { english = "Macrocephaly"
                    , kinyarwanda = Just "Umutwe munini cyane"
                    , kirundi = Just "Umutwe munini urengeye ibisanzwe"
                    }

                DefectGastroschisis ->
                    { english = "Gastroschisis"
                    , kinyarwanda = Just "Umwobo ku nda bituma imyanya yo mu nda iba hanze"
                    , kirundi = Just "Ingwara umwana avukana ituma usanga ikinigo mu nda iwe hama amara agasohoka"
                    }

                DefectHearingLoss ->
                    { english = "Hearing Loss"
                    , kinyarwanda = Just "Ubumuga bwo kutumva"
                    , kirundi = Just "Gutakaza kumva"
                    }

                DefectUndescendedTestes ->
                    { english = "Undescended Testes"
                    , kinyarwanda = Just "Udusabo tw'itanga tutari mu mwanya watwo"
                    , kirundi = Just "Ibipimo bidategeretse"
                    }

                DefectHypospadias ->
                    { english = "Hypospadias"
                    , kinyarwanda = Just "Umwenge unyuramo inkari ku gice cyo hasi cy'imboro"
                    , kirundi = Just "Ingwara canke ikintu kidasanzwe aho intoboro y'imboro y'umwana ija aho itagenewe"
                    }

                DefectInguinalHernia ->
                    { english = "Inguinal Hernia"
                    , kinyarwanda = Just "Urura rwamanutse ruva mu gice cyarwo"
                    , kirundi = Just "Ikivyimba mu mara"
                    }

                DefectMicrocephaly ->
                    { english = "Microcephaly"
                    , kinyarwanda = Just "Umutwe muto cyane"
                    , kirundi = Just "Umutwe mutoya birengeye ugereranije niy'abandi bana bafise imyaka ingana"
                    }

                DefectNeuralTubes ->
                    { english = "Neural Tubes Defects"
                    , kinyarwanda = Just "Urutirigongo rudafunze neza"
                    , kirundi = Nothing
                    }

                DefectDownSyndrome ->
                    { english = "Down Syndrome"
                    , kinyarwanda = Just "Ikibazo giterwa no kuvukana uturangamuntu(Chromosomes) turenze utwangomwa"
                    , kirundi = Just "Ingwara ya Down (ingwara ituma haba ingorane mu bwenge canke k'umubiri)"
                    }

                DefectCongenitalHeart ->
                    { english = "CongenitalHeart Defects (CHD)"
                    , kinyarwanda = Just "Yavukanye ibibazo by'umutima"
                    , kirundi = Just "Ingwara z'umutima zavukanywe"
                    }

                DefectVentricalSeptal ->
                    { english = "Ventrical Septal Defect"
                    , kinyarwanda = Just "Ibibazo by'umutima"
                    , kirundi = Just "Ingwara y'umutima aho hari ikinogo mu kibambazi gitandukanya ivyumba 2 vyo hepfo mu mutima indani"
                    }

                DefectPulmonaryValveAtresiaAndStenosis ->
                    { english = "Pulmonary Valve Atresia and Stenosis"
                    , kinyarwanda = Just "Ibibazo by'umutima n'ibihaha"
                    , kirundi = Just "Ingwara yo mu mahaha itewe n'ubuke bw'amaraso aje ava m'umutima kubera hariho uturingoti/uturyango twugaye"
                    }

                NoBirthDefects ->
                    { english = "None"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Nta na kimwe"
                    }

        BirthDefectLabel ->
            { english = "Birth Defect"
            , kinyarwanda = Just "Yavukanye ubumuga"
            , kirundi = Just "Yavukanye ubumuga"
            }

        BirthDefectsPresentQuestion ->
            { english = "Does the child have any birth defects"
            , kinyarwanda = Just "Hari ubumuga/bibazo umwana yaba yaravukanye"
            , kirundi = Just "Mbega umwana yavukanye ubumuga"
            }

        BirthDefectsSelectionLabel ->
            { english = "Which of the following"
            , kinyarwanda = Just "Ni ubuhe muri ubu bukurikira"
            , kirundi = Just "Ni ubuhe muri ubu"
            }

        BloodGlucose ->
            { english = "Blood Glucose"
            , kinyarwanda = Just "Ingano y'Isukari mu Maraso"
            , kirundi = Just "Umurwi wa \"Glucose\" (umurwi ndemamubiri)"
            }

        BloodPressure ->
            { english = "Blood Pressure"
            , kinyarwanda = Just "Umuvuduko w'amaraso"
            , kirundi = Just "Umuvuduko w'amaraso"
            }

        BloodPressureDiaLabel ->
            { english = "Diastolic"
            , kinyarwanda = Just "Umuvuduko w'amaraso wo hasi"
            , kirundi = Nothing
            }

        BloodPressureSysLabel ->
            { english = "Systolic"
            , kinyarwanda = Just "Umubare w'umuvuduko w'amaraso wo hejuru"
            , kirundi = Just "Umuvuduko w'amaraso urenze mu gihe umutima uyarungitse"
            }

        BloodSmearQuestion ->
            { english = "Did you perform a blood smear"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega warigeze ugira igipimo c'amaraso"
            }

        BloodSmearLabel ->
            { english = "Malaria Blood Smear"
            , kinyarwanda = Nothing
            , kirundi = Just "Gupima Malariya mu maraso"
            }

        BloodSmearResult value ->
            case value of
                BloodSmearNegative ->
                    translationSet NegativeLabel

                BloodSmearPlus ->
                    { english = "+"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                BloodSmearPlusPlus ->
                    { english = "++"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                BloodSmearPlusPlusPlus ->
                    { english = "+++"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                BloodSmearNotTaken ->
                    { english = "Not taken"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Nticafashwe"
                    }

        BMI ->
            { english = "BMI"
            , kinyarwanda = Nothing
            , kirundi = Just "IMC"
            }

        BMIHelper ->
            { english = "Calculated based on Height and Weight"
            , kinyarwanda = Just "Byabazwe hashingiwe ku burebure n'ibiro"
            , kirundi = Just "Uguharura ukurikije uburebure hamwe n'uburemere"
            }

        BodyTemperature ->
            { english = "Body Temperature"
            , kinyarwanda = Just "Ubushyuhe bw'umubiri"
            , kirundi = Just "Ubushuhe bw'umubiri"
            }

        BornUnderweight ->
            { english = "Born Underweight"
            , kinyarwanda = Just "Yavukanye ibiro bidashyitse"
            , kirundi = Just "Yavutse afise ibiro bikeya"
            }

        BoughtClothesQuestion ->
            { english = "Have you bought clothes and other essential items for the child"
            , kinyarwanda = Just "Waba waraguze imyenda n'ibindi bikoresho by'ibanze bikenewe ku mwana"
            , kirundi = Just "Mbega waraguze impundu n'ibindi vyangombwa vy'umwana"
            }

        BowedLegs ->
            { english = "Bowed Legs"
            , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
            , kirundi = Just "Amaguru y'ingonze"
            }

        BpmUnit respiratoryRate ->
            { english = String.fromInt respiratoryRate ++ " bpm"
            , kinyarwanda = Just <| "Inshuro ahumeka ku munota " ++ String.fromInt respiratoryRate
            , kirundi = Just <| "Uguhema k'umunota " ++ String.fromInt respiratoryRate
            }

        BreathsPerMinuteUnitLabel ->
            { english = "Breaths per minute"
            , kinyarwanda = Just "Inshuro ahumeka ku munota"
            , kirundi = Just "Uguhema k'umunota"
            }

        BreastExam ->
            { english = "Breast Exam"
            , kinyarwanda = Just "Gusuzuma amabere"
            , kirundi = Just "Igipimo c'amabere"
            }

        BreastExamDischargeQuestion ->
            { english = "What kind of discharge"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni ubuhe buryo bwo gusohoka"
            }

        BreastExamDischargeType value ->
            case value of
                DischargeMilky ->
                    { english = "Milky"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                DischargeClear ->
                    { english = "Clear"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Bimeze nk'amata"
                    }

                DischargeBrownOrBloody ->
                    { english = "Brown or Bloody"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Inzobe canke Ikiremve"
                    }

                DischargeYellow ->
                    { english = "Yellow"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ibara risa n'umutoto uhishiye"
                    }

                DischargeGreen ->
                    { english = "Green"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ibara ry'icatsi kibisi"
                    }

        BreastExamQuestion ->
            { english = "Did you show the patient how to perform a self breast exam"
            , kinyarwanda = Just "Weretse umubyeyi uko yakwisuzuma amabere?"
            , kirundi = Just "Mbega wareretse umugwayi ingene yokwigirira igipimo c'ibere"
            }

        BreastExamSign option ->
            case option of
                Mass ->
                    { english = "Mass"
                    , kinyarwanda = Just "Utubyimba mu Ibere"
                    , kirundi = Just "Imisa"
                    }

                Discharge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Gusohoka kw'ibintu bidasanzwe"
                    , kirundi = Just "Gucugwa"
                    }

                Infection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Just "Ivyanduza"
                    }

                NormalBreast ->
                    translationSet Normal

                Warmth ->
                    { english = "Warmth"
                    , kinyarwanda = Just "Ubushyuhe"
                    , kirundi = Just "Ubushuhe"
                    }

        BrittleHair ->
            { english = "Brittle Hair"
            , kinyarwanda = Just "Gucurama no guhindura ibara ku misatsi"
            , kirundi = Just "Imishatsi y'irende"
            }

        ByMouthDaylyForXDays days ->
            { english = "by mouth daily x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "ku munsi / mu  minsi " ++ String.fromInt days
            , kirundi = Just <| "Gucisha mu kanwa buri munsi mu iminsi " ++ String.fromInt days
            }

        ByMouthTwiceADayForXDays days ->
            { english = "by mouth twice per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            , kirundi = Just <| "Gucisha mu kanwa incuro 2 k'umunsi mu iminsi " ++ String.fromInt days
            }

        ByMouthThreeTimesADayForXDays days ->
            { english = "by mouth three times per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            , kirundi = Just <| "Kumira incuro 3 k'umunsi mu kiringo (Igitigiri) iminsi"
            }

        Call114 ->
            { english = "Call 114"
            , kinyarwanda = Just "Hamagara 114"
            , kirundi = Nothing
            }

        Called114Question ->
            { english = "Were you able to talk with 114"
            , kinyarwanda = Just "Wabashije kuvugana n’abantu bo kuri 114"
            , kirundi = Just "Woba warashoboye kuvugana kuri 114"
            }

        Cancel ->
            { english = "Cancel"
            , kinyarwanda = Just "Guhagarika"
            , kirundi = Just "Guhagarika"
            }

        CandidiasisRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Candidiasis"
            , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso bya Kandidoze"
            , kirundi = Just "Uyu mugwayi yerekana ibimenyetso vy'ingwara ya \"candidose\""
            }

        CandidiasisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Just "Hitamo imiti n'igipimo/ibipimo (idoze) uzotanga k'umugwayi"
            }

        CannotStartEncounterLabel ->
            { english = "You cannot open a new encounter, as there's already a completed encounter today for"
            , kinyarwanda = Just "Ntago bishoboka gutangira isuzuma rishya, kuko hari isuzuma ryarangiye uyu munsi rya"
            , kirundi = Just "Ntushobora kugurura uguhura gushasha, kuko hari uguhura uno munsi gusanzwe kujije"
            }

        CardiacDisease ->
            { english = "Cardiac Disease"
            , kinyarwanda = Just "Indwara z'umutima"
            , kirundi = Just "Ingwara y'umutima"
            }

        CaregiverAccompanyQuestion ->
            { english = "Do you have a caregiver to accompany you to the health center when you give birth"
            , kinyarwanda = Just "Ufite umuntu wo kuguherekeza ku kigo nderabuzima igihe ugiye kubyara"
            , kirundi = Just "Mbega urafise umugwaza ashobora kukuguherekeza kwa muganga/kw'ivuriro mu gihe co kwibaruka"
            }

        CaregiverMessage ->
            { english = "This person is a caregiver. There are no activities to complete."
            , kinyarwanda = Just "Uyu ni umurezi w'umwana. Nta bikorwa usabwa kumukorera."
            , kirundi = Nothing
            }

        Caring ->
            { english = "Caring"
            , kinyarwanda = Just "Kwita ku mwana"
            , kirundi = Just "Ukwita ku mwana"
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Just "Akagali"
            , kirundi = Just "cellule"
            }

        CaseManagement ->
            { english = "Case Management"
            , kinyarwanda = Just "Gukurikirana Umurwayi"
            , kirundi = Just "Ugucungera ingwara"
            }

        CaseManagementFilterLabel filter ->
            case filter of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    , kirundi = Just "Ingwara ikaze"
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterNutrition ->
                    { english = "Home Visit"
                    , kinyarwanda = Just "Gusura Umurwayi"
                    , kirundi = Just "Kugendera muhira"
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Just "Kurondera uwo mwavuganye"
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa umubyeyi utwite"
                    , kirundi = Just "Ibipimo vya laboratware vy'imbanyi imbere yo kuvyara"
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa ufite indwara zitandura"
                    , kirundi = Just "Ibipimo vy'ingwara zitandukira"
                    }

        CaseManagementPaneHeader encounterType ->
            case encounterType of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi wavuwe indwara zifatiyeho"
                    , kirundi = Just "Gukurikirana ingwara ikaze"
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterNutrition ->
                    { english = "Child Nutrition Follow Up"
                    , kinyarwanda = Just "Gukurikirana imirire y'umwana"
                    , kirundi = Just "Ikurikiranwa ryo gufungura k'umwana"
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Just "Kurondera uwo mwavuganye"
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa umubyeyi utwite"
                    , kirundi = Just "Ibipimo vya laboratware vy'imbanyi imbere yo kuvyara"
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa ufite indwara zitandura"
                    , kirundi = Just "Ibipimo vy'ingwara zitandukira"
                    }

        Celsius ->
            { english = "Celsius"
            , kinyarwanda = Just "Serisiyusi"
            , kirundi = Nothing
            }

        CelsiusAbbrev ->
            { english = "°C"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChartPhrase phrase ->
            translateChartPhrase phrase

        CheckAllThatApply ->
            { english = "Please check all that apply"
            , kinyarwanda = Just "Emeza ibiribyo/ibishoboka byose"
            , kirundi = Just "Muraraba ibisabwa vyose"
            }

        CheckIn ->
            { english = "Check in:"
            , kinyarwanda = Just "Kureba abaje"
            , kirundi = Just "Kwinjira"
            }

        Child0to5 ->
            { english = "Child (0-5)"
            , kinyarwanda = Just "Umwa (0-5)"
            , kirundi = Just "Umwana (0-5)"
            }

        Child6to24 ->
            { english = "Child (6-24)"
            , kinyarwanda = Just "Umwana (6-24)"
            , kirundi = Just "Umwnana (6-24)"
            }

        ChildCleanQuestion ->
            { english = "Is the sick child clean"
            , kinyarwanda = Just "Ese umwana urwaye afite isuku"
            , kirundi = Just "Mbega umwana agwaye arafise isuku"
            }

        ChildHasMalnutritionPhrase ->
            { english = "According to nutrition measurements, this child has acute malnutrition"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChildHmisNumber ->
            { english = "Child HMIS Number"
            , kinyarwanda = Just "Numero y'umwana muri HMIS"
            , kirundi = Just "Inomero y'umwana muri SIGS"
            }

        ChildIdentification ->
            { english = "Child Identification"
            , kinyarwanda = Just "Umwirondoro w'Umwana"
            , kirundi = Just "Umwirondoro w'umwana"
            }

        ChildNutritionSignLabel sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    , kirundi = Just "Ukwaguka kw'inda"
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    , kirundi = Just "Kutitaho ibintu"
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    , kirundi = Just "Uruhu gumye"
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "Kubura apeti /kunanirwa kurya"
                    , kirundi = Just "Akayabagu gake"
                    }

        Children ->
            { english = "Children"
            , kinyarwanda = Just "Abana"
            , kirundi = Just "Abana"
            }

        ChildrenNames ->
            { english = "Children's names"
            , kinyarwanda = Just "Amazina y'umwana"
            , kirundi = Just "Amazina y'abana"
            }

        ChildrenNationalId ->
            { english = "Children's National ID"
            , kinyarwanda = Just "Indangamuntu y'umwana"
            , kirundi = Just "Indangamuntu y'abana"
            }

        ChildScoreboardActivityTitle activity ->
            case activity of
                ChildScoreboardNCDA ->
                    { english = "Child Scorecard"
                    , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana"
                    , kirundi = Just "Ikarata y'ikurikiranwa ry'umwana"
                    }

                ChildScoreboardVaccinationHistory ->
                    { english = "Vaccination History"
                    , kinyarwanda = Just "Amakuru ku Nkingo"
                    , kirundi = Just "Akahise k'Urucanco"
                    }

        ChildScorecard ->
            { english = "Child Scorecard"
            , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana"
            , kirundi = Just "Ikarata y'ikurikiranwa ry'umwana"
            }

        ChooseOne ->
            { english = "Choose one"
            , kinyarwanda = Nothing
            , kirundi = Just "Hitamwo kimwe"
            }

        CHWAction value ->
            case value of
                ActionPregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    , kirundi = Just "Imibonano ijanye n'ivy'imbanyi"
                    }

                ActionLabs ->
                    { english = "Labs"
                    , kinyarwanda = Just "Ibizamini byafashwe"
                    , kirundi = Just "Ibipimo vy'ingwara"
                    }

                ActionDangerSignsPresent ->
                    { english = "Danger Signs Present"
                    , kinyarwanda = Just "Hagaragaye Ibimenyetso Mpuruza"
                    , kirundi = Just "Ibimenyetso vy'ingorane zikabije bihari"
                    }

                ActionReferredToHealthCenter ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe Ku Kigonderbuzima"
                    , kirundi = Just "Yarungitswe kw'ivuriro"
                    }

                ActionAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Just "Kwemeza Itariki yo Kugarukaho"
                    , kirundi = Just "Kwemeza umubonano"
                    }

                ActionHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku Buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                ActionBirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    , kirundi = Just "Umupango wo kuvyara"
                    }

        ChwActivity ->
            { english = "Chw Activity"
            , kinyarwanda = Just "Igikorwa cy'Umujyana w'Ubuzima"
            , kirundi = Just "Igikorwa c'Abaremeshakiyago"
            }

        ChildName ->
            { english = "Child Name"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Clear ->
            { english = "Clear"
            , kinyarwanda = Just "Gukuraho"
            , kirundi = Just "biragaragara"
            }

        ClickTheCheckMark ->
            { english = "Click the check mark if the mother / caregiver is in attendance. The check mark will appear green when a mother / caregiver has been signed in."
            , kinyarwanda = Just "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
            , kirundi = Just "Fyonda ku kemeza ko umurezi canke umuvyeyi ahari. Akamenyetso kemeza gaca gasa n'icatsi kibisi mu gihe umurezi/umuvyeyi ariwe kandi yinjijwe mu mashine."
            }

        ClinicType clinicType ->
            case clinicType of
                Achi ->
                    { english = "Achi"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Chw ->
                    { english = "CHW"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Fbf ->
                    { english = "Fbf"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Pmtct ->
                    { english = "Pmtct"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Sorwathe ->
                    { english = "Sorwathe"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Clinical ->
            { english = "Clinical"
            , kinyarwanda = Just "Amakuru y’ubuvuzi"
            , kirundi = Just "Ivyo kwa muganga"
            }

        Dashboard dashboard ->
            translateDashboard dashboard

        ClinicalProgressReport ->
            { english = "Clinical Progress Report"
            , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
            , kirundi = Just "Icegeranyo c'iterambera mu kuvugwa co kwa muganga"
            }

        CloseAcuteIllnessLabel ->
            { english = "or Close an Acute Illness"
            , kinyarwanda = Just "Cyangwa Ufunge Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Just "Canke Ugara ubugwayi bukomeye"
            }

        CloseAndContinue ->
            { english = "Close & Continue"
            , kinyarwanda = Nothing
            , kirundi = Just "Ugara hanyuma ubandanye"
            }

        Colline ->
            { english = "Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CollineSub ->
            { english = "Sub-Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ColorAlertIndication indication ->
            case indication of
                ColorAlertRed ->
                    translationSet ColorRed

                ColorAlertYellow ->
                    translationSet ColorYellow

                ColorAlertGreen ->
                    translationSet ColorGreen

        ColorGreen ->
            { english = "Green"
            , kinyarwanda = Just "Icyatsi"
            , kirundi = Just "Icatsi kibisi"
            }

        ColorRed ->
            { english = "Red"
            , kinyarwanda = Just "Umutuku"
            , kirundi = Just "Gitukura"
            }

        ColorYellow ->
            { english = "Yellow"
            , kinyarwanda = Just "Umuhondo"
            , kirundi = Just "Ibara risa n'umutoto uhishiye"
            }

        Commune ->
            { english = "Commune"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CompleteFacilityReferralForm facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Complete a health center referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi ku kigo Nderabuzima"
                    , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga rutangwa n'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Complete a hospital referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza ku bitaro"
                    , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga rutangwa n'ibitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Complete a referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo koherza umurwayi"
                    , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga"
                    }

                FacilityARVProgram ->
                    { english = "Complete an ARV services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza muri serivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga rutangwa n'igisata kiraba abafata imiti ya SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Complete a NCD services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi muri service y'indwara zitandura"
                    , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga rutangwa n'igisata kiraba ingwara zitandukira"
                    }

                FacilityANCServices ->
                    { english = "Complete an ANC services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi muri service serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FacilityUltrasound ->
                    { english = "Complete an ultrasound referral form"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Contacted114 ->
            { english = "Contacted 114"
            , kinyarwanda = Just "Namenyesheje 114"
            , kirundi = Just "Inimero yo kuvuganako 114"
            }

        ContactedHC ->
            { english = "Contacted health center"
            , kinyarwanda = Just "Yamenyesheje ikigo nderabuzima"
            , kirundi = Just "Ivuriro ryarondewe"
            }

        ContactedHCQuestion ->
            { english = "Have you contacted the health center"
            , kinyarwanda = Just "Wamenyesheje ikigo nderabuzima"
            , kirundi = Just "Mbega waravuganye n'ivuriro"
            }

        ContactedRecommendedSiteQuestion ->
            { english = "Did you contact the recommended site"
            , kinyarwanda = Just "Wamenyesheje urwego rushinzwe gukurikirana umurwayi"
            , kirundi = Just "Mbega waravuganye n'ikigigo ca bigenewe"
            }

        ContactInitiatedQuestion ->
            { english = "Where you able to speak with the contact"
            , kinyarwanda = Nothing
            , kirundi = Just "Vyoba vyarakunze ko muvugana n'awa muntu"
            }

        ContactName ->
            { english = "Contact Name"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ContactsTracingCompleteDetails ->
            { english = "Please fill in contact details"
            , kinyarwanda = Just "Uzuza amakuru arambuye y'umuntu wahuye n'umurwayi"
            , kirundi = Just "Nimwinjize umwirondoro ukwiye w'umuntu wo kurondera"
            }

        ContactsTracingHelper ->
            { english = "Please record everyone that the patient has come into contact within 2 days of their symptoms beginning"
            , kinyarwanda = Just "Andika umuntu wese wahuye n'umurwayi mu minshi 2 ishize ibimenyetso bigaragaye"
            , kirundi = Just "Andika abantu bose bahuye n'umugwayi mu minsi 2 ikurikira kuva ibimenyetso bitanguye"
            }

        ContactWithCOVID19SymptomsHelper ->
            { english = "Symptoms include:!!!! fever, dry cough, and shortness of breath"
            , kinyarwanda = Just "Ibimenyetso birimo: umuriro, inkorora y'akayi no guhumeka nabi"
            , kirundi = Just "Ibimenyetso harimwo: ubushuhe, inkorora yumye, hamwe n'ingabanuka ry'impwemu canke ukubura impwemu"
            }

        ContactWithCOVID19SymptomsQuestion ->
            { english = "Have you had contacts with others who exhibit symptoms or have been exposed to COVID-19"
            , kinyarwanda = Just "Waba warigeze uhura n'abantu bagaragaje ibimenyetso bya covid-19 cyangwa n'abari bafite ibyago byo kuyandura"
            , kirundi = Just "Waba warigeze guhura n'abandi bafise ibimenyetso canke bahuye n'abafise COVID-19"
            }

        Continued ->
            { english = "Continued"
            , kinyarwanda = Just "Yakomeje"
            , kirundi = Just "Yabandanije"
            }

        ContributingFactor factor ->
            case factor of
                FactorLackOfBreastMilk ->
                    { english = "Lack of breast milk (for children < 6 months)"
                    , kinyarwanda = Just "Kubura kw'amashereka (ku mwana uri munsi y'amezi atandatu)"
                    , kirundi = Just "Ibura ry'amaberebere (ku bana bari munsi ya mezi 6)"
                    }

                FactorMaternalMastitis ->
                    { english = "Maternal mastitis (for children < 6 months)"
                    , kinyarwanda = Just "Umubyeyi yabyimbye amabere (ku mwana uri munsi y'amezi atandatu)"
                    , kirundi = Just "Ingwara y'imoko kub'abavyeyi (ku bana bari munsi y'amezi 6)"
                    }

                FactorPoorSuck ->
                    { english = "Poor suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Just "Ugukwega kutakwiye"
                    }

                FactorDiarrheaOrVomiting ->
                    { english = "Diarrhea or vomiting"
                    , kinyarwanda = Just "Impiswi cyangwa kuruka"
                    , kirundi = Just "Gucibwamwo hamwe n'ukudahwa"
                    }

                NoContributingFactorsSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        ContributingFactorsQuestion ->
            { english = "Has patient or patient’s mother experienced any of the following"
            , kinyarwanda = Just "Umurwayi cyangwa umubyeyi we bagaragaje ibimenyetso bikurikira"
            , kirundi = Just "Mbega umurwayi canke nyina w'umurwayi arigera yumva canke abona ibi bikurikira"
            }

        ConvulsionsAndUnconsciousPreviousDelivery ->
            { english = "Experienced convulsions and resulted in becoming unconscious after delivery"
            , kinyarwanda = Just "Ubushize yaragagaye bimuviramo kutumva akimara kubyara"
            , kirundi = Just "Umurwayi/umuvyeyi yagize ibizunguzungu hama haziramwo uguta ubwenge mu gihe yarahejeje kuvyara"
            }

        ConvulsionsPreviousDelivery ->
            { english = "Experienced convulsions in previous delivery"
            , kinyarwanda = Just "Ubushize yaragagaye abyara"
            , kirundi = Just "Yarumvise ibizunguzungu igihe aheruka kwibaruka"
            }

        CSectionScar scar ->
            case scar of
                Vertical ->
                    { english = "Vertical"
                    , kinyarwanda = Just "Irahagaze"
                    , kirundi = Just "Irahagaze"
                    }

                Horizontal ->
                    { english = "Horizontal"
                    , kinyarwanda = Just "Iratambitse"
                    , kirundi = Just "Iraryamye"
                    }

                NoScar ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        Group ->
            { english = "Group"
            , kinyarwanda = Just "Itsinda"
            , kirundi = Just "Umurwi"
            }

        Groups ->
            { english = "Groups"
            , kinyarwanda = Just "Itsinda"
            , kirundi = Just "Imirwi"
            }

        Close ->
            { english = "Close"
            , kinyarwanda = Just "Gufunga"
            , kirundi = Just "Ugara"
            }

        Closed ->
            { english = "Closed"
            , kinyarwanda = Just "Gufunga"
            , kirundi = Just "Yugaye"
            }

        DeliveryComplication complication ->
            case complication of
                ComplicationGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yaje umugore utwite"
                    , kirundi = Just "Diyabete y'imbanyi"
                    }

                ComplicationEmergencyCSection ->
                    { english = "Emergency C-Section"
                    , kinyarwanda = Just "Kubagwa bitewe n'impamvu zihutirwa"
                    , kirundi = Just "Ikorwa ry'ihuta"
                    }

                ComplicationPreclampsia ->
                    { english = "Preeclampsia"
                    , kinyarwanda = Just "Umuvuduko w'amaraso uza uje k'umugore twite (Preclampsia)"
                    , kirundi = Just "Ivuduga ry'amaraso igihe c'imbanyi"
                    }

                ComplicationMaternalHemmorhage ->
                    { english = "Maternal Hemorrhage"
                    , kinyarwanda = Just "Kuva amaraso ku mubyeyi utwite cyangwa nyuma yo kubyara"
                    , kirundi = Just "Ukuva amaraso kw'abavyeyi"
                    }

                ComplicationHiv ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                ComplicationMaternalDeath ->
                    { english = "Maternal Death"
                    , kinyarwanda = Just "Urupfu rw'umubyeyi"
                    , kirundi = Just "Urupfu rw'abavyeyi"
                    }

                ComplicationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoDeliveryComplications ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        DeliveryComplicationsPresentQuestion ->
            { english = "Were there any complications with the delivery"
            , kinyarwanda = Just "Haba hari ibibazo umubyeyi yagize abyara"
            , kirundi = Just "Hoba hari ingorane zijanye no kuvyara, zijanye no gutanga"
            }

        DeliveryComplicationsSelectionLabel ->
            { english = "Which of the following were present"
            , kinyarwanda = Just "Ni ibiki byagaragaye muri ibi bikurikira"
            , kirundi = Just "Ninde muri aba bakurikira yari yaje"
            }

        ConditionImproving isImproving ->
            if isImproving then
                { english = "Improving"
                , kinyarwanda = Just "Ari koroherwa"
                , kirundi = Just "Guterimbere"
                }

            else
                { english = "Not improving"
                , kinyarwanda = Just "Ntabwo ari koroherwa"
                , kirundi = Just "Nta guterimbere"
                }

        ConditionImprovingQuestion ->
            { english = "Is your condition improving"
            , kinyarwanda = Just "Urumva uri koroherwa"
            , kirundi = Just "Mbega ubuzima bwawe buratera imbere"
            }

        ContactExposure ->
            { english = "Contact Exposure"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ContactInformation ->
            { english = "Contact Information"
            , kinyarwanda = Just "Uburyo bwakwifashishwa mu kugera ku mugenerwabikorwa"
            , kirundi = Just "Amakuru y'uwo mwavuganye"
            }

        Continue ->
            { english = "Continue"
            , kinyarwanda = Just "Gukomeza"
            , kirundi = Just "Ukubandanya"
            }

        CounselingTopic topic ->
            { english = topic.english
            , kinyarwanda = topic.kinyarwanda
            , kirundi = Nothing
            }

        CounselorReviewed ->
            { english = "I have reviewed the above with the participant."
            , kinyarwanda = Nothing
            , kirundi = Just "Nasuzumye ivyo biri hejuru hamwe n'uwitavye"
            }

        CovidContactTracing ->
            { english = "Covid Contact Tracing"
            , kinyarwanda = Just "Gushakisha abahuye n'uwanduye Covid-19"
            , kirundi = Just "Kurondera uwo babonanye afise Korona"
            }

        CovidTestingInstructions ->
            { english = "Perform a COVID-19 Rapid Test to confirm patient’s diagnosis"
            , kinyarwanda = Just "Kora ikizamini cyihuse cya Covid-19 kugira ngo hemezwe icyo umurwayi arwaye"
            , kirundi = Just "Igipimo cihuta ca Korona - 19 kugirango vyemerwe ko umugwayi asuzumwa"
            }

        CounselorSignature ->
            { english = "Entry Counselor Signature"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CSectionInPreviousDelivery ->
            { english = "C-section in previous delivery"
            , kinyarwanda = Just "Yarabazwe ku nda ishize"
            , kirundi = Just "Ugukorwa mu kwibaruka guheruka"
            }

        CSectionReason ->
            { english = "Reason for C-section"
            , kinyarwanda = Just "Impamvu yo kubagwa"
            , kirundi = Just "Impamvu yo gukorwa"
            }

        CSectionReasons reason ->
            case reason of
                Breech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Just "Ikirasi"
                    }

                Emergency ->
                    { english = "Emergency"
                    , kinyarwanda = Just "Ibyihutirwa"
                    , kirundi = Just "Igihe co kwihuta"
                    }

                FailureToProgress ->
                    { english = "Failure to Progress"
                    , kinyarwanda = Just "Ntibyiyongera"
                    , kirundi = Just "Kunanirwa gutera imbere"
                    }

                Backend.Measurement.Model.None ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

                Other ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                PreviousCSection ->
                    { english = "Previous c-section"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugokwa guheruka"
                    }

        CreateRelationship ->
            { english = "Create Relationship"
            , kinyarwanda = Just "Ibijyanye no guhuza amasano"
            , kirundi = Just "Shiraho umubonano"
            }

        CurrentlyPregnant ->
            { english = "Currently Pregnant"
            , kinyarwanda = Just "Aratwite"
            , kirundi = Just "Afise imbanyi ubu"
            }

        CurrentlyPregnantQuestion ->
            { english = "Is the patient currently pregnant"
            , kinyarwanda = Just "Umurwayi aratwite"
            , kirundi = Just "Mbega ubu umugwayi arafise imbanyi"
            }

        CurrentStock ->
            { english = "Current Stock"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChwDashboardLabel ->
            { english = "CHW Snapshot"
            , kinyarwanda = Just "Ishusho y'ibyagezweho"
            , kirundi = Just "Igicapo c'Abaremeshakiyago"
            }

        DashboardLabel ->
            { english = "Dashboard"
            , kinyarwanda = Just "Ikibaho cy’amakuru y’ingenzi"
            , kirundi = Just "Urubaho"
            }

        DateReceived ->
            { english = "Date Received"
            , kinyarwanda = Just "Italiki yakiriweho"
            , kirundi = Just "Itarike yakiriwe"
            }

        DeliveryLocation ->
            { english = "Delivery Location"
            , kinyarwanda = Just "Aho yabyariye"
            , kirundi = Just "Ahantu ho kuvyarira"
            }

        DangerSign sign ->
            case sign of
                VaginalBleeding ->
                    { english = "Vaginal bleeding"
                    , kinyarwanda = Just "Kuva"
                    , kirundi = Just "Ukuva amaraso mu gitsina"
                    }

                HeadacheBlurredVision ->
                    { english = "Severe headaches with blurred vision"
                    , kinyarwanda = Just "Kuribwa umutwe bidasanzwe ukareba ibikezikezi"
                    , kirundi = Just "Ukumeneka umutwe gukaze hamwe n'ukutabona neza"
                    }

                Convulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Just "Ukujugumira"
                    }

                AbdominalPain ->
                    { english = "Severe Abdominal pain"
                    , kinyarwanda = Just "Kuribwa mu nda bikabije"
                    , kirundi = Just "Ububabare bukaze bwo mu nda"
                    }

                DifficultyBreathing ->
                    { english = "Difficulty breathing"
                    , kinyarwanda = Just "Guhumeka nabi"
                    , kirundi = Just "Ingorane yo guhema"
                    }

                Backend.Measurement.Model.Fever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                ExtremeWeakness ->
                    { english = "Extreme weakness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    , kirundi = Just "Amagara make cane"
                    }

                ImminentDelivery ->
                    { english = "Imminent delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Just "Gutanga bigaragara"
                    }

                Labor ->
                    { english = "Labor"
                    , kinyarwanda = Just "Kujya ku nda"
                    , kirundi = Just "Ibise"
                    }

                LooksVeryIll ->
                    { english = "Looks very ill"
                    , kinyarwanda = Just "Ararembye cyane"
                    , kirundi = Just "Aboneka nk'uwugwaye cane"
                    }

                SevereVomiting ->
                    { english = "Severe vomiting"
                    , kinyarwanda = Just "Araruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    }

                Unconscious ->
                    { english = "Unconscious"
                    , kinyarwanda = Just "Yataye ubwenge"
                    , kirundi = Just "Uguta ubwenge"
                    }

                GushLeakingVaginalFluid ->
                    { english = "Gush or leaking of vaginal fluid"
                    , kinyarwanda = Just "Ibintu biva mu gitsina by'uruzi"
                    , kirundi = Just "Isohoka canke itakara ry'amazi yo mu gihimba c'irondoka c'umugore"
                    }

                PrematureOnsetContractions ->
                    { english = "Premature Onset of Contractions"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ibise vyatanguye hataragera"
                    }

                NoDangerSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso/nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        DangerSignsLabelForNurse ->
            { english = "Patient shows signs of"
            , kinyarwanda = Just "Umurwayi aragaragaza ibimenyetso bya"
            , kirundi = Just "Umugwayi yerekana ibimenyetso vya"
            }

        DangerSignsTask task ->
            case task of
                ReviewDangerSigns ->
                    { english = "Review Danger Signs"
                    , kinyarwanda = Just "Kureba ibimenyetso mpuruza"
                    , kirundi = Just "Subiramwo ibimenyetso vya bikomeye"
                    }

        Date ->
            { english = "Date"
            , kinyarwanda = Just "Itariki"
            , kirundi = Just "Itarike"
            }

        DateConcludedEstimatedQuestion ->
            { english = "What was the estimated due date for the child"
            , kinyarwanda = Just "Ni iyihe taliki yari iteganyijwe ko umubyeyi azabyariraho"
            , kirundi = Just "Ni iyihe tarike yari yagenwe k'umwana"
            }

        DateOfContact ->
            { english = "Date of Contact"
            , kinyarwanda = Just "Itariki bahuriyeho"
            , kirundi = Just "Itarike yo kubonana"
            }

        DatePregnancyConcluded ->
            { english = "Date Pregnancy Concluded"
            , kinyarwanda = Just "Itariki y'iherezo ryo gutwita"
            , kirundi = Just "Itarike imbanyi yarangiriyeko/yaherejeko"
            }

        DayAbbrev ->
            { english = "Day"
            , kinyarwanda = Just "Umu"
            , kirundi = Just "Umu"
            }

        DaySinglePlural value ->
            if value == 1 then
                { english = "1 Day"
                , kinyarwanda = Just "1 Umunsi"
                , kirundi = Just "1 Umunsi"
                }

            else
                { english = String.fromInt value ++ " Days"
                , kinyarwanda = Just <| String.fromInt value ++ " Iminsi"
                , kirundi = Just <| String.fromInt value ++ " Iminsi"
                }

        DateOfBirth ->
            { english = "Date of Birth"
            , kinyarwanda = Just "Itariki y'amavuko"
            , kirundi = Just "Itarike y'amavuko"
            }

        DaysAbbrev ->
            { english = "days"
            , kinyarwanda = Just "Imi"
            , kirundi = Just "Imi"
            }

        DaysPresent ->
            { english = "Days present"
            , kinyarwanda = Just "Igihe gishize"
            , kirundi = Just "Iminsi ihari"
            }

        DaysSinglePlural value ->
            if value == 1 then
                { english = "1 day"
                , kinyarwanda = Just "Umunsi 1"
                , kirundi = Just "Umunsi 1"
                }

            else
                { english = String.fromInt value ++ " days"
                , kinyarwanda = Just <| "Iminsi " ++ String.fromInt value
                , kirundi = Just <| "Iminsi " ++ String.fromInt value
                }

        Delete ->
            { english = "Delete"
            , kinyarwanda = Just "Gusiba"
            , kirundi = Just "Ugufuta"
            }

        DemographicInformation ->
            { english = "Demographic Information"
            , kinyarwanda = Just "Umwirondoro"
            , kirundi = Just "Amakuru y'ibiharuro vy'abantu"
            }

        DemographicsReport ->
            { english = "Demographics Report"
            , kinyarwanda = Just "Raporo y'umwirondoro"
            , kirundi = Just "Icegeranyo c'ibiharuro vy'abantu"
            }

        Details ->
            { english = "Details"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DetectableViralLoad ->
            { english = "Detectable Viral Load"
            , kinyarwanda = Just "Ingano ya virusi itera SIDA iracyagaragara mu maraso"
            , kirundi = Just "Imenyekana/igaragara ry'umugera (virisi)"
            }

        Device ->
            { english = "Device"
            , kinyarwanda = Just "Igikoresho"
            , kirundi = Just "Igikoresho"
            }

        DeviceNotAuthorized ->
            { english =
                """This device has not yet been authorized to sync data with the backend, or the
                authorization has expired or been revoked. To authorize or re-authorize this
                device, enter a pairing code below. This will permit sensitive data to be stored
                on this device and updated to the backend. You should only authorize devices that
                are under your control and which are secure."""
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DeviceStatus ->
            { english = "Device Status"
            , kinyarwanda = Just "Uko igikoresho cy'ikoranabuhanga gihagaze"
            , kirundi = Just "Ingene igikoresho kimeze"
            }

        Diabetes ->
            { english = "Diabetes"
            , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
            , kirundi = Just "Diyabete"
            }

        DiagnosedAtAnotherFacilityPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe"
            , kirundi = Just "Wasuzumwe"
            }

        DiagnosedAtAnotherFacilitySuffix ->
            { english = "at another facility and were given medication. Which medication was given?"
            , kinyarwanda = Just "Ku rindi vuriro wagiyeho ugahabwa imiti. Ni iyihe miti wahawe?"
            , kirundi = Just "Kurindi vuriro kandi wararonse imiti. wahawe imiti iyihe?"
            }

        DiagnosedByOutsideCare ->
            { english = "Diagnosed by outside care"
            , kinyarwanda = Just "Yasuzumiwe ku rindi vuriro"
            , kirundi = Just "Gusuzumwa vyakorewe hanze"
            }

        DiagnosedOn ->
            { english = "Diagnosed on"
            , kinyarwanda = Nothing
            , kirundi = Just "Yasuzumwe"
            }

        Diagnosis ->
            { english = "Diagnosis"
            , kinyarwanda = Just "Uburwayi bwabonetse"
            , kirundi = Just "Isuzumwa"
            }

        DiagnosisDate ->
            { english = "Diagnosis Date"
            , kinyarwanda = Just "Itariki y'Isuzuma"
            , kirundi = Just "Itarike yo gusuzuma"
            }

        DifferenceBetweenDueAndDeliveryDates ->
            { english = "Difference between due date and delivery date"
            , kinyarwanda = Just "Ikinyuranyo kiri hagati y'amatariki"
            , kirundi = Just "Itandukanirizo riri hagati y'itarike itegekanijwe kuvyara niy'umunsi avyariyeko"
            }

        Disabled ->
            { english = "Disabled"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DistributionNotice notice ->
            case notice of
                DistributedFully ->
                    { english = "Complete"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                DistributedPartiallyLackOfStock ->
                    { english = "Lack of stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    , kirundi = Just "Ibura ry'ububiko"
                    }

                DistributedPartiallyOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Izindi mpamvu"
                    , kirundi = Just "Ibindi"
                    }

        District ->
            { english = "District"
            , kinyarwanda = Just "Akarere"
            , kirundi = Just "Akarere"
            }

        DOB ->
            { english = "DOB"
            , kinyarwanda = Nothing
            , kirundi = Just "DDN"
            }

        DropzoneDefaultMessage ->
            { english = "Touch here to take a photo, or drop a photo file here."
            , kinyarwanda = Just "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
            , kirundi = Just "Fyonda ngaha mu gufata ifoto, canke ukororere ngaha amafoto"
            }

        DueDate ->
            { english = "Due Date"
            , kinyarwanda = Just "Itariki azabyariraho"
            , kirundi = Just "Itarike ntarengwa"
            }

        DueTo ->
            { english = "Due to"
            , kinyarwanda = Just "Kubera"
            , kirundi = Just "Bitarenze"
            }

        EarlyChildhoodDevelopment ->
            { english = "Early Childhood Development"
            , kinyarwanda = Just "Gahunda ikomatanije y'imikurire"
            , kirundi = Just "Iterambere mu gukura ry'Umwana Mutoyi (DPE -IUM)"
            }

        EarlyMastitisOrEngorgmentReliefMethod method ->
            case method of
                ReliefMethodBreastMassage ->
                    { english = "Massage"
                    , kinyarwanda = Just "Korera amabere masage"
                    , kirundi = Just "Ubutumwa"
                    }

                ReliefMethodIncreaseFluid ->
                    { english = "Increase fluid"
                    , kinyarwanda = Just "Ongera ibyo kunywa"
                    , kirundi = Just "Kongereza amazi"
                    }

                ReliefMethodBreastfeedingOrHandExpression ->
                    { english = "continue breastfeeding or use hand expression"
                    , kinyarwanda = Just "komeza konsa cyangwa ukoreshe ikiganza wikame"
                    , kirundi = Just "Kubandanya wonsa umwana canke ukoreshe ukuboka ukuremwo amata"
                    }

        ECDSignQuestion sign ->
            case sign of
                FollowMothersEyes ->
                    { english = "Does the child follow their mothers eyes"
                    , kinyarwanda = Just "Umwana akurikiza amaso nyina"
                    , kirundi = Just "Mbega umwana arakurikira yihweza amaso ya nyina"
                    }

                MoveArmsAndLegs ->
                    { english = "Does the child move their arms and legs"
                    , kinyarwanda = Just "Umwana anyeganyeza amaboko n'amaguru"
                    , kirundi = Just "Mbega umwana arashobora gukoresha amaguru hamwe n'amaboko yiwe"
                    }

                RaiseHandsUp ->
                    { english = "Does the child raise their hands up"
                    , kinyarwanda = Just "Umwana azamura ibiganza"
                    , kirundi = Just "Mbega umwana araduza ibiganza"
                    }

                Smile ->
                    { english = "Does the child smile"
                    , kinyarwanda = Just "Umwana araseka"
                    , kirundi = Just "Mbega umwana aratwenga"
                    }

                RollSideways ->
                    { english = "Does the child roll from left to right and right to left"
                    , kinyarwanda = Just "Umwana yihindukiza ku mpande, iburyo ni’ibumoso ikindi gihe akagana ibumoso n'iburyo"
                    , kirundi = Just "Mbega umwana aritembagaza kuva ibubamfu gushika iburyo no kumva iburyo gushika ibubamfu"
                    }

                BringHandsToMouth ->
                    { english = "Does the child bring their hands to their mouth"
                    , kinyarwanda = Just "Umwana akoza ibiganza bye ku murwa"
                    , kirundi = Just "Mbega umwana arashira ibiganza vyiwe mu kanwa"
                    }

                HoldHeadWithoutSupport ->
                    { english = "Does the child hold their head steady without support"
                    , kinyarwanda = Just "Umwana abasha kwemesha umutwe we ubwe ntawumufashe"
                    , kirundi = Just "Mbega umwana arashobora guhagarika umutwe atawumufashije"
                    }

                HoldAndShakeToys ->
                    { english = "Does the child hold and shake toys and swing at dangling toys"
                    , kinyarwanda = Just "Umwana ashobora gufata akanazunguza ibikinisho ndetse akabinyeganyeza iyo afite ibikinisho bivuga"
                    , kirundi = Just "Mbega umwana arafata kandi akazungangiza igipupe akondera akaja ku kivuma/uruvuma"
                    }

                ReactToSuddenSounds ->
                    { english = "Does the child react to sudden noises or sounds"
                    , kinyarwanda = Just "Umwana agaragaza ko yumvise amajwi cg urusaku bitunguranye"
                    , kirundi = Just "Mbega umwana arashiduka iyo yumvise induru atari yiteze canke ijwi"
                    }

                UseConsonantSounds ->
                    { english = "Is the child using consonant sounds in babbling, for example “da, da, da”"
                    , kinyarwanda = Just "Umwana akoresha amajwi yumvikanamo inyuguti igihe yivugisha, urugero:da,da,da,.."
                    , kirundi = Nothing
                    }

                RespondToSoundWithSound ->
                    { english = "Does the child respond to sound by making sound"
                    , kinyarwanda = Just "Umwana asubirisha ijwi igihe yumvise irindi ijwi"
                    , kirundi = Just "Mbega umwana arishura n'ijwi ryiwe kw'ijwi yumvise"
                    }

                TurnHeadWhenCalled ->
                    { english = "Does the child turn their head when their name is called"
                    , kinyarwanda = Just "Umwana ahindukiza umutwe iyo hari uhamagaye izina rye"
                    , kirundi = Just "Mbega umwana arahindukiza umutwe iyo izina ryiwe urihamagaye"
                    }

                SitWithoutSupport ->
                    { english = "Can the child sit without support for a short while, for example sit on the floor on their own"
                    , kinyarwanda = Just "Umwana ashobora kwicara akanya gato nta kintu cyangwa umuntu umufashe"
                    , kirundi = Just "Mbega umwana arashobora kwicara gatoya atawumufashije, nk'akarorero kwicara hasi (mu nzu) wenyene?"
                    }

                SmileBack ->
                    { english = "Does the child smile back at you"
                    , kinyarwanda = Just "Umwana yaba agusekera iyo umusekeye"
                    , kirundi = Just "Mbega umwana aragutwengera"
                    }

                RollTummyToBack ->
                    { english = "Can the child roll from their tummy to their back on their own"
                    , kinyarwanda = Just "Umwana ashobora kubura inda akagarama nta muntu umufashije"
                    , kirundi = Just "Mbega umwana arashobora kwitembagaza kuva ku nda gushika k'umugongo wenyene"
                    }

                ReachForToys ->
                    { english = "Does the child reach for nearby toys on their own"
                    , kinyarwanda = Just "Umwana ashobora gufata ibikinisho bimwegereye"
                    , kirundi = Just "Mbega umwana arashikira igipupe kiri hagufi yiwe"
                    }

                UseSimpleGestures ->
                    { english = "Does the child use simple gestures such as waving “bye-bye”"
                    , kinyarwanda = Just "Umwana akoresha ibimenyetso byoroheje nko gupepera iyo musezeranaho"
                    , kirundi = Just "Mbega umwana arakoresha ibimenyetso bisanzwe nko kumanika amaboko avuga “ni agasaga“/“bayi-bayi“"
                    }

                StandOnTheirOwn ->
                    { english = "Can the child stand on their own"
                    , kinyarwanda = Just "Umwana ashobora guhagarara nta muntu umufashe"
                    , kirundi = Just "Mbega umwana arashobora guhagarara wenyene, atawumufashe?"
                    }

                CopyDuringPlay ->
                    { english = "Does the child copy you during play"
                    , kinyarwanda = Just "Umwana yigana ibyo urimo gukora igihe mukina"
                    , kirundi = Just "Mbega umwana arakwigana mu gihe co gukina"
                    }

                SayMamaDada ->
                    { english = "Does the child say “mama” and “dada”"
                    , kinyarwanda = Just "Umwana ashobora kuvuga “mama” cyangwa “dada”"
                    , kirundi = Just "Mbega umwana aravuga “mama“ canke “papa“"
                    }

                CanHoldSmallObjects ->
                    { english = "Can the child hold small objects that fit inside their hands"
                    , kinyarwanda = Just "Umwana ashobora gufata ibintu bito bikwiye mu kiganza cye"
                    , kirundi = Just "Mbega umwana arashobora gufata mu ntoke utuntu dutoduto dukwigwa mu kiganza ciwe"
                    }

                LooksWhenPointedAt ->
                    { english = "Does the child look at something when you point to it and say “look”"
                    , kinyarwanda = Just "Umwana ashobora kwerekeza amaso ku kintu cyose umweretse"
                    , kirundi = Just "Mbega umwana araraba ikintu iyo igutumye urutoke hama ukamubwira “raba“"
                    }

                UseSingleWords ->
                    { english = "Does the child use several single words to get what they want"
                    , kinyarwanda = Just "Umwana akoresha amagambo mato mato kandi yungikanye ashaka kugira icyo akubwira /agusaba"
                    , kirundi = Just "Mbega umwana arakoresha amajambo menshi yigenga kugira aronke ico ashaka"
                    }

                WalkWithoutHelp ->
                    { english = "Does the child walk without help"
                    , kinyarwanda = Just "Umwana ashobora kugenda nta muntu umufashije"
                    , kirundi = Just "Mbega umwana arashobora kugenda ata muntu amufashije"
                    }

                PlayPretend ->
                    { english = "Does the child play pretend - like talking on a toy phone"
                    , kinyarwanda = Just "Umwana ajya akina asa nk'uvugira kuri telefoni"
                    , kirundi = Just "Mbega umwana arakina yigirisha mukuvugira kw'iterefone y'igipupe"
                    }

                PointToThingsOfInterest ->
                    { english = "Does the child point to interesting things"
                    , kinyarwanda = Just "Umwana atunga intoki ibintu bimunejeje"
                    , kirundi = Just "Mbega umwana aratuma urutoke ku bintu bimuhimbara"
                    }

                UseShortPhrases ->
                    { english = "Does the child use 2-4 word phrases"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro igizwe n'amagambo 2 kugera kuri 4"
                    , kirundi = Just "Mbega umwana arakoresha amajambo 2-4"
                    }

                InterestedInOtherChildren ->
                    { english = "Does the child show interest in other children"
                    , kinyarwanda = Just "Umwana agaragaza ko yishimiye abandi bana"
                    , kirundi = Just "Mbega umwana arerekana ko yitayeho abandi bana"
                    }

                FollowSimpleInstructions ->
                    { english = "Does the child follow simple instructions"
                    , kinyarwanda = Just "Umwana akurikiza amabwiriza yoroheje ahawe"
                    , kirundi = Just "Mbega umwana arakurikira inyigisho zisazwe"
                    }

                KickBall ->
                    { english = "Can the child kick a ball"
                    , kinyarwanda = Just "Umwana ashobora gutera umupira"
                    , kirundi = Just "Mbega umwana arashobora gukubita umupira?"
                    }

                PointAtNamedObjects ->
                    { english = "Does the child point to something - like a toy or a picture - when you name it"
                    , kinyarwanda = Just "Umwana ashobora kukwereka ikintu agitunga urutoki iyo uvuze izina ryacyo, Urugero:Igikinisho cg ifoto"
                    , kirundi = Just "Mbega umwana aratuma urutoke ku kintu - nk'igipupe canke isanamu - mu gihe ukivuze izina"
                    }

                DressThemselves ->
                    { english = "Can the child dress themselves"
                    , kinyarwanda = Just "Umwana ashobora kwiyambika"
                    , kirundi = Just "Mbega umwana arashobora kwiyambika impunzu?"
                    }

                WashHandsGoToToiled ->
                    { english = "Can the child wash their hands on their own and go to the toilet in the designated area on their own"
                    , kinyarwanda = Just "Umwana ashobora kwikarabya intoki, akanijyana mu bwiherero ahateganijwe wenyine"
                    , kirundi = Just "Mbega umwana arashobora kwikarabisha intoke wenyene kandi akagenda kwiherera/kwituma mu kazu ka sugumwe kari mu kibaza ca bigenewe atawumufashije/wenyene?"
                    }

                KnowsColorsAndNumbers ->
                    { english = "Does the child know basic colors and numbers"
                    , kinyarwanda = Just "Umwana azi amabara n'imibare by'ibanze"
                    , kirundi = Just "Mbega umwana arazi amabara hamwe n'ibiharuro vy'ibanze/vy'intango/vy'umushinge"
                    }

                UseMediumPhrases ->
                    { english = "Does the child use 4-5 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo 2 kugera kuri 4"
                    , kirundi = Just "Mbega umwana arakoresha amungane 4-5"
                    }

                PlayMakeBelieve ->
                    { english = "Does the child play make-believe"
                    , kinyarwanda = Just "Umwana akunda gukina yigana"
                    , kirundi = Just "Mbega umwana arakina ivyo kwigirisha"
                    }

                FollowThreeStepInstructions ->
                    { english = "Does the child follow 3-step commands - like “get dressed, comb your hair, and wash your face“"
                    , kinyarwanda = Just "Umwana ashobora gukurikiza amabwiriza nka 3 aherewe icyarimwe, Urugero:Ambara, Sokoza umusatsi unakarabe mu maso"
                    , kirundi = Just "Mbega umwana arakurikira intambwe 3 zagenwe/ibice 3 vyagenwe nka “kwambara, kwisokoza hamwe no kwiyonga mu maso“"
                    }

                StandOnOneFootFiveSeconds ->
                    { english = "Can the child hop and stand on one foot for up to 5 seconds"
                    , kinyarwanda = Just "Umwana ashobora guhagarara ku kaguru kamwe akandi kanenetse mu gihe cy'amasegonda 5"
                    , kirundi = Just "Mbega umwana arashobora gusimba no guhagarara ku kirenge kimwe/k'ukuguru kumwe gushika ku masegonda atanu (5)"
                    }

                UseLongPhrases ->
                    { english = "Does the child use 5-6 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo atanu cyangwa atandatu"
                    , kirundi = Just "Mbega umwana arakoresha amungane 5-6"
                    }

                ShareWithOtherChildren ->
                    { english = "Does the child share and take turns with other children"
                    , kinyarwanda = Just "Umwana ashobora gusangira no kujya ahererekanya ibintu n'abandi bana"
                    , kirundi = Just "Mbega umwana arasangira kandi agasabikanya n'abandi bana"
                    }

                CountToTen ->
                    { english = "Can the child count to 10"
                    , kinyarwanda = Just "Umwana ashobora kubara kugeza ku 10"
                    , kirundi = Just "Mbega umwana arashobora guharura gushika kw'icumi (10)"
                    }

                NoECDSigns ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        ECDStatus status ->
            case status of
                StatusOnTrack ->
                    { english = "On Track"
                    , kinyarwanda = Just "Biri ku gihe"
                    , kirundi = Just "Ku mrurongo"
                    }

                StatusECDBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Biri inyuma"
                    , kirundi = Just "Inyuma"
                    }

                StatusOffTrack ->
                    { english = "Off Track"
                    , kinyarwanda = Just "Ntibyakozwe"
                    , kirundi = Just "Kureka inzira"
                    }

                NoECDStatus ->
                    { english = "No Status"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Edd ->
            { english = "EDD"
            , kinyarwanda = Just "Itariki y'agateganyo yo kubyara"
            , kirundi = Just "Itarike Yagereranijwe yo Kuvyara"
            }

        EddHeader ->
            { english = "Estimated Date of Delivery"
            , kinyarwanda = Just "Itariki y'agateganyo azabyariraho"
            , kirundi = Just "Itarike Yagereranijwe yo Kuvyara"
            }

        Edema ->
            { english = "Edema"
            , kinyarwanda = Just "Kubyimba"
            , kirundi = Just "Ukuvyimba amaguru"
            }

        Ega ->
            { english = "EGA"
            , kinyarwanda = Just "Ibyumweru inda imaze"
            , kirundi = Just "Ikigereranyo c'Imyaka yo Gusama"
            }

        EgaHeader ->
            { english = "Estimated Gestational Age"
            , kinyarwanda = Just "Amezi y'agateganyo y'inda"
            , kirundi = Just "Ikigereranyo c'Imyaka yo Gusama"
            }

        EgaWeeks ->
            { english = "EGA (Weeks)"
            , kinyarwanda = Just "EGA (Ibyumweru)"
            , kirundi = Just "AGE (Indwi)"
            }

        ElevatedRespiratoryRate ->
            { english = "Elevated respiratory rate"
            , kinyarwanda = Just "Inshuro ahumeka zazamutse"
            , kirundi = Just "Umuvuduko ukabije m'uguhema"
            }

        EmergencyReferralHelperReferToHC ->
            { english = "Refer patient to health center immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku kigonderabuzima byihuse"
            , kirundi = Just "Rungika umugwayi kw'ivuriro ubu nyene"
            }

        EmergencyReferralHelperReferToHospitalForImmediateDelivery ->
            { english = "Refer patient to hospital for immediate delivery"
            , kinyarwanda = Just "Ohereza umubyeyi aka kanya ku bitaro abyarireyo"
            , kirundi = Just "Rungika umuvyeyi ku bitaro kwibaruka ubu nyene"
            }

        EmergencyReferralHelperReferToHospitalImmediately ->
            { english = "Refer patient to hospital immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku bitaro byihuse"
            , kirundi = Just "Rungika umugwayi ku bitaro ubu nyene"
            }

        EmergencyReferralHelperReferToMaternityWard ->
            { english = "Refer to Maternity Ward Immediately"
            , kinyarwanda = Just "Ihutire kohereza umubyeyi aho babyarira"
            , kirundi = Just "Rungika mu gisata kiraba abavyeyi ubu nyene"
            }

        EmergencyReferralHelperReferToEmergencyObstetricCareServices ->
            { english = "Stabilize and Refer to Emergency Obstetric Care Services"
            , kinyarwanda = Just "Tanga umuti w'ibanze uhite wohereza umubyeyi muri serivice zita ku babyeyi"
            , kirundi = Just "Tekanya hama urungike umuvyeyi mu gisata kiraba ivyo kwibaruka vyihuta"
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        EncounterDate ->
            { english = "Encounter Date"
            , kinyarwanda = Just "Itariki igikorwa cyakoreweho"
            , kirundi = Just "Itarike y'umubonano"
            }

        EncounterTypeFollowUpQuestion encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Do you want to start a subsequent Acute Illness encounter for"
                    , kinyarwanda = Just "Urashaka Gutangira Ibikorwa bikurikiyeho ku burwayi bwa"
                    , kirundi = Just "Mbega urashaka gutangura gukurikirana ubugwayi bukomeye bwa"
                    }

                AntenatalEncounter ->
                    { english = "What type of Antenatal encounter would you like to start for"
                    , kinyarwanda = Just "Ni irihe suzuma ku mugore utwite ushaka gutangira kuri"
                    , kirundi = Just "Ni ubuhe bwoko bwo guhura mu gihe c'imbanyi ipfuza gutangura"
                    }

                ChildScoreboardEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "Do you want to start a Home Visit assessment for"
                    , kinyarwanda = Just "Urashaka gutangira igikorwa cyo gusura mu rugo"
                    , kirundi = Just "Mbega urashaka gutangura kugendera ingo kugira ukore isuzuma ry'ibikorwa rya"
                    }

                InmmunizationEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        EncounterTypePageLabel page ->
            case page of
                AcuteIllnessPage subPage ->
                    case subPage of
                        OverviewPage ->
                            { english = "Overview"
                            , kinyarwanda = Just "Ishusho Rusange"
                            , kirundi = Just "Incamake"
                            }

                        Covid19Page ->
                            { english = "COVID-19"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        MalariaPage ->
                            { english = "Malaria"
                            , kinyarwanda = Just "Malariya"
                            , kirundi = Just "Malariya"
                            }

                        GastroPage ->
                            { english = "Gastro"
                            , kinyarwanda = Just "Indwara yo mu nda"
                            , kirundi = Just "Amara"
                            }

                NutritionPage ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'Umwana"
                    , kirundi = Just "Ugufungura k'umwana"
                    }

                AntenatalPage ->
                    translationSet AntenatalCare

        EncounterWarningForDiagnosisPane warning suffix ->
            let
                suffix_ =
                    if not (String.isEmpty suffix) then
                        " - " ++ suffix

                    else
                        ""
            in
            case warning of
                WarningECDMilestoneBehind ->
                    { english = "Missed ECD Milestone" ++ suffix_
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WarningECDMilestoneReferToSpecialist ->
                    { english = "Missed ECD Milestone" ++ suffix_
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WarningHeadCircumferenceMicrocephaly ->
                    { english = "Microcephaly"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WarningHeadCircumferenceMacrocephaly ->
                    { english = "Macrocephaly"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        EndEncounter ->
            { english = "End Encounter"
            , kinyarwanda = Just "Rangiza ibyo wakoraga"
            , kirundi = Just "Guheza kubonana"
            }

        EndEncounterQuestion ->
            { english = "End Encounter?"
            , kinyarwanda = Just "Gusoza igikorwa?"
            , kirundi = Just "Guheza kubonana?"
            }

        EndGroupEncounter ->
            { english = "End Group Encounter"
            , kinyarwanda = Just "Gusoza igikorwa"
            , kirundi = Just "Guheza kubonano y'umurwi"
            }

        EnrolNewborn ->
            { english = "Enroll Newborn"
            , kinyarwanda = Just "Andika Uruhinja"
            , kirundi = Just "Kwandika uruyoya"
            }

        EnrolNewbornHelper enrolled ->
            if enrolled then
                { english = "Newborn is already enrolled"
                , kinyarwanda = Just "Uruhinja rusanzwe rwanditse"
                , kirundi = Just "Umwana yavutse yamaze kwandikwa"
                }

            else
                { english = "Click on 'Enroll Newborn' button to perform enrollment"
                , kinyarwanda = Just "Kanda kuri 'Andika Uruhinja' kugira ngo urwandike"
                , kirundi = Just "Fyonda kuri 'Injiza umwana akivuka' kugira yinjizwe mu mashine"
                }

        EnrollToProgramAction ->
            { english = "Enroll patient in program and direct them to the next program session"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        EnrollToProgramQuestion ->
            { english = "Have you enrolled the patient in the appropriate nutrition program"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        EnterPairingCode ->
            { english = "Enter pairing code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            , kirundi = Just "andika igitigiri kabanga co guhuza"
            }

        EntryStatusAntenatal status ->
            case status of
                StatusOngoing ->
                    { english = "Open"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                StatusResolved ->
                    { english = "Concluded"
                    , kinyarwanda = Just "Byasojwe"
                    , kirundi = Just "Uguheraheza"
                    }

        EntryStatusDiagnosis status ->
            case status of
                StatusOngoing ->
                    { english = "Ongoing"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kubandanya"
                    }

                StatusResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Cakemutse"
                    }

        MemoryQuota quota ->
            { english = "Memory used " ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya wa memori (ushobora kubika amakuru igihe gito) ungana na MB" ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " kuri MB" ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024))
            , kirundi = Just <| "Memoire imaze gukoreshwa ungana na MO(Mégaoctets) " ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " kuri MO(Mégaoctets)" ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024))
            }

        StorageQuota quota ->
            { english = "Storage used " ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.quota // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya ungana na MB" ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " umwanya wose ungana na MB " ++ String.fromInt (quota.quota // (1024 * 1024))
            , kirundi = Just <| "Ububiko bwakoreshejwe bungana na MO(Mégaoctets) " ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " umwanya wose ungana na MO(Mégaoctets) " ++ String.fromInt (quota.quota // (1024 * 1024))
            }

        SubmitPairingCode ->
            { english = "Submit Pairing Code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            , kirundi = Just "Tanga ikode yo kuringanisha"
            }

        EPDSPreformedOn ->
            { english = "EPDS performed on"
            , kinyarwanda = Just "Igipimo cy'ukuntu yiyumva nyuma yo kubyara cyakozwe kuwa"
            , kirundi = Just "Igipimo co kwihebura inyuma yo kuvyara ca Edinburgh "
            }

        EpisiotomyOrPerinealTearQuestion ->
            { english = "Did the patient have an episiotomy or a perineal tear"
            , kinyarwanda = Just "Umubyeyi baramwongereye cg yaracitse abyara"
            , kirundi = Just "Mbega umugwayi yaragize 'épisiotomie' canke ukwagura igihimba c'irondoka kugira umwana avuke"
            }

        EpisiotomyOrPerinealTearHealingQuestion ->
            { english = "Is it healing normally"
            , kinyarwanda = Just "Igisebe kiri gukira neza"
            , kirundi = Just "Hariko harakira bisanzwe"
            }

        ErrorCheckLocalConfig ->
            { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorConfigurationError ->
            { english = "Configuration error"
            , kinyarwanda = Just "Ikosa mu igena miterere"
            , kirundi = Just "Ikosa ry'iboneza"
            }

        Estimated ->
            { english = "Estimated"
            , kinyarwanda = Just "Itariki y'amavuko igenekerejwe"
            , kirundi = Just "Bigereranijwe"
            }

        ExaminationTask task ->
            case task of
                Vitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Just "Ivyangombwa"
                    }

                Pages.Prenatal.Activity.Types.NutritionAssessment ->
                    translationSet NutritionAssessmentLabel

                CorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Just "Isuzuma ryimbitse"
                    , kirundi = Just "Igipimo c'umubiri c'intango"
                    }

                ObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Just "Ibipimo by'inda"
                    , kirundi = Just "Gupima ivyara"
                    }

                Pages.Prenatal.Activity.Types.BreastExam ->
                    translationSet BreastExam

                GUExam ->
                    { english = "GU Exam"
                    , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina n'inzira z'inkari"
                    , kirundi = Just "Igipimo c'Ibihimba vy'irondoka hamwe n'amafyigo"
                    }

        ExaminationTaskRecurrent task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.ExaminationVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Just "Ivyangombwa"
                    }

        ExpirityDate ->
            { english = "Expirity Date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ExposureTask task ->
            case task of
                ExposureTravel ->
                    { english = "Travel History"
                    , kinyarwanda = Just "Amakuru y'ingendo wakoze"
                    , kirundi = Just "Akahise k'ingendo"
                    }

                ExposureExposure ->
                    { english = "Contact Exposure"
                    , kinyarwanda = Just "Abantu mwahuye"
                    , kirundi = Just "Ukwerekana kumenyana"
                    }

        Extremities ->
            { english = "Extremities"
            , kinyarwanda = Just "Ku mpera z'ibice by'umubiri (ibiganza,ibirenge)"
            , kirundi = Just "Ibirenze"
            }

        Eyes ->
            { english = "Eyes"
            , kinyarwanda = Just "Amaso"
            , kirundi = Just "Amaso"
            }

        Facility ->
            { english = "Facility"
            , kinyarwanda = Just "Ivuriro"
            , kirundi = Just "Ikigo"
            }

        FamilyInformation ->
            { english = "Family Information"
            , kinyarwanda = Just "Amakuru ku muryango"
            , kirundi = Just "Amakuru y'umuryango"
            }

        FamilyMembers ->
            { english = "Family Members"
            , kinyarwanda = Just "Abagize umuryango"
            , kirundi = Just "Abagize umuryango"
            }

        FamilyPlanningCurentlyQuestion ->
            { english = "Which, if any, of the following methods do you use"
            , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha"
            , kirundi = Just "Ni ubuhe, nimba buhari, m'uburyo bukurikira ukoresha"
            }

        FamilyPlanningInFutureQuestion ->
            { english = "Which, if any, of these methods will you use after your pregnancy"
            , kinyarwanda = Just "Niba buhari, ni ubuhe buryo uzakoresha nyuma yo kubyara?"
            , kirundi = Just "Ni ubuhe, nimba buhari, uburyo uzokoresha inyuma y'imbanyi yawe"
            }

        FamilyPlanningSignLabel sign ->
            case sign of
                AutoObservation ->
                    { english = "Auto-observation"
                    , kinyarwanda = Just "Kwigenzura ururenda"
                    , kirundi = Just "Ukwiyihweza"
                    }

                Condoms ->
                    { english = "Condoms"
                    , kinyarwanda = Just "Udukingirizo"
                    , kirundi = Just "udukingirizo"
                    }

                CycleBeads ->
                    { english = "Cycle beads"
                    , kinyarwanda = Just "Urunigi"
                    , kirundi = Just "Ubudede bwo guharura kuja mu kwezi"
                    }

                CycleCounting ->
                    { english = "Cycle counting"
                    , kinyarwanda = Just "Kubara "
                    , kirundi = Just "Uguharura kuja mu kwezi"
                    }

                Hysterectomy ->
                    { english = "Hysterectomy"
                    , kinyarwanda = Just "Bakuyemo nyababyeyi"
                    , kirundi = Just "Ugukuramwo igitereko"
                    }

                Implants ->
                    { english = "Implants"
                    , kinyarwanda = Just "Akapira ko mu kaboko"
                    , kirundi = Just "Ibibandikano"
                    }

                Injectables ->
                    { english = "Injectables"
                    , kinyarwanda = Just "Urushinge"
                    , kirundi = Nothing
                    }

                IUD ->
                    { english = "IUD"
                    , kinyarwanda = Just "Akapira ko mu mura (agapira ko munda ibyara)"
                    , kirundi = Just "Akanyuzi"
                    }

                LactationAmenorrhea ->
                    { english = "Lactation amenorrhea"
                    , kinyarwanda = Just "Uburyo bwo konsa"
                    , kirundi = Just "Kutaja mu kwezi (mu gihe umuvyeyi wonsa)"
                    }

                NoFamilyPlanning ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta buryo bwo kuboneza urubyaro yahisemo"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                OralContraceptives ->
                    { english = "Oral contraceptives"
                    , kinyarwanda = Just "Ibinini"
                    , kirundi = Nothing
                    }

                Spermicide ->
                    { english = "Spermicide"
                    , kinyarwanda = Just "Ibinini byica intangangabo bicishwa mu gitsina"
                    , kirundi = Just "Ivyo guhagarika gusama imbanyi ucishije mu kanwa"
                    }

                TubalLigatures ->
                    { english = "Tubal ligatures"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagore"
                    , kirundi = Just "Ugukata imiringoti izana amagi y'umugore mu gitereko"
                    }

                Vasectomy ->
                    { english = "Vasectomy"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagabo"
                    , kirundi = Just "ugukata imiringoti izana intanga z'abagabo"
                    }

        FamilyUbudehe ->
            { english = "Family Ubudehe"
            , kinyarwanda = Just "Icyiciro cy'ubudehe umuryango uherereyemo"
            , kirundi = Just "Amakoperative"
            }

        FbfDistribution clinicType ->
            case clinicType of
                Achi ->
                    { english = "Aheza Distribution"
                    , kinyarwanda = Just "Gutanga Aheza"
                    , kirundi = Just "Ugutanga kwa Aheza"
                    }

                _ ->
                    { english = "FBF Distribution"
                    , kinyarwanda = Just "Gutanga FBF (Shishakibondo)"
                    , kirundi = Just "Itangwa rya FBF"
                    }

        Feeding ->
            { english = "Feeding"
            , kinyarwanda = Just "Kugaburira umwana"
            , kirundi = Just "Kugaburira umwana"
            }

        FatherOrChiefId ->
            { english = "Father or Chief of Family ID"
            , kinyarwanda = Just "Indangamuntu y'Umukuru w'Umuryango"
            , kirundi = Just "Karangamuntu ya Serugo canke Umukuru w'umuryango"
            }

        FatherOrChiefName ->
            { english = "Fathers or Chief of Family Name"
            , kinyarwanda = Just "Amazina y'Umukuru w'muryango"
            , kirundi = Just "Amazina ya Serugo canke Umukuru w'umuryango"
            }

        FavoriteToggle isFavorite ->
            if isFavorite then
                { english = "Unfavorite"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "Favorite"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        FetalHeartRate ->
            { english = "Fetal Heart Rate"
            , kinyarwanda = Just "Uko umutima w'umwana utera"
            , kirundi = Just "Urugero rw'umwana ari mu nda"
            }

        FetalMovement ->
            { english = "Fetal Movement"
            , kinyarwanda = Just "Uko umwana akina mu nda"
            , kirundi = Just "Ukwikomanga k'umwana mu nda"
            }

        FetalPresentationLabel ->
            { english = "Fetal Presentation"
            , kinyarwanda = Just "Uko umwana ameze mu nda"
            , kirundi = Just "Ukugaragara kw'imbanyi iri mu nda"
            }

        FetalPresentation option ->
            case option of
                FetalBreech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Just "Kibanza"
                    }

                Cephalic ->
                    { english = "Cephalic"
                    , kinyarwanda = Just "Umwana abanje umutwe"
                    , kirundi = Just "Igihe umutwe n'izosi biri imbere"
                    }

                Transverse ->
                    { english = "Transverse"
                    , kinyarwanda = Just "Gitambitse (Umwana aritambitse)"
                    , kirundi = Just "Umwana arakikamye"
                    }

                Twins ->
                    { english = "Twins"
                    , kinyarwanda = Just "Impanga"
                    , kirundi = Just "Amahasa"
                    }

                Backend.Measurement.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    , kirundi = Just "Bitazwi"
                    }

        FillTheBlanks ->
            { english = "Fill in the Blanks: Cyatsi, Hondo, Tuku & Ibipimo"
            , kinyarwanda = Just "Uzuza ukoresheje: Cyatsi, Hondo, Tuku & Ibipimo"
            , kirundi = Just "Uzuza aho atacanditsemwo: Urwatsi Rubisi, Umutoto uhishiye, Gutukura & Ibipimo"
            }

        FilterByName ->
            { english = "Filter by name"
            , kinyarwanda = Just "Hitamo izina ryuwo ushaka"
            , kirundi = Just "Cagura ukoresheje izina"
            }

        Finish ->
            { english = "Finish"
            , kinyarwanda = Just "Soza igikorwa"
            , kirundi = Just "Guheza"
            }

        FirstName ->
            { english = "First Name"
            , kinyarwanda = Just "Izina ry'idini"
            , kirundi = Just "Izina"
            }

        FiveVisits ->
            { english = "Five visits"
            , kinyarwanda = Just "Inshuro eshanu"
            , kirundi = Just "Ingendo zitanu"
            }

        FoodGroup group ->
            case group of
                FoodGroupVegetables ->
                    { english = "Vegetables"
                    , kinyarwanda = Just "Imboga"
                    , kirundi = Just "Ivyamwa n'imboga"
                    }

                FoodGroupCarbohydrates ->
                    { english = "Carbohydrates"
                    , kinyarwanda = Just "Ibinyamasukari"
                    , kirundi = Just "Ntanganguvu"
                    }

                FoodGroupProtein ->
                    { english = "Protein"
                    , kinyarwanda = Just "Ibyubakumubiri"
                    , kirundi = Just "Indemamubiri"
                    }

        FoodSecurity ->
            { english = "Food Security"
            , kinyarwanda = Just "Kwihaza ku biribwa"
            , kirundi = Just "Umutekano mu mfunguro"
            }

        FollowPostpartumProtocols ->
            { english = "Follow Postpartum Protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kwita ku mubyeyi wabyaye"
            , kirundi = Just "Gukurikiza Inyandiko Ntumberezo zerekeye inyuma yo kwibaruka"
            }

        FollowUpWithPatientIn ->
            { english = "Follow up with patient in"
            , kinyarwanda = Just "Kurikirana umurwayi uri mu bitaro"
            , kirundi = Just "Kurikirana umugwayi wawe "
            }

        FollowUpWithPatientOn ->
            { english = "Follow up with patient on"
            , kinyarwanda = Just "Gukurikirana Umurwayi Ku itariki"
            , kirundi = Just "Kurikirana umugwayi wawe iyo ari"
            }

        FollowUpByChwLabel ->
            { english = "CHW should follow up with patient in"
            , kinyarwanda = Just "Umujyanama w'ubuzima agomba gukurikirana umurwayi mu"
            , kirundi = Just "Abaremeshakiyago bategerezwa gukurikirana/kubandanya baraba umugwayi ari mu"
            }

        FollowUpLabel ->
            { english = "Follow up with the patient in"
            , kinyarwanda = Just "Gukurikirana umurwayi mu"
            , kirundi = Just "Kurikirana umugwayi mu"
            }

        FollowUpWithMotherLabel ->
            { english = "Follow up with the mother in"
            , kinyarwanda = Just "Gukurikirana umubyeyi mu"
            , kirundi = Just "Kurikirana umuvyeyi mu"
            }

        FollowUpOption option ->
            case option of
                OneDay ->
                    { english = "1 Day"
                    , kinyarwanda = Just "Umunsi 1"
                    , kirundi = Just "Umunsi 1"
                    }

                ThreeDays ->
                    { english = "3 Days"
                    , kinyarwanda = Just "Iminsi 3"
                    , kirundi = Just "Iminsi 3"
                    }

                OneWeek ->
                    { english = "1 Week"
                    , kinyarwanda = Just "Icyumweru 1"
                    , kirundi = Just "Indwi 1"
                    }

                TwoWeeks ->
                    { english = "2 Weeks"
                    , kinyarwanda = Just "Ibyumweru 2"
                    , kirundi = Just "Indwi 2"
                    }

                OneMonth ->
                    { english = "1 Month"
                    , kinyarwanda = Just "Ukwezi 1"
                    , kirundi = Just "Ukwezi 1"
                    }

                TwoMonths ->
                    { english = "2 Months"
                    , kinyarwanda = Just "Amezi 2"
                    , kirundi = Just "Amezi 2"
                    }

                ThreeMonths ->
                    { english = "3 Months"
                    , kinyarwanda = Just "Amezi 3"
                    , kirundi = Just "Amezi 3"
                    }

        FollowUpDueOption option ->
            case option of
                OverDue ->
                    { english = "Past Due"
                    , kinyarwanda = Just "Itariki yarenze"
                    , kirundi = Just "Igihe carenze"
                    }

                DueToday ->
                    { english = "Due Today"
                    , kinyarwanda = Just "Itariki yageze uyu munsi"
                    , kirundi = Just "Bitarenze uno munsi"
                    }

                DueThisWeek ->
                    { english = "This week"
                    , kinyarwanda = Just "Iki cyumweru"
                    , kirundi = Just "Iyi ndwi"
                    }

                DueThisMonth ->
                    { english = "This Month"
                    , kinyarwanda = Just "Uku kwezi"
                    , kirundi = Just "Uku kwezi"
                    }

                DueNextMonth ->
                    { english = "Next Month"
                    , kinyarwanda = Just "Ukwezi gutaha"
                    , kirundi = Just "Ukwezi gukurikira"
                    }

        ForIllustrativePurposesOnly ->
            { english = "For illustrative purposes only"
            , kinyarwanda = Just "Ku mpamvu zumvikana gusa"
            , kirundi = Just "Ku mpavu zo kwerekana gusa"
            }

        FormError errorValue ->
            translateFormError errorValue

        FormField field ->
            translateFormField field

        FundalHeight ->
            { english = "Fundal Height"
            , kinyarwanda = Just "Uburebure bwa Nyababyeyi"
            , kirundi = Just "Uburebure bw'igitereko"
            }

        FundalPalpableQuestion ->
            { english = "Is fundal palpable"
            , kinyarwanda = Just "Ese nyababyeyi irumvikana igihe usuzuma umubyeyi"
            , kirundi = Just "Igitereko kimeze neza"
            }

        FundalPalpableWarning ->
            { english = "Inconsistent with documented gestational age, recommended ultrasound."
            , kinyarwanda = Just "Ntibihura n'ibyumweru by'inda byanditswe, urasabwa guca mu cyuma gisuzuma ababyeyi batwite."
            , kirundi = Just "Ntibihuye n'imyaka yo gusama imbanyi yanditse mu bitabo, Iradiyo irategerezwa gukorwa"
            }

        Gender gender ->
            case gender of
                Male ->
                    { english = "Male"
                    , kinyarwanda = Just "Gabo"
                    , kirundi = Just "Gabo"
                    }

                Female ->
                    { english = "Female"
                    , kinyarwanda = Just "Gore"
                    , kirundi = Just "Gore"
                    }

        GenderLabel ->
            { english = "Gender"
            , kinyarwanda = Just "Igitsina"
            , kirundi = Just "Igitsina"
            }

        GestationalDiabetesPreviousPregnancy ->
            { english = "Gestational Diabetes in previous pregnancy"
            , kinyarwanda = Just "Ubushize yarwaye Diyabete itewe no gutwita"
            , kirundi = Just "Diyabete y'imbanyi mu gihe c'imbanyi iheruka"
            }

        Glass value ->
            { english = value ++ " Glass"
            , kinyarwanda = Just <| "Ikirahuri " ++ value
            , kirundi = Just <| "Ikirahuri " ++ value
            }

        GoHome ->
            { english = "Go to main page"
            , kinyarwanda = Just "Kujya ahabanza"
            , kirundi = Just "Ja k'urupapuro nyamukuru"
            }

        GotResultsPreviouslyQuestion ->
            { english = "Has patient previously performed HBA1C test and got results"
            , kinyarwanda = Just "Umurwayi yaba yarakorewe ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize abona n'ibisubizo"
            , kirundi = Just "Mbega umugwayi yarigeze agira igipimo ca 'hémoglobine A1C (HbA1c)'"
            }

        GroupAssessment ->
            { english = "Group Encounter"
            , kinyarwanda = Just "Gukorera itsinda"
            , kirundi = Just "Kubonano y'umurwi"
            }

        Grams ->
            { english = "grams"
            , kinyarwanda = Just "Amagarama"
            , kirundi = Just "Amagarama"
            }

        GroupOfFoods value ->
            case value of
                Staples ->
                    { english = "Staples (grains, roots and tubers)"
                    , kinyarwanda = Just "Ibinyabijumba/Ibitera imbaraga"
                    , kirundi = Just "Ibifungurwa vy'umushinge/vy'intango/vy'ibanze (intete, imizi hamwe n'amateke/amagoma)"
                    }

                Legumes ->
                    { english = "Legumes (beans, peas, cereals)"
                    , kinyarwanda = Just "Ibibyamisogwe (Ibishyimbo, amashyaza, ibinyampeke)"
                    , kirundi = Just "Ibifungugwa bimeze nk'ibiharage/intete ziri mu bishishwa (ibiharage, ubushaza, intete)"
                    }

                DairyProducts ->
                    { english = "Dairy products"
                    , kinyarwanda = Just "Ibikomoka ku mata"
                    , kirundi = Just "Ivyavuye mu mata"
                    }

                AnimalSourceFoods ->
                    { english = "Animal-source foods (flesh meats, eggs, fish, small fish (indagara))"
                    , kinyarwanda = Just "Ibikomoka ku matungo (inyama, amagi, amafi, indagara)"
                    , kirundi = Just "Ibifungurwa biva ku bitungwa/ibikoko (inyama, amagi, amafi, indagara)"
                    }

                Eggs ->
                    { english = "Eggs"
                    , kinyarwanda = Just "Amagi"
                    , kirundi = Just "Amagi"
                    }

                FruitsVegetables ->
                    { english = "Fruits and vegetables"
                    , kinyarwanda = Just "Imbuto n'Imboga"
                    , kirundi = Just "Ivyamwa n'imboga"
                    }

                BreastMilk ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Just "Konka"
                    , kirundi = Just "Ukonka"
                    }

                MealsWithEdibleOil ->
                    { english = "Meals with added edible oil"
                    , kinyarwanda = Just "Ifunguro ryongewemo amavuta"
                    , kirundi = Just "Imfungugwa zongewemwo amavuta yo kurya"
                    }

        Growth ->
            { english = "Growth"
            , kinyarwanda = Just "Imikurire"
            , kirundi = Just "Ugukura"
            }

        Gravida ->
            { english = "Gravida"
            , kinyarwanda = Just "Inda zose watwise"
            , kirundi = Just "Umugore afise imbanyi canke yigeze gutwara inda"
            }

        HalfOfDosage dosage ->
            { english = "half of " ++ dosage ++ " dosage"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HandedReferralFormQuestion ->
            { english = "Did you hand the referral form to the patient"
            , kinyarwanda = Just "Wahaye umurwayi urupapuro rumwohereza"
            , kirundi = Just "Mbega warashubije urupapu rwo kurungika umugwayi kwivuza ahandi uwo mugwayi"
            }

        HandPallor ->
            { english = "Hand Pallor"
            , kinyarwanda = Just "Ikiganza cyerurutse"
            , kirundi = Just "Kweragurika mu ntoke"
            }

        Hands ->
            { english = "Hands"
            , kinyarwanda = Just "Ibiganza"
            , kirundi = Just "Ibiganza"
            }

        HandsCPESign option ->
            case option of
                PallorHands ->
                    translationSet Pallor

                EdemaHands ->
                    translationSet Edema

                NormalHands ->
                    translationSet Normal

        HbA1c ->
            { english = "HBA1c"
            , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
            , kirundi = Just "Igipimo co kuraba ingene isukari ingana mu maraso"
            }

        HbA1cPercentage ->
            { english = "Percentage (%)"
            , kinyarwanda = Just "Ku ijana (%)"
            , kirundi = Just "Ivyo kw'ijana (%)"
            }

        HbA1cMostRecentTestResultInstruction ->
            { english = "Please input the most recent HBA1C test result"
            , kinyarwanda = Just "Injiza ibisubizo biheruka ku kizamini gipima ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
            , kirundi = Just "Nimwinjize inyishu y'igipimo ca \"HBA1C\" cakozwe vuba hashize"
            }

        HCRecommendation recommendation ->
            case recommendation of
                SendAmbulance ->
                    { english = "agreed to call the District Hospital to send an ambulance"
                    , kinyarwanda = Just "bemeranya guhamagara ibitaro ngo byohereze imbangukiragutabara"
                    , kirundi = Just "yemeye guhamagara ku Bitaro vy'Akarere ngo barungike Rusehabaniha (ambiranse)"
                    }

                HomeIsolation ->
                    { english = "advised patient to stay home in isolation"
                    , kinyarwanda = Just "bagira inama umurwayi yo kuguma mu rugo mu kato"
                    , kirundi = Just "Guhanura umugwayi kuguma i muhira wenyene"
                    }

                ComeToHealthCenter ->
                    { english = "advised patient to go to the health center for further evaluation"
                    , kinyarwanda = Just "kimugira inama yo kujya ku kigo nderabuzima gukoresha isuzuma ryimbitse"
                    , kirundi = Just "Guhanura umugwayi kugira angende kw'Ivuriro (kwa muganga) hama bamugirire ibipimo vy'umwihariko"
                    }

                ChwMonitoring ->
                    { english = "CHW should continue to monitor"
                    , kinyarwanda = Just "cyemeza ko umujyanama w’ubuzima agomba gukomeza gukurikirana umurwayi"
                    , kirundi = Just "Abaremeshakiyago bategerezwa gukurikirana/kubandanya baraba"
                    }

                HCRecommendationNotApplicable ->
                    translationSet NotApplicable

        HCResponseQuestion ->
            { english = "What was the Health Center's response"
            , kinyarwanda = Just "Ni ikihe gisubizo cyavuye ku kigo nderabuzima"
            , kirundi = Just "Ni iyihe nyishu ivuriro ryatanze"
            }

        HCResponsePeriodQuestion ->
            { english = "How long did it take the Health Center to respond"
            , kinyarwanda = Just "Byatwaye igihe kingana gute ngo ikigo nderabuzima gisubize"
            , kirundi = Just "Mbega Ivuriro ryafashe umanye ungana gute ngo bishure"
            }

        HeadCircumferenceHelper ->
            { english = "Using a tape measure, wrap the tape around the widest possible circumference; above the ears and midway between the eyebrows and the hairline to the occipital prominence on the back of the head."
            , kinyarwanda = Just "Wifashishije metero bushumi kandi umwana aryamye agaramye, zengurutsa iyo metero ku mutwe w'umwana hejuru y'amatwi uhereye inyuma, izenguruke ku gahanga  kugeza ugeze aho watangiriye."
            , kirundi = Just "Ukoresheje igipimo c'umugozi/imetero, zunguru umutwe uciye hejuru y'amatwi hama ingohe gushika kw'izosi, ku giti c'umugongo"
            }

        HeadCircumferenceNotTakenLabel ->
            { english = "Please check if the head circumference was not taken today"
            , kinyarwanda = Just "Reba niba ibipimo by'umuzenguruko w'umutwe bitafashwe uyu munsi"
            , kirundi = Just "Muraraba nimba umuzingi w'umutwe utafashwe uno munsi"
            }

        HeadHair ->
            { english = "Head/Hair"
            , kinyarwanda = Just "Umutwe/Umusatsi"
            , kirundi = Just "Umutwe/umushatsi"
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Just "Ikigo Nderabuzima"
            , kirundi = Just "Ivuriro"
            }

        HealthCenterDetermined ->
            { english = "Health center determined this is a"
            , kinyarwanda = Just "Ikigo nderabuzima cyagaragaje ko"
            , kirundi = Just "Ivuriro ryatoye ko iki ari"
            }

        HealthEducationNotProvided ->
            { english = "No health education provided"
            , kinyarwanda = Just "Nta nyigisho ku buzima zatanzwe"
            , kirundi = Just "Nta nyigisho z'amagara meza zatanzwe"
            }

        HealthEducationProvided ->
            { english = "Health education provided"
            , kinyarwanda = Just "Hatanzwe inyigisho ku buzima"
            , kirundi = Just "Inyigisho z'amagara zitanzwe"
            }

        HealthEducationProvidedQuestion ->
            { english = "Have you provided health education (or anticipatory guidance)"
            , kinyarwanda = Just "Watanze ikiganiro ku buzima (Cyangwa ubujyanama bw'ibanze)"
            , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara (canke gutanga intumbero hakiri kare)"
            }

        HealthInsuranceQuestion ->
            { english = "Do you have health insurance"
            , kinyarwanda = Just "Ufite ubwishingizi bwo kwivuza"
            , kirundi = Just "Mbega urafise ikigo kikuvuza (Asiransi y'amagara/Asiransi ikuvuza)"
            }

        Heart ->
            { english = "Heart"
            , kinyarwanda = Just "Umutima"
            , kirundi = Just "Umutima"
            }

        HeartburnReliefMethod method ->
            case method of
                ReliefMethodAvoidLargeMeals ->
                    { english = "Avoid large, fatty meals"
                    , kinyarwanda = Just "Irinde ibiribwa byinshi, byongera ibinure"
                    , kirundi = Just "Kwirinda/kureka gufungura/kurya ibifungugwa vyinshi, ibifungugwa birimwo ibinure"
                    }

                ReliefMethodCeaseSmoking ->
                    { english = "Cease smoking "
                    , kinyarwanda = Just "Hagarika kunywa itabi"
                    , kirundi = Just "Guhagarika kunywa itabi"
                    }

                ReliefMethodAvoidAlcohom ->
                    { english = "Avoid alcohol consumption "
                    , kinyarwanda = Just "Irinde kunywa ibisindisha"
                    , kirundi = Just "Kwirinda/kureka kunywa inzonga"
                    }

                ReliefMethodSleepWithHeadRaised ->
                    { english = "Sleep with their head raised in the bed"
                    , kinyarwanda = Just "Gerageza kuryama umutwe wegutse/useguye"
                    , kirundi = Just "Kuryama umutwe usangamuye mu gitanda"
                    }

        HeartburnRecommendedTreatmentHeader ->
            { english = "This patient has signs of persistent heartburn"
            , kinyarwanda = Just "Umubyeyi afite ikirungurira gihoraho"
            , kirundi = Just "Uyu mugwayi afise ibimenyetso vyo gusha mu nda bibandanya"
            }

        HeartburnRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye wo guha uyu murwayi"
            , kirundi = Just "Hitamo uburyo bwiza bwo kuvura umurwayi hepfo"
            }

        HeartMurmur ->
            { english = "Heart Murmur"
            , kinyarwanda = Just "Ijwi ry'umutima igihe utera"
            , kirundi = Just "Kongorera k'umutima"
            }

        HeartCPESign sign ->
            case sign of
                IrregularRhythm ->
                    { english = "Irregular Rhythm"
                    , kinyarwanda = Just "Injyana ihindagurika"
                    , kirundi = Just "Ingendo ihindagurika"
                    }

                NormalRateAndRhythm ->
                    { english = "Normal Rate And Rhythm"
                    , kinyarwanda = Just "Bimeze neza/Injyana imeze neza"
                    , kirundi = Just "Igipimo hamwe n'umudundo bisazwe"
                    }

                SinusTachycardia ->
                    { english = "Sinus Tachycardia"
                    , kinyarwanda = Just "Gutera k'umutima birenze cyane igipimo gisanzwe"
                    , kirundi = Just "Itera ry'umutima ridasanzwe"
                    }

        HeartRate ->
            { english = "Heart Rate"
            , kinyarwanda = Just "Gutera k'umutima (inshuro umutima utera)"
            , kirundi = Just "Ugutera k'umutima"
            }

        Height ->
            { english = "Height"
            , kinyarwanda = Just "Uburebure"
            , kirundi = Just "Uburebure"
            }

        High ->
            { english = "High"
            , kinyarwanda = Nothing
            , kirundi = Just "Hejuru"
            }

        HighRiskCase ->
            { english = "high-risk case"
            , kinyarwanda = Just "afite ibyago byinshi byo kuba yaranduye"
            , kirundi = Just "Ingorane idasanzwe"
            }

        HighRiskCaseHelper ->
            { english = "This patient is a high risk case and should be sent to a hospital for further treatment"
            , kinyarwanda = Just "Uyu murwayi afite ibyago byinshi, agomba koherezwa ku bitaro bakamuha ubuvuzi bwimbitse"
            , kirundi = Nothing
            }

        HighRiskFactor factor ->
            case factor of
                HighRiskConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery and became unconscious after delivery"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umuvyeyi yaragize ibizunguzungu mu gihe co kwibaruka guheruka hama aca arata ubwenge/araraba ahjeje kwibaruka"
                    }

                HighRiskConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umuvyeyi yaragize ibizunguzungu mu gihe co kwibaruka guheruka"
                    }

        HighRiskFactors ->
            { english = "High Risk Factors"
            , kinyarwanda = Just "Abafite ibyago byinshi byo"
            , kirundi = Just "Impamvu z'ingorane zaduze"
            }

        HighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BodyTemperature ->
                    { english = "Body Temperature"
                    , kinyarwanda = Just "Ubushyuhe bw'umubiri"
                    , kirundi = Just "Ubushuhe bw'umubiri"
                    }

                Backend.PrenatalActivity.Model.FetalHeartRate ->
                    { english = "No fetal heart rate noted"
                    , kinyarwanda = Just "Umutima w'umwana ntutera"
                    , kirundi = Just "Nta mutima w'umwana ari mu nda uboneka"
                    }

                Backend.PrenatalActivity.Model.FetalMovement ->
                    { english = "No fetal movement noted"
                    , kinyarwanda = Just "Umwana ntakina mu nda"
                    , kirundi = Just "Nta kugendagenda k'umwana mu nda vyagaragaye"
                    }

                Backend.PrenatalActivity.Model.HeartRate ->
                    { english = "Heart Rate"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugutera k'umutima"
                    }

                Backend.PrenatalActivity.Model.RespiratoryRate ->
                    { english = "Respiratory Rate"
                    , kinyarwanda = Just "Inshuro ahumeka"
                    , kirundi = Just "Igipimo co guhema"
                    }

        HighSeverityAlerts ->
            { english = "High Severity Alerts"
            , kinyarwanda = Just "Bimenyetso mpuruza bikabije"
            , kirundi = Just "Ibimenyesha vyinshi vyaduze"
            }

        History ->
            { english = "History"
            , kinyarwanda = Just "Amakuru"
            , kirundi = Just "Akahise"
            }

        HistoryTask task ->
            case task of
                Obstetric ->
                    { english = "Obstetric History"
                    , kinyarwanda = Just "Amateka y'inda zibanza (ku nda yatwise)"
                    , kirundi = Just "Akahise k'ivyara"
                    }

                Medical ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    , kirundi = Just "Akahise ko m'ubuvuzi"
                    }

                Social ->
                    { english = "Partner Information"
                    , kinyarwanda = Just "Amakuru y'uwo bashakanye (umugabo)"
                    , kirundi = Just "Amakuru y'umushingantahe"
                    }

                Pages.Prenatal.Activity.Types.OutsideCare ->
                    translationSet OutsideCareLabel

        HIV ->
            { english = "HIV"
            , kinyarwanda = Just "Virusi itera SIDA"
            , kirundi = Just "Umugera wa SIDA"
            }

        HIVPCRResult result ->
            case result of
                ResultSuppressedViralLoad ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    , kirundi = Just "Munsi ya kopi 20"
                    }

                ResultDetectibleViralLoad value ->
                    { english = String.fromFloat value
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        HIVStatus status ->
            case status of
                HIVExposedInfant ->
                    { english = "HIV-exposed Infant"
                    , kinyarwanda = Just "Umwana uvuka ku mubyeyi ubana n'ubwandu bwa virusi ya SIDA"
                    , kirundi = Just "Umwana ufite abavyeyi bafite umugera wa SIDA"
                    }

                Negative ->
                    translationSet NegativeLabel

                NegativeDiscordantCouple ->
                    { english = "Negative - discordant couple"
                    , kinyarwanda = Just "Nta bwandu afite ariko abana n'ubufite"
                    , kirundi = Just "Umugwayi ata mugera wa Sida afise ariko mugenziwe ayifise"
                    }

                Positive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Just "Igipimo + "
                    }

                Backend.Person.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntabizi"
                    , kirundi = Just "Bitazwi"
                    }

        HIVStatusLabel ->
            { english = "HIV Status"
            , kinyarwanda = Just "Uko ahagaze ku bijyanye n'ubwandu bwa virusi ya SIDA"
            , kirundi = Just "Ingene imeze VIH"
            }

        HIVTreatmentSign sign ->
            case sign of
                HIVTreatmentNoMedicineNotSeenAtPMTCT ->
                    { english = "Never seen at PMTCT"
                    , kinyarwanda = Just "Ntiyigeze agera muri PMTCT"
                    , kirundi = Just "Nta na rimwe araboneka mu nyigisho za PTME"
                    }

                HIVTreatmentNoMedicineOutOfStock ->
                    { english = "Stock Out"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Just "Ibiri mu bubiko vyaheze"
                    }

                HIVTreatmentNoMedicinePatientRefused ->
                    { english = "Patient Refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Just "Umugwayi yanse"
                    }

                HIVTreatmentNoMedicineOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                -- We do not require translation for other signs.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Home ->
            { english = "Home"
            , kinyarwanda = Just "Mu rugo"
            , kirundi = Just "Muhira"
            }

        HomeVisitActivityTitle activity ->
            case activity of
                Backend.HomeVisitActivity.Model.Feeding ->
                    translationSet Feeding

                Backend.HomeVisitActivity.Model.Caring ->
                    translationSet Caring

                Backend.HomeVisitActivity.Model.Hygiene ->
                    translationSet Hygiene

                Backend.HomeVisitActivity.Model.FoodSecurity ->
                    translationSet FoodSecurity

        HowManyDoses ->
            { english = "How many doses"
            , kinyarwanda = Just "Ingahe"
            , kirundi = Just "Mbega ibipimo vy'imiti ni bingahe"
            }

        HaveAnyOfTheFollowingQuestion ->
            { english = "Do you have any of the following"
            , kinyarwanda = Just "Waba wagize ibi bikurikira?"
            , kirundi = Just "Mbega urafise ibi bintu bikurikira"
            }

        HttpError error ->
            translateHttpError error

        HomeVisit ->
            { english = "Home Visit"
            , kinyarwanda = Just "Gusura Umurwayi"
            , kirundi = Just "Kugendera muhira"
            }

        HoursSinglePlural value ->
            if value == 1 then
                { english = "1 Hour"
                , kinyarwanda = Nothing
                , kirundi = Just "Isaha 1"
                }

            else
                { english = String.fromInt value ++ " Hours"
                , kinyarwanda = Nothing
                , kirundi = Just <| "Amasaha " ++ String.fromInt value
                }

        Hygiene ->
            { english = "Hygiene"
            , kinyarwanda = Just "Isuku"
            , kirundi = Just "Isuku"
            }

        HowManyPerWeek ->
            { english = "How many per week"
            , kinyarwanda = Just "Unywa imiti y'itabi ingahe ku cyumweru"
            , kirundi = Just "Mbega ni bingahe ku ndwi"
            }

        Hypertension ->
            { english = "Hypertension"
            , kinyarwanda = Just "Indwara y'umuvuduko w'amaraso"
            , kirundi = Just "Umuvuduko urenze w'amaraso"
            }

        HypertensionAndPregnantHeader ->
            { english = "This patient has Hypertension and is pregnant"
            , kinyarwanda = Just "Uyu murwayi afite indwara y'umuvuduko w'amaraso kandi aratwite"
            , kirundi = Just "Uyu mugwayi afise umuvuduko w'amaraso kandi aribungenze/afise imbanyi"
            }

        HypertensionBeforePregnancy ->
            { english = "Hypertension before pregnancy"
            , kinyarwanda = Just "Umuvuduko w'amaraso mbere yo gutwita"
            , kirundi = Just "Umuvuduko urenze w'amaraso imbere y'imbanyi"
            }

        HypertensionRecommendedTreatmentHeader isChronic ->
            if isChronic then
                { english = "This patient shows signs of Chronic hypertension"
                , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                , kirundi = Just "Uyu mugwayi yerekana ibimenyetso vy'umuvuduko w'amaraso wamaho"
                }

            else
                { english = "This patient shows signs of Pregnancy-Induced hypertension"
                , kinyarwanda = Just "Uyu Murwayi agaragaza ibimenyetso by'Umuvuduko w'amaraso watewe no gutwita"
                , kirundi = Just "Uyu mugwayi yerekana ibimenyetso vy'imbanyi ifise Umuvuduko w'amaraso"
                }

        HypertensionRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            , kirundi = Just "Hitamo uburyo bwiza bwo kuvura umurwayi hepfo"
            }

        HypertensionRecommendedTreatmentUpdateHeader forModeratePreeclamsia ->
            if forModeratePreeclamsia then
                { english = "This patient was previously diagnosed with Moderate Preeclamsia"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize preekalampusi"
                , kirundi = Just "Uyu mugwayi bamutoye ubushize Umuvuduko w'amaraso uru hagati na hagati kandi afise imbanyi"
                }

            else
                { english = "This patient was previously diagnosed with Hypertension"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize umuvuduko w'amaraso"
                , kirundi = Just "Uyu mugwayi baramaze kumutora/bamutoye ubushize Umuvuduko w'amaraso"
                }

        HypertensionRecommendedTreatmentUpdateBPLabel ->
            { english = "The patients current BP is"
            , kinyarwanda = Just "Ubu umubyeyi afite umuvuduko w'amaraso ungana na"
            , kirundi = Just "Umuvuduko w'amaraso w'umugwayi ubu ni"
            }

        HypertensionRecommendedTreatmentUpdateCurrentTreatment ->
            { english = "The patient is currently prescribed"
            , kinyarwanda = Just "Ubu umubyeyi afata imiti ikurikira"
            , kirundi = Just "Umugwayi ubu yandikiwe"
            }

        HypertensionRecommendedTreatmentUpdateNewTreatment value ->
            case value of
                TreatementUpdateMaintainCurrentDoasage ->
                    { english = "It is recommended that the medication remain unchanged -"
                    , kinyarwanda = Just "Birasabwa ko imiti idahinduka -"
                    , kirundi = Just "Birategerezwa ko igipimo c'imiti kidahinduka -"
                    }

                TreatementUpdateIncreaseOneDose ->
                    { english = "It is recommended that the medication is increased one dosage level to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze imwe kuri"
                    , kirundi = Just "Birategerezwa ko igipimo c'imiti congerwa urugero rumwe kuri"
                    }

                TreatementUpdateIncreaseTwoDoses ->
                    { english = "It is recommended that the medication is increased two dosage levels to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze ebyiri kuri"
                    , kirundi = Just "Birategerezwa ko igipimo c'imiti congerwa ingero zibiri kuri"
                    }

                -- We're not required to view this option.
                TreatementUpdateHospitalize ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        HypertensionRecommendedTreatmentUpdateNoCurrentTreatment ->
            { english = "The patient is currently receiving no treatment"
            , kinyarwanda = Just "Ubu umubyeyi nta muti ari gufata"
            , kirundi = Just "Umugwayi ubu nta muti ariko aravugwa"
            }

        HypertensionRecommendedTreatmentUpdateStartTreatment ->
            { english = "It is recommended to start treatment with"
            , kinyarwanda = Just "Arasabwa gutangirira kuri iyi miti"
            , kirundi = Just "Birategerezwa ko ari ugutangura umuti na"
            }

        HypertensionStageAndRenalComplicationsHeader renalComplications diagnosis ->
            case diagnosis of
                DiagnosisHypertensionStage1 ->
                    if renalComplications then
                        { english = "This patient has Stage One Hypertension with Renal Complications"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri ku rwego rwa mbere n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Just "Uyu mugwayi afise Umuvuduko w'amaraso uri mu c'iciro/mu gice ca mbere hamwe n'ingorane z'amafyigo"
                        }

                    else
                        { english = "This patient has Stage One Hypertension"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri ku rwego rwa mbere"
                        , kirundi = Just "Uyu mugwayi afise Umuvuduko w'amaraso uri mu c'iciro/mu gice ca mbere"
                        }

                DiagnosisHypertensionStage2 ->
                    if renalComplications then
                        { english = "This patient has Stage Two Hypertension with Renal Complications"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Just "Uyu mugwayi afise Umuvuduko w'amaraso uri mu c'iciro/mu gice ca kabiri hamwe n'ingorane z'amafyigo"
                        }

                    else
                        { english = "This patient has Stage Two Hypertension"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri"
                        , kirundi = Just "Uyu mugwayi afise Umuvuduko w'amaraso uri mu c'iciro ca kabiri"
                        }

                DiagnosisHypertensionStage3 ->
                    { english = "This patient has Stage Three Hypertension"
                    , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu"
                    , kirundi = Just "Uyu mugwayi afise Umuvuduko w'amaraso uri mu c'iciro/mu gice ca gatatu"
                    }

                -- We should never get here.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        IdentityPopupHeader ->
            { english = "You are not properly logged in to E-Heza."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IdentityPopupInstructions ->
            { english = "To proceed, you must log out and log back in as then correct user."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IdleWaitingForSync ->
            { english = "Idle, waiting for next Sync cycle"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Ignore ->
            { english = "Ignore"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IllnessSymptom symptom ->
            case symptom of
                IllnessSymptomHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                IllnessSymptomVisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Just "Uko areba byahindutse"
                    , kirundi = Just "Impinduka y'icerekezo"
                    }

                IllnessSymptomRash ->
                    { english = "Rash on body, feet or hands"
                    , kinyarwanda = Just "Ari kwishimagura ku mubiri: ku birenge cg ibiganza"
                    , kirundi = Just "Amaherehere k'umubiri, ku birenge canke mu ntoke"
                    }

                IllnessSymptomPainlessUlcerMouth ->
                    { english = "Painless ulcer in mouth"
                    , kinyarwanda = Just "Agasebe kataryana mu kanwa"
                    , kirundi = Just "Igikomere kitababaza mu kanwa"
                    }

                IllnessSymptomPainlessUlcerGenitals ->
                    { english = "Painless ulcer in genital area"
                    , kinyarwanda = Just "Agasebe kataryana mu myanya ndangagitsina"
                    , kirundi = Just "Igikomere kitababaza mu bihimba vy'irondoka"
                    }

                NoIllnessSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        Immunisation ->
            { english = "Immunization"
            , kinyarwanda = Just "Inkingo"
            , kirundi = Just "Incanco"
            }

        ImmunizationHistory ->
            { english = "Immunization History"
            , kinyarwanda = Just "Amakuru ku nkingo yafashe"
            , kirundi = Just "Akahise k'urucanco"
            }

        IncompleteCervixPreviousPregnancy ->
            { english = "Incomplete Cervix in previous pregnancy"
            , kinyarwanda = Just "Ubushize inkondo y'umura ntiyashoboye kwifunga neza"
            , kirundi = Just "Umuringoti w'igitereko utari ukwiye mu gihe c'imbanyi iheruka"
            }

        IndexPatient ->
            { english = "Index Patient"
            , kinyarwanda = Just "Umubare w'umurwayi"
            , kirundi = Just "Ironderero ry'umugwayi"
            }

        IndividualEncounter ->
            { english = "Individual Encounter"
            , kinyarwanda = Just "Gukorera umuntu umwe"
            , kirundi = Just "Umubonano n'umuntu ku giti ciwe"
            }

        IndividualEncounterFirstVisit encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "First Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa cya mbere ku burwayi"
                    , kirundi = Just "Uguhura n'ingwara ikaze/igoye ubwa mbere"
                    }

                AntenatalEncounter ->
                    { english = "First Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mugore utwite"
                    , kirundi = Just "Ukubonana ubwa mbere mu gihe c'imbanyi/imbere yo kuvyara"
                    }

                ChildScoreboardEncounter ->
                    { english = "First Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku ifishi y'imikurire y'umwana"
                    , kirundi = Just "Ukubonana ubwa mbere kw'ikarata y'ikurikiranwa ry'umwana"
                    }

                HomeVisitEncounter ->
                    { english = "First Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo bwambere"
                    , kirundi = Just "Ukugendera urugo ubwa mbere hamwe no kubonana/guhura"
                    }

                InmmunizationEncounter ->
                    { english = "First Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kuronka urucanco ubwa mbere"
                    }

                NCDEncounter ->
                    { english = "First NCD Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere kuburwayi butandura"
                    , kirundi = Just "Ingwara idandukira muhuye ubwa mbere"
                    }

                NutritionEncounter ->
                    { english = "First Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mirire"
                    , kirundi = Just "Umubonano wa mbere mu vyerekeye gufungura"
                    }

                WellChildEncounter ->
                    { english = "First Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura rya mbere ku mwana"
                    , kirundi = Just "Umubonano wa mbere mu kugendera abana"
                    }

        IndividualEncounterLabel encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa ku burwayi butunguranye"
                    , kirundi = Just "Guhura n'ingwara ibabaza cane/ikaze"
                    }

                AntenatalEncounter ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    , kirundi = Just "Umubonano imbere yo kuvyara"
                    }

                ChildScoreboardEncounter ->
                    { english = "Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifish y'Imikurire y'Umwana"
                    , kirundi = Just "Ukubonana kw'ikarata y'ikurikiranwa ry'umwana"
                    }

                HomeVisitEncounter ->
                    { english = "Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo"
                    , kirundi = Just "Umubonano mu gihe co kugendera muhira"
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Just "Isuzuma ku Burwayi Butandura"
                    , kirundi = Just "Umubonano werekeye ingwara zitandukira"
                    }

                NutritionEncounter ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Well Child Visit"
                        , kinyarwanda = Nothing
                        , kirundi = Nothing
                        }

                    else
                        { english = "Standard Pediatric Visit Encounter"
                        , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                        , kirundi = Just "Inama mu gihe c'urugendo rusanzwe go mu gisata kiraba abana"
                        }

        IndividualEncounterSelectVisit encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Select Acute Illness Visit"
                    , kinyarwanda = Just "Hitamo inshuro aje kuri ubwo burwayi butunguranye"
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    { english = "Select Antenatal Visit"
                    , kinyarwanda = Just "Hitamo isuzuma k’umugore utwite"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounter ->
                    { english = "Select Child Scorecard Visit"
                    , kinyarwanda = Just "Hitamo isuzuma ku ifishi y'imikurire y'umwana"
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "Select Home Visit"
                    , kinyarwanda = Just "Hitamo Gusura Umurwayi"
                    , kirundi = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "Select Inmmunization Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "Select NCD Visit"
                    , kinyarwanda = Just "Hitamo Isuzuma Kuburwayi Butandura"
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = "Select Nutrition Visit"
                    , kinyarwanda = Just "Hitamo isuzuma ry’imirire"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Select Well Child Visit"
                        , kinyarwanda = Nothing
                        , kirundi = Nothing
                        }

                    else
                        { english = "Select Standard Pediatric Visit"
                        , kinyarwanda = Just "Hitamo isura ry'umwana"
                        , kirundi = Nothing
                        }

        IndividualEncounterSubsequentVisit encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Subsequent Acute Illness Encounter"
                    , kinyarwanda = Just "Ibikorwa bikurikiyeho kuri ubwo burwayi butunguraye"
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    { english = "Subsequent Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma rikurikiyeho ku mugore utwite"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounter ->
                    { english = "Subsequent Child Scorecard Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "Subsequent Home Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "Subsequent Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "Subsequent NCD Visit"
                    , kinyarwanda = Just "Isuzuma Rikurikiyeho ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = "Subsequent Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma rikurikiyeho ku mugore utwite"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    { english = "Subsequent Standard Pediatric Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        IndividualEncounterType encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    , kirundi = Just "Ingwara ikaze"
                    }

                AntenatalEncounter ->
                    translationSet AntenatalCare

                ChildScoreboardEncounter ->
                    translationSet ChildScorecard

                HomeVisitEncounter ->
                    translationSet HomeVisit

                InmmunizationEncounter ->
                    { english = "Inmmunization"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Incanco"
                    }

                NCDEncounter ->
                    { english = "Noncommunicable Diseases"
                    , kinyarwanda = Just "Indwara Zitandura"
                    , kirundi = Just "Ingwara zitandukira"
                    }

                NutritionEncounter ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'umwana"
                    , kirundi = Just "Ugufungura k'umwana"
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Well Child Visit"
                        , kinyarwanda = Nothing
                        , kirundi = Nothing
                        }

                    else
                        { english = "Standard Pediatric Visit"
                        , kinyarwanda = Just "Kujyana Umwana mu Isuzumiro"
                        , kirundi = Nothing
                        }

        InfrastructureEnvironment ->
            { english = "Infrastructure, Environment"
            , kinyarwanda = Just "Ibikorwa remezo n’ibidukikije"
            , kirundi = Just "Inyubako, Ibidukikije"
            }

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Just "Ibikorwaremezo, Ibidukikije n'Amazi"
            , kirundi = Just "Inyubako, Ibidukikije & isuku"
            }

        InitialResultsDisplay display ->
            case display of
                InitialResultsHidden ->
                    { english = "Display all mothers / caregivers"
                    , kinyarwanda = Just "Kugaragaza ababyeyi bose / abarezi"
                    , kirundi = Just "Kwerekana abavyeyi bose / abarezi bose"
                    }

                InitialResultsShown ->
                    { english = "Hide all mothers / caregivers"
                    , kinyarwanda = Just "Hisha ababyeyi bose / abarezi"
                    , kirundi = Just "Hisha ababyeyi / abarezi bose"
                    }

        IntractableVomiting isIntractable ->
            if isIntractable then
                { english = "Intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bikabije"
                , kirundi = Just "Ukudahwa kutavugwa"
                }

            else
                { english = "Non-intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bidakabije"
                , kirundi = Just "Kudahwa kudashobora kuvurwa"
                }

        IntractableVomitingQuestion ->
            { english = "Is Vomiting Intractable"
            , kinyarwanda = Just "Kuruka bikabije"
            , kirundi = Just "Mbega kudahwa ntibivugwa"
            }

        InstructionsChooseOneMedication ->
            { english = "Choose one of the medications from the list to prescribe to the patient"
            , kinyarwanda = Just "Hitamo umuti umwe ku rutonde uwuhe umurwayi"
            , kirundi = Nothing
            }

        InstructionsChooseTwoMedications ->
            { english = "Choose two of the medications from the list to prescribe to the patient"
            , kinyarwanda = Just "Hitamo imiti ibiri ku rutonde uyihe umurwayi"
            , kirundi = Nothing
            }

        IsCurrentlyBreastfeeding ->
            { english = "Is the mother currently breastfeeding her infant"
            , kinyarwanda = Just "Muri iki gihe, umubyeyi yonsa umwana we?"
            , kirundi = Nothing
            }

        IsolatedAtHome ->
            { english = "Isolated at home"
            , kinyarwanda = Just "Yashyizwe mu kato mu rugo"
            , kirundi = Just "Umugwayi arategerezwa kwitandukanya mu kuja mu kibanza cawenyene m'urugo"
            }

        IsThisYouQuestion ->
            { english = "Is this you"
            , kinyarwanda = Nothing
            , kirundi = Just "Uyu ni wewe"
            }

        Issued ->
            { english = "Issued"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IssuedTo ->
            { english = "Issued To"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        KilogramShorthand ->
            { english = "kg"
            , kinyarwanda = Just "kg"
            , kirundi = Just "kg"
            }

        KnownAsPositiveQuestion task ->
            case task of
                TaskHIVTest ->
                    { english = "Is this patient known to be HIV positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite ubwandu bwa virusi itera SIDA"
                    , kirundi = Just "Mbega umugwayi arazwi ko afise umugera wa SIDA SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Is this patient known to be Syphilis - RPR positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite uburwayi bwa Mburugu"
                    , kirundi = Just "Mbega umugwayi arazwi ko afise Sifirisi - igipimo ca RPR"
                    }

                TaskHepatitisBTest ->
                    { english = "Is this patient known to be Hepatitis B positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara y'umwijima yo mu bwoko bwa B"
                    , kirundi = Just "Mbega umugwayi arazi ko agwaye igitigu (afise Hépatite B)"
                    }

                TaskMalariaTest ->
                    { english = "Is this patient known to be Malaria positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara ya Malariya"
                    , kirundi = Just "Mbega umugwayi/umuvyeyi arazwi ko afise Malariya"
                    }

                TaskPregnancyTest ->
                    { english = "Is this patient known to be pregnant"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko atwite?"
                    , kirundi = Just "Mbega umugwayi/umuvyeyi arazwi ko afise imbanyi"
                    }

                TaskBloodGpRsTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TaskUrineDipstickTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TaskHemoglobinTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskHIVPCRTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskCreatinineTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskLiverFunctionTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskLipidPanelTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskHbA1cTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskPartnerHIVTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        KnownPositive ->
            { english = "Known Positive"
            , kinyarwanda = Just "Asanzwe arwaye"
            , kirundi = Just "Aragwaye"
            }

        KnownPositiveHepatitisB ->
            { english = "Known Hepatitis B positive"
            , kinyarwanda = Just "Asanzwe afite indwara y'Umwijima wo mu bwoko bwa B"
            , kirundi = Just "Birazwi ko afise ingwara y'igitigu (Hépatite B)"
            }

        KnownPositiveHIV ->
            { english = "Known HIV positive"
            , kinyarwanda = Just "Asanzwe afite Ubwandu bw'agakoko gatera SIDA"
            , kirundi = Just "Birazwi ko afise umugera wa SIDA"
            }

        LabelOnePregnancyEpisodeOpen ->
            { english = "There is one pregnancy episode that is open"
            , kinyarwanda = Just "Hari isuzuma rigifunguye ku mugore utwite"
            , kirundi = Just "Hano hari ikiringo/igice kimwe cuguruwe ku mbanyi"
            }

        LabelSeenHealthcareProviderForPregnancy ->
            { english = "Have you seen a healthcare provider for current pregnancy"
            , kinyarwanda = Just "Waba warigeze usuzumwa n'umuganga kuri iyinda utwite"
            , kirundi = Just "Mbega warabonanye n'umuvuzi kuri iyi mbanyi"
            }

        LabelDocumentPregnancyOutcome ->
            { english = "No - document pregnancy outcome"
            , kinyarwanda = Just "Ntabwo iherezo ry'inda ryanditswe"
            , kirundi = Just "Oya - Ivyanditse vy'inyishu y'imbanyi (Ntibiraboneka)"
            }

        Lab ->
            { english = "Lab"
            , kinyarwanda = Just "Ibizamini"
            , kirundi = Just "Igipimo c'ingwara"
            }

        LabHistory ->
            { english = "Lab History"
            , kinyarwanda = Just "Amakuru ku bizamini byakozwe"
            , kirundi = Just "Akahise k'igipimo c'ingwara"
            }

        LaboratoryBloodGroupLabel ->
            { english = "Blood Group"
            , kinyarwanda = Just "Ubwoko bw'Amaraso"
            , kirundi = Just "Umurwi wa'amaraso"
            }

        LaboratoryBloodGroupTestResult ->
            { english = "Blood Group Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cy'ubwoko bw'amaraso"
            , kirundi = Just "Inyishu y'igipimo c'umurwi w'amaraso"
            }

        LaboratoryBloodGroup value ->
            case value of
                BloodGroupA ->
                    { english = "A"
                    , kinyarwanda = Just "Ubwoko bwa A"
                    , kirundi = Just "Umurwi w'amaraso wa A"
                    }

                BloodGroupB ->
                    { english = "B"
                    , kinyarwanda = Just "Ubwoko bwa B"
                    , kirundi = Just "Umurwi w'amaraso wa B"
                    }

                BloodGroupAB ->
                    { english = "AB"
                    , kinyarwanda = Just "Ubwoko bwa AB"
                    , kirundi = Just "Umurwi w'amaraso wa AB"
                    }

                BloodGroupO ->
                    { english = "O"
                    , kinyarwanda = Just "Ubwoko bwa O"
                    , kirundi = Just "Umurwi w'amaraso wa O"
                    }

        LaboratoryRhesusLabel ->
            { english = "Rhesus"
            , kinyarwanda = Just "Rezisi"
            , kirundi = Nothing
            }

        LaboratoryRhesusTestResult ->
            { english = "Rhesus Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cya Rezisi"
            , kirundi = Just "Inyishu y'igipimo ca Rhesus"
            }

        LaboratoryRhesus value ->
            case value of
                RhesusPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite Resisi pisitifu"
                    , kirundi = Just "Indangakamere ya \"Rhesus Positif\""
                    }

                RhesusNegative ->
                    translationSet NegativeLabel

        LaboratoryProteinLabel ->
            { english = "Protein"
            , kinyarwanda = Just "Proteyine"
            , kirundi = Just "Poroteyine"
            }

        LaboratoryProteinTestResult ->
            { english = "Protein Test Result"
            , kinyarwanda = Just "Ibisubizo bya proteyine"
            , kirundi = Just "Inyishu y'igipimo ca Poroteyine"
            }

        LaboratoryProteinValue value ->
            case value of
                Protein0 ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ProteinPlus1 ->
                    { english = "+1"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ProteinPlus2 ->
                    { english = "+2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ProteinPlus3 ->
                    { english = "+3"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ProteinPlus4 ->
                    { english = "+4"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryPHLabel ->
            { english = "pH"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LaboratoryPHTestResult ->
            { english = "pH Test Result"
            , kinyarwanda = Just "Igisubizo cya pH"
            , kirundi = Just "Inyishu y'igipimo ca pH"
            }

        LaboratoryPHValue value ->
            case value of
                Ph40 ->
                    { english = "4.0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph45 ->
                    { english = "4.5"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph50 ->
                    { english = "5.0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph60 ->
                    { english = "6.0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph65 ->
                    { english = "6.5"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph70 ->
                    { english = "7.0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph75 ->
                    { english = "7.5"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph80 ->
                    { english = "8.0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ph85 ->
                    { english = "8.5"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryGlucoseLabel ->
            { english = "Glucose"
            , kinyarwanda = Just "Isukari"
            , kirundi = Just "Isukari ibitse m'umubiri"
            }

        LaboratoryGlucoseTestResult ->
            { english = "Glucose Test Result"
            , kinyarwanda = Just "Ibisubizo by'isukari mu nkari"
            , kirundi = Just "Inyishu y'igipimo c'Isukari m'umubiri "
            }

        LaboratoryGlucoseValue value ->
            case value of
                Glucose0 ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GlucosePlus1 ->
                    { english = "+1"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GlucosePlus2 ->
                    { english = "+2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GlucosePlus3 ->
                    { english = "+3"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GlucosePlus4 ->
                    { english = "+4"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryLeukocytesLabel ->
            { english = "Leukocytes"
            , kinyarwanda = Just "Uturemangingo twera"
            , kirundi = Just "Leucocytes"
            }

        LaboratoryLeukocytesTestResult ->
            { english = "Leukocytes Test Result"
            , kinyarwanda = Just "Igisubizo k'uturemangingo twera"
            , kirundi = Just "Inyishu y'igipimo ca \"Leucocytes\""
            }

        LaboratoryLeukocytesValue value ->
            case value of
                LeukocytesNegative ->
                    translationSet NegativeLabel

                LeukocytesSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Insoro zera nke zigaragara mu nkari (+)"
                    , kirundi = Just "Gitoya(+)"
                    }

                LeukocytesMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Insoro zera ziringaniye zigaragara mu nkari (++)"
                    , kirundi = Just "Hagati (++)"
                    }

                LeukocytesLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Insoro zera nyinshi zigaragara mu nkari (+++)"
                    , kirundi = Just "Ubwaguke (+++)"
                    }

        LaboratoryNitriteLabel ->
            { english = "Nitrite"
            , kinyarwanda = Just "Umunyu wapimwe mu nkari"
            , kirundi = Just "Nitrite"
            }

        LaboratoryNitriteTestResult ->
            { english = "Nitrite Test Result"
            , kinyarwanda = Just "Ibisubizo kumunyu wapimwe mu nkari"
            , kirundi = Just "Inyishu y'igipimo ca Nitrite"
            }

        LaboratoryNitriteValue value ->
            case value of
                NitriteNegative ->
                    translationSet NegativeLabel

                NitritePlus ->
                    { english = "+"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NitritePlusPlus ->
                    { english = "++"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryUrobilinogenLabel ->
            { english = "Urobilinogen"
            , kinyarwanda = Just "urobilinogene (mu nkari)"
            , kirundi = Just "Urobilinogène"
            }

        LaboratoryUrobilinogenTestResult ->
            { english = "Urobilinogen Test Result"
            , kinyarwanda = Just "Igisubizo cya urobilinogene (mu nkari)"
            , kirundi = Just "Inyishu y'igipimo ca \"Urobilinogène\""
            }

        LaboratoryUrobilinogenValue value ->
            case value of
                Urobilinogen002 ->
                    { english = "0-0.2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Urobilinogen10 ->
                    { english = "1"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Urobilinogen20 ->
                    { english = "2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Urobilinogen40 ->
                    { english = "4"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Urobilinogen80 ->
                    { english = "8"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryHaemoglobinLabel ->
            { english = "Hemoglobin"
            , kinyarwanda = Just "Ingano y'Amaraso"
            , kirundi = Just "Hémoglobine"
            }

        LaboratoryHaemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo by'ikizamini gipima ingano y'amaraso"
            , kirundi = Just "Inyishu y'igipimo ca \"Hémoglobine\""
            }

        LaboratoryHaemoglobinValue value ->
            case value of
                HaemoglobinNegative ->
                    translationSet NegativeLabel

                HaemoglobinNonHemolyzedTrace ->
                    { english = "Non-Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura nkeya zidashwanyutse"
                    , kirundi = Just "Ntakononekara kw'abasoda b'umubiri batukura kwa bonetse"
                    }

                HaemoglobinNonHemolyzedModerate ->
                    { english = "Non-Hemolyzed Moderate"
                    , kinyarwanda = Just "Insoro  zitukura  ziri mu rugero zidashwanyutse"
                    , kirundi = Just "Ntakononekara kw'abasoda b'umubiri batukura kuri hagati na hagati"
                    }

                HaemoglobinHemolyzedTrace ->
                    { english = "Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura zashwanyutse"
                    , kirundi = Just "Ugukurikirana \"hémolyse\""
                    }

                HaemoglobinSmall ->
                    { english = "Small"
                    , kinyarwanda = Just "Ikigero gito cy'amaraso agaragara mu nkari"
                    , kirundi = Just "Gitoya"
                    }

                HaemoglobinModerate ->
                    { english = "Moderate"
                    , kinyarwanda = Just "Ikigero kiringaniye cy'amaraso agaragara mu nkari"
                    , kirundi = Just "Hagati na hagati"
                    }

                HaemoglobinLarge ->
                    { english = "Large"
                    , kinyarwanda = Just "Ikigero kinini cy'amaraso (hemoglobini)  agaragara mu nkari"
                    , kirundi = Just "Kinini"
                    }

        LaboratoryKetoneLabel ->
            { english = "Ketone Test"
            , kinyarwanda = Just "Ikizamini cya Ketone mu nkari"
            , kirundi = Just "Igipimo ca \"cétone\""
            }

        LaboratoryKetoneTestResult ->
            { english = "Ketone Test Result"
            , kinyarwanda = Just "Igisubizo cya Ketone (mu nkari)"
            , kirundi = Just "Inyishu y'igipimo ca \"cétone\""
            }

        LaboratoryKetoneValue value ->
            case value of
                KetoneNegative ->
                    translationSet NegativeLabel

                Ketone5 ->
                    { english = "5"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ketone10 ->
                    { english = "10"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ketone15 ->
                    { english = "15"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ketone40 ->
                    { english = "40"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ketone80 ->
                    { english = "80"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ketone100 ->
                    { english = "100"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryBilirubinLabel ->
            { english = "Bilirubin"
            , kinyarwanda = Just "Bililibine (mu nkari)"
            , kirundi = Just "Bilirubine"
            }

        LaboratoryBilirubinTestResult ->
            { english = "Bilirubin Test Result"
            , kinyarwanda = Just "Igisubizo cya Bililibine (mu nkari)"
            , kirundi = Just "Inyishu z'ibipimo vya Bilirubine"
            }

        LaboratoryBilirubinValue value ->
            case value of
                BilirubinNegative ->
                    translationSet NegativeLabel

                BilirubinSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Byoroheje"
                    , kirundi = Just "Gitoya(+)"
                    }

                BilirubinMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Bikabije"
                    , kirundi = Just "Hagati (++)"
                    }

                BilirubinLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Bikabije cyane"
                    , kirundi = Just "Ubwaguke (+++)"
                    }

        LaboratoryHemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'amaraso"
            , kirundi = Just "Inyishu y'igipimo ca \"Hémoglobine\""
            }

        LaboratoryRandomBloodSugarTestResult ->
            { english = "Random Blood Sugar Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
            , kirundi = Just "Inyishu y'igipimo c'Isukari mu Maraso umwanya uwariwo wose"
            }

        LaboratoryHIVPCRTestResult ->
            { english = "HIV PCR Test Result"
            , kinyarwanda = Just "Ibisubizo by'ikizamini cya PCR gipima Virusi itera SIDA"
            , kirundi = Just "Inyishu y'igipimo ca PCR ya VIH"
            }

        LaboratoryHIVPCRViralLoadStatusQuestion ->
            { english = "Are there less than 20 copies/mm3"
            , kinyarwanda = Just "Haba hari kopi ziri munsi ya 20 kuri mirimrtro kibe"
            , kirundi = Just "Hari ikopi ziri munsi ya mirongo ibiri kuri milimetero kibe (ikopi 20/mm3)"
            }

        LaboratoryCreatinineLabel ->
            { english = "Creatinine"
            , kinyarwanda = Just "Keleyatinine"
            , kirundi = Just "Créatinine"
            }

        LaboratoryBUNLabel ->
            { english = "BUN"
            , kinyarwanda = Just "Ikizamini cy'Impyiko"
            , kirundi = Just "Inyishu ya \"Urée\" (Ire)"
            }

        LaboratoryALTLabel ->
            { english = "ALT"
            , kinyarwanda = Just "Ikizamini cy'Impyiko"
            , kirundi = Nothing
            }

        LaboratoryASTLabel ->
            { english = "AST"
            , kinyarwanda = Just "Ikizamini cy'Umwijima"
            , kirundi = Just "AST (Aspartate Aminotrasnférase)"
            }

        LaboratoryPregnancyLabel ->
            { english = "Pregnancy"
            , kinyarwanda = Just "Ikizamini cyo Gutwita"
            , kirundi = Just "Imbanyi"
            }

        LaboratoryTest value ->
            case value of
                TestBloodGpRs ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    , kirundi = Just "Umurwi wa'amaraso"
                    }

                TestHemoglobin ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    , kirundi = Just "Hémoglobine"
                    }

                TestHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                TestRandomBloodSugar ->
                    { english = "Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    , kirundi = Just "Isukari yo mu maraso"
                    }

                TestSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                TestUrineDipstick ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    , kirundi = Just "Ugupima umukoyo"
                    }

                TestVitalsRecheck ->
                    { english = "Vitals Recheck"
                    , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
                    , kirundi = Just "Ugusubiramwo ivyangombwa"
                    }

                TestHIVPCR ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "PCR ipima Virusi itera SIDA"
                    , kirundi = Just "PCR ya VIH"
                    }

                TestCreatinine ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Just "Créatinine"
                    }

                TestLiverFunction ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Just "Ugukora kw'Igitigu"
                    }

                TestLipidPanel ->
                    translationSet LipidPanel

        PrenatalLabsCaseManagementEntryTypeResults ->
            { english = "ANC Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe ku mugore utwite"
            , kirundi = Just "Inyishu z'ibipimo vyo muri laboratware vy'imbanyi imbere yo kuvyara"
            }

        PrenatalLabsCaseManagementEntryTypeVitals ->
            { english = "Vitals Recheck"
            , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
            , kirundi = Just "Ugusubiramwo ivyangombwa"
            }

        LabsEntryState state ->
            case state of
                LabsEntryPending ->
                    { english = "Pending"
                    , kinyarwanda = Just "Birategerejwe"
                    , kirundi = Just "Kurindira"
                    }

                LabsEntryClosingSoon ->
                    { english = "Closing Soon"
                    , kinyarwanda = Just "Birafunga vuba"
                    , kirundi = Just "Agiye kugara vuba"
                    }

        LabsHistoryCompletedQuestion ->
            { english = "Have you updated all results that have been returned for this patient"
            , kinyarwanda = Just "Waba wujuje ibisubizo byose byaba byabonetse kuri uyu mubyeyi"
            , kirundi = Just "Mbega warashize ku gihe inyishu zose zahawe uyu muvyeyi"
            }

        LaboratoryCreatinineCreatinineResult ->
            { english = "Creatinine Result"
            , kinyarwanda = Just "Ibisubizo by'ikizamini cya Keleyatinine"
            , kirundi = Just "Inyishu ya \"créatinine\""
            }

        LaboratoryCreatinineBUNResult ->
            { english = "BUN Result"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'impyiko"
            , kirundi = Just "Inyishu ya \"Urée\""
            }

        LaboratoryLipidPanelUnitOfMeasurementQuestion ->
            { english = "What units are the test results in"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni ibihe bice vy'inyishu z'igipimo"
            }

        LaboratoryLipidPanelTotalCholesterolLabel ->
            { english = "Total Cholesterol"
            , kinyarwanda = Just "Igipimo cy'ibinure byose mu maraso (Total cholesterol)"
            , kirundi = Just "Icegeranyo c'amavuta m'umubiri"
            }

        LaboratoryLipidPanelLDLCholesterolLabel ->
            { english = "LDL"
            , kinyarwanda = Just "Ingano y'ibinure bibi mu maraso (LDL)"
            , kirundi = Nothing
            }

        LaboratoryLipidPanelHDLCholesterolLabel ->
            { english = "HDL"
            , kinyarwanda = Just "Ingano y'ibinure byiza mu maraso (HDL)"
            , kirundi = Nothing
            }

        LaboratoryLipidPanelTriglyceridesLabel ->
            { english = "Triglycerides"
            , kinyarwanda = Just "Ingano y'ibinure bibitse mu mubiri (Triglycerides)"
            , kirundi = Nothing
            }

        LaboratoryLiverFunctionAltResult ->
            { english = "ALT Result"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'impyiko"
            , kirundi = Nothing
            }

        LaboratoryLiverFunctionAstResult ->
            { english = "AST Result"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'umwijima"
            , kirundi = Nothing
            }

        LaboratoryTask task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu Bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                TaskMalariaTest ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    , kirundi = Just "Umurwi wa'amaraso"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini k'Inkari"
                    , kirundi = Just "Ugupima umukoyo"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    , kirundi = Just "Hémoglobine"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    , kirundi = Just "Isukari mu Maraso umwanya uwariwo wose"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "Ikizamini cya PCR gipima ubwandu bwa Virusi itera SIDA"
                    , kirundi = Just "PCR ya VIH"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Just "Imbanyi"
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Just "Créatinine"
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Just "Ugukora kw'Igitigu"
                    }

                TaskLipidPanelTest ->
                    translationSet LipidPanel

                TaskHbA1cTest ->
                    { english = "HBA1C"
                    , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Just "Igipimo co kuraba ingene isukari ingana mu maraso"
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV"
                    , kinyarwanda = Just "Ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Just "Igipimo co kuraba umugera wa SIDA k'umushingantahe"
                    }

                TaskCompletePreviousTests ->
                    translationSet History

        LaboratoryTaskLabel task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse Gipima Virusi Itera SIDA"
                    , kirundi = Just "RDT ya VIH"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Ikizamini cyihuse gipima Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Ikizamini gipima umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse cya Malariya"
                    , kirundi = Just "Igipimo kinyaruka ca Malariya "
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus"
                    , kinyarwanda = Just "Ikizamini cyubwoko bw'amaraso na ReZisi"
                    , kirundi = Just "Umurwi w'amaraso + Igipimo ca Rhesus"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    , kirundi = Just "Ugupima umukoyo"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ikizamini gipima ingano y'amaraso"
                    , kirundi = Just "Hémoglobine"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ikizamini gipima ingano y' isukari mu maraso"
                    , kirundi = Just "Isukari mu Maraso umwanya uwariwo wose"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "Ikizamini cya PCR gipima ubwandu bwa Virusi itera SIDA"
                    , kirundi = Just "PCR ya VIH"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Just "Imbanyi"
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Just "Créatinine"
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Just "Ugukora kw'Igitigu"
                    }

                TaskLipidPanelTest ->
                    translationSet LipidPanel

                TaskHbA1cTest ->
                    { english = "HBA1C"
                    , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Just "Igipimo co kuraba ingene isukari ingana mu maraso"
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV"
                    , kinyarwanda = Just "Ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Just "Igipimo co kuraba umugera wa SIDA k'umushingantahe"
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryTaskDate task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV Antibody Test Date"
                    , kinyarwanda = Just "Itariki yakorereweho ikizamini cya Virus itera SIDA"
                    , kirundi = Just "Itarike y'igipimo c'abasoda b'Umugera wa SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR Test Date"
                    , kinyarwanda = Just "Itariki yakorereweho ikizamini cya Mburugu"
                    , kirundi = Just "Itarike y'igipimo ca Syphilis-RPR"
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Itarike y'igipimo c'Ingwara y'igitigu ya B"
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cya Malariya"
                    , kirundi = Just "Itarike y'igipimo kinyaruka ca Malariya "
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'ubwoka bw'amaraso na Rezisi yayo"
                    , kirundi = Just "Umurwi w'amaraso + Itarike y'igipimo ca Rhesus"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'inkari"
                    , kirundi = Just "Itarike y'igipimo c'umukoyo"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'amaraso"
                    , kirundi = Just "Itarike y'igipimo ca \"Hémoglobine\""
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'isukari mu maraso"
                    , kirundi = Just "Itarike y'igipimo c'Isukari mu Maraso umwanya uwariwo wose"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya PCR gipima virusi itera SIDA"
                    , kirundi = Just "Itarike y'igipimo ca PCR ya VIH"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cyo gutwita"
                    , kirundi = Just "Itarike y'igipimo c'imbanyi"
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya Keleyatinine"
                    , kirundi = Just "Itarike y'igipimo ca \"créatinine\""
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Date"
                    , kinyarwanda = Just "itariki y'ikizamini cy'Imikorere y'Umwijima"
                    , kirundi = Just "Itarike y'igipimo c'ikora ry'Igitigu"
                    }

                TaskLipidPanelTest ->
                    { english = "Lipid Panel Test Date"
                    , kinyarwanda = Just "Itariki y'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Just "Itarike y'igipimo c'igicapo/ikibaho c'amavuta m'umubiri"
                    }

                TaskHbA1cTest ->
                    { english = "HBA1C Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Just "Itarike y'igipimo co kuraba ingene isukari ingana mu maraso"
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Just "Itarike y'igipimo c'umugera wa SIDA k'umushingantahe"
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryTaskResult task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV Antibody Test Result"
                    , kinyarwanda = Just "Igisubizo cy'abasirikari barwanya Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR Test Result"
                    , kinyarwanda = Just "Igisubizo cy'ikizamini cya Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B Test Result"
                    , kinyarwanda = Just "Ibisubizo bya hepatite B"
                    , kirundi = Nothing
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Malariya"
                    , kirundi = Nothing
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'ubwoka bw'amaraso na Rezisi yayo"
                    , kirundi = Nothing
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari"
                    , kirundi = Nothing
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin Test Result"
                    , kinyarwanda = Just "Igisubizo by'ikizamini gipima ingano y'amaraso"
                    , kirundi = Just "Inyishu y'igipimo ca \"Hémoglobine\""
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Result"
                    , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
                    , kirundi = Just "Inyishu y'igipimo c'Isukari mu Maraso umwanya uwariwo wose"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya PCR gipima Virusi itera SIDA"
                    , kirundi = Just "Inyishu y'igipimo ca PCR ya VIH"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cyo gutwita"
                    , kirundi = Just "Inyishu y'igipimo c'imbanyi"
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Keleyatinine"
                    , kirundi = Just "Inyishu y'igipimo ca \"créatinine\""
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'Imikorere y'Umwijima"
                    , kirundi = Just "Inyishu y'igipimo c'ikora ry'Igitigu"
                    }

                TaskLipidPanelTest ->
                    { english = "Lipid Panel Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Just "Inyishu y'igipimo c'igicapo c'amavuta m'umubiri"
                    }

                TaskHbA1cTest ->
                    { english = "HBA1C Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Just "Inyishu y'igipimo co kuraba ingene isukari ingana mu maraso"
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Just "Inyishu y'igipimo c'umugera wa SIDA k'umushingantahe"
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LaboratoryTaskResultsHelper ->
            { english = "When ready, update test results via case management"
            , kinyarwanda = Just "Ibisubizo nibiboneka, uhite ubyandika unyuze ku Gukurikirana umurwayi"
            , kirundi = Just "Mugihe witeguye, shira k'umunsi/ku mwanya inyishu z'ibipimo uciye mw'icungengwa c'ingwara"
            }

        LabResults ->
            { english = "Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
            , kirundi = Just "Inyishu y'igipimo c'ingwara"
            }

        LabResultsHistoryModeLabel mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    { english = "HIV Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Vursi itera SIDA"
                    , kirundi = Just "Akahise k'igipimo ca VIH"
                    }

                LabResultsHistoryHIVPCR _ ->
                    { english = "HIV PCR Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya PCR gipima Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                LabResultsHistoryPartnerHIV _ ->
                    { english = "Partner HIV Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Just "Akashize k'igipimo c'umugera wa SIDA k'umushingantahe"
                    }

                LabResultsHistorySyphilis _ ->
                    { english = "Syphilis Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Mburugu"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHepatitisB _ ->
                    { english = "Hepatitis B Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                LabResultsHistoryMalaria _ ->
                    { english = "Malaria Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Malariya"
                    , kirundi = Nothing
                    }

                LabResultsHistoryBloodSmear _ ->
                    { english = "Malaria Blood Smear Test History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryProtein _ ->
                    { english = "Protein Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Proteyine"
                    , kirundi = Nothing
                    }

                LabResultsHistoryPH _ ->
                    { english = "pH Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya pH"
                    , kirundi = Nothing
                    }

                LabResultsHistoryGlucose _ ->
                    { english = "Glucose Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Isukari mu nkari"
                    , kirundi = Nothing
                    }

                LabResultsHistoryLeukocytes _ ->
                    { english = "Leukocytes Test History"
                    , kinyarwanda = Just "Amakuru ku kizami cy' Uturemangingo twera"
                    , kirundi = Nothing
                    }

                LabResultsHistoryNitrite _ ->
                    { english = "Nitrite Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Umunyu wapimwe mu nkari"
                    , kirundi = Nothing
                    }

                LabResultsHistoryUrobilinogen _ ->
                    { english = "Urobilinogen Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya urobilinogene (mu nkari)"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHaemoglobin _ ->
                    { english = "Hemoglobin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'amaraso"
                    , kirundi = Nothing
                    }

                LabResultsHistoryKetone _ ->
                    { english = "Ketone Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Ketone mu nkari"
                    , kirundi = Nothing
                    }

                LabResultsHistoryBilirubin _ ->
                    { english = "Bilirubin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Bililibine (mu nkari)"
                    , kirundi = Nothing
                    }

                LabResultsHistoryRandomBloodSugar _ ->
                    { english = "Random Blood Sugar Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'isukari mu maraso"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHemoglobin _ ->
                    { english = "Hemoglobin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'amaraso"
                    , kirundi = Nothing
                    }

                LabResultsHistoryBloodGroup _ ->
                    { english = "Blood Group Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ubwoko bw'Amaraso"
                    , kirundi = Nothing
                    }

                LabResultsHistoryRhesus _ ->
                    { english = "Rhesus Test History"
                    , kinyarwanda = Just "Amakuru kuri kizamini cya Rezisi"
                    , kirundi = Nothing
                    }

                LabResultsHistoryCreatinine _ ->
                    { english = "Creatinine Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Keleyatinine"
                    , kirundi = Nothing
                    }

                LabResultsHistoryBUN _ ->
                    { english = "BUN Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Impyiko"
                    , kirundi = Nothing
                    }

                LabResultsHistoryALT _ ->
                    { english = "ALT Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Impyiko"
                    , kirundi = Nothing
                    }

                LabResultsHistoryAST _ ->
                    { english = "AST Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Umwijima"
                    , kirundi = Nothing
                    }

                LabResultsHistoryPregnancy _ ->
                    { english = "Pregnancy Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cyo Gutwita"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHbA1c _ ->
                    { english = "HBA1C"
                    , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Just "Igipimo co kuraba ingene isukari ingana mu maraso"
                    }

                LabResultsHistoryTotalCholesterol _ ->
                    { english = "Total Cholesterol"
                    , kinyarwanda = Just "Igipimo cy'ibinure byose mu maraso (Total cholesterol)"
                    , kirundi = Just "Icegeranyo c'amavuta m'umubiri"
                    }

                LabResultsHistoryLDLCholesterol _ ->
                    { english = "LDL Cholesterol"
                    , kinyarwanda = Just "Ingano y'ibinure bibi mu maraso (LDL Cholesterol)"
                    , kirundi = Just "igipimo ca cholestérol LBD"
                    }

                LabResultsHistoryHDLCholesterol _ ->
                    { english = "HDL Cholesterol"
                    , kinyarwanda = Just "Ingano y'ibinure byiza mu maraso (HDL Cholesterol)"
                    , kirundi = Just "Cholestérol LHD"
                    }

                LabResultsHistoryTriglycerides _ ->
                    { english = "Triglycerides"
                    , kinyarwanda = Just "Ingano y'ibinure bibitse mu mubiri (Triglycerides)"
                    , kirundi = Just "Triglycérides"
                    }

        LabResultsNormalRange mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    translationSet NegativeLabel

                LabResultsHistoryHIVPCR _ ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    , kirundi = Just "Munsi ya kopi 20"
                    }

                LabResultsHistoryPartnerHIV _ ->
                    translationSet NegativeLabel

                LabResultsHistorySyphilis _ ->
                    translationSet NegativeLabel

                LabResultsHistoryHepatitisB _ ->
                    translationSet NegativeLabel

                LabResultsHistoryMalaria _ ->
                    translationSet NegativeLabel

                LabResultsHistoryBloodSmear _ ->
                    translationSet NegativeLabel

                LabResultsHistoryProtein _ ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryPH _ ->
                    { english = "4.5-8"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryGlucose _ ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryLeukocytes _ ->
                    translationSet NegativeLabel

                LabResultsHistoryNitrite _ ->
                    translationSet NegativeLabel

                LabResultsHistoryUrobilinogen _ ->
                    { english = "1 mg/dL or less"
                    , kinyarwanda = Just "1 mg/dl cyangwa munsi"
                    , kirundi = Just "1 mg/dl canke munsi"
                    }

                LabResultsHistoryHaemoglobin _ ->
                    translationSet NegativeLabel

                LabResultsHistoryKetone _ ->
                    translationSet NegativeLabel

                LabResultsHistoryBilirubin _ ->
                    translationSet NegativeLabel

                LabResultsHistoryRandomBloodSugar _ ->
                    -- This one is not in use, because normal range
                    -- depends on prerequesit - has patient eaten before
                    -- the test or not.
                    -- Therefore RandomBloodSugarResultNormalRange set is used.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryHemoglobin _ ->
                    { english = "11-16.5 g/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryBloodGroup _ ->
                    { english = "NA"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryRhesus _ ->
                    { english = "Positive"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryCreatinine _ ->
                    { english = "0.5-1.3 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryBUN _ ->
                    { english = "6-24 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryALT _ ->
                    { english = "7-56 IU/L"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryAST _ ->
                    { english = "8-33 IU/L"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryPregnancy _ ->
                    translationSet NegativeLabel

                LabResultsHistoryHbA1c _ ->
                    { english = "Below 6%"
                    , kinyarwanda = Just "Munsi ya 6 ku ijana"
                    , kirundi = Just "Munsi ya gatandatu kw'ijana)"
                    }

                LabResultsHistoryTotalCholesterol _ ->
                    { english = "Below 200 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryLDLCholesterol _ ->
                    { english = "130-160 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryHDLCholesterol _ ->
                    { english = "40-60 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LabResultsHistoryTriglycerides _ ->
                    { english = "54-150 mg/dL"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LabResultsPaneHeader mode ->
            case mode of
                LabResultsCurrentMain ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    , kirundi = Just "Inyishu y'igipimo c'ingwara"
                    }

                LabResultsCurrentDipstickShort ->
                    { english = "Short Dipstick Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari gitanga ibisubizo bike"
                    , kirundi = Just "Inyishu z'ibipimo ukoresheje akuma ko kwinjizamwo"
                    }

                LabResultsCurrentDipstickLong ->
                    { english = "Long Dipstick Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari gitanga ibisubizo byinshi"
                    , kirundi = Just "Inyishu z'ibipimo vy'isukari m'umukoyo"
                    }

                LabResultsCurrentLipidPanel ->
                    { english = "Lipid Panel Results"
                    , kinyarwanda = Just "Ibisubizo by'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Just "Inyishu z'igicapo c'amavuta m'umubiri"
                    }

        LastChecked ->
            { english = "Last checked"
            , kinyarwanda = Just "Isuzuma riheruka"
            , kirundi = Just "Kurabwa ubwanyuma"
            }

        LastContacted ->
            { english = "Last Contacted"
            , kinyarwanda = Just "Igihe baheruka guhurira"
            , kirundi = Just "Uwo bavuganye ubwanyuma"
            }

        LastSuccesfulContactLabel ->
            { english = "Last Successful Contact"
            , kinyarwanda = Just "Itariki n'isaha yanyuma igikoresho giheruka gukoresherezaho interineti bikagenda neza"
            , kirundi = Just "Kuvugana kwa genze neza ubwanyuma"
            }

        LeaveEncounter ->
            { english = "Leave Encounter"
            , kinyarwanda = Just "Reka iki Gikorwa"
            , kirundi = Just "Reka umubonano"
            }

        Left ->
            { english = "Left"
            , kinyarwanda = Just "Ibumoso"
            , kirundi = Just "Ububamfu"
            }

        LegCrampsReliefMethod method ->
            case method of
                ReliefMethodMuscleStretching ->
                    { english = "Muscle stretching"
                    , kinyarwanda = Just "Kurambura imitsi"
                    , kirundi = Just "Ukugorora imitsi"
                    }

                ReliefMethodDorsiflexion ->
                    { english = "Dorsiflexion"
                    , kinyarwanda = Just "Imyitozo ngororamubiri inanura amaguru & ibirenge"
                    , kirundi = Just "Ukugonda ikirenge ujana hejuru"
                    }

                ReliefMethodRelaxation ->
                    { english = "Relaxation"
                    , kinyarwanda = Just "Kuruhuka"
                    , kirundi = Just "Kuruhuka"
                    }

                ReliefMethodSleepWithPillowBetweenLegs ->
                    { english = "Sleep with a pillow between the legs"
                    , kinyarwanda = Just "Ryama ushyize umusego hagati y'amaguru"
                    , kirundi = Just "kuryama ufise umusego hagati y'amaguru"
                    }

                ReliefMethodHeatTherapy ->
                    { english = "Heat therapy"
                    , kinyarwanda = Just "Kuvura hakoreshejwe ubushyuhe"
                    , kirundi = Just "Kuvura ubushuhe"
                    }

                ReliefMethodMassage ->
                    { english = "Massage"
                    , kinyarwanda = Just "Ubugororangingo"
                    , kirundi = Nothing
                    }

        LegLeft ->
            { english = "Left leg"
            , kinyarwanda = Just "Ukuguru kw'ibumoso"
            , kirundi = Just "Ukuguru kw'ibubamfu"
            }

        LegRight ->
            { english = "Right leg"
            , kinyarwanda = Just "Ukuguru kw'iburyo"
            , kirundi = Just "Ukuguru kw'iburyo"
            }

        Legs ->
            { english = "Legs"
            , kinyarwanda = Just "Amaguru"
            , kirundi = Just "Amaguru"
            }

        LegsCPESign option ->
            case option of
                PallorLegs ->
                    translationSet Pallor

                EdemaLegs ->
                    translationSet Edema

                NormalLegs ->
                    translationSet Normal

        LevelOfEducationLabel ->
            { english = "Level of Education"
            , kinyarwanda = Just <| "Amashuri wize"
            , kirundi = Just "urwego gw'amashule"
            }

        LevelOfEducation educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    , kirundi = Just "Nta mashule"
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    , kirundi = Just "Amashule matoya"
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational Training School"
                    , kinyarwanda = Just "Imyuga"
                    , kirundi = Just "Inyigisho zo kw'ishule ry'imyiga"
                    }

                SecondarySchool ->
                    { english = "Secondary School"
                    , kinyarwanda = Just "Ayisumbuye"
                    , kirundi = Just "Yahejeje Amashule Yisumbuye"
                    }

                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    , kirundi = Just "Umugambi wo kuronka urupapuro rw'umutsindo (Imyaka Ibiri (2) kuri Kaminuza)"
                    }

                HigherEducation ->
                    { english = "Higher Education (University)"
                    , kinyarwanda = Just "(A0)"
                    , kirundi = Just "Amashule Makuru (Kaminuza)"
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma"
                    , kinyarwanda = Just "(A1)"
                    , kirundi = Just "Urupapuro rw'umutsindo rwo hejuru"
                    }

                -- Not in use.
                MastersDegree ->
                    { english = "Masters Degree"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Urupapuro rw'umutsindo rw'igice ca 3 mashule (Maîtrise)"
                    }

        LevelOfEducationForResilience educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    , kirundi = Just "Nta mashule"
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    , kirundi = Just "Amashule matoya"
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational School"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Inyigisho zo kw'ishule ry'imyiga"
                    }

                SecondarySchool ->
                    { english = "Finished Secondary School"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Yahejeje Amashule Yisumbuye"
                    }

                -- Not it use.
                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    , kirundi = Just "Umugambi wo kuronka urupapuro rw'umutsindo (Imyaka Ibiri (2) kuri Kaminuza)"
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma (A1)"
                    , kinyarwanda = Just "(A1)"
                    , kirundi = Just "Urupapuro rw'umutsindo rwo hejuru(A1)"
                    }

                HigherEducation ->
                    { english = "Bachelors Degree (A0)"
                    , kinyarwanda = Just "(A0)"
                    , kirundi = Just "Urupapuro rw'umutsindo gwa Kaminuza (A0)"
                    }

                MastersDegree ->
                    { english = "Masters Degree"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Urupapuro rw'umutsindo rw'igice ca 3 mashule (Maîtrise)"
                    }

        LipidPanel ->
            { english = "Lipid Panel"
            , kinyarwanda = Just "Itsinda ry'ibizamini bipima ibinure (Lipid Panel)"
            , kirundi = Just "Ikibaho c'amavuta m'umubiri"
            }

        LiveChildren ->
            { english = "Live Children"
            , kinyarwanda = Just "Abana bariho"
            , kirundi = Just "Abana bariho"
            }

        LmpDateConfirmationLabel ->
            { english = "Please confirm the last menstrual period submitted by the CHW"
            , kinyarwanda = Just "Emeza itariki aherukira mu mihango yujujwe n' umujyanama w'ubuzima"
            , kirundi = Just "Muraraba mwemeze igihe ca nyuma co kuja mu kwezi/m'ubutinyanka catanzwe n'abaremeshakiyago"
            }

        LmpDateConfirmationQuestion ->
            { english = "Do you want to confirm the above LMP"
            , kinyarwanda = Just "Urashaka kwemeza itariki uherukira mu mihango yavuzwe hejuru"
            , kirundi = Just "Mbega urashobora kwemeza Igihe canyuma co kuja mu kwezi cavuzwe aho hejuru"
            }

        LmpDateConfidentHeader ->
            { english = "Is the Patient confident of LMP Date"
            , kinyarwanda = Just "Ese umubyeyi azi neza itariki aherukira mu mihango?"
            , kirundi = Just "Mbega umuvyeyi arizeye itarike ya nyuma y'igihe aherukira kuja mu kwezi?"
            }

        LmpDateNotConfidentQuestion ->
            { english = "What is the reason the patient is unsure"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni iyihe mpamvu ituma umugwayi atavyizera"
            }

        LmpDateNotConfidentReason value ->
            case value of
                ReasonIrregularMenses ->
                    { english = "Irregular Menses"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ukuja mu kwezi bihindagurika"
                    }

                ReasonOnFamilyPlanningMethod ->
                    { english = "On family planning method"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Uburyo bwo kuvyara k'urugero"
                    }

                ReasonCanNotRememberDates ->
                    { english = "Can't remember dates"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ntashobora kwibuka itarike"
                    }

        LmpDateNotConfidentReasonforReport value ->
            case value of
                ReasonIrregularMenses ->
                    { english = "Uncertain dating due to irregular menses"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igora ryo kumenya amatarike kubera ihindagurika ryo kuja mu kwezi"
                    }

                ReasonOnFamilyPlanningMethod ->
                    { english = "Uncertain dating since patient is on family planning method"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igora ryo kumenya amatarike kuko umugwayi ari k'uburyo bwo kuvyara k'urugero"
                    }

                ReasonCanNotRememberDates ->
                    { english = "Uncertain dating since patient can't remember dates"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igora ryo kumenya amatarike kuko umugwayi atayibuka"
                    }

        LmpDateHeader ->
            { english = "Last Menstrual Period Date"
            , kinyarwanda = Just "Itariki aherukira mu mihango"
            , kirundi = Just "Itarike yanyuma y'igihe co kuja mu kwezi"
            }

        LmpLabel ->
            { english = "Last Menstrual Period"
            , kinyarwanda = Just "Igihe aherukira mu mihango"
            , kirundi = Just "Igihe canyuma co kuja mu kwezi"
            }

        LmpRangeHeader ->
            { english = "When was the Patient's Last Menstrual Period"
            , kinyarwanda = Just "Ni ryari umubyeyi aherukira mu mihango?"
            , kirundi = Just "umunsi wanyuma umugwayi umugwayi yagiye mu kwezi"
            }

        LmpRange range ->
            case range of
                Pages.Prenatal.Activity.Types.OneMonth ->
                    { english = "Within 1 month"
                    , kinyarwanda = Just "Mu kwezi kumwe"
                    , kirundi = Just "Mu kwezi kumwe"
                    }

                Pages.Prenatal.Activity.Types.ThreeMonths ->
                    { english = "Within 3 months"
                    , kinyarwanda = Just "Mu mezi atatu"
                    , kirundi = Just "Mu mezi atatu"
                    }

                Pages.Prenatal.Activity.Types.SixMonthsOrMore ->
                    { english = "Within 6 months, or more"
                    , kinyarwanda = Just "Mu mezi atandatu, no hejuru"
                    , kirundi = Just "Mu mezi atandatu canke arenga"
                    }

        LoggedInAsPhrase ->
            { english = "You are logged in as"
            , kinyarwanda = Nothing
            , kirundi = Just "Winjiye nka"
            }

        Location ->
            { english = "Location"
            , kinyarwanda = Just "Aho Ruzatangirwa"
            , kirundi = Nothing
            }

        LoginPhrase phrase ->
            translateLoginPhrase phrase

        Low ->
            { english = "Low"
            , kinyarwanda = Just "Kwemeza amakosa"
            , kirundi = Nothing
            }

        LowRiskCase ->
            { english = "low-risk case"
            , kinyarwanda = Just "afite ibyago bike byo kuba yaranduye"
            , kirundi = Just "Ingorane ntoya"
            }

        Lungs ->
            { english = "Lungs"
            , kinyarwanda = Just "Ibihaha"
            , kirundi = Just "Amahaha"
            }

        LungsCPESign option ->
            case option of
                Wheezes ->
                    { english = "Wheezes"
                    , kinyarwanda = Just "Ijwi ryumvikana igihe umuntu ahumeka"
                    , kirundi = Just "Uguhema nabi nk'uwuzwigira"
                    }

                Crackles ->
                    { english = "Crackles"
                    , kinyarwanda = Just "Ijwi ryumvikana umuntu ahumeka ariko afite indwara z'ubuhumekero"
                    , kirundi = Just "Induru canke utururu"
                    }

                NormalLungs ->
                    translationSet Normal

        MainIncomeSource source ->
            case source of
                HomeBasedAgriculture ->
                    { english = "Homebased Agriculture / Livestock"
                    , kinyarwanda = Just "Ubuhinzi / Ubworozi"
                    , kirundi = Just "Uburimyi n'ubworozi bushingiye k'Urugo"
                    }

                CommercialAgriculture ->
                    { english = "Commercial Agriculture / Livestock"
                    , kinyarwanda = Just "Ubucuruzi bw'imyaka / Amatungo"
                    , kirundi = Just "Uburimyi/ubworozi bwo mu bucuruzi"
                    }

                PublicEmployee ->
                    { english = "Public Employee"
                    , kinyarwanda = Just "Umukozi wa Leta"
                    , kirundi = Just "Umukozi wa Leta"
                    }

                PrivateBusinessEmpployee ->
                    { english = "Private Business Employee"
                    , kinyarwanda = Just "Umukozi w'igenga"
                    , kirundi = Just "Umukozi w'igenga"
                    }

        MainIncomeSourceQuestion ->
            { english = "What is the most important source of income for the household"
            , kinyarwanda = Just "Ese nihe urugo rukura ubushobozi bwo gutunga urugo"
            , kirundi = Just "Mbega isoko nyamukuru ry'amafaranga mu muryango ni irihe"
            }

        MainMenuActivity activity ->
            case activity of
                MenuClinical ->
                    translationSet Clinical

                MenuParticipantDirectory ->
                    { english = "Participant Directory"
                    , kinyarwanda = Just "Ububiko bw'amakuru y'umurwayi"
                    , kirundi = Just "Urutonde igitabo c'abitavye inyigisho"
                    }

                MenuDashboards ->
                    { english = "Dashboards"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Imbaho"
                    }

                MenuCaseManagement ->
                    translationSet CaseManagement

                MenuDeviceStatus ->
                    translationSet DeviceStatus

                MenuWellbeing ->
                    { english = "Wellbeing"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Imibereho myiza"
                    }

                MenuStockManagement ->
                    { english = "Stock Management"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugucunga ububiko"
                    }

        MainWaterSource source ->
            case source of
                PipedWaterToHome ->
                    { english = "Piped Water to Home"
                    , kinyarwanda = Just "Amazi agera mu rugo"
                    , kirundi = Just "Amazi yo mu miringoti gushika muhira"
                    }

                PublicWaterTap ->
                    { english = "Public Water Tap"
                    , kinyarwanda = Just "Ivomo rusange"
                    , kirundi = Just "Amabombo rusangi"
                    }

                RainWaterCollectionSystem ->
                    { english = "Rain Water Collection System"
                    , kinyarwanda = Just "Amazi y'imvura"
                    , kirundi = Just "Uburyo bwo kwegerenya amazi y'imvura"
                    }

                NaturalSourceFlowingWater ->
                    { english = "Natural Source - Flowing Water"
                    , kinyarwanda = Just "Umugezi utemba"
                    , kirundi = Just "Ikomoko kamere - Amazi atemba"
                    }

                NaturalSourceStandingWater ->
                    { english = "Natural Source - Standing Water"
                    , kinyarwanda = Just "Amazi y'ibiyaga"
                    , kirundi = Just "Ikomoko kamere - Amazi adatemba/ahagaze"
                    }

                BottledWater ->
                    { english = "Bottled Water"
                    , kinyarwanda = Just "Amazi akorwa mu nganda (aza mu macupa)"
                    , kirundi = Just "Amazi ari mw'icupa"
                    }

        MainWaterPreparationOption option ->
            case option of
                Boiled ->
                    { english = "Boiled"
                    , kinyarwanda = Just "Barayateka"
                    , kirundi = Just "Ukubiza Amazi"
                    }

                PurificationSolution ->
                    { english = "Purification solution"
                    , kinyarwanda = Just "Bakoresha umuti usukura amazi"
                    , kirundi = Just "umuti wo wo gukurako ubucafu"
                    }

                Filtered ->
                    { english = "Filtered"
                    , kinyarwanda = Just "Barayayungurura"
                    , kirundi = Just "Barayayungurura"
                    }

                Bottled ->
                    { english = "Bottled"
                    , kinyarwanda = Just "Amazi yo mu nganda (afunze mu macupa)"
                    , kirundi = Just "Amazi ari mw'icupa"
                    }

                NoWaterPreparationOption ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        MainWaterSourceQuestion ->
            { english = "What is the household's main source of water"
            , kinyarwanda = Just "Ni hehe h'ibanze urugo ruvana amazi"
            , kirundi = Just "Mbega isoko nyamukuru ry'amazi yo m'urugo nirihe"
            }

        MainWaterPreparationQuestion ->
            { english = "How is drinking water prepared"
            , kinyarwanda = Just "Ni gute amazi yo kunywa ategurwa"
            , kirundi = Just "Mbega amazi yo kunywa ategurwa gute"
            }

        MalariaRapidDiagnosticTest ->
            { english = "Malaria Rapid Diagnostic Test"
            , kinyarwanda = Just "Igikoresho gipima Malariya ku buryo bwihuse"
            , kirundi = Just "Igipimo ca Malariya kinyaruka"
            }

        MalariaRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Malaria"
            , kinyarwanda = Just "Uyu murwayi afite agakoko gateram Malariya"
            , kirundi = Just "Uyu murwayi bamutoye Malariya"
            }

        MalariaRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye ku murwayi"
            , kirundi = Just "Hitamo uburyo bwiza bwo kuvura umurwayi hepfo"
            }

        MalariaWithGIComplications ->
            { english = "Malaria with GI complications"
            , kinyarwanda = Just "Malariya iherekejwe no guhitwa cyangwa kuruka"
            , kirundi = Just "Malariya kumwe n'ingorane zo mu nda"
            }

        RapidTestResult result ->
            case result of
                RapidTestNegative ->
                    translationSet NegativeLabel

                RapidTestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Just "Afise ingwara"
                    }

                RapidTestPositiveAndPregnant ->
                    { english = "Positive and Pregnant"
                    , kinyarwanda = Just "Afite ubwandu kandi aratwite"
                    , kirundi = Just "Afise Ingwara kandi arafise imbanyi"
                    }

                RapidTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Just "kutamenyekana"
                    }

                RapidTestUnableToRun ->
                    { english = "Unable to run"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    , kirundi = Just "Ntibishoboka kwiruka"
                    }

                RapidTestUnableToRunAndPregnant ->
                    { english = "Unable to run and Pregnant"
                    , kinyarwanda = Just "Ntibishoboka gukorwa"
                    , kirundi = Just "Ntibishoboka kwiruka kandi ufise imbanyi"
                    }

        MalnutritionWithComplications ->
            { english = "Malnutrition with complications"
            , kinyarwanda = Just "Imirire mibi n'indwara ziyikomokaho"
            , kirundi = Just "Ingwara yo gufungura nabi hamwe n'ingorane zikomeye zayo"
            }

        MaritalStatusLabel ->
            { english = "Marital Status"
            , kinyarwanda = Just "Irangamimerere"
            , kirundi = Just "Indangakamere y'ubuzima"
            }

        MaritalStatus status ->
            case status of
                Divorced ->
                    { english = "Divorced"
                    , kinyarwanda = Just "Yatandukanye n'uwo bashakanye"
                    , kirundi = Just "Ukwahukana"
                    }

                Married ->
                    { english = "Married"
                    , kinyarwanda = Just "Arubatse"
                    , kirundi = Just "Arubatse"
                    }

                Single ->
                    { english = "Single"
                    , kinyarwanda = Just "Ingaragu"
                    , kirundi = Just "Umusore canke inkumi "
                    }

                Widowed ->
                    { english = "Widowed"
                    , kinyarwanda = Just "Umupfakazi"
                    , kirundi = Just "Umupfakazi"
                    }

                LivingWithPartner ->
                    { english = "Living with partner"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kubana n'umufasha"
                    }

                Religious ->
                    { english = "Religious"
                    , kinyarwanda = Nothing
                    , kirundi = Just "umunyedini"
                    }

        MastitisRecommendedTreatmentHeader forEarlyMastitisOrEngorgment ->
            if forEarlyMastitisOrEngorgment then
                { english = "This patient shows signs of Early Mastitis or Engorgement"
                , kinyarwanda = Nothing
                , kirundi = Just "Uyu mugwayi yerekana ibimenyetso vy'ingwara  yo mu mamoko (kuvyimba ambere)"
                }

            else
                { english = "This patient has Mastitis"
                , kinyarwanda = Just "Uyu mubyeyi afite uburwayi bw'amabere"
                , kirundi = Just "Uyu muvyeyi afise ingwara iri mu mabere (mu mamoko)"
                }

        MastitisRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            , kirundi = Just "Hitamo uburyo bwiza bwo kuvura umurwayi hepfo"
            }

        MeasurementNotTaken ->
            { english = "Unable to take measurements, skip this step"
            , kinyarwanda = Just "Ibipimo ntibyafashwe, komeza ku bikurikira"
            , kirundi = Nothing
            }

        MedicationCausingHypertension medication ->
            case medication of
                MedicationOestrogens ->
                    { english = "Oestrogens (Family Planning)"
                    , kinyarwanda = Just "Umusemburo wa Estrogene"
                    , kirundi = Just "Umusemburo wa Oestrogène"
                    }

                MedicationSteroids ->
                    { english = "Steroids (Prednisolone)"
                    , kinyarwanda = Just "Umusemburo wa iteroyide"
                    , kirundi = Just "Umusemburo wa Stéroïdes (prednisolone)"
                    }

                MedicationAmitriptyline ->
                    { english = "Amitriptyline"
                    , kinyarwanda = Just "Amitiributiline"
                    , kirundi = Just "Amitriptyline"
                    }

                MedicationIbuprofen ->
                    { english = "Ibuprofen (Diclofenac)"
                    , kinyarwanda = Just "Ibiporofene cg Dikolofenake"
                    , kirundi = Just "Ibuprofène (Diclofénac)"
                    }

                NoMedicationCausingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        MedicationCausingHypertensionQuestion ->
            { english = "Has the patient taken or currently take any of the following hypertension causing medications"
            , kinyarwanda = Just "Umurwayi yaba yarafashe cg ari ku miti itera izamuka ry'umuvuduko w'amaraso"
            , kirundi = Nothing
            }

        MedicalCondition condition ->
            case condition of
                MedicalConditionHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                MedicalConditionDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete(Indwara y'Igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                MedicalConditionKidneyDisease ->
                    { english = "Kidney Disease"
                    , kinyarwanda = Just "Indwara y'impyiko"
                    , kirundi = Just "Ingwara y'amafyigo"
                    }

                MedicalConditionPregnancy ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Just "Imbanyi"
                    }

                MedicalConditionHypertension ->
                    { english = "Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso"
                    , kirundi = Just "Umuvuduko urenze w'amaraso"
                    }

                MedicalConditionGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no utwite"
                    , kirundi = Just "Diyabete y'imbanyi"
                    }

                MedicalConditionPregnancyRelatedHypertension ->
                    { english = "Pregnancy Related Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso utewe no gutwita"
                    , kirundi = Just "Umuvuduko w'amaraso ujanye n'imbanyi"
                    }

                MedicalConditionNeuropathy ->
                    { english = "Neuropathy"
                    , kinyarwanda = Just "Indwara z'imyakura"
                    , kirundi = Just "Neuropathie"
                    }

                MedicalConditionRentalComplications ->
                    { english = "Rental Complications"
                    , kinyarwanda = Just "Ibibazo bitewe no kwangirika kw'impyiko"
                    , kirundi = Just "Ingorane zo gukota"
                    }

                MedicalConditionMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                MedicalConditionTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Just "Igituntu"
                    }

                MedicalConditionHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                MedicalConditionSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                MedicalConditionEyeComplications ->
                    { english = "Eye Complications"
                    , kinyarwanda = Just "Ibibazo by'amaso"
                    , kirundi = Just "Ingorane zirenze z'amaso"
                    }

                MedicalConditionAnemia ->
                    { english = "Anemia"
                    , kinyarwanda = Just "Indwara y'amaraso make"
                    , kirundi = Just "Igabanuka ry'amaraso"
                    }

                MedicalConditionOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoMedicalConditions ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        MedicalConditionQuestion ->
            { english = "Have you ever been diagnosed with any of these conditions"
            , kinyarwanda = Just "Waba warigeze urwara imwe muri izi ndwara"
            , kirundi = Nothing
            }

        MedicationDistribution ->
            { english = "Medication Distribution"
            , kinyarwanda = Just "Gutanga Imiti"
            , kirundi = Just "Itangwa ry'imiti"
            }

        MedicationTreatingDiabetes medication ->
            case medication of
                MedicationMetformin ->
                    { english = "Metformin"
                    , kinyarwanda = Just "Metiforumine"
                    , kirundi = Just "Metformine"
                    }

                MedicationGlibenclamide ->
                    { english = "Glibenclamide"
                    , kinyarwanda = Just "Girimbenkalamide"
                    , kirundi = Just "Glibenclamide"
                    }

                MedicationInsulin ->
                    { english = "Insulin"
                    , kinyarwanda = Just "Insuline"
                    , kirundi = Just "Insuline"
                    }

                NoMedicationTreatingDiabetes ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        MedicationTreatingDiabetesQuestion ->
            { english = "Has the patient taken or currently take any of the following medications that treat diabetes"
            , kinyarwanda = Just "Umurwayi yaba yarafashe cg ari gufata imwe mu miti ikurikira ivura Diyabete"
            , kirundi = Just "Mbega umugwayi yarafashe canke ariko arafata imiti ikurikira ivura Diyabete"
            }

        MedicationTreatingHypertension medication ->
            case medication of
                MedicationAceInhibitors ->
                    { english = "Ace-Inhibitors (Example: Captopril)"
                    , kinyarwanda = Just "Caputopili"
                    , kirundi = Just "Imiti ibuza ECA (Enzyme de Conversion Angiotensine) (akarorero: captopril)"
                    }

                MedicationARBs ->
                    { english = "Angiotensine Receptor Blockers (ARBs)"
                    , kinyarwanda = Just "Anjiyotensine"
                    , kirundi = Just "Ibihagarika ukwakira vya Angiotensine"
                    }

                MedicationHCTZ ->
                    { english = "HCTZ"
                    , kinyarwanda = Just "Idolokotiyazide"
                    , kirundi = Just "Hydrochlorothiazide"
                    }

                MedicationCalciumChannelBlockers ->
                    { english = "Calcium Channel Blockers"
                    , kinyarwanda = Just "Kalisiyumu"
                    , kirundi = Just "Ibibuza ukuyobora kwa Kalisiyumu"
                    }

                MedicationBetaBlockers ->
                    { english = "Beta-Blockers"
                    , kinyarwanda = Just "Beta boloka"
                    , kirundi = Just "Bêta-bloquants"
                    }

                MedicationHydralazine ->
                    { english = "Hydralazine"
                    , kinyarwanda = Just "Idaralazine"
                    , kirundi = Just "Hydralazine"
                    }

                MedicationMethyldopa ->
                    { english = "Methyldopa"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoMedicationTreatingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        MedicationTreatingHypertensionQuestion ->
            { english = "Has the patient taken or currently take any of the following medications that treat hypertension"
            , kinyarwanda = Just "Umurwayi yaba yarafashe cg ari gufata imwe mu miti ikurikira ivura umuvuduko w'amaraso"
            , kirundi = Just "Mbega umugwayi yarafashe canke ariko arafata imiti ikurikira ivura Umuvuduko udasanzwe w'amaraso"
            }

        MedicalDiagnosis ->
            { english = "Medical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe na Muganga"
            , kirundi = Just "Isuzuma ryo kwa muganga"
            }

        MedicalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisUterineMyoma ->
                    { english = "Uterine Myoma"
                    , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
                    , kirundi = Just "Ibivyimba vyo mu gitereko"
                    }

                Backend.PrenatalActivity.Model.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                DiagnosisCardiacDisease ->
                    { english = "Cardiac Disease"
                    , kinyarwanda = Just "Indwara z'umutima"
                    , kirundi = Just "Ingwara y'umutima"
                    }

                DiagnosisRenalDisease ->
                    { english = "Renal Disease"
                    , kinyarwanda = Just "Indwara z'impyiko"
                    , kirundi = Just "Ingwara yo mu mafyigo"
                    }

                DiagnosisHypertensionBeforePregnancy ->
                    { english = "Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso"
                    , kirundi = Just "Umuvuduko urenze w'amaraso"
                    }

                Backend.PrenatalActivity.Model.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Just "Igituntu"
                    }

                DiagnosisAsthma ->
                    { english = "Asthma"
                    , kinyarwanda = Just "Asthma (Agahema)"
                    , kirundi = Just "Asima"
                    }

                DiagnosisBowedLegs ->
                    { english = "Bowed Legs"
                    , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
                    , kirundi = Just "Amaguru y'ingonze"
                    }

                DiagnosisKnownHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virus itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                DiagnosisMentalHealthHistory ->
                    { english = "History of Mental Health Problems"
                    , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
                    , kirundi = Just "Akahise k'ingorane y'ingwara yo mu mutwe"
                    }

        MedicationCausesSideEffectsQuestion ->
            { english = "Did you experience adverse events of the medication"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            , kirundi = Just "Mbega warumvise inkurikizi zitari nziza zivuye ku miti wafashe"
            }

        MedicationDistributionHelperAnemia ->
            { english = "Patient shows signs of Mild - Moderate Anemia"
            , kinyarwanda = Just "Umurwayi afite amaraso make byoroheje"
            , kirundi = Just "Umugwayi yerekana ibimenyetso vy'ibura ry'amaraso ryorohejeè - hagati"
            }

        MedicationDistributionHelperDiscordantPartnership ->
            { english = "This patient is part of a discordant partnership"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite"
            , kirundi = Just "Uyu muvyeyi ari m'umubano mpuzabitsina aho umwe afise umugera wa SIDA kandi uwundi atawafise"
            }

        MedicationDistributionHelperDiscordantPartnershipNoARVs ->
            { english = "This patient is part of a discordant partnership in which the partner is not on ARVs"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite kandi ntago afata imiti igabanya ubukana"
            , kirundi = Just "Uyu muvyeyi ari m'umubano mpuzabitsina aho umwe afise umugera wa SIDA kandi uwundi atawafise. Uwo agwaye nta ma ARVs afata."
            }

        MedicationDistributionHelperEarlyMastitisOrEngorgment ->
            { english = "This patient has signs of Early Mastitis or Engorgement"
            , kinyarwanda = Just "Uyu mubyeyi afite ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
            , kirundi = Just "Uyu muvyeyi afise ibimenyetso vy'ingwara yo mu mamoko ikiri nshasha (ukuvyimba amabere)"
            }

        MedicationDistributionHelperHIV ->
            { english = "This patient is HIV positive"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bute"
            , kirundi = Just "Uyu mugwayi bamutoye umugera wa SIDA"
            }

        MedicationDistributionHelperMebendazole ->
            { english = "This patient is over 24 weeks EGA and has not had a dewormer in the last 6 months"
            , kinyarwanda = Just "Uyu mubyeyi atwite inda y'ibyumweru 24 kandi nta muti w'inzoka yafashe mu mezi 6 ashize"
            , kirundi = Just "Uyu muvyeyi arenza indwi 24 za \"AGE=Age Gestationnel Estimé\" kandi ntararonka imiti y'inzoka mu kiringo c'amezi 6 ashize"
            }

        MedicationDistributionHelperGonorrhea ->
            { english = "This patient has signs of possible Gonorrhea"
            , kinyarwanda = Just "Uyu mubyeyi agaragaza ibimenyetso by'umitezi"
            , kirundi = Just "Uyu mugwayi afise ibimenyetso bishoboka vy'ingwara ya \"gonorrhée\""
            }

        MedicationDistributionHelperTrichomonasOrBacterialVaginosis ->
            { english = "This patient has signs of possible Trichomonas or Bacterial Vaginosis"
            , kinyarwanda = Just "Umubyeyii afite ibimenyetso bishobora kuba ari ibya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
            , kirundi = Just "Uyu mugwayi afise ibimenyetso bishoboka vy'ingwara ya \"Trichomonas\" canke umugera (bactérie) uri mu gitsina gore"
            }

        MedicationDistributionHelperVitaminA ->
            { english = "This patient did not receive Vitamin A"
            , kinyarwanda = Just "Uyu mubyeyi ntiyahawe Vitamine A"
            , kirundi = Just "Uyu muvyeyi ntiyaronse Vitamine A"
            }

        MedicationDistributionNoticeGonorrhea ->
            { english = "Note: It is also recommended to prescribe the partner"
            , kinyarwanda = Just "Icyitonderwa: Ni ngombwa kuvura uwo babana/bashakanye"
            , kirundi = Just "Ibuka: Birategerezwa no kwandikira guha umuti umufasha/umugabo/umushingantahe"
            }

        MedicationDistributionNoticeGonorrheaPartnerMed1 ->
            { english = "Ciprofloxacin (1000mg): by mouth as a single dose"
            , kinyarwanda = Just "Kunywa ikinini cya Ciplofoloxacine (1000mg) inshuro imwe"
            , kirundi = Just "Ciprofloxacine (1000 mg) : Kumira igipimo kimwe/idozi imwe"
            }

        MedicationDistributionNoticeGonorrheaPartnerMed2 ->
            { english = "Doxycycline (100mg): by mouth 2x a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini cya Doxycycline (100mg) inshuro ebyri ku munsi mu minsi irindwi"
            , kirundi = Just "Doxycycline (100 mg): umuti wo kumira 2 ku munsi mu kiringo c'iminsi indwi"
            }

        MedicationDistributionSign sign ->
            case sign of
                Amoxicillin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Amoxicillin"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Coartem ->
                    { english = "Coartem"
                    , kinyarwanda = Just "Kowariteme"
                    , kirundi = Nothing
                    }

                ORS ->
                    { english = "Oral Rehydration Solution (ORS)"
                    , kinyarwanda = Just "SRO"
                    , kirundi = Just "SRO"
                    }

                Zinc ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Zinc"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LemonJuiceOrHoney ->
                    { english = "Lemon Juice and/or Honey"
                    , kinyarwanda = Just "Umutobe w'indimu n'ubuki"
                    , kirundi = Nothing
                    }

                Albendazole ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Mebendezole ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.VitaminA ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Paracetamol ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Paracetamol"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Tenofovir ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Tenofovir"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Lamivudine ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Lamivudine"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dolutegravir ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Dolutegravir"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TDF3TC ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "TDF + 3TC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Iron ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Iron"
                    , kinyarwanda = Just "Fer"
                    , kirundi = Nothing
                    }

                FolicAcid ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Folic Acid"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Acide Folique"
                    }

                Ceftriaxone ->
                    { english = "Ceftriaxone"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Azithromycin ->
                    { english = "Azithromycin"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Metronidazole ->
                    { english = "Metronidazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoMedicationDistributionSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoMedicationDistributionSignsInitialPhase ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoMedicationDistributionSignsRecurrentPhase ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        MedicationDoxycycline ->
            -- Names of Medication, therefore,
            -- no translation is needed.
            { english = "Doxycycline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MedicationDosesMissedQuestion ->
            { english = "Did you miss any doses of medications"
            , kinyarwanda = Just "Waba hari imiti wibagiwe gufata"
            , kirundi = Just "Mbega warigeze wibagira gufata igipimo c'imiti (amadoze) wandikiwe"
            }

        MedicationForFeverPast6Hours ->
            { english = "Patient took medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Umurwayi yanyoye imiti y’umuriro mu masaha atandatu ashize"
            , kirundi = Just "Umugwayi yafashe imiti yo kuvura ubushuhe mu masaha 6 aheze"
            }

        MedicationHelpedEnding helped ->
            if helped then
                { english = "and improved"
                , kinyarwanda = Just "none yorohewe"
                , kirundi = Just "Kandi vyateye imbere"
                }

            else
                { english = "but no improvement"
                , kinyarwanda = Just "ariko ntiyorohewe"
                , kirundi = Just "Ariko ntaguterimbere"
                }

        MedicationFeelBetterAfterTakingQuestion ->
            { english = "Do you feel better after taking medications"
            , kinyarwanda = Just "Wumva umeze neza nyuma yo gufata imiti"
            , kirundi = Just "Mbega wumva umerewe neza/wumva vyopfuma/wumva ufise mitende umaze gufata imiti"
            }

        MedicationForMalariaToday ->
            { english = "Patient received medication for malaria today before this visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya uyu munsi mbere yuko aza mu isuzuma"
            , kirundi = Just "Umugwayi yaronse imiti ya malariya imbere yuko aza/imbere y'umubonano"
            }

        MedicationForMalariaPastMonth ->
            { english = "Patient received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya mu kwezi gushize mbere yuko aza mu isuzuma uyu munsi "
            , kirundi = Just "Umugwayi yaronse imiti ya malariya mu kwezi guheze imbere yuko aza/imbere y'umubonano"
            }

        MedicalFormHelper ->
            { english = "Please record if the mother was diagnosed with the following medical issues"
            , kinyarwanda = Just "Andika niba umubyeyi yaragaragaweho indwara zikurikira"
            , kirundi = Just "Andika nimba nyina yarasuzumwe ingorane z'ubuvuzi zikurikira"
            }

        MedicationForFeverPast6HoursQuestion ->
            { english = "Have you taken any medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Hari imiti y'umuriro waba wafashe mu masaha atandatu ashize"
            , kirundi = Just "Mbega wafashe umuti w'ubushuhe mu masaha 6 aheze"
            }

        MedicationForMalariaTodayQuestion ->
            { english = "Did you receive medication for malaria today before this visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba wanyoye mbere y'uko uza kwivuza"
            , kirundi = Just "Mbega wafashe imiti ya malariya imbere yo kuza kwa muganga uno munsi"
            }

        MedicationForMalariaWithinPastMonthQuestion ->
            { english = "Have you received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba waranyoye mukwezi gushize mbere yuko uza hano kwivuza"
            , kirundi = Just "Mbega warafashe umuti wa Malariya mu kiringo c'ukwezi guheze imbere canke imbere y'uko uza uno munsi"
            }

        MedicationHelpedQuestion ->
            { english = "Do you feel better after taking this"
            , kinyarwanda = Just "Urumva umeze neza nyuma yo kunywa iyi miti"
            , kirundi = Just "Mbega wumva ufise mitende umaze gufata uyu"
            }

        MedicationTaken ->
            { english = "Medication taken"
            , kinyarwanda = Just "Imiti yafashe"
            , kirundi = Just "Imiti yafashwe"
            }

        MedicationTakenAsPrescribedQuestion ->
            { english = "Did you take the medication as prescribed"
            , kinyarwanda = Just "Wafashe imiti neza uko wayandikiwe na muganga"
            , kirundi = Just "Mbega wafashe imiti uko bitegekanijwe m'urwandiko"
            }

        MentalHealthHistory ->
            { english = "History of Mental Health Problems"
            , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
            , kirundi = Just "Akahise k'ingorane y'ingwara yo mu mutwe"
            }

        MessagingTab tab ->
            case tab of
                TabUnread ->
                    translationSet (ReadToggle True)

                TabFavorites ->
                    { english = "Favorites"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TabGrowth ->
                    { english = "Growth"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TabConnecting ->
                    { english = "Connecting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TabSelfcare ->
                    { english = "Selfcare"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TabStress ->
                    { english = "Stress"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TabMindfullnes ->
                    { english = "Mindfullnes"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        MMHGUnit ->
            { english = "mmHG"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Minutes minutes ->
            { english =
                if minutes == 1 then
                    "1 Minute"

                else
                    String.fromInt minutes ++ " Minutes"
            , kinyarwanda = Just <| "Iminota " ++ String.fromInt minutes
            , kirundi = Just <| "Iminota " ++ String.fromInt minutes
            }

        MinutesAgo minutes ->
            { english =
                if minutes == 0 then
                    "just now"

                else if minutes == 1 then
                    "one minute ago"

                else
                    String.fromInt minutes ++ " minutes ago"
            , kinyarwanda =
                if minutes == 0 then
                    Just "Nonaha"

                else if minutes == 1 then
                    Just "Umunota umwe ushize"

                else
                    Just <| String.fromInt minutes ++ " hashize iminota micye"
            , kirundi =
                if minutes == 0 then
                    Just "Ubu nyene"

                else if minutes == 1 then
                    Just "Umunota umwe uheze"

                else
                    Just <| String.fromInt minutes ++ " iminota iheze"
            }

        MissedDosesOfMedicatgion val ->
            if val == 0 then
                { english = "No missed doses of medication"
                , kinyarwanda = Just "Yafashe kandi arangiza neza imiti uko yayandikiwe"
                , kirundi = Just "Nta gipimo c'umuti cabuze cabuze /nta doze yabuze"
                }

            else
                { english = "Missed " ++ String.fromInt val ++ " doses of medication"
                , kinyarwanda = Just <| "Yasimbutse gufata imiti inshuro " ++ String.fromInt val
                , kirundi = Just <| "Kwibagira amadoze " ++ String.fromInt val ++ "y'imiti"
                }

        ModeOfDelivery mode ->
            case mode of
                VaginalDelivery (Spontaneous True) ->
                    { english = "Spontaneous vaginal delivery with episiotomy"
                    , kinyarwanda = Just "Yabyaye neza ariko bamwongereye"
                    , kirundi = Just "Kuvyara m'uburyo busanzwe aho nyene ariko biherekejwe n'ugukata igihimba c'irondoka c'umupfasoni"
                    }

                VaginalDelivery (Spontaneous False) ->
                    { english = "Spontaneous vaginal delivery without episiotomy"
                    , kinyarwanda = Just "Yabyaye neza"
                    , kirundi = Just "kuvyara m'uburyo busanzwe aho nyene ariko atagukata igihimba c'irondoka c'umupfasoni"
                    }

                VaginalDelivery WithVacuumExtraction ->
                    { english = "Vaginal delivery with vacuum extraction"
                    , kinyarwanda = Just "Yabyaye neza ariko hanifashishijwe icyuma gikurura umwana"
                    , kirundi = Just "Kuvyara ucishije mu gitsina hamwe no gukwega ibisigaye ukoreshe Vacuum"
                    }

                CesareanDelivery ->
                    { english = "Cesarean delivery"
                    , kinyarwanda = Just "Yabyaye bamubaze"
                    , kirundi = Just "Ugukorwa mu kuvyara"
                    }

        ModeOfDeliveryLabel ->
            { english = "Mode of delivery"
            , kinyarwanda = Just "Uburyo yabyayemo"
            , kirundi = Just "Uburyo bwo kuvyara"
            }

        ModeratePreeclampsia ->
            { english = "Moderate Preeclampsia"
            , kinyarwanda = Just "Preklampusi Yoroheje"
            , kirundi = Just "Ingorane z'imbanyi kuva bisanzwe gushika hagati na hagati (izo ngorane ziba zivuye k'umuvuduko urenze w'amaraso)"
            }

        Month ->
            { english = "Month"
            , kinyarwanda = Just "Ukwezi"
            , kirundi = Just "Ukwezi"
            }

        MonthAbbrev ->
            { english = "mo"
            , kinyarwanda = Just "am"
            , kirundi = Just "am"
            }

        MonthlySurveyScoreInterpretation score ->
            if score < 14 then
                { english = "Low resilient copers"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else if score < 17 then
                { english = "Medium resilient copers"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "High resilient copers"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        MonthSinglePlural value ->
            if value == 1 then
                { english = "1 Month"
                , kinyarwanda = Just "Ukwezi 1"
                , kirundi = Just "Ukwezi 1"
                }

            else
                { english = String.fromInt value ++ " Months"
                , kinyarwanda = Just <| "Amezi " ++ String.fromInt value
                , kirundi = Just <| "Amezi " ++ String.fromInt value
                }

        MonthsOfStock ->
            { english = "Months of Stock"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MotherId ->
            { english = "Mother ID"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MotherName name ->
            { english = "Mother/Caregiver: " ++ name
            , kinyarwanda = Just <| "Umubyeyi: " ++ name
            , kirundi = Just <| "Mama/umurezi: " ++ name
            }

        MotherNameLabel ->
            { english = "Mother's Name"
            , kinyarwanda = Just "Izina ry'umubyeyi"
            , kirundi = Just "Izina rya mama"
            }

        MTDIn ->
            { english = "MTD in"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MTDOut ->
            { english = "MTD out"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MUAC ->
            { english = "MUAC"
            , kinyarwanda = Just "Ikizigira"
            , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
            }

        MyRelatedBy relationship ->
            translateMyRelatedBy relationship

        MyRelatedByQuestion relationship ->
            translateMyRelatedByQuestion relationship

        Name ->
            { english = "Name"
            , kinyarwanda = Just "Izina"
            , kirundi = Just "Izina"
            }

        NationalIdNumber ->
            { english = "National ID Number"
            , kinyarwanda = Just "Numero y'irangamuntu"
            , kirundi = Just "Inomero y'akarangamuntu"
            }

        NCDAANCVisitsCounseling ->
            { english = "Provide counseling on the consequences that may occur to her and the baby if she doesn't attend ANC visit as per guidance"
            , kinyarwanda = Just "Gira umubyeyi inama umusobanurire ingaruka byagira ku mubyeyi no ku mwana kutitabira gahunda yo gupimisha inda inshuro zagenwe"
            , kirundi = Nothing
            }

        NCDABirthweightQuestion ->
            { english = "What was the child's birthweight"
            , kinyarwanda = Just "Umwana yavukanye ibiro bingahe"
            , kirundi = Just "Vyari ibiro bingahe igihe umwana avuka"
            }

        NCDADiarrheaPopupMessage ->
            { english = "The child has diarrhea. Please continue to an Acute Illness encounter."
            , kinyarwanda = Just "Umwana afite impiswi. Komereza ku kuvura Uburwayi butunguranye"
            , kirundi = Nothing
            }

        NCDAMealFrequency6to9 ->
            { english = "A child between 6 to 9 months: Feed him/her complementary foods 2-3 times a day."
            , kinyarwanda = Just "Umwana ufite amezi 6 kugeza ku 9, mugaburire ifashabere inshuro 2 kugera kuri 3 ku munsi"
            , kirundi = Nothing
            }

        NCDAMealFrequency9to12 ->
            { english = "A child between 9 to 12 months: Feed him/her complementary foods 3-4 times a day."
            , kinyarwanda = Just "Umwana w'amezi 9 kugeza ku 12, mugaburire ifashabere nibura inshuro 3 kugera kuri 4 ku munsi."
            , kirundi = Nothing
            }

        NCDAMealFrequency12to24 ->
            { english = "A child between 12 to 24 months: Feed him/her complementary foods at least 5 times a day."
            , kinyarwanda = Just "Umwana w'amezi 12 kugeza ku 24, mugaburire ifashabere nibura inshuro 5 ku munsi."
            , kirundi = Nothing
            }

        NCDASignCounseling sign ->
            case sign of
                SupplementsDuringPregnancy ->
                    { english = "Provide counseling to the mother on the consequences that may occur to the mother and the baby and refer the mother to the HC to receive the Iron/Folic Acid/MMS"
                    , kinyarwanda = Just "Gira umubyeyi inama ku ngaruka mbi zaba ku mwana cyangwa umubyeyi igihe atafashe neza ibinini by'ubutare niba akibifata umwohereze ku kigo nderabuzima gufata ibinini"
                    , kirundi = Nothing
                    }

                ChildBehindOnVaccination ->
                    { english = "Provide counseling to the mother to update the child's vaccination record with a Nurse through a Standard Pediatric Visit"
                    , kinyarwanda = Just "Gira inama umubyeyi yo kuzuza inkingo zitanditse muri sisiteme ya E-heza abifashijwe n'umuforomo banyuze mu Kujyana Umwana mu Isuzumiro"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.OngeraMNP ->
                    { english = "Provide counseling on the importance of Ongera and advise them to go to the Health center to recieve them"
                    , kinyarwanda = Just "Gira inama umubyeyi cg undi umurera ibyiza byo gufata ongera unamugire inama yo kujya kuyifata ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                FiveFoodGroups ->
                    { english = "Provide counseling on how the mother can combine different food items based on the ones they have in their area"
                    , kinyarwanda = Just "Gira inama umubyeyi uko yavanga amako atandukanye y'ibiribwa biboneka aho mutuye"
                    , kirundi = Nothing
                    }

                BreastfedForSixMonths ->
                    { english = "Provide counseling on the importance of breastfeeding a baby for 6 months without interruption"
                    , kinyarwanda = Just "Gira inama umubyeyi ku kamaro ko konsa umwana nta kindi avangiye kugera ku mezi 6"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.AppropriateComplementaryFeeding ->
                    { english = "Provide counseling on the consequences of not feeding the child complementary food at the appropriate times, as per the guidance"
                    , kinyarwanda = Just "Gira inama umubyeyi ku ngaruko zo kudaha umwana ifashabere igizwe n'indyo yuzuye ku gihe kandi uko byagenwe"
                    , kirundi = Nothing
                    }

                BeneficiaryCashTransfer ->
                    { english = "Provide counseling to the mother to go to the local government in charge and advocate for them"
                    , kinyarwanda = Just "Gira inama umubyeyi yo kugana inzego zifite mu nshigano kubarura abagenerwabikorwa, unamukorere ubuvugizi"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.ConditionalFoodItems ->
                    { english = "Provide counseling to the beneficiaries to go to the local government in charge and advocate for them"
                    , kinyarwanda = Just "Gira inama abagenerwabikorwa yo kugana inzego zifite mu nshigano kubarura abagenerwabikorwa, unamukorere ubuvugizi"
                    , kirundi = Nothing
                    }

                TreatedForAcuteMalnutrition ->
                    { english = "Provide counseling about preventing acute malnutrition and send the child to the Health center"
                    , kinyarwanda = Just "Gira inama umubyeyi uburyo barinda imirire mibi unohereze umwana ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                ReceivingSupport ->
                    { english = "Provide counseling to the mother to take the child to the Health center so that they can get the support needed"
                    , kinyarwanda = Just "Gira inama umubyeyi yo kujyana umwana ku kigo nderabuzima bamuhe ubufasha bukwiriye"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasCleanWater ->
                    { english = "Provide counseling on how to prepare clean water like boiling it and using water purifier"
                    , kinyarwanda = Just "Bagire inama y'uburyo bwo gusukura amazi nko kuyateka no gukoresha Sur’eau"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasHandwashingFacility ->
                    { english = "Provide counseling on the importance of handwashing facility, and tell them to buy it  and use it. If they don't have means advocate for them"
                    , kinyarwanda = Just "Bagire inama y'akamaro ka kandagirukarabe, abayifite bayikoreshe. Abadafite ubushobozi bakorerwe ubuvugizi."
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasToilets ->
                    { english = "Provide counseling by telling them to build toilets. If they don't have means advocate for them"
                    , kinyarwanda = Just "Bagire inama yo kubaka ubwiherero. Abadafite ubushobozi bakorerwe ubuvugizi"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasKitchenGarden ->
                    { english = "Provide counseling on the importance of eating fruits and vegetables from kitchen garden. And tell them to build one. If they don't have means advocate for them"
                    , kinyarwanda = Just "Bagire inama ku kamaro ko kurya imbuto n’imboga ubashishikarize kugira umurima w’igikoni. Abadafite ubushobozi bakorerwe ubuvugizi"
                    , kirundi = Nothing
                    }

                InsecticideTreatedBednets ->
                    { english = "Provide counseling on the importance of using the insecticide-treated bednets and advise them to have one"
                    , kinyarwanda = Just "Bagire inama ku byiza byo kuryama mu nzitiramibu iteye umuti unabashishikarize kuyigira"
                    , kirundi = Nothing
                    }

                MealsAtRecommendedTimes ->
                    { english = "Provide counseling on the consequences of not feeding the child at recommended times, as per the guidance"
                    , kinyarwanda = Just "Gira umubyeyi inama ku ngaruko zo kutagaburira umwana inshuro zagenwe, umushishikarize kugaburira umwna inshuro zagenwe"
                    , kirundi = Nothing
                    }

                ChildReceivesFBF ->
                    { english = "Provide counseling on the importance of FBF and if they haven't received it advise them to go to the Health center to recieve them"
                    , kinyarwanda = Just "Niba ari umugenerwabikorwa wacikanywe, gira inama umubyeyi cg undi urera umwana kugana ikigo nderabuzima gufata Shisha Kibondo"
                    , kirundi = Nothing
                    }

                ChildReceivesVitaminA ->
                    { english = "Provide counseling on the importance of Vitamin A and advise them not to miss it again"
                    , kinyarwanda = Just "Gira inama umubyeyi ku kamaro ko gufata ikinini cya vitamini A unamugire inama yo kutongera gucikanwa"
                    , kirundi = Nothing
                    }

                ChildReceivesDewormer ->
                    { english = "Provide counseling on the importance of deworming medication and advise them not to miss it again"
                    , kinyarwanda = Just "Gira inama umubyeyi ku kamaro ko gufata ikinini cy’inzoka ku mikurire myiza y’umwana unamugire inama yo kutongera gucikanwa"
                    , kirundi = Nothing
                    }

                ChildReceivesECD ->
                    { english = "Provide counseling on the importance of brain stimulation activities for the development of the child"
                    , kinyarwanda = Just "Gira inama umubyeyi cyangwa urera umwana ku kamaro ko gukangura ubwonko bw’umwana umushishikarize kubikora unamwereka uko bikorwa kandi"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDASignHelperHeader sign ->
            case sign of
                FiveFoodGroups ->
                    { english = "Food groups"
                    , kinyarwanda = Just "Amoko y'ibiribwa"
                    , kirundi = Nothing
                    }

                MealsAtRecommendedTimes ->
                    { english = "Ask and check if the daily frequency of the complementary food is enough"
                    , kinyarwanda = Just "Baza unarebe niba inshuro umwana afata ifunguro ku munsi zihagije"
                    , kirundi = Nothing
                    }

                -- Other signs don't have helper dialog.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDASignQuestion sign ->
            case sign of
                BornWithBirthDefect ->
                    { english = "Was the child born with a birth defect"
                    , kinyarwanda = Just "Umwana yaba yaravukanye ubumuga"
                    , kirundi = Just "Mbega umwana yavukanye ubumuga"
                    }

                SupplementsDuringPregnancy ->
                    { english = "Did the mother receive Iron, Folic Acid/MMS"
                    , kinyarwanda = Just "Umubyeyi yahawe ibinini bya Fer, Folic Acid cg MMS byongera amaraso"
                    , kirundi = Nothing
                    }

                TakenSupplementsPerGuidance ->
                    { english = "Has she taken it as per guidance (CHW observed)"
                    , kinyarwanda = Just "Yabifashe nkuko byagenwe (Umujyanama w'ubuzima abisuzume)"
                    , kirundi = Nothing
                    }

                ChildBehindOnVaccination ->
                    { english = "According to E-Heza the child is behind on vaccinations, is this correct"
                    , kinyarwanda = Just "Urebeye muri sisiteme ya E-heza, umwana ntabwo afite inkingo zose zagenwe, ese ni byo"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.OngeraMNP ->
                    { english = "Did the child receive Ongera-MNP"
                    , kinyarwanda = Just "Umwana yahawe Ongera intungamubiri"
                    , kirundi = Nothing
                    }

                TakingOngeraMNP ->
                    { english = "Is Ongera-MNP being consumed"
                    , kinyarwanda = Just "Ongera intungamubiri ihabwa umwana nkuko bikwiriye"
                    , kirundi = Nothing
                    }

                FiveFoodGroups ->
                    { english = "Did the child receive food items from the 5 food groups in the last 24 hours"
                    , kinyarwanda = Just "Umwana yahawe amafunguro yo mu moko atanu kuva ejo hashize"
                    , kirundi = Nothing
                    }

                BreastfedForSixMonths ->
                    { english = "Was the child breastfed for 6 months without interruption"
                    , kinyarwanda = Just "Umwana yahawe amashereka gusa mu mezi 6 nta kindi bamuvangiye"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.AppropriateComplementaryFeeding ->
                    { english = "Does the child receive appropriate complementary feeding"
                    , kinyarwanda = Just "Umwana ku mezi 6 yatangiye gufata ifashabere igizwe n’indyo yuzuye akomeza no konswa"
                    , kirundi = Nothing
                    }

                BeneficiaryCashTransfer ->
                    { english = "Is the mother or the child beneficiary of cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Umubyeyi cg umwana ni abagenerwa bikorwa b'amafaranga y’inkunga (e.g. VUP, NSDS)"
                    , kirundi = Nothing
                    }

                ReceivingCashTransfer ->
                    { english = "Are they receiving it"
                    , kinyarwanda = Just "Bahabwa inkunga"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.ConditionalFoodItems ->
                    { english = "Receipt of other support (e.g., food items or small livestock,...)"
                    , kinyarwanda = Just "Izindi nkunga z’ingoboka (urugero: ibiryo, amatungo,...)"
                    , kirundi = Nothing
                    }

                TreatedForAcuteMalnutrition ->
                    { english = "Is the child being treated"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildWithDisability ->
                    { english = "Does the child have disability"
                    , kinyarwanda = Just "Umwana afite ubumuga"
                    , kirundi = Nothing
                    }

                ReceivingSupport ->
                    { english = "Does the child receive support"
                    , kinyarwanda = Just "Umwana ahabwa ubufasha"
                    , kirundi = Nothing
                    }

                ChildGotDiarrhea ->
                    { english = "Does the child have diarrhea"
                    , kinyarwanda = Just "Umwana afite impiswi"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasCleanWater ->
                    { english = "Does the house have clean water"
                    , kinyarwanda = Just "Urugo rufite amazi asukuye"
                    , kirundi = Just "Mbega urugo barafise amazi meza"
                    }

                Backend.Measurement.Model.HasHandwashingFacility ->
                    { english = "Does the house have a handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe kandi irakoreshwa"
                    , kirundi = Just "Mbega urugo barafise aho bakarabira mu ntoke"
                    }

                Backend.Measurement.Model.HasToilets ->
                    { english = "Does the household have toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero bwujuje ibyangombwa"
                    , kirundi = Just "Mbega umuryango barafise utuzu twasugumwe/utuzu twubwiherero"
                    }

                Backend.Measurement.Model.HasKitchenGarden ->
                    { english = "Does the house have a kitchen garden"
                    , kinyarwanda = Just "Urugo rufite umurima w’igikoni"
                    , kirundi = Just "Mbega urugo barafise akarima k'igikoni"
                    }

                InsecticideTreatedBednets ->
                    { english = "Is the mother using the insecticide-treated bednets"
                    , kinyarwanda = Just "Umubyeyi akoresha inzitiramubu iteye umuti"
                    , kirundi = Nothing
                    }

                MealsAtRecommendedTimes ->
                    { english = "Does the child eat at the recommended times per day"
                    , kinyarwanda = Just "Umwana afata ifunguro ku munsi inshuro zihagije kandi zagenwe"
                    , kirundi = Nothing
                    }

                ChildReceivesFBF ->
                    { english = "Did the child receive FBF"
                    , kinyarwanda = Just "Umwana yahawe Shisha Kibondo"
                    , kirundi = Nothing
                    }

                ChildTakingFBF ->
                    { english = "Is FBF being consumed"
                    , kinyarwanda = Just "FBF ihabwa umwana nkuko bikwiriye"
                    , kirundi = Nothing
                    }

                ChildReceivesVitaminA ->
                    { english = "Did the child receive Vitamin A in the last six months"
                    , kinyarwanda = Just "Mu mezi atandatu ashize, umwana yahawe ikinini cya vitamini A"
                    , kirundi = Nothing
                    }

                ChildReceivesDewormer ->
                    { english = "Did the child receive deworming medication in the last six months"
                    , kinyarwanda = Just "Mu mezi atandatu ashize, umwana yahawe ikinini cy’inzoka"
                    , kirundi = Nothing
                    }

                ChildReceivesECD ->
                    { english = "Do you sing lullabies, poems, and read books to your child, or play games with your child"
                    , kinyarwanda = Just "Uririmbira umwana ibihozo, n'imivugo, ukamusomera ibitabo mukanakina"
                    , kirundi = Nothing
                    }

                ShowsEdemaSigns ->
                    { english = "Does the child show signs of Edema"
                    , kinyarwanda = Just "Umwana agaragaza ibimenyetso b’ububyimbe"
                    , kirundi = Nothing
                    }

                NoNCDASigns ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta na kimwe"
                    }

        NCDAUpdateVaccineRecordMessage ->
            { english = "Please update the child's vaccine record with information from the vaccine card at the end of this scorecard visit"
            , kinyarwanda = Just "Uzuza inkingo zitanditse ukoresheje amakuru ari ku ifishi y'inkingo y'umwana nyuma yo kurangiza ibikorwa byo ku ifishi y'imikurire y'umwana"
            , kirundi = Nothing
            }

        NCDActivityTitle activity ->
            case activity of
                Backend.NCDActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    , kirundi = Just "Ibimenyetso vy'akaga"
                    }

                Backend.NCDActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Just "Ikibazo"
                    }

                Backend.NCDActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Just "Kuvyara k'urugero"
                    }

                Backend.NCDActivity.Model.MedicalHistory ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    , kirundi = Just "Akahise ko m'ubuvuzi"
                    }

                Backend.NCDActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Just "Aho bapimira ingwara"
                    }

                Backend.NCDActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

                Backend.NCDActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Just "Isubiramwo ry'ikimenyetso"
                    }

                Backend.NCDActivity.Model.OutsideCare ->
                    translationSet OutsideCareLabel

        NCDANCServicesInstructions ->
            { english = "Refer patient to ANC services for further management of hypertension during pregnancy"
            , kinyarwanda = Just "Ohereza umubyeyi muri serivise yita ku babyeyi batwite bakurikrane byimbitse umuvuduko w'amaraso"
            , kirundi = Just "Rungika umuvyeyi mu gisata kiraba ingwara zitandukira kugira barabe neza ibijanye n'umuvuduko w'amaraso mu gihe c'imbanyi"
            }

        NCDAANCNewbornItemLabel item ->
            case item of
                RegularCheckups ->
                    { english = "Regular prenatal and postpartum checkups"
                    , kinyarwanda = Just "Yisuzumishije uko bikwiye atwite na nyuma yo kubyara"
                    , kirundi = Just "Kwisuzumisha buri igihe imbere n'inyuma yo kuvyara"
                    }

                IronDuringPregnancy ->
                    { english = "Iron during pregnancy"
                    , kinyarwanda = Just "Yafashe umuti wongera amaraso atwite"
                    , kirundi = Just "Icunyunyu c'icuma mu gihe c'imbanyi"
                    }

        NCDAInfrastructureEnvironmentWashItemLabel item ->
            case item of
                Pages.WellChild.ProgressReport.Model.HasToilets ->
                    { english = "Household has toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero"
                    , kirundi = Just "Umuryango urafise utuzu twasugumwe"
                    }

                Pages.WellChild.ProgressReport.Model.HasCleanWater ->
                    { english = "Household has clean water"
                    , kinyarwanda = Just "Urugo rufite amazi meza"
                    , kirundi = Just "Umuryango urafise amazi meza"
                    }

                Pages.WellChild.ProgressReport.Model.HasHandwashingFacility ->
                    { english = "Household has handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe"
                    , kirundi = Just "Umuryango urafise aho bakarabira mu ntoke"
                    }

                Pages.WellChild.ProgressReport.Model.HasKitchenGarden ->
                    { english = "Household has kitchen garden"
                    , kinyarwanda = Just "Urugo rufite akarima k'igikoni"
                    , kirundi = Just "Umuryango urafise akarima k'igikoni"
                    }

                Pages.WellChild.ProgressReport.Model.InsecticideTreatedBedNets ->
                    { english = "Insecticide treated bed nets"
                    , kinyarwanda = Just "Urugo rufite nzitiramibu ikoranye umuti"
                    , kirundi = Just "Imisegetera itewemwo imiti y'ica imibu"
                    }

        NCDANutritionBehaviorItemLabel item ->
            case item of
                Pages.WellChild.ProgressReport.Model.BreastfedSixMonths ->
                    { english = "Breastfed baby for 6 mo without interruption"
                    , kinyarwanda = Just "Konsa umwana amezi 6 utamuvangiye"
                    , kirundi = Just "Umwana yonse mu kiringo c'amezi atandatu (6) ataguhagarika"
                    }

                Pages.WellChild.ProgressReport.Model.AppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 mo)"
                    , kinyarwanda = Just "Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)"
                    , kirundi = Just "Iyongerezwa ry'ifunguro ribereye/ryiza (iyongerezwa ryo gufungura neza)"
                    }

                DiverseDiet ->
                    { english = "Does the child have a diverse diet?"
                    , kinyarwanda = Just "Umwana afata indyo yuzuye"
                    , kirundi = Just "Mbega umwana arafise ifunguro rinyuranye?"
                    }

                MealsADay ->
                    { english = "Number of times a child eats a day"
                    , kinyarwanda = Just "Inshuro umwana afata ifunguro ku munsi"
                    , kirundi = Just "Igitigiri c'incuro umwana afungurako k'umunsi"
                    }

        NCDATargetedInterventionsItemLabel item ->
            case item of
                FBFGiven ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentForAcuteMalnutrition ->
                    { english = "Treatment for acute malnutrition (severe or moderate)"
                    , kinyarwanda = Just "Kuvura imiritre mibi  ifatiyeho(Ikabije cg yoroheje)"
                    , kirundi = Just "Ukuvura ingwara yo gufungura nabi ikaze (Ikaze canke igereranye)"
                    }

                TreatmentForDiarrhea ->
                    { english = "Treatment of diarrhea (ORS & Zinc)"
                    , kinyarwanda = Just "Kuvura impiswi(Ukoresheje Zinc cg ORS)"
                    , kirundi = Just "Kuvura guhitwa (ORS & Zinc)"
                    }

                SupportChildWithDisability ->
                    { english = "Provide support to a child with a disability "
                    , kinyarwanda = Just "Guha umwana ufite ubumuga ubufasha bwihariye"
                    , kirundi = Just "Tanga indemesho/remesha umwana afise ubumuga"
                    }

                ConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP"
                    , kirundi = Just "Ukuronka ry'itegeko ry'amafaranga arungikwa; Akarorero: NSDS, VUP"
                    }

                Pages.WellChild.ProgressReport.Model.ConditionalFoodItems ->
                    { english = "Receipt of other support (e.g., food items or small livestock)"
                    , kinyarwanda = Just "Gufata Izindi nkunga z’ingoboka (ibiryo, amatungo)"
                    , kirundi = Nothing
                    }

        NCDAUniversalInterventionsItemLabel item ->
            case item of
                Immunization ->
                    { english = "Immunization"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Just "Incanco"
                    }

                Pages.WellChild.ProgressReport.Model.VitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Deworming ->
                    { english = "Deworming"
                    , kinyarwanda = Just "Imiti y'inzoka"
                    , kirundi = Just "vermifugation"
                    }

                Pages.WellChild.ProgressReport.Model.OngeraMNP ->
                    { english = "Use additional nutrients (Ongera)"
                    , kinyarwanda = Just "Koresha Ongera intungamubiri"
                    , kirundi = Just "Koresha intungamara ziyongera"
                    }

                ECDServices ->
                    { english = "ECD services provided to child"
                    , kinyarwanda = Just "Umwana yahawe servise n'ikigo mboneza mikurire"
                    , kirundi = Just "Ibikorwa vya IUM (DPE) bikorerwa umwana"
                    }

        NCDAFillTheBlanksItemLabel item ->
            case item of
                HeightToAge ->
                    { english = "Level of stunting using child length mat"
                    , kinyarwanda = Just "Ikigero cyo kugwingira hakoreshejwe agasambi"
                    , kirundi = Nothing
                    }

                WeightToAge ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Just "Uburemere"
                    }

                MuacValue ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
                    }

                EdemaPresent ->
                    { english = "Edema"
                    , kinyarwanda = Just "Kubyimba"
                    , kirundi = Just "Ukuvyimba amagur"
                    }

        NCDANoANVCVisitsOnRecord ->
            { english = "There are no recorded ANC visits for the mother of this child"
            , kinyarwanda = Just "Nta makuru agaragara yo gupimisha inda ku mubyeyi w'uyu mwana"
            , kirundi = Nothing
            }

        NCDANumberImmunizationAppointmentLabel maybeDate ->
            Maybe.map
                (\date ->
                    { english = "According to E-Heza, you have immunization appointment scheduled for " ++ formatDDMMYYYY date
                    , kinyarwanda = Just <| "Urebeye muri sisiteme ya E-heza, ufite gahunda yo gukingiza ku itariki " ++ formatDDMMYYYY date
                    , kirundi = Nothing
                    }
                )
                maybeDate
                |> Maybe.withDefault
                    { english = "According to E-Heza, you have no immunization appointment scheduled"
                    , kinyarwanda = Just "Urebeye muri Sisiteme ya E-heza, nta tariki ya gahunda y'ikingiza igaragara"
                    , kirundi = Nothing
                    }

        NCDAStep step ->
            case step of
                NCDAStepAntenatalCare ->
                    translationSet ANCNewborn

                NCDAStepNutritionAssessment ->
                    translationSet NutritionAssessmentLabel

                NCDAStepUniversalInterventions ->
                    translationSet UniversalInterventions

                NCDAStepNutritionBehavior ->
                    translationSet NutritionBehavior

                NCDAStepTargetedInterventions ->
                    translationSet TargetedInterventions

                NCDAStepInfrastructureEnvironment ->
                    translationSet InfrastructureEnvironment

        NCDDangerSign sign ->
            case sign of
                Dyspnea ->
                    { english = "Acute Shortness of Breath (Dyspnea)"
                    , kinyarwanda = Just "Guhumeka nabi biziyeho"
                    , kirundi = Just "Ukubura impwemu bikabije cane (dyspinée)"
                    }

                VisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Just "Uko areba byahindutse"
                    , kirundi = Just "Impinduka y'icerekezo"
                    }

                ChestPain ->
                    { english = "Chest Pain"
                    , kinyarwanda = Just "Kubabara mu gatuza"
                    , kirundi = Just "Ububabare bw'igikiriza"
                    }

                FlankPain ->
                    { english = "Flank Pain"
                    , kinyarwanda = Just "Kubabara mu Ibondo"
                    , kirundi = Just "Ububabare bwo m'urubavu"
                    }

                Hematuria ->
                    { english = "Blood in Urine (Hematuria)"
                    , kinyarwanda = Just "Amaraso mu nkari"
                    , kirundi = Just "Amaraso m'umukoyo (hématurie)"
                    }

                SevereHeadaches ->
                    { english = "Severe Headaches"
                    , kinyarwanda = Just "Kuribwa umutww bikabije"
                    , kirundi = Just "Ukumeneka umutwe gukaze"
                    }

                LossOfConciousness ->
                    { english = "Loss of Consciousness Since Last Visit"
                    , kinyarwanda = Just "Yataye ubwenge kandi ntiyumva kuva isura riheruka"
                    , kirundi = Just "Yarataye ubwenge kuva k'urugendo rwanyuma"
                    }

                NoNCDDangerSigns ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        NCDDiagnosisForProgressReport withRenalComplications isPregnant diagnosis ->
            let
                hypertensionInPregnancy =
                    { english = "Hypertension in Pregnancy"
                    , kinyarwanda = Just "Umuvuduko w'amaraso mu gihe utwite"
                    , kirundi = Just "Umuvuduko urenze w'amaraso mu gihe c'imbanyi"
                    }
            in
            case diagnosis of
                DiagnosisHypertensionStage1 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage One Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Just "Umuvuduko w'amaraso uri mu c'iciro/mu gice ca mbere hamwe n'ingorane z'amafyigo"
                        }

                    else
                        { english = "Stage One Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere"
                        , kirundi = Just "Umuvuduko w'amaraso uri mu c'iciro/mu gice ca mbere"
                        }

                DiagnosisHypertensionStage2 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage Two Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Just "Umuvuduko w'amaraso uri mu c'iciro/mu gice ca kabiri hamwe n'ingorane z'amafyigo"
                        }

                    else
                        { english = "Stage Two Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri"
                        , kirundi = Just "Umuvuduko w'amaraso uri mu c'iciro/mu gice ca kabiri"
                        }

                DiagnosisHypertensionStage3 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage Three Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Just "muvuduko w'amaraso uri mu c'iciro/mu gice ca gatatu hamwe n'ingorane z'amafyigo"
                        }

                    else
                        { english = "Stage Three Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu"
                        , kirundi = Just "muvuduko w'amaraso uri mu c'iciro/mu gice ca gatatu"
                        }

                DiagnosisDiabetesInitial ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                DiagnosisDiabetesRecurrent ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                DiagnosisRenalComplications ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoNCDDiagnosis ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDExaminationTask task ->
            case task of
                TaskCoreExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Just "Isuzuma ryimbitse"
                    , kirundi = Just "Igipimo c'umubiri c'intango"
                    }

                TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Just "Ivyangombwa"
                    }

        NCDFamilyHistorySignQuestion sign ->
            case sign of
                SignHypertensionHistory ->
                    { english = "Has anyone in your family been told they have hypertension"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wabwiwe ko afite Umuvuduko w'amaraso"
                    , kirundi = Just "Has anyone in your family been told they have hypertension"
                    }

                SignHeartProblemHistory ->
                    { english = "Has anyone in your family been told they have a problem with their heart"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wagize ibibazo by'umutima"
                    , kirundi = Just "Mbega hari umuntu mu muryango yigeze kubwigwa ko afise ingorane z'umutima"
                    }

                SignDiabetesHistory ->
                    { english = "Has anyone in your family been told they have a problem with diabetes"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wagize ibibazo bya Diyabete"
                    , kirundi = Just "Mbega hari umuntu mu muryango yigeze kubwigwa ko afise ingorane za Diyabete"
                    }

                NoNCDFamilyHistorySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDGuidanceSignQuestion sign ->
            case sign of
                ReturnInOneMonth ->
                    { english = "Have you advised the patient to return in one month for a check-up"
                    , kinyarwanda = Just "Waba wagiriye umubyeyi inama ko azagaruka kwisuzumisha nyuma y'ukwezi"
                    , kirundi = Just "Woba warahanuye umugwayo ngo zogaruke mu kiringo c'ukwezi kugira agirishe isuzuma"
                    }

                NoNCDGuidanceSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDHealthEducationHeader ->
            { english = "Stage One Hypertension"
            , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere"
            , kirundi = Just "Umuvuduko w'amaraso uri mu c'iciro/mu gice ca mbere"
            }

        NCDHealthEducationInstructions ->
            { english = "Counsel patient on lifestyle changes and the root causes of hypertension"
            , kinyarwanda = Just "Igisha umurwayi ku bijyanye no guhindura imibereho n'iby'ibanze bishobora kuzamura umuvuduko"
            , kirundi = Just "Uguhanura umugwayi kugira ahindure imibereho y'ubuzima hamwe n'impamvuvyinyegeje - vy'indani bitera umuvuduko udasanzwe wo hejuru w'amaraso"
            }

        NCDHealthEducationQuestion ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Just "Wahaye umubyeyi inyigisho zabugenewe ku buzima"
            , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara meza k'umugwayi"
            }

        NCDLabsCaseManagementEntryTypeResults ->
            { english = "NCD Lab Results"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'indwara zitandura"
            , kirundi = Just "Inyishu z'ibipimo z'ingwara zitandukira"
            }

        NCDMedicalHistoryTask task ->
            case task of
                TaskCoMorbidities ->
                    { english = "Co-Morbidities"
                    , kinyarwanda = Just "Ubundi burwayi asanganywe."
                    , kirundi = Just "Ingwara zirihamwe canke zegeranye"
                    }

                TaskMedicationHistory ->
                    { english = "Medication History"
                    , kinyarwanda = Just "Amakuru ku miti yafashe"
                    , kirundi = Just "Medication History"
                    }

                TaskSocialHistory ->
                    { english = "Social History"
                    , kinyarwanda = Just "Amakuru ku mibereho ye"
                    , kirundi = Just "Akahise k'imibereho"
                    }

                TaskFamilyHistory ->
                    { english = "Family History"
                    , kinyarwanda = Just "Amakuru ku muryango"
                    , kirundi = Just "Kahise k'umuryango"
                    }

                TaskOutsideCare ->
                    { english = "Outside Care"
                    , kinyarwanda = Just "Kuvurirwa ku rindi vuriro"
                    , kirundi = Just "Ukuvurirwa hanze"
                    }

        NCDNextStepsTask task ->
            case task of
                Pages.NCD.Activity.Types.TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                Pages.NCD.Activity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.Activity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Just "Kurungika"
                    }

        NCDGroup1Symptom symptom ->
            case symptom of
                SwellingInLegs ->
                    { english = "Swelling in Legs"
                    , kinyarwanda = Just "Kubyimba amaguru"
                    , kirundi = Just "Kuvyimba amaguru"
                    }

                UrinaryFrequency ->
                    { english = "Urinary Frequency"
                    , kinyarwanda = Just "Yihagarika inshuro nyinshi"
                    , kirundi = Just "Incuro z'ugusoba"
                    }

                Anxiety ->
                    { english = "Anxiety"
                    , kinyarwanda = Just "Kubura amahoro"
                    , kirundi = Just "Umuhangayiko"
                    }

                WeightLoss ->
                    { english = "Weight Loss"
                    , kinyarwanda = Just "Gutakaza ibiro"
                    , kirundi = Just "Uguta ibiro"
                    }

                Palpitations ->
                    { english = "Palpitations"
                    , kinyarwanda = Just "Umutima urasimbagurika"
                    , kirundi = Just "Itera ry'umutima"
                    }

                Tremor ->
                    { english = "Tremor"
                    , kinyarwanda = Just "Ibicuro"
                    , kirundi = Just "Kujugumira"
                    }

                SwellingInFace ->
                    { english = "Swelling in Face"
                    , kinyarwanda = Just "Kubyimba mu maso"
                    , kirundi = Just "Kuvyimba mu maso"
                    }

                SwellingInAbdomen ->
                    { english = "Swelling in Abdomen"
                    , kinyarwanda = Just "Kubyimba Inda"
                    , kirundi = Just "Kuvyimba mu nda"
                    }

                DizzinessWithChangingPosition ->
                    { english = "Dizziness with Changing Position"
                    , kinyarwanda = Just "Iyo ahinduye uko yari ameze ahita agira isereri"
                    , kirundi = Just "Kuzungurirwa hamwe no guhindura ikibanza"
                    }

                MildHeadache ->
                    { english = "Mild Headache"
                    , kinyarwanda = Just "Kubabara umutwe byoroheje"
                    , kirundi = Just "Kubabara umutwe bisanzwe"
                    }

                NoNCDGroup1Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        NCDGroup2Symptom symptom ->
            case symptom of
                WeaknessOfOneSideOfTheBody ->
                    { english = "Weakness of One Side of the Body"
                    , kinyarwanda = Just "Kunanirwa igice kimwe cy'umubiri"
                    , kirundi = Just "Intege nke zuruhande rumwe rw'umubiri"
                    }

                ProblemsWithWalking ->
                    { english = "Problems with Walking"
                    , kinyarwanda = Just "Kunanirwa kugenda"
                    , kirundi = Just "Ingorane zo gutambuka"
                    }

                ProblemsWithTalking ->
                    { english = "Problems with Talking"
                    , kinyarwanda = Just "Kunanirwa kuvuga"
                    , kirundi = Just "Ingorane zo kuvuga"
                    }

                DecreasedVision ->
                    { english = "Decreased Vision"
                    , kinyarwanda = Just "Kutareba neza"
                    , kirundi = Just "Ipunguka ryo kubona"
                    }

                BlurryVision ->
                    { english = "Blurry Vision"
                    , kinyarwanda = Just "Kureba ibikezikezi"
                    , kirundi = Just "Ugukanura nabi"
                    }

                IncreasedFatigueWithDailyActivities ->
                    { english = "Increased Fatigue with Daily Activities"
                    , kinyarwanda = Just "Kwiyongera ku munaniro"
                    , kirundi = Just "Kongereza uburuhe mu gukora ibikorwa vyari buri munsi"
                    }

                ShortOfBreathWhenLayingDown ->
                    { english = "Short of Breath When Laying Down"
                    , kinyarwanda = Just "Guhumeka nabi igihe aryamye"
                    , kirundi = Just "Ibura ry'impwemu mu gihe aryamye hasi"
                    }

                ShortOfBreathAtNight ->
                    { english = "Short of Breath at Night"
                    , kinyarwanda = Just "Guhumeka nabi nijoro"
                    , kirundi = Just "Ibura ry'impwemu mw'ijoro"
                    }

                KidneyProblems ->
                    { english = "Kidney Problems"
                    , kinyarwanda = Just "Ibibazo by'impyiko"
                    , kirundi = Just "Ingorane z'amafyigo"
                    }

                NCDIncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Just "Kugira inyota cyane"
                    , kirundi = Just "Kongereza inyota"
                    }

                NoNCDGroup2Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        NCDPainSymptom symptom ->
            case symptom of
                PainFlank ->
                    { english = "Flank"
                    , kinyarwanda = Just "Ibindo"
                    , kirundi = Just "Urubavu"
                    }

                PainLowerBack ->
                    { english = "Lower Back"
                    , kinyarwanda = Just "Umugongo wo hasi"
                    , kirundi = Just "Lower Back"
                    }

                PainFeet ->
                    { english = "Feet"
                    , kinyarwanda = Just "Ibirenge"
                    , kirundi = Just "Ibirenge"
                    }

                PainNeck ->
                    { english = "Neck"
                    , kinyarwanda = Just "Ijosi"
                    , kirundi = Just "Izosi"
                    }

                PainAbdomen ->
                    { english = "Abdomen"
                    , kinyarwanda = Just "Mu nda"
                    , kirundi = Just "Inda"
                    }

                NoNCDPainSymptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        NCDProgressReport ->
            { english = "NCD Progress Report"
            , kinyarwanda = Just "Raporo ku Burwayi Butandura"
            , kirundi = Just "Uguterintambwe mu cegeranyo c'ingwara zitandukira"
            }

        NCDRecurrentActivitiesTitle activity ->
            case activity of
                Backend.NCDActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    , kirundi = Just "Inyishu y'igipimo c'ingwara"
                    }

                Backend.NCDActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

        NCDRecurrentNextStepsTask task ->
            case task of
                Pages.NCD.RecurrentActivity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.RecurrentActivity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Just "Kurungika"
                    }

        NCDRiskFactor value ->
            case value of
                RiskFactorSmokeCigarettes ->
                    { english = "Smokes Cigarettes"
                    , kinyarwanda = Just "Anywa Itabi"
                    , kirundi = Just "Aranywa itabi"
                    }

                RiskFactorConsumeSalt ->
                    { english = "Adds Salt to Diet"
                    , kinyarwanda = Just "Wongera Umunyu mu biryo"
                    , kirundi = Just "Kongeramwo umunyu mw'ifunguro"
                    }

                RiskFactorHypertensionHistory ->
                    { english = "Family History of Hypertension"
                    , kinyarwanda = Just "Amakuru y'Uburwayi bw'Umuvuduko mu Muryango"
                    , kirundi = Just "Kahise k'umuryango kuvyerekeye umuvuduko udaszwe w'amaraso"
                    }

                RiskFactorHearProblemHistory ->
                    { english = "Family History of Heart Problems"
                    , kinyarwanda = Just "Amakuru y'Indwara z'Umutima mu Muryango"
                    , kirundi = Just "Kahise k'umuryango kuvyerekeye ingorane z'umutima"
                    }

                RiskFactorDiabetesHistory ->
                    { english = "Family History of Diabetes"
                    , kinyarwanda = Just "Amakuru y'Indwara ya Diyabete mu Muryango"
                    , kirundi = Just "Kahise k'umuryango kuvyerekeye ingwara ya Diyabete"
                    }

        NCDSocialHistoryFoodQuestion ->
            { english = "What foods do you eat most"
            , kinyarwanda = Just "Ni ibihe biryo ukunda kurya cyane"
            , kirundi = Just "Ibifungurwa wihereza cane n'ibihe"
            }

        NCDSocialHistoryFoodQuestionInstructions ->
            { english = "Please check the most fitting group"
            , kinyarwanda = Just "Hitamo Itsinda rikwiriye"
            , kirundi = Just "Muraraba umurwi ukwiye kuvy'ukuri"
            }

        NCDSocialHistorySignQuestion site sign ->
            case sign of
                SignDrinkAlcohol ->
                    { english = "Do you drink any alcoholic beverages"
                    , kinyarwanda = Just "Ujya unywa ibikomoka kunzoga"
                    , kirundi = Just "Mbega uranywa inzoga/ibinyobwa biboreza"
                    }

                SignSmokeCigarettes ->
                    { english = "Do you smoke cigarettes"
                    , kinyarwanda = Just "Ujya unywa itabi"
                    , kirundi = Just "Mbega uranywa itabi"
                    }

                SignConsumeSalt ->
                    case site of
                        SiteBurundi ->
                            { english = "Do you add salt to your food after it is served"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        _ ->
                            { english = "Do you add salt to your food"
                            , kinyarwanda = Just "Ujya wongera umunyu mu biryo"
                            , kirundi = Nothing
                            }

                SignDifficult4TimesAYear ->
                    { english = "Would it be difficult for you to come to the health center 4 times a year"
                    , kinyarwanda = Just "Byakugora kuza ku kigo nderabuzima inshuro 4 mu mwaka"
                    , kirundi = Just "Vyoba bigoye ko uza incuro 4 mu mwaka kw'ivuriro"
                    }

                SignHelpWithTreatmentAtHome ->
                    { english = "Are there people at home who can help you with treatment"
                    , kinyarwanda = Just "Hari umuntu mubana wagufasha gufata imiti"
                    , kirundi = Just "Hari abantu bari muhira bofasha mu kuvura"
                    }

                NoNCDSocialHistorySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Neck ->
            { english = "Neck"
            , kinyarwanda = Just "Ijosi"
            , kirundi = Just "Izosi"
            }

        NeckCPESign option ->
            case option of
                EnlargedThyroid ->
                    { english = "Enlarged Thyroid"
                    , kinyarwanda = Just "Umwingo"
                    , kirundi = Just "Tiroyide yagutse"
                    }

                EnlargedLymphNodes ->
                    { english = "Enlarged Lymph Nodes"
                    , kinyarwanda = Just "Inturugunyu/Amatakara"
                    , kirundi = Just "Ganglions lymphatiques hypertrophiés"
                    }

                NormalNeck ->
                    translationSet Normal

        NegativeLabel ->
            { english = "Negative"
            , kinyarwanda = Just "Nta bwandu afite"
            , kirundi = Just "ibibi"
            }

        Never ->
            { english = "Never"
            , kinyarwanda = Nothing
            , kirundi = Just "Nta na rimwe"
            }

        NextAppointment ->
            { english = "Next Appointment"
            , kinyarwanda = Just "Itariki yo kugarukaho"
            , kirundi = Just "Umubonano uzokurikira"
            }

        NextDue ->
            { english = "Next Due"
            , kinyarwanda = Just "itariki azahabwaho urukingo rukurikira"
            , kirundi = Just "Ikiringo kizokurikira"
            }

        NextImmunisationVisit isChw ->
            if isChw then
                { english = "Next immunization visit at the health center"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "Next immunization visit"
                , kinyarwanda = Just "Ikingira rikurikira"
                , kirundi = Just "Urugendo kubw'urucanco ruzokurikira umunsi ruzobako"
                }

        NextPediatricVisit isChw ->
            if isChw then
                { english = "Next pediatric visit at the health center"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "Next pediatric visit"
                , kinyarwanda = Just "Isura ry'umwana rikurikira"
                , kirundi = Just "Urugendo ruzokurikira rw'abana"
                }

        NextSteps ->
            { english = "Next Steps"
            , kinyarwanda = Just "Ibikurikiyeho"
            , kirundi = Just "Intambwe zikurkira"
            }

        NextStepsTask isChw task ->
            case task of
                NextStepsIsolation ->
                    if isChw then
                        { english = "Isolate Patient"
                        , kinyarwanda = Just "Shyira umurwayi mu kato"
                        , kirundi = Just "Umugwayi arategerezwa kwitandukanya mu kuja mu kibanza cawenyene"
                        }

                    else
                        { english = "Monitor at Home"
                        , kinyarwanda = Just "Gukurikiranira umurwayi mu rugo"
                        , kirundi = Just "Gukurikiranira muhira"
                        }

                NextStepsContactHC ->
                    { english = "Contact Health Center"
                    , kinyarwanda = Just "Menyesha ikigo nderabuzima"
                    , kirundi = Just "Ukuvugana n'ivuriro"
                    }

                NextStepsCall114 ->
                    { english = "Call 114"
                    , kinyarwanda = Just "Hamagara 114"
                    , kirundi = Just "Hamagara kuri 114"
                    }

                NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.AcuteIllness.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Just "Rungika kw'ivuriro"
                        }

                    else
                        { english = "Refer to Hospital"
                        , kinyarwanda = Just "Ohereza ku Bitaro"
                        , kirundi = Just "Rungika ku bitaro"
                        }

                Pages.AcuteIllness.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Just "Kurikirana"
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsContactTracing ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Just "Kurondera uwo mwavuganye"
                    }

                NextStepsSymptomsReliefGuidance ->
                    -- We qualify it as Medication distribution, to keep
                    -- consistant with other types of Covid steps.
                    translationSet MedicationDistribution

        No ->
            { english = "No"
            , kinyarwanda = Just "Oya"
            , kirundi = Just "Oya"
            }

        NoActivitiesCompleted ->
            { english = "No activities are entirely completed for the attending participants."
            , kinyarwanda = Just "Nta gikorwa cyarangiye cyose kubitabiriye."
            , kirundi = Nothing
            }

        NoActivitiesPending ->
            { english = "All activities are completed for the attending participants."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            , kirundi = Just "Ibikorwa vyose vyaheze kubitavye."
            }

        NoActivitiesCompletedForThisParticipant ->
            { english = "No activities are completed for this participant."
            , kinyarwanda = Just "Nta gikorwa cyarangiye kubitabiriye."
            , kirundi = Just "Nta bikorwa vy'abitavye inyigisho birahera"
            }

        NoActivitiesPendingForThisParticipant ->
            { english = "All activities are completed for this participant."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            , kirundi = Just "Ibikorwa vyose vyaheze kubitavye"
            }

        NoContactReason reason ->
            case reason of
                ReasonNoAnswer ->
                    { english = "Did not answer"
                    , kinyarwanda = Just "Ntago yitabye"
                    , kirundi = Just "Ntiyatanze inyishu"
                    }

                ReasonWrongContactInfo ->
                    { english = "Wrong contact information"
                    , kinyarwanda = Just "Amakuru atariyo"
                    , kirundi = Just "Amakuru y'uwo mwavuganye atariyo canke Ibiranga uwo mwovugana atarivyo"
                    }

                ReasonDeclinedFollowUp ->
                    { english = "Declined Follow Up"
                    , kinyarwanda = Just "Yanze gukurikiranwa"
                    , kirundi = Just "Yanse gukurikiranwa"
                    }

        NoMatchesFound ->
            { english = "No matches found"
            , kinyarwanda = Just "Ibyo wifuza ntibiboneste"
            , kirundi = Just "Nta nyishu yabonetse"
            }

        NormalRange ->
            { english = "Normal Range"
            , kinyarwanda = Just "Ibimeze neza"
            , kirundi = Just "Icanya gisanzwe"
            }

        NotApplicable ->
            { english = "Not Applicable "
            , kinyarwanda = Just "Ibi ntibikorwa"
            , kirundi = Just "Ntibikenewe"
            }

        NoTreatmentAdministered ->
            { english = "No treatment administered"
            , kinyarwanda = Just "Nta muti watanzwe"
            , kirundi = Just "Nta muti watanzwe"
            }

        NoTreatmentRecorded ->
            { english = "No treatment recorded"
            , kinyarwanda = Just "Nta muti yanditswe"
            , kirundi = Just "Nta muti wanditse"
            }

        NutritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            , kirundi = Just "Ibimenyetso vyo gufungura"
            }

        ReasonForNonReferral reason ->
            case reason of
                ClientRefused ->
                    { english = "Client refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Just "Umugwayi yaranse"
                    }

                NoAmbulance ->
                    { english = "No ambulance available"
                    , kinyarwanda = Just "Nta mbangukiragutabara ihari"
                    , kirundi = Just "Nta Rusehabaniha ihari"
                    }

                ClientUnableToAffordFees ->
                    { english = "Client unable to afford fees"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Just "Umugwayi adashobora kwishura amafaranga"
                    }

                ClientAlreadyInCare ->
                    { english = "Client already in care"
                    , kinyarwanda = Just "Umukiriya ari kwitabwaho"
                    , kirundi = Just "Umugwayi asanzwe yitahweho"
                    }

                ReasonForNonReferralNotIndicated ->
                    { english = "Not indicated"
                    , kinyarwanda = Just "Ntibyasabwe"
                    , kirundi = Just "Ntivyerekanywe"
                    }

                ReasonForNonReferralOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoReasonForNonReferral ->
                    { english = "No Reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    , kirundi = Just "Nta citwazo"
                    }

        AdministrationNote note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Just "Ntabiriyo m'ububiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    , kirundi = Just "Ihindagurika ry'umuiri rizwi"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Just "Umugwayi yaranse"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Just "Umugwayi ntashobora kuronka"
                    }

                NonAdministrationHomeBirth ->
                    { english = "Home Birth"
                    , kinyarwanda = Just "Yabyariye mu rugo"
                    , kirundi = Just "Kuvyarira muhira"
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Just "Aragwaye cane"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                AdministeredToday ->
                    { english = "Administered Today"
                    , kinyarwanda = Just "Yahawe umuti uyu munsi"
                    , kirundi = Nothing
                    }

                AdministeredPreviously ->
                    { english = "Already Received"
                    , kinyarwanda = Just "Byamaze kwakirwa"
                    , kirundi = Just "Vyakiriwe kera"
                    }

        AdministrationNoteForPrenatalImmunisation note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    , kirundi = Just "Ntabiriyo m'ububiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    , kirundi = Just "Ihindagurika ry'umuiri rizwi"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Just "Umugwayi yaranse"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Just "Umugwayi ntashobora kuronka"
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Just "Aragwaye cane"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                -- Other options are not relevant for Immunisation.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        AdministrationNoteForWellChildImmunisation note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    , kirundi = Just "Ntabiriyo m'ububiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    , kirundi = Just "Ihindagurika ry'umuiri rizwi"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Mother / Caregiver Declined"
                    , kinyarwanda = Just "Umubyeyi / Umurezi yanze"
                    , kirundi = Just "Mama / umurezi yaranse"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Just "Umugwayi ntashobora kuronka"
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Just "Aragwaye cane"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                -- Other options are not relevant for Immunisation.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NoParticipantsCompleted ->
            { english = "No participants have completed all their activities yet."
            , kinyarwanda = Just "Ntagikorwa nakimwe kirarangira kubitabiriye."
            , kirundi = Just "Ntabitavye inyigisho bahejeje inikorwa vyabo vyose"
            }

        NoParticipantsPending ->
            { english = "All attending participants have completed their activities."
            , kinyarwanda = Just "Abaje bose barangirijwe"
            , kirundi = Just "Abitavye inyigisho (canke ibikorwa) bose bahejeje ibikorwa vyabo"
            }

        NoParticipantsCompletedForThisActivity ->
            { english = "No participants have completed this activity yet."
            , kinyarwanda = Just "Ntawaje warangirijwe kukorerwa."
            , kirundi = Just "Ntabitavye inyigisho barashobora guheza iki gikorwa"
            }

        NoReferralRecorded ->
            { english = "No referral recorded"
            , kinyarwanda = Just "Nta koherezwa kwagaragaye"
            , kirundi = Just "Ntakurungika umugwayi ahandi (kubindi bitaro/kurindi vuriro) vyanditse"
            }

        NoParticipantsPendingForThisActivity ->
            { english = "All attending participants have completed this activitity."
            , kinyarwanda = Just "Ababje bose barangirijwe."
            , kirundi = Just "Abitavye ibikorwa bose bahejeje iki gikorwa"
            }

        Normal ->
            { english = "Normal"
            , kinyarwanda = Just "Bimeze neza/Nta kibazo gihari"
            , kirundi = Just "Bisanzwe"
            }

        NoChildrenRegisteredInTheSystem ->
            { english = "No children registered in the system"
            , kinyarwanda = Just "Ntamwana wanditswe muriyi sisiteme"
            , kirundi = Just "Nta bana biyandikishije muri ubu buryo bugezweho (muri sisiteme)"
            }

        NotAvailable ->
            { english = "not available"
            , kinyarwanda = Just "Ntibiboneste"
            , kirundi = Just "Ntiboneka"
            }

        NotFollowingRecommendationQuestion ->
            { english = "Why recommendations were not followed"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            , kirundi = Just "Kubera iki ivyifuzo bitakurikijwe"
            }

        NotTaken ->
            { english = "Not taken"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            , kirundi = Just "Nticafashwe"
            }

        NumberOfAbortions ->
            { english = "Number of Abortions"
            , kinyarwanda = Just "Umubare w'inda zavuyemo"
            , kirundi = Just "Igitigiri c'inda zakorotse"
            }

        NumberOfChildrenUnder5 ->
            { english = "Number of Children under 5"
            , kinyarwanda = Just "Umubare w'abana bari munsi y'imyaka 5"
            , kirundi = Just "Igitigiri c'abana bari munsi y'imyaka 5"
            }

        NumberOfCSections ->
            { english = "Number of C-Sections"
            , kinyarwanda = Just "Umubare w'inshuro yabazwe"
            , kirundi = Just "Igitigiri c'abakozwe mu kwibaruka"
            }

        NumberOfLiveChildren ->
            { english = "Number of Live Children"
            , kinyarwanda = Just "Umubare w'abana bariho"
            , kirundi = Just "Igitigiri c'abana bariho"
            }

        NumberOfStillbirthsAtTerm ->
            { english = "Number of Stillbirths at Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda bashyitse"
            , kirundi = Just "Igitigiri c'abavutse kw'itarike/ku gihe, bamaze gupfa "
            }

        NumberOfStillbirthsPreTerm ->
            { english = "Number of Stillbirths pre Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda badashyitse"
            , kirundi = Just "Igitigiri c'abavutse itarike itaragera/igihe kitarashika, bamaze gupfa"
            }

        NutritionActivityHelper activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    , kirundi = Just "Urabe neza ko wapimye hagati na hagati hejuru y'ukuboko k'umwana"
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    , kirundi = Just "Saba umuvyeyi afatire umutwe w'umwana wiwe ku mpera z'urubahu gw'ipimiro. Egereza akanyerezo ku gitsintsiri c'umwana hama ukwege ukuguru rimwe na rimwe."
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    , kirundi = Just "Sigurira umuvyeyi ingene yoraba ibimenyetso vyo gufungura nabi k'umwana wiwe."
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    , kirundi = Just "Fata ifoto y'umwana wese kuri buri isuzuma ry'amagara. Amafoto ategerezwa kwerekana umubiri wose wa buri mwana"
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    , kirundi = Just "Tumbereza neza umunzane imbere yo gupima ibiro vya mbere vy'uruyoya/umwana mutoya canke akivuka. Shira umwana mu gisipi/igishipi/ igikoresho kimufata mu gikiriza kandi ata mpuzu yambaye."
                    }

                Backend.NutritionActivity.Model.NCDA ->
                    translationSet ChildScorecard

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

        NutritionActivityTitle activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Just "Uburebure"
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Just "Ugufungura"
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Just "Ifoto"
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Just "Uburemere"
                    }

                Backend.NutritionActivity.Model.NCDA ->
                    translationSet ChildScorecard

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

        NutritionAssessment assessment ->
            case assessment of
                AssesmentAcuteMalnutritionModerate ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi yoroheje ije vuba"
                    , kirundi = Just "Ingwara yo gufungura nabi iri hejuru gato"
                    }

                AssesmentAcuteMalnutritionSevere ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi ikabije ije vuba"
                    , kirundi = Just "Ingwara yo gufungura nabi ikaze"
                    }

                AssesmentUnderweightModerate ->
                    { english = "Moderately Underweight"
                    , kinyarwanda = Just "Imirire mibi yoroheje ku biro"
                    , kirundi = Just "Ubuke bw'ibiro bisanzwe"
                    }

                AssesmentUnderweightSevere ->
                    { english = "Severely Underweight"
                    , kinyarwanda = Just "Imirire mibi ikabije ku biro"
                    , kirundi = Just "Ibiro bike cane"
                    }

                AssesmentDangerSignsNotPresent ->
                    { english = "Without Danger Signs"
                    , kinyarwanda = Just "Nta bimenyetso mpuruza"
                    , kirundi = Just "Without Danger Signs"
                    }

                AssesmentDangerSignsPresent ->
                    { english = "With Danger Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    , kirundi = Just "Hamwe n'ibimenyetso vya hatari"
                    }

                AssesmentMalnutritionSigns _ ->
                    { english = "Malnutrition Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    , kirundi = Just "Ibimenyetso vy'ingwara yo gufungura nabi"
                    }

                AssesmentConsecutiveWeightLoss ->
                    { english = "Consecutive Weight Loss"
                    , kinyarwanda = Just "Gutakaza ibiro mu buryo bwikurikiranije"
                    , kirundi = Just "Ukugabanuka ibiro vyikurikiranije"
                    }

                NoNutritionAssessment ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        NutritionAssessmentLabel ->
            { english = "Nutrition Assessment"
            , kinyarwanda = Just "Gusuzuma imirire"
            , kirundi = Just "Isuzuma ryo gufungura"
            }

        NutritionAssessmentTask task ->
            case task of
                TaskHeight ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Just "Uburebure"
                    }

                TaskHeadCircumference ->
                    { english = "Head Circumference"
                    , kinyarwanda = Just "Umuzenguruko w'umutwe"
                    , kirundi = Just "Umuzingi w'umutwe"
                    }

                TaskMuac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
                    }

                TaskNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Just "Ugufungura"
                    }

                TaskWeight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Just "Uburemere"
                    }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Just "Imyumvire ku bijyanye n’imirire"
            , kirundi = Just "Imitwarire yo gufungura neza"
            }

        NutritionCaringOption option ->
            case option of
                CaredByParent ->
                    { english = "Parent"
                    , kinyarwanda = Just "Umubyeyi"
                    , kirundi = Just "Umuvyeyi"
                    }

                CaredByGrandparent ->
                    { english = "Grandparent"
                    , kinyarwanda = Just "Nyirakuru/Sekuru"
                    , kirundi = Just "Sogokuru/Nyogokuru"
                    }

                CaredBySibling ->
                    { english = "Sibling"
                    , kinyarwanda = Just "Umuvandimwe"
                    , kirundi = Just "umuvukanyi"
                    }

                CaredByNeighbor ->
                    { english = "Neighbor"
                    , kinyarwanda = Just "Umuturanyi"
                    , kirundi = Just "Umubanyi"
                    }

                CaredByHouseHelper ->
                    { english = "House helper"
                    , kinyarwanda = Just "Umukozi wo mu rugo"
                    , kirundi = Just "Umufasha wo m'urugo"
                    }

                CaredByDaycare ->
                    { english = "Daycare"
                    , kinyarwanda = Just "Irerero"
                    , kirundi = Just "Irerero ry'abana"
                    }

        NutritionFeedingSignQuestion sign ->
            case sign of
                ReceiveSupplement ->
                    { english = "Did you receive food supplement"
                    , kinyarwanda = Just "Waba warahawe inyongeramirire"
                    , kirundi = Just "Did you receive food supplement"
                    }

                RationPresentAtHome ->
                    { english = "Is the ration of the food supplement present in the home"
                    , kinyarwanda = Just "Haba hari inyongeramirire usigaranye mu nzu"
                    , kirundi = Nothing
                    }

                EnoughTillNextSession ->
                    { english = "Is the available food supplement enough to last until the next health center session"
                    , kinyarwanda = Just "Iyo nyongeramiriee ufite yaba ihagije kugeza igihe uzasubirira ku kigonderabuzima"
                    , kirundi = Nothing
                    }

                SupplementShared ->
                    { english = "Is the food supplement being shared or eaten only by the sick child"
                    , kinyarwanda = Just "Ese inyongeramirire yaba ifatwa n'umwana urwaye gusa cyangwa yaba ayisangira n'abandi"
                    , kirundi = Just "Mbega ibifungugwa vyiyongerako bigaburirwa canke biribwa n'umwana agwaye gusa"
                    }

                EncouragedToEat ->
                    { english = "Does someone help / encourage the sick child to eat"
                    , kinyarwanda = Just "Hari umuntu waba afasha cyangwa ashishikariza umwana kurya"
                    , kirundi = Just "Mbega hari uwufasha/uwuremesha umwana agwaye gufungura"
                    }

                RefusingToEat ->
                    { english = "Is the child refusing to eat"
                    , kinyarwanda = Just "Ese umwana yanga kurya"
                    , kirundi = Just "Mbega umwana aranka kurya"
                    }

                FeedingSignBreastfeeding ->
                    { english = "Is the child currently breastfeeding (for children < 2)"
                    , kinyarwanda = Just "Umwana yaba yonka (ku bana bari munsi y'imyaka 2)"
                    , kirundi = Just "Mbega ubu umwana aronswa (ku bana bari < 2)"
                    }

                CleanWaterAvailable ->
                    { english = "Is clean water available"
                    , kinyarwanda = Just "Ese mazi asukuye arahari"
                    , kirundi = Just "Mbega amazi meza arahari"
                    }

                EatenWithWater ->
                    { english = "Is water given to the child when eating the food supplement"
                    , kinyarwanda = Just "Ese umwana yaba ahabwa amazi yo kunwa igihe afata inyongeramirire"
                    , kirundi = Just "Mbega umwana araronswa amazi mu gihe ariko arafungura imfungurwa ziyongerako"
                    }

                NoNutritionFeedingSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NutritionFoodSecuritySignQuestion sign ->
            case sign of
                HouseholdGotFood ->
                    { english = "Does the household currently have food available"
                    , kinyarwanda = Just "Ese ubu urugo rufite ibyo kurya"
                    , kirundi = Nothing
                    }

                NoNutritionFoodSecuritySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NutritionHelper ->
            { english = "Explain to the mother how to check the malnutrition signs for their own child."
            , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
            , kirundi = Just "Sigurira umuvyeyi ingene yoraba ibimenyetso vyo gufungura nabi k'umwana wiwe."
            }

        NutritionHygieneSignQuestion sign ->
            case sign of
                SoapInTheHouse ->
                    { english = "Is there soap for washing in the house"
                    , kinyarwanda = Just "Ese mu rugo haba hari isabune yo koga"
                    , kirundi = Just "Mbega mu rugo hariho isabuni yo gukaraba"
                    }

                WashHandsBeforeFeeding ->
                    { english = "Do the caregiver and child wash hands before the child is fed"
                    , kinyarwanda = Just "Ese umurezi n'umwana bakaraba intoki mbere y'uko umwana agaburirwa"
                    , kirundi = Just "Mbega umuvyeyi/umurezi hamwe n'umwana barakaraba imbere yuko umwana afungura"
                    }

                FoodCovered ->
                    { english = "Is the food / RUTF covered and free from flies"
                    , kinyarwanda = Just "Ese ibiryo/RUTUFU birapfundikiye kandi nta sazi zibiriho"
                    , kirundi = Just "Mbenga ibifungugwa - ATPE: Aliment Thérapeutique prêt à l'Emploi birapfutswe kandi birakingiwe insazi"
                    }

                NoNutritionHygieneSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NutritionNextStepsTask task ->
            case task of
                Measurement.Model.NextStepsSendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Just "Rungika kw'ivuriro"
                    }

                Measurement.Model.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                NextStepContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Just "Ivyazanye intererano"
                    }

                NextStepFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Just "Kurikirana"
                    }

        NutritionSupplementType type_ ->
            case type_ of
                FortifiedPorridge ->
                    { english = "Fortified Porridge"
                    , kinyarwanda = Just "Igikoma kirimo Imyunyu ngugu na Vitamin"
                    , kirundi = Just "Ubuyi bukomejwe"
                    }

                Rutf ->
                    { english = "RUTF"
                    , kinyarwanda = Just "RUTUFU"
                    , kirundi = Just "ATPE: Aliment Thérapeutique prêt à l'Emploi"
                    }

                Ongera ->
                    { english = "Ongera intungamubiri at the village level / CHW"
                    , kinyarwanda = Just "Ongera Intungamubiri mu mudugudu/Ku mujyanama w'Ubuzima"
                    , kirundi = Just "Kongereza intungamara k'urwego gw'ikigwati/umusozi - Abaremeshakiyago"
                    }

                TherapeuticMilk ->
                    { english = "Therapeutic Milk"
                    , kinyarwanda = Just "Amata avura"
                    , kirundi = Just "Amata yo kuvura"
                    }

                NoNutritionSupplementType ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta na kimwe"
                    }

        NitritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            , kirundi = Just "Ibimenyetso vyo gufungura"
            }

        Observations ->
            { english = "Observations"
            , kinyarwanda = Nothing
            , kirundi = Just "Imyihwezo"
            }

        ObstetricalDiagnosis ->
            { english = "Obstetrical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe n'inzobere mu gusuzuma abagore batwite"
            , kirundi = Just "Isuzuma ry'ivyara"
            }

        ObstetricalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisRhNegative ->
                    { english = "Patient is RH Negative"
                    , kinyarwanda = Just "Umurwayi afite Rezisi negatifu"
                    , kirundi = Just "Umugwayi afise Rhesus Négatif"
                    }

                DiagnosisModerateUnderweight ->
                    { english = "Moderate underweight"
                    , kinyarwanda = Just "Ibiro bike bidakabije ugendeye ku myaka"
                    , kirundi = Just "Ubuke bw'ibiro bisanzwe"
                    }

                DiagnosisSevereUnderweight ->
                    { english = "Severe underweight"
                    , kinyarwanda = Just "Afite ibiro bikie bikabije"
                    , kirundi = Just "Ibiro bike cane"
                    }

                DiagnosisOverweight ->
                    { english = "Overweight"
                    , kinyarwanda = Just "Aftie ibiro byinshi"
                    , kirundi = Just "Ubunini burenzeko"
                    }

                DiagnosisObese ->
                    { english = "Obese"
                    , kinyarwanda = Just "Kubyibuha gukabije"
                    , kirundi = Just "Ubuvyibuhe burenzeko"
                    }

                DisgnosisPeripheralEdema ->
                    { english = "Peripheral Edema"
                    , kinyarwanda = Just "Kubyimba amaguru n'amaboko"
                    , kirundi = Just "Ukuvyimba kw'amaguru"
                    }

                DiagnosisFetusBreech ->
                    { english = "Fetus is in breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Nothing
                    }

                DiagnosisFetusTransverse ->
                    { english = "Fetus is transverse"
                    , kinyarwanda = Just "Umwana aritambitse"
                    , kirundi = Just "Umwana arakikamye"
                    }

                DiagnosisBreastExamination ->
                    { english = "Breast exam showed"
                    , kinyarwanda = Just "Gusuzuma amabere byagaragaje"
                    , kirundi = Just "Ivyo igipimo c'amabere cerekanye"
                    }

                DiagnosisHypotension ->
                    { english = "Hypotension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso uri hasi"
                    , kirundi = Just "Umuvuduko wo hasi w'amaraso"
                    }

                DiagnosisPregnancyInducedHypertension ->
                    { english = "Pregnancy-induced hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Just "Umuvuduko w'amaraso utewe n'imbanyi"
                    }

                DiagnosisPreeclampsiaHighRisk ->
                    { english = "High Risk for Preeclampsia"
                    , kinyarwanda = Just "Afite ibyago byinshi byo kugira Preklampusi"
                    , kirundi = Just "Impavu y'ingoran ya Prééclampsie"
                    }

        OK ->
            { english = "OK"
            , kinyarwanda = Just "Nibyo, yego"
            , kirundi = Just "Nivyo"
            }

        On ->
            { english = "On"
            , kinyarwanda = Just "Ku itariki"
            , kirundi = Just "Kuri"
            }

        OneVisit ->
            { english = "One visit"
            , kinyarwanda = Just "Inshuro imwe"
            , kirundi = Just "Urugendo rumwe"
            }

        OnceYouEndTheEncounter ->
            { english = "Once you end the Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe cyose urangije igikorwa ,nta bushobozi wongera kugira bwo guhindura ibyo winjije cyangwa amakuru."
            , kirundi = Just "Niwamara guheza umubonano, ntibikunda ko uhindura canke ngo wongeremwo amakuru/amatohoza"
            }

        OnceYouEndYourGroupEncounter ->
            { english = "Once you end your Group Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe ushoze igikorwa, ntabwo ushobora guhindura cg wongeremo andi makuru."
            , kirundi = Just "Mugihe umubonano w'umurwi uheze, ntibizokunda ko uhindura canke wongeremwo amakuru/amatohoza"
            }

        OngoingTreatmentTask task ->
            case task of
                OngoingTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Just "Isubiramwo ry'imiti"
                    }

        OnlySickChild ->
            { english = "Only Sick Child"
            , kinyarwanda = Just "Umwana urwaye gusa"
            , kirundi = Just "Umwana agwaye gusa"
            }

        Or ->
            { english = "or"
            , kinyarwanda = Just "cyangwa"
            , kirundi = Just "Canke"
            }

        OutsideCareLabel ->
            { english = "Outside Care"
            , kinyarwanda = Nothing
            , kirundi = Just "Ukuvurirwa hanze"
            }

        PackagesPerMonth ->
            { english = "packages / month"
            , kinyarwanda = Just "Amapaki ku kwezi"
            , kirundi = Just "Amapaki ku kwezi"
            }

        Page ->
            { english = "Page"
            , kinyarwanda = Just "Paji"
            , kirundi = Just "Urupapuro"
            }

        PageNotFoundMsg ->
            { english = "Sorry, nothing found in this URL."
            , kinyarwanda = Just "Mutwihanganire ntabwo ubufasha mwasabye mubashije kuboneka."
            , kirundi = Just "Ihangane, nta nakimwe cabonetse muri iyi URL"
            }

        Pallor ->
            { english = "Pallor"
            , kinyarwanda = Just "Kweruruka (k'urugingo rw'umubiri)"
            , kirundi = Just "Ibara ryahindutse (ku mubiri)"
            }

        Para ->
            { english = "Para"
            , kinyarwanda = Just "Imbyaro"
            , kirundi = Just "Para term"
            }

        ParacetamolPrescriptionForAdult ->
            { english = "Every 4-6 hours as needed. Not to exceed 4g in 24h."
            , kinyarwanda = Nothing
            , kirundi = Just "Buri masaha 4-6 nkuko bikenewe. Kutarenza 4g mu  masaha 24."
            }

        ParentsAliveAndHealthyQuestion ->
            { english = "Are both parents alive and healthy"
            , kinyarwanda = Just "Ese ababyeyi bombi bariho kandi bafite ubuzima bwiza"
            , kirundi = Just "Mbega abavyeyi uko ari babiri (2) bariho bose kandi bafise amagara meza"
            }

        PaleConjuctiva ->
            { english = "Pale Conjunctiva"
            , kinyarwanda = Just "Ibihenehene byeruruka"
            , kirundi = Just "Ihinduka ry'ibara ku maso"
            }

        PartialPlacentaPreviousDelivery ->
            { english = "Partial Placenta in previous delivery"
            , kinyarwanda = Just "Ubwo aheruka kubyara iya nyuma ntiyavuyeyo  yose (yaje igice)"
            , kirundi = Just "Igice c'isimbizo y'umwana ari mu nda mu gihe c'ukwibaruka guheruka (Placenta: Isimbizo iri mu gitereko)"
            }

        Participants ->
            { english = "Participants"
            , kinyarwanda = Just "Ubwitabire"
            , kirundi = Just "Abitavye"
            }

        ParticipantReviewed ->
            { english = "I have reviewed and understand the above."
            , kinyarwanda = Just "Nasomye kandi numva ibyavzwe haruguru"
            , kirundi = Just "Nasuzumye nongera ndategera vyo biri hejuru"
            }

        ParticipantSignature ->
            { english = "Participant Signature"
            , kinyarwanda = Just "Umukono w'umugenerwabikorwa"
            , kirundi = Just "Umukono w'abitavye"
            }

        ParticipantDemographicInformation ->
            { english = "Participant Demographic Information"
            , kinyarwanda = Just "Umwirondoro w'umugenerwabikorwa"
            , kirundi = Just "Amakuru y'ibiharuro vy'abitavye"
            }

        PartnerReceivedHivCounseling ->
            { english = "Did partner receive HIV Counseling during this pregnancy"
            , kinyarwanda = Just "Umugabo yahawe ubujyanama kuri Virusi itera SIDA"
            , kirundi = Just "Mbega umufasha yarakiriye impanuro kuvyerekeye umugera wa SIDA mu kiringo ciyi mbanyi"
            }

        PastDiagnosisReportReason ->
            { english = "As a result of entering lab results from past encounter"
            , kinyarwanda = Just "Nk'igisubizo cyo kwinjiza ibisubizo by'ibizamini by'ubushize"
            , kirundi = Just "Nk'inyishu yo kwinjiza ibipimo vyabaye muri kahise n'inyishu zavyo"
            }

        PatientDiagnosedWithLabel ->
            { english = "The patient has been diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwo"
            , kirundi = Just "Umugwayi bamusuzumye"
            }

        PatientExhibitAnyFindings ->
            { english = "Does the patient exhibit any of these findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bikurikira"
            , kirundi = Just "Mbega umugwayi arerekana imwe muri izo nyishu/ivyo vyegeranyo"
            }

        PatientExhibitAnyRespiratoryFindings ->
            { english = "Does the patient exhibit any of these Respiratory findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bimenyetso by'ubuhumekero"
            , kirundi = Just "Mbega umugwayi arerekana imwe muri ivyo vyegeranyo vy'ingwara yo guhema nabi"
            }

        PatientGotAnyDangerSigns ->
            { english = "Does the patient have any of these danger signs"
            , kinyarwanda = Just "Umurwayi afite kimwe muri ibi bimenyetso mpuruza"
            , kirundi = Just "Mbega umugwayi arafise kimwe muri ivyo bimenyetso vy'akaga/ bikabije/bikaze"
            }

        PatientGotAnySymptoms ->
            { english = "Does the patient have any of these symptoms"
            , kinyarwanda = Just "Umurwayi yaba afite bimwe muri ibi bimenyetso"
            , kirundi = Just "Mbega umurwayi arafise kimwe muri ivyo bimenyetso"
            }

        PatientGotPainAnywhewre ->
            { english = "Does the patient have pain anywhere"
            , kinyarwanda = Just "Umurwayi hari aho yaba ababara"
            , kirundi = Just "Mbega umugwayi arafise umusonga ahariho hose"
            }

        PatientGotDiabetesHeader ->
            { english = "This patient has Diabetes"
            , kinyarwanda = Just "Uyu murwayi afite indwara ya Diyabete"
            , kirundi = Just "Uyu mugwayi afise Diyabete"
            }

        PatientGotDiabetesByGlucoseHeader fasting value ->
            if fasting then
                { english = "This patient has Diabetes with glucose levels before a meal (fasting) of " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu maraso mbere yo kurya binga na " ++ String.fromFloat value ++ " mg/dL"
                , kirundi = Just <| "Afise Diyabete hamwe n'ibipimo vy'isukari mu maraso atarafungura bingana " ++ String.fromFloat value ++ " mg/dL"
                }

            else
                { english = "This patient has Diabetes with glucose levels after a meal (non-fasting) of " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu maraso nyuma yo kurya binga na " ++ String.fromFloat value ++ " mg/dL"
                , kirundi = Just <| "Afise Diyabete hamwe n'ibipimo vy'isukari mu maraso ahejeje gufungura bingana " ++ String.fromFloat value ++ " mg/dL"
                }

        PatientGotDiabetesByUrineDip value ->
            { english = "This patient has Diabetes with Urine Dip glucose levels of " ++ value
            , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu nkari bingana na " ++ value
            , kirundi = Just <| "Afise Diyabete hamwe n'ibipimo vy'isukari iri mu mukoyo (glucose) bingana " ++ value
            }

        PatientProgress ->
            { english = "Patient Progress"
            , kinyarwanda = Just "Uruhererekane rw'ibyakorewe umubyeyi"
            , kirundi = Just "Iterambere ry'umugwayi"
            }

        PatientRecord ->
            { english = "Patient Record"
            , kinyarwanda = Just "Amakuru y'Umurwayi"
            , kirundi = Just "Icegeranyo c'umugwayi"
            }

        PatientInformation ->
            { english = "Patient Information"
            , kinyarwanda = Just "Amakuru k'umurwayi"
            , kirundi = Just "Amakuru y'umugwayi/umuvyeyi"
            }

        PatientIsolatedQuestion isChw ->
            if isChw then
                { english = "Have you isolated the patient"
                , kinyarwanda = Just "Washyize umurwayi mu kato"
                , kirundi = Just "Wigeze ushira m'ubwiherero umurwayi"
                }

            else
                { english = "Is the patient able to self-isolate at home"
                , kinyarwanda = Just "Umurwayi ashobora kwishyira mu kato ka wenyine mu rugo"
                , kirundi = Just "Mbega umugwayi arashoboye kwiyugaranira ahantu hawenyene muhira"
                }

        PatientNotYetSeenAtHCLabel ->
            { english = " has not yet been seen at the health center for this pregnancy"
            , kinyarwanda = Just " ntiyigeze asuzumwa ku kigo nderabuzima kuri iyi nda atwite"
            , kirundi = Just "Ntarigera aboneka kw'ivuriro muri iki kiringo c'imbanyi"
            }

        PatientRecordFilter filter ->
            case filter of
                Pages.PatientRecord.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    , kirundi = Just "Ingwara ikaze"
                    }

                Pages.PatientRecord.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Just "Umwirondoro"
                    , kirundi = Just "Ibiharuro vy'abantu"
                    }

                FilterFamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Just "Kuvyara k'urugero"
                    }

        PauseEncounter ->
            { english = "Pause Encounter"
            , kinyarwanda = Just "Igikorwa cyahagaritswe"
            , kirundi = Just "Kuruhuka"
            }

        PatientShowsNoSignsOfCovid ->
            { english = "Patient shows no signs of Covid"
            , kinyarwanda = Just "Umurwayi nta bimenyetso bya Koronavirusi agaragaza"
            , kirundi = Just "Umugwayi ntiyerekana ikimenyetso na kimwe ca Korona"
            }

        Patients ->
            { english = "Patients"
            , kinyarwanda = Nothing
            , kirundi = Just "Abagwayi"
            }

        PediatricVisit ->
            { english = "Pediatric Visit"
            , kinyarwanda = Just "Isura ry'umwana"
            , kirundi = Just "Kugendera abana"
            }

        PediatricCareMilestone milestone ->
            case milestone of
                Milestone6Weeks ->
                    { english = "6 weeks"
                    , kinyarwanda = Just "Ibyumweru 6"
                    , kirundi = Just "Indwi 6"
                    }

                Milestone14Weeks ->
                    { english = "14 weeks"
                    , kinyarwanda = Just "Ibyumweru 14"
                    , kirundi = Just "Indwi 14"
                    }

                Milestone6Months ->
                    { english = "6 months"
                    , kinyarwanda = Just "Amezi 6"
                    , kirundi = Just "Amezi 6"
                    }

                Milestone9Months ->
                    { english = "9 months"
                    , kinyarwanda = Just "Amezi 9"
                    , kirundi = Just "Amezi 9"
                    }

                Milestone12Months ->
                    { english = "12 months"
                    , kinyarwanda = Just "Amezi 12"
                    , kirundi = Just "Amezi 12"
                    }

                Milestone15Months ->
                    { english = "15 months"
                    , kinyarwanda = Just "Amezi 15"
                    , kirundi = Just "Amezi 15"
                    }

                Milestone18Months ->
                    { english = "18 months"
                    , kinyarwanda = Just "Amezi 18"
                    , kirundi = Just "Amezi 18"
                    }

                Milestone2Years ->
                    { english = "2 years"
                    , kinyarwanda = Just "Imyaka 2"
                    , kirundi = Just "Imyaka 2"
                    }

                Milestone3Years ->
                    { english = "3 years"
                    , kinyarwanda = Just "Imyaka 3"
                    , kirundi = Just "Imyaka 3"
                    }

                Milestone4Years ->
                    { english = "4 years"
                    , kinyarwanda = Just "Imyaka 4"
                    , kirundi = Just "Imyaka 4"
                    }

        People ->
            { english = "People"
            , kinyarwanda = Just "Abantu"
            , kirundi = Just "Abantu"
            }

        Percentage ->
            { english = "%"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PersistentStorage authorized ->
            if authorized then
                { english = "Persistent storage has been authorized. The browser will not delete locally cached data without your approval."
                , kinyarwanda = Just "Ububiko buhoraho bwaremejwe,amakuru wabitse ntabwo yatsibama udatanze uburenganzira/utabyemeje"
                , kirundi = Just "Ububiko bushemeye bwaremewe. Mucukumbuzi ntishobora gufuta amakuru yabitswe neza ataruhusha watanze."
                }

            else
                { english = "Persistent storage has not been authorized. The browser may delete locally cached data if storage runs low."
                , kinyarwanda = Just "Ibikwa ry'amakuru ntabwo remejwe. Sisiteme mushakisha ukoreramo ishobora kubisiba umwanya ubaye muto."
                , kirundi = Just "Ububiko bushemeye ntibwemewe. Mucukumbuzi irashobora gufuta amakuru yabitswe ikibanza c'ububiko kiriko kiragabanuka cane."
                }

        Person ->
            { english = "Person"
            , kinyarwanda = Just "Umuntu"
            , kirundi = Just "Umuntu"
            }

        PersonHasBeenSaved ->
            { english = "Person has been saved"
            , kinyarwanda = Just "Amakuru kuri uyu muntu yabitswe"
            , kirundi = Just "Umuntu yakize"
            }

        PertinentSymptoms ->
            { english = "Pertinent Symptoms"
            , kinyarwanda = Just " Ibimenyetso by'ingenzi"
            , kirundi = Just "Ibimenyetso bibandanya"
            }

        PhotosTransferStatus ->
            { english = "Photos Transfer Status"
            , kinyarwanda = Just "Uko kohereza amafoto bihagaze"
            , kirundi = Just "Indangakamere ry'irungikwa ry'amafoto"
            }

        PhysicalExam ->
            { english = "Physical Exam"
            , kinyarwanda = Just "Gusuzuma umurwayi"
            , kirundi = Just "Igipimo c'umubiri"
            }

        PhysicalExamTask task ->
            case task of
                PhysicalExamVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    , kirundi = Just "Ivyangombwa"
                    }

                PhysicalExamCoreExam ->
                    { english = "Core Exam"
                    , kinyarwanda = Just "Ikizamini cy'ingenzi"
                    , kirundi = Just "Ikibazo c'umushinge"
                    }

                PhysicalExamMuac ->
                    { english = "Muac"
                    , kinyarwanda = Just "Ikizigira"
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi"
                    }

                PhysicalExamAcuteFindings ->
                    { english = "Acute Findings"
                    , kinyarwanda = Just "Ibimenyetso biziyeho"
                    , kirundi = Just "Ivyatowe bikaze"
                    }

                PhysicalExamNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Just "Ugufungura"
                    }

        PlaceholderEnterHeight ->
            { english = "Enter height here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            , kirundi = Just "Andika uburebure hano..."
            }

        PlaceholderEnterMUAC ->
            { english = "Enter MUAC here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            , kirundi = Just "Andika uburinganira bw'umuzingi w'ukoboko ngaha..."
            }

        PlaceholderEnterParticipantName ->
            { english = "Enter participant name here"
            , kinyarwanda = Just "Andika izina ry'umurwayi hano"
            , kirundi = Just "Andika izina ry'uwitavye hano"
            }

        PlaceholderEnterWeight ->
            { english = "Enter weight here…"
            , kinyarwanda = Just "Andika ibiro hano…"
            , kirundi = Just "Andika ibiro ngaha..."
            }

        PlaceholderSearchContactName ->
            { english = "Search contact name here"
            , kinyarwanda = Just "Shakisha izina ry'uwo bahuye"
            , kirundi = Just "Rondera izina ry'umuntu aha"
            }

        PleaseCall ->
            { english = "Please call"
            , kinyarwanda = Just "Hamagara"
            , kirundi = Just "Murahamagara"
            }

        PleaseContact ->
            { english = "Please contact"
            , kinyarwanda = Just "Vugisha"
            , kirundi = Just "Muravugana"
            }

        PleaseSync ->
            { english = "Please sync data for selected Health Center."
            , kinyarwanda = Nothing
            , kirundi = Just "Raba uhuze amakuru yo kw'ivuriro wacaguye"
            }

        PointOfCare ->
            { english = "Point of Care"
            , kinyarwanda = Nothing
            , kirundi = Just "Impamvu yo kuvura"
            }

        PostpartumEncounter ->
            { english = "Postpartum Encounter"
            , kinyarwanda = Just "Igikorwa cya nyuma yo kubyara"
            , kirundi = Just "Umubonano inyuma yo kwibaruka"
            }

        PostpartumHealingProblem problem ->
            case problem of
                NormalPostpartumHealing ->
                    { english = "Healing Normally"
                    , kinyarwanda = Just "Ari gukira neza"
                    , kirundi = Just "Ugukira bisanzwe"
                    }

                HealingProblemSwelling ->
                    { english = "Swelling"
                    , kinyarwanda = Just "Harabyimbye"
                    , kirundi = Just "Kuvyimba"
                    }

                HealingProblemDischarge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Harasohoka ibintu bidasanzwe"
                    , kirundi = Just "Gucugwa"
                    }

                HealingProblemReleaseOfSutures ->
                    { english = "Release (lâchage) of sutures"
                    , kinyarwanda = Just "Indodo zavuyemo"
                    , kirundi = Just "Gufundurura inyuzi"
                    }

                HealingProblemHematoma ->
                    { english = "Hematoma"
                    , kinyarwanda = Just "Igisebe cyajemo amaraso"
                    , kirundi = Just "Hématome"
                    }

                HealingProblemBruising ->
                    { english = "Bruising"
                    , kinyarwanda = Just "imfunira/ahantu hasa n'umukara kubera amaraso atasohotse mu mubiri"
                    , kirundi = Just "Ugupfubirana"
                    }

        PostpartumHealingProblemQuestion ->
            { english = "What issues are presented"
            , kinyarwanda = Just "Ni ibihe bibazo byagaragaye"
            , kirundi = Nothing
            }

        PostpartumChildDangerSign sign ->
            case sign of
                PostpartumChildInabilityToSuckle ->
                    { english = "Inability to Suckle"
                    , kinyarwanda = Just "Ntashobora konka"
                    , kirundi = Just "Kudashobora konka"
                    }

                PostpartumChildParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    , kirundi = Just "Ubumuga"
                    }

                PostpartumChildLabouredBreathing ->
                    { english = "Laboured or Rapid Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye cg guhumeka vuba vuba"
                    , kirundi = Nothing
                    }

                PostpartumChildAbnormalTemperature ->
                    { english = "High (Fever) or Low Temperature"
                    , kinyarwanda = Just "Igipimo cy'ubushyuhe kiri hejuru cg kiri hasi"
                    , kirundi = Just "Ubushuhe buri hejuru (Ubushuhe/umuriro) canke hasi"
                    }

                PostpartumChildInactiveNoMovement ->
                    { english = "Inactive or No Movement"
                    , kinyarwanda = Just "Uruhinja ntacyo rwumva cg ntirunyeganyega"
                    , kirundi = Just "Kidakora canke kidagenda"
                    }

                PostpartumChildBodyTurnedYellow ->
                    { english = "Whole Body Has Turned Yellow"
                    , kinyarwanda = Just "Umubiri wose wabaye umuhondo"
                    , kirundi = Just "Umubiri wose wahindutse umutoto uhishiye"
                    }

                NoPostpartumChildDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        PostpartumMotherDangerSign sign ->
            case sign of
                PostpartumMotheUterineBleeding ->
                    { english = "Excessive Uterinal Bleeding"
                    , kinyarwanda = Just "Umubyeyi ava bikabije cyane"
                    , kirundi = Just "Kuva amaraso menshi/kuva amaraso cane mu gitereko"
                    }

                PostpartumMotherFever ->
                    { english = "High Temperature / Fever"
                    , kinyarwanda = Just "Guhinda umuriro mwinshi/Umuriro"
                    , kirundi = Just "Ubushuhe bwaduze"
                    }

                PostpartumMotherMigraine ->
                    { english = "Migraine"
                    , kinyarwanda = Just "Umutwe umurya cyane"
                    , kirundi = Just "Ukumeneka umutwe igipande kimwe"
                    }

                PostpartumMotherParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    , kirundi = Just "Ubumuga"
                    }

                PostpartumMotherAcuteAbdominalPain ->
                    { english = "Acute Abdominal Pain"
                    , kinyarwanda = Just "Kuribwa mu nda cyane"
                    , kirundi = Just "Ububabare bukaze mu nda"
                    }

                PostpartumMotherLabouredBreathing ->
                    { english = "Laboured Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye"
                    , kirundi = Just "Uguhema uri ku gise"
                    }

                NoPostpartumMotherDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        Predecessor predecessor ->
            case predecessor of
                PredecessorFather ->
                    { english = "Father"
                    , kinyarwanda = Just "Se"
                    , kirundi = Just "papa"
                    }

                PredecessorMother ->
                    { english = "Mother"
                    , kinyarwanda = Just "Nyina"
                    , kirundi = Just "Mama"
                    }

                PredecessorGrandFather ->
                    { english = "Grand-Father"
                    , kinyarwanda = Just "Sekuru"
                    , kirundi = Just "Sogokuru"
                    }

                PredecessorGrandMother ->
                    { english = "Grand-Mother"
                    , kinyarwanda = Just "Nyirakuru"
                    , kirundi = Just "Nyogokuru"
                    }

                NoPredecessors ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        PreeclampsiaPreviousPregnancy ->
            { english = "Preeclampsia in previous pregnancy "
            , kinyarwanda = Just "Ubushize yagize ibimenyetso bibanziriza guhinda umushyitsi"
            , kirundi = Just "Umuvyeyi yaragize umuvuduko w'amaraso udasanwze igihe c'imbanyi iheruka"
            }

        Pregnancy ->
            { english = "Pregnancy (1-9)"
            , kinyarwanda = Just "Gutwita (1-9)"
            , kirundi = Nothing
            }

        PregnancyConclusion ->
            { english = "Pregnancy Conclusion"
            , kinyarwanda = Just "Iherezo ry'Inda"
            , kirundi = Just "Icemezo c'imbanyi n'igihe cayo"
            }

        PregnancyStart ->
            { english = "Pregnancy Start"
            , kinyarwanda = Just "Itangira ryo Gutwita"
            , kirundi = Just "Intango y'imbanyi"
            }

        PregnancySummarySignQuestion sign ->
            case sign of
                ApgarScores ->
                    { english = "Are APGAR scores available for this patient"
                    , kinyarwanda = Just "Ibipimo byubuzima ku ruhinja rukimara kuvuka birahari"
                    , kirundi = Just "Mbega ibipimo vy'urugero vya \"APGAR\" biraboneka kuri uyu murwayi"
                    }

                BirthLength ->
                    { english = "Is birth length available"
                    , kinyarwanda = Just "Uburebure umwana yavukanye burazwi"
                    , kirundi = Just "Mbega uburebure bw'avuka burahari"
                    }

                NoPregnancySummarySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PregnancyTestResult result ->
            case result of
                PregnancyTestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Aratwite"
                    , kirundi = Just "Afise imbanyi"
                    }

                PregnancyTestNegative ->
                    translationSet NegativeLabel

                PregnancyTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Just "kutamenyekana"
                    }

                PregnancyTestUnableToConduct ->
                    { english = "Unable to conduct test"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    , kirundi = Just "Ntibishoboka gukora igipimo"
                    }

        PregnancyTrimester trimester ->
            case trimester of
                FirstTrimester ->
                    { english = "First Trimester"
                    , kinyarwanda = Just "Igihembwe cya mbere"
                    , kirundi = Just "Igice ca mbere"
                    }

                SecondTrimester ->
                    { english = "Second Trimester"
                    , kinyarwanda = Just "Igihembwe cya kabiri"
                    , kirundi = Just "Igice ca kabiri"
                    }

                ThirdTrimester ->
                    { english = "Third Trimester"
                    , kinyarwanda = Just "Igihembwe cya gatatu"
                    , kirundi = Just "Igice ca 3"
                    }

        PregnancyUrineTest ->
            { english = "Urine Pregnancy Test"
            , kinyarwanda = Just "Ikizamini cy'inkari gisuzuma ko umugore atwite"
            , kirundi = Just "Igipimo c'umukoyo w'imbanyi"
            }

        PrenatalActivityTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    , kirundi = Just "Ibimenyetso vy'akaga"
                    }

                Backend.PrenatalActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Just "Ikibazo"
                    }

                Backend.PrenatalActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Just "Kuvyara k'urugero"
                    }

                Backend.PrenatalActivity.Model.History ->
                    { english = "History"
                    , kinyarwanda = Just "Amateka y'ibyamubayeho"
                    , kirundi = Just "Akahise"
                    }

                PregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    , kirundi = Just "Imibonano ijanye n'ivy'imbanyi"
                    }

                PrenatalPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Just "Ifoto"
                    }

                Backend.PrenatalActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Just "Aho bapimira ingwara"
                    }

                Backend.PrenatalActivity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                BirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    , kirundi = Just "Umupango wo kuvyara"
                    }

                Backend.PrenatalActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

                Backend.PrenatalActivity.Model.PregnancyOutcome ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Just "Iherezo ry'inda"
                    , kirundi = Just "Inyishu yerekeye imbanyi"
                    }

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    { english = "Malaria Prevention"
                    , kinyarwanda = Just "Kwirinda Malariya"
                    , kirundi = Just "Kwikingira Malariya"
                    }

                Backend.PrenatalActivity.Model.Medication ->
                    { english = "Medication"
                    , kinyarwanda = Just "Gufata Imiti"
                    , kirundi = Just "Gufata Imiti"
                    }

                Backend.PrenatalActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Just "Isubiramwo ry'ikimenyetso"
                    }

                PrenatalTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Just "Isubiramwo ry'imiti"
                    }

                MaternalMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    , kirundi = Just "Ingwara yo mu mutwe ku muvyeyi ufise imbanyi"
                    }

                PrenatalImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Just "Incanco"
                    }

                Backend.PrenatalActivity.Model.Breastfeeding ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Just "Konsa"
                    , kirundi = Just "Ukonsa"
                    }

                SpecialityCare ->
                    { english = "Specialty Care"
                    , kinyarwanda = Just "Ubuvuzi bw'inzobere"
                    , kirundi = Just "Ubuvuzi bw'ubuhinga"
                    }

                PostpartumTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Just "Isubiramwo ry'imiti"
                    }

        PrenatalRecurrentActivitiesTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    , kirundi = Just "Inyishu y'igipimo c'ingwara"
                    }

                Backend.PrenatalActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

                RecurrentExamination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Just "Ikibazo"
                    }

                RecurrentMalariaPrevention ->
                    { english = "Malaria Prevention"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kwikingira Malariya"
                    }

        PrenatalAssesment assesment ->
            case assesment of
                AssesmentNormalPregnancy ->
                    { english = "Routine Pregnancy Follow Up"
                    , kinyarwanda = Just "Gukurikirana Umubyeyi Utwite Bisanzwe"
                    , kirundi = Just "Ikurikiranwa ry'imbanyi rya buri munsi"
                    }

                AssesmentHighRiskPregnancy ->
                    { english = "High Risk Pregnancy"
                    , kinyarwanda = Just "Inda Ibangamiwe n'ibibazo Bikomeye"
                    , kirundi = Just "Imbanyi iri mu ngorane yo hejuru"
                    }

        PrenatalDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    , kirundi = Just "Umuvuduko ukabije w'amaraso wamaho"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Just "Umuvuduko w'amaraso utewe n'imbanyi"
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
                    , kirundi = Just "Umuvuduko w'amaraso mu gihe c'imbanyi woroshe"
                    }

                DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosis DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisModeratePreeclampsiaRecurrentPhase ->
                    translationSet <| PrenatalDiagnosis DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosis DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaInitialPhase ->
                    { english = "Severe Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Ikabije"
                    , kirundi = Just "Severe Preeclampsia"
                    }

                DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosis DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaRecurrentPhase ->
                    translationSet <| PrenatalDiagnosis DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosis DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisEclampsia ->
                    { english = "Eclampsia"
                    , kinyarwanda = Just "Ekalampusi"
                    , kirundi = Just "Éclampsie"
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Just "Afise umugera wa SIDA ugaragara"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    , kirundi = Just "Umwe afise umugera wa SIDA kandi uwundi atawafise"
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    , kirundi = Just "Syphilis hamwe n'ingorane"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Neurosyphilis"
                    , kinyarwanda = Just "Mburugu yageze mu bwonko"
                    , kirundi = Just "Ingwara yo m'ubwonko"
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    , kirundi = Just "Malariya irabandanya"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    , kirundi = Just "Malariya hamwe n'igabanuka ry'amaraso m'umubiri"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    , kirundi = Just "Malariya hamwe n'igabanuka ry'amaraso m'umubiri birabandanya"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    , kirundi = Just "Malariya kumwe n'igabanuka ry'amaraso m'umubiri ridasanzwe"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Mild to Moderate Anemia"
                    , kinyarwanda = Just "Amaraso Macye byoroheje"
                    , kirundi = Just "Igabanuka ry'amaraso kuva bisanzwe"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Severe Anemia"
                    , kinyarwanda = Just "Amaraso Macye Cyane"
                    , kirundi = Just "Ibura ry'amaraso rikaze"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Severe Anemia with Complications"
                    , kinyarwanda = Just "Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    , kirundi = Just "Ibura ry'amaraso rikaze hamwe n'ingorane bijanye"
                    }

                DiagnosisMiscarriage ->
                    { english = "Miscarriage"
                    , kinyarwanda = Just "Inda yavuyemo"
                    , kirundi = Just "Ugukoroka kw'imbanyi"
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Molar Pregnancy"
                    , kinyarwanda = Just "Atwite amahuri"
                    , kirundi = Just "Imbanyi idakomeye"
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Placenta Previa"
                    , kinyarwanda = Just "Ingobyi iri hasi ku nkondo y'umura"
                    , kirundi = Just "Igitereko cugaye isohokera"
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Placental Abruption"
                    , kinyarwanda = Just "Ingobyi yomotse hakiri kare"
                    , kirundi = Just "Itabuka ry'igitereko"
                    }

                DiagnosisUterineRupture ->
                    { english = "Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi yaturitse"
                    , kirundi = Just "uguturika kw'igitereko"
                    }

                DiagnosisObstructedLabor ->
                    { english = "Obstructed Labor"
                    , kinyarwanda = Just "Inda yanze kuvuka "
                    , kirundi = Just "Igikorwa cabujijwe"
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Post Abortion Sepsis"
                    , kinyarwanda = Just "Afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    , kirundi = Just "Birashoboka ko ingwara y'igitereko izamwo"
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Ectopic Pregnancy"
                    , kinyarwanda = Just "Yasamiye hanze y'umura"
                    , kirundi = Just "Imbanyi iri hanze y'Igitereko"
                    }

                DiagnosisPROM ->
                    { english = "Premature Rupture of Membranes (PROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    , kirundi = Just "Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo/igihe cayo"
                    }

                DiagnosisPPROM ->
                    { english = "Preterm Premature Rupture of Membranes (PPROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    , kirundi = Just "Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo"
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Hyperémèse gravidique"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Hyperémèse gravidique"
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    , kirundi = Just "Ingorane z'abavyeyi"
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Just "Ivyanduza"
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Just "Gutanga bigaragara/"
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    , kirundi = Just "Ibise + Kuvyara"
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Just "Ugusha k'umutima"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Persistent Heartburn"
                    , kinyarwanda = Just "Ikirungurira gihoraho"
                    , kirundi = Just "Ugusha mu mutima/mu nda bibandanya"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Deep Vein Thrombosis"
                    , kinyarwanda = Just "Gufatana(Kuvura) gukabije kw'amaraso"
                    , kirundi = Just "Umutsi w'indani ufise Thrombose"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Intense Pelvic Pain"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda"
                    , kirundi = Just "Ububabare bukomeye bwo mu nda yo hepfo"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent Pelvic Pain"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda"
                    , kirundi = Just "Ukubandanya kw'ububabare bwo mu nda yo hepfo"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Urinary Tract Infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari"
                    , kirundi = Just "Ingwara yo mu miringoti y'umukoyo"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Urinary Tract Infection Continued"
                    , kinyarwanda = Just "Indwara y'ubwandu bukomeje bw'umuyoboro w'inkari"
                    , kirundi = Just "Ingwara yo mu miringoti y'umukoyo ibandanya"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Pyelonephritis"
                    , kinyarwanda = Just "Indwara yo kubyimba impyiko"
                    , kirundi = Just "Ingwara y'Amafyigo"
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    , kirundi = Just "Candidose"
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis Continued"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    , kirundi = Just "Ingwara ya candidose irabandanya"
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea Continued"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka (Irabandanya)"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka igaragazwa kenshi no kuhiyagaza"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis Continued"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka igaragazwa kenshi no kuhiyagaza (irabandanya)"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Just "Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no utwite"
                    , kirundi = Just "Diyabete y'imbanyi"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    , kirundi = Just "Rhesus Négatif"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura ntibishoboka"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura birashoboka"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Birashoboka cane kwihebura"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Ukwihebura gushoboka"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    , kirundi = Just "Ingorane zimutuma ashobora kwiyahura"
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Just "Uburuhe"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Just "Ububabare bw'umugongo hepfo"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    , kirundi = Just "Ivyanduza"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    , kirundi = Just "Kuva amaraso cane"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Just "Iyuzura ry'amaberebere (Mastite précoce)"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Just "Ingwara y'imoko ituma amaberebere adasohoka"
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        PrenatalDiagnosisForProgressReport diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    , kirundi = Just "Umuvuduko ukabije w'amaraso wamaho"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Just "Umuvuduko w'amaraso utewe n'imbanyi"
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
                    , kirundi = Just "Umuvuduko w'amaraso mu gihe c'imbanyi woroshe"
                    }

                DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisModeratePreeclampsiaRecurrentPhase ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisModeratePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaInitialPhase ->
                    { english = "Severe Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Ikabije"
                    , kirundi = Just "Umuvuduko w'amaraso mu gihe c'imbanyi ukaze"
                    }

                DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaRecurrentPhase ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisSeverePreeclampsiaInitialPhase

                DiagnosisEclampsia ->
                    { english = "Eclampsia"
                    , kinyarwanda = Just "Ekalampusi"
                    , kirundi = Just "Éclampsie"
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi Itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Just "Afise umugera wa SIDA ugaragara"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    , kirundi = Just "Umwe afise umugera wa SIDA kandi uwundi atawafise"
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    , kirundi = Just "Syphilis hamwe n'ingorane"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Suspected Neurosyphilis"
                    , kinyarwanda = Just "Arakekwaho Mburugu yageze mu bwonko"
                    , kirundi = Just "Hiketswe ingwara y'ubwonko canke y'igiti c'umugongo"
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    , kirundi = Just "Malariya irabandanya"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    , kirundi = Just "Malariya hamwe n'igabanuka ry'amaraso m'umubiri"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    , kirundi = Just "Malariya hamwe n'igabanuka ry'amaraso m'umubiri birabandanya"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    , kirundi = Just "Malariya kumwe n'igabanuka ry'amaraso m'umubiri ridasanzwe"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Anemia (Mild to Moderate)"
                    , kinyarwanda = Just "Amaraso Macye (byoroheje)"
                    , kirundi = Just "Igabanuka ry'amaraso (ryo hasi gushika hagati na hagati)"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Anemia (Severe)"
                    , kinyarwanda = Just "Amaraso Macye (bikabije)"
                    , kirundi = Just "Igabanuka ry'amaraso (rikomeye)"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Anemia (Severe with Complications)"
                    , kinyarwanda = Just "Amaraso Macye (Bikabije n'Ibibazo Bishamikiyeho)"
                    , kirundi = Just "Igabanuka ry'amaraso (rikomeye hamwe n'inkurikizi zigoye)"
                    }

                DiagnosisMiscarriage ->
                    { english = "Possible Miscarriage"
                    , kinyarwanda = Just "Ashobora kuba yavanyemo inda"
                    , kirundi = Just "Birashoboka ko inda ikoroka"
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Possible Molar Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba atwite amahuri"
                    , kirundi = Just "Birashoboka ko mu mbanyi ata mwana ariyo"
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Possible Placenta Previa"
                    , kinyarwanda = Just "Ingobyi ishobora kuba iri hasi ku nkondo y'umura"
                    , kirundi = Just "Birashoboka ko isimbizo irimwo umwana yamanutse hepfo bigatuma umwana atavuka aciye aho yategerezwa guca"
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Possible Placental Abruption"
                    , kinyarwanda = Just "Ingobyi ishobora kuba yomotse hakiri kare"
                    , kirundi = Just "Birashoboka ko isimbizo yitandukanye n'igitereko"
                    }

                DiagnosisUterineRupture ->
                    { english = "Possible Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi ishobora kuha yaturitse"
                    , kirundi = Just "Birashoboka ko habaye ugutabuka kw'igitereko"
                    }

                DiagnosisObstructedLabor ->
                    { english = "Possible Obstructed Labor"
                    , kinyarwanda = Just "Inda ishobora kuba yanze kuvuka "
                    , kirundi = Just "Birashoboka ko umwana atasohoka ava mu gitereko kuko umuhora acamwo wiyugaye"
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Possible Post Abortion Sepsis"
                    , kinyarwanda = Just "Ashobora kuba afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    , kirundi = Just "Birashoboka ko igitereko cagwaye inyuma yaho imbanyi yavuyeyo giturumbuka"
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Possible Ectopic Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba yarasamiye hanze y'umura"
                    , kirundi = Just "Birashoboka ko afise imbanyi iri hanze y'igitereko"
                    }

                DiagnosisPROM ->
                    { english = "PROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    , kirundi = Just " Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo"
                    }

                DiagnosisPPROM ->
                    { english = "PPROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    , kirundi = Just "Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo"
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Hyperémèse gravidique"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Hyperémèse gravidique"
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    , kirundi = Just "Ingorane z'abavyeyi"
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Just "Ivyanduza"
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Just "Gutanga bigaragara/"
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    , kirundi = Just "Ibise + Kuvyara"
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn in pregnancy"
                    , kinyarwanda = Just "Ikirungurira mu gihe umubyeyi atwite"
                    , kirundi = Just "Ugusha k'umutima mu gihe c'imbanyi"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Heartburn in pregnancy (persistent)"
                    , kinyarwanda = Just "Ikirungurira gihoraho mu gihe umubyeyi atwite"
                    , kirundi = Just "Ugusha k'umutima mu gihe c'imbanyi (birabandanya)"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Possible DVT"
                    , kinyarwanda = Just "Ashobora kuba afite ibibazo by'imitsi, bituma amaraso adatembera neza mu mubiri"
                    , kirundi = Just "Birashoboka ko amaraso y'avuze indani mu mutsi (Thrombose)"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Severe pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda igihe umubyeyi atwite"
                    , kirundi = Just "Ububabare bwo mu nda yo hepfo bukaze mu gihe c'imbanyi"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite"
                    , kirundi = Just "Ukubandanya kw'ububabare bwo mu nda yo hepfo mu gihe c'imbanyi"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Lower urinary tract infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi"
                    , kirundi = Just "Ingwara (yandukiye) yo m'uturingoti tw'umukoyo two munsi"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Lower urinary tract infection (continued)"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi bukomeje kugaragara"
                    , kirundi = Just "Ingwara (yandukiye) yo m'uturingoti tw'umukoyo two munsi (kubandanya)"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Possible Pyelonephritis"
                    , kinyarwanda = Just "Ashobora kuba afite Indwara yo kubyimba impyiko"
                    , kirundi = Just "Birashoboka ko amafyigo yagwaye bivuye ku miringoti y'umukoyo yagwaye nayo nyene"
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    , kirundi = Just "Candidose"
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis (continued)"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    , kirundi = Just "Ingwara ya candidose irabandanya"
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea (continued)"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka (Irabandanya)"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka igaragazwa kenshi no kuhiyagaza"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis (continued)"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka igaragazwa kenshi no kuhiyagaza (irabandanya)"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Possible Active Tuberculosis"
                    , kinyarwanda = Just "Ashobora kuba afite Igituntu"
                    , kirundi = Just "Birashoboka ko agwaye Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yatewe no gutwita"
                    , kirundi = Just "Diyabete y'imbanyi"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    , kirundi = Just "Rhesus Négatif"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura ntibishoboka"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura birashoboka"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Birashoboka cane kwihebura"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Ukwihebura gushoboka"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    , kirundi = Just "Ingorane zimutuma ashobora kwiyahura"
                    }

                DiagnosisOther ->
                    { english = "Received a diagnosis from a different health care facility - please follow up with patient"
                    , kinyarwanda = Just "Yabwiwe uburwayi n'irindi vuriro - Gerageza ukurikirane umurwayi"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Just "Ukutihangana hageze gusoba"
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Just "Uburuhe"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Just "Ububabare bw'umugongo hepfo"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    , kirundi = Just "Ivyanduza"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    , kirundi = Just "Kuva amaraso cane"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Just "Iyuzura ry'amaberebere (Mastite précoce)"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Just "Ingwara y'imoko ituma amaberebere adasohoka"
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        PrenatalDiagnosisNonUrgentMessage diagnosis ->
            case diagnosis of
                DiagnosisHIV ->
                    { english = "Patient has tested positive for HIV"
                    , kinyarwanda = Just "Afite ubwandu bwa Virusi itera SIDA"
                    , kirundi = Just "Afise umugera wa SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Patient has a detectable HIV Viral Load"
                    , kinyarwanda = Just "Umurwayi agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Just "Umugwayi afise umugera wa SIDA ugaragara"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Patient is HIV Negative with a discordant partner"
                    , kinyarwanda = Just "Umwe mubashakanye afite ubwandu bwa virusi itera SIDA"
                    , kirundi = Just "Umugwayi ata mugera wa Sida afise ariko mugenziwe ayifise"
                    }

                DiagnosisSyphilis ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    , kirundi = Just "Umugwayi bamutoye ingwara yo mu bihimba vy'irondoka yitwa Syphilis"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    , kirundi = Just "Umugwayi bamutoye ingwara yo mu bihimba vy'irondoka yitwa Syphilis"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Patient has tested positive for Syphilis and shows signs of Neurosyphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu kandi afite ibimenyetso bigaragaza ko yageze mu bwonko"
                    , kirundi = Just "Umuvyeyi bamutoye ingwara yo mu bihimba vy'irondoka yitwa Syphilis hamwe n'ingwara yo m'ubwonko yitwa neurosyphilis"
                    }

                DiagnosisHepatitisB ->
                    { english = "Patient has tested positive for Hepatitis B"
                    , kinyarwanda = Just "Afite ubwandu bw'umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Umugwayi bamutoye ingwara y'igitigu"
                    }

                DiagnosisMalaria ->
                    { english = "Patient has tested positive for Malaria"
                    , kinyarwanda = Just "Afite ubwandu bwa Malariya"
                    , kirundi = Just "Umugwayi bamutoye Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya ikomeje kugaragara"
                    , kirundi = Just "Umuvyeyi bamutoye Malariya ibandanya hamwe n'ibura ry'amaroso"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Patient has tested positive for Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite Malariya n'amaraso macye"
                    , kirundi = Just "Umuvyeyi bamutoye Malariya ibandanya"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso make bikomeje kugaragara"
                    , kirundi = Just "Umuvyeyi afise Malariya hamwe n'igabanuka ry'amaraso m'umubiri"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Patient has tested positive for Malaria with Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso macye cyane"
                    , kirundi = Just "Umuvyeyi afise Malariya kumwe n'igabanuka ry'amaraso m'umubiri ridasanzwe"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Patient shows signs of Mild to Moderate Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'amaraso Macye byoroheje"
                    , kirundi = Just "Umuvyeyi afise Igabanuka ry'amaraso bisanzwe"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Patient shows signs of Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Amaraso Macye Cyane"
                    , kirundi = Just "Umuvyeyi afise Ibura ry'amaraso rikaze"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Patient has tested positive for Severe Anemia with Complications"
                    , kinyarwanda = Just "Umubyeyi afite Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    , kirundi = Just "Umuvyeyi afise Ibura ry'amaraso rikaze hamwe n'ingorane bijanye"
                    }

                DiagnosisChronicHypertensionImmediate ->
                    { english = "Patient shows signs of Chronic Hypertension"
                    , kinyarwanda = Just "Agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                    , kirundi = Just "Yerekana ibimenyetso vy'umuvuduko w'amaraso wamaho"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Patient shows signs of Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Aragaragaza ibimenyetso by'Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Just "Yerekana ibimenyetso vy'imbanyi ifise Umuvuduko w'amaraso"
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    , kirundi = Just "Yerekana ibimenyetso vy'umuvuduko w'amaraso mu gihe c'imbanyi woroshe"
                    }

                DiagnosisModeratePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    , kirundi = Just "Yerekana ibimenyetso vy'umuvuduko w'amaraso mu gihe c'imbanyi woroshe"
                    }

                DiagnosisSeverePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    , kirundi = Just "Yerekana ibimenyetso vy'Umuvuduko w'amaraso mu gihe c'imbanyi ukaze"
                    }

                DiagnosisSeverePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    , kirundi = Just "Yerekana ibimenyetso vy'Umuvuduko w'amaraso mu gihe c'imbanyi ukaze"
                    }

                DiagnosisHeartburn ->
                    { english = "Patient shows signs of Persistent Heartburn"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyo gusha k'umutima"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Patient shows signs of Persistent Heartburn that is not responding to treatment"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho ariko imiti itari kuvura"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vy'ingwara yo gusha mu nda/mu mutima ibandanya kandi imiti itariko irayivura"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Patient shows signs of Deep Vein Thrombosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo Gufatana(Kuvura) gukabije kw'amaraso"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy' Umutsi w'indani ufise Thrombose"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Patient shows signs of Intense Pelvic Pain"
                    , kinyarwanda = Just "mubyeyi agaragaza ibimenyetso by'ububabare bukabije mu kiziba cy'inda"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'bubabare bukomeye bwo mu nda yo hepfo"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Patient shows signs of Persistent Pelvic Pain"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ububabare buhoraho mu kiziba cy'inda"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ukubandanya kw'ububabare bwo mu nda yo hepfo"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Patient shows signs of Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yo mu miringoti y'umukoyo"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Patient shows signs of Persistant Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yo mu miringoti y'umukoyo ibandanya"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Patient shows signs of Pyelonephritis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Indwara yo kubyimba impyiko"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'Ingwara y'Amafyigo"
                    }

                DiagnosisCandidiasis ->
                    { english = "Patient shows signs of a Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara y'icuririzi "
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Patient shows signs of a Persistant Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo ikomeje kugaragara"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyo kwandura ingwara y'icuririzi ibandanya"
                    }

                DiagnosisGonorrhea ->
                    { english = "Patient shows signs of Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yo mu bihimba vy'irondoka"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Patient shows signs of Persistant Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi ikomeje kugaragara"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yo mu bihimba vy'irondoka (Irabandanya)"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Patient shows signs of Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yitwa trichomonase"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Patient shows signs of Persistant Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yitwa trichomonase ibandanya"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Patient shows signs of Tuberculosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Igituntu"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara y'Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Patient shows signs of Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vya Diyabete"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Patient shows signs of Gestational Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete yatewe no gutwita"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vya Diyabete igihe c'imbanyi"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Patient has Rh-Negative status"
                    , kinyarwanda = Just "Umubyeyi afite Rezisi Negatifu"
                    , kirundi = Just "Umugwayi/umuvyeyi afise indangakamere ya Rhesus Négatif"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Patient shows signs of Hyperemesis Gravidum"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyo kudahwa no kugira iseseme birenze"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Patient shows signs of Severe Vomiting"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyo kudahwa bikomeye"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura ntibishoboka"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Patient shows signs of possible depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso ko bishoboka kwihebura"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Patient shows signs of fairly high possibility of depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyerekana ko bishoboka cane kwihebura"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Patient shows signs of probable depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko ashobora kuba afite indwara y'agahinda gakabije"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso ko hari naho yo kwihebura"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Patient shows signs of being a suicide risk"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso yuko ashobora kwiyahura"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Patient shows signs of Urinary Incontinence"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kutabasha kunyara"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vy'ingwara yo kutifata umukoyo ugira usohoke"
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Just "Uburuhe"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Just "Ububabare bw'umugongo hepfo"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Patient shows signs of Infection"
                    , kinyarwanda = Just "Umubyei agaragaza ibimenyetso bya infegisiyo"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vyo kwandukiza"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Patient shows signs of Excessive Bleeding"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuva cyane"
                    , kirundi = Just "Umuvyeyi yerekana ibimenyetso vyo kuva amaraso bidasanzwe"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Patient shows signs of Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vyo ukubabara mw'ibere canke rikavyimba hakiri kare"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Patient shows signs of Mastitis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bw'amabere"
                    , kirundi = Just "Umugwayi yerekana ibimenyetso vy'ububabare mu mabere"
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                -- Non Not Urgent diagnoses.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalEncounterType encounterType ->
            case encounterType of
                NurseEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NursePostpartumEncounter ->
                    { english = "Postpartum"
                    , kinyarwanda = Just "Igihe cya nyuma cyo kubyara"
                    , kirundi = Just "Inyuma yo kwibaruka"
                    }

                ChwFirstEncounter ->
                    { english = "First Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya mbere umugore utwite"
                    , kirundi = Just "Ukugenderwa ubwa mbere imbere yo kuvyara"
                    }

                ChwSecondEncounter ->
                    { english = "Second Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya kabiri umugore utwite"
                    , kirundi = Just "Ukugenderwa ubwa kabiri imbere yo kuvyara"
                    }

                ChwThirdPlusEncounter ->
                    { english = "Third Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya gatatu umugore utwite"
                    , kirundi = Just "Ukugenderwa ubwa gatatu imbere yo kuvyara"
                    }

                ChwPostpartumEncounter ->
                    { english = "Postpartum"
                    , kinyarwanda = Just "Igihe cya nyuma cyo kubyara"
                    , kirundi = Just "Inyuma yo kwibaruka"
                    }

        PrenatalFlankPainSign sign ->
            case sign of
                FlankPainLeftSide ->
                    { english = "Left side"
                    , kinyarwanda = Just "Uruhande rw'ibumoso"
                    , kirundi = Just "Uruhande rw'ibubamfu"
                    }

                FlankPainRightSide ->
                    { english = "Right side"
                    , kinyarwanda = Just "Uruhande rw'iburyo"
                    , kirundi = Just "Uruhande rw'iburyo"
                    }

                FlankPainBothSides ->
                    { english = "Both sides"
                    , kinyarwanda = Just "Impande zose"
                    , kirundi = Just "Impande zose"
                    }

                NoFlankPain ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        PrenatalHealthEducationSignsDiagnosis isInitial date sign ->
            case sign of
                EducationNauseaVomiting ->
                    if isInitial then
                        { english = "Nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Iseseme + kudahwa mu gihe c'imbanyi - gutanga inyigisho kuvyerekeye amagara kuri " ++ date
                        }

                    else
                        { english = "Persistent nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka  bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'iseseme + kudahwa mu gihe c'imbanyi - gutanga inyigisho kuvyerekeye/yerekeye amagara kuri " ++ date
                        }

                EducationLegCramps ->
                    if isInitial then
                        { english = "Leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ibinyanya vy'amaguru mu gihe c'imbanyi - inyigisho ku vyerekeye amagara yatanzwe " ++ date
                        }

                    else
                        { english = "Persistent leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru bikomeza kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'ibinyanya vy'amaguru mu gihe c'imbanyi - inyigisho ku vyerekeye amagara yatanzwe " ++ date
                        }

                EducationLowBackPain ->
                    if isInitial then
                        { english = "Lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ububabare bw'umugongo wo hasi/wo hepfo mu gihe c'imbanyi - Gutanga inyigisho kuvyerekeye amagara " ++ date
                        }

                    else
                        { english = "Persistent lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'ububabare bw'umugongo wo hasi/wo hepfo mu gihe c'imbanyi - Gutanga inyigisho kuvyerekeye amagara " ++ date
                        }

                EducationConstipation ->
                    if isInitial then
                        { english = "Constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukuzura inda mu gihe c'imbanyi - Inyigisho z'amagara zitegerezwa gutangwa " ++ date
                        }

                    else
                        { english = "Persistent constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kwo kuzura inda mu gihe c'imbanyi - Inyigisho z'amagara zitegerezwa gutangwa " ++ date
                        }

                EducationVaricoseVeins ->
                    if isInitial then
                        { english = "Varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukuvyimba kw'imitsi (cane cane ku maguru - kw'ifero) mu gihe c'imbanyi - Tanga inyigisho zerekeye amagara meza " ++ date
                        }

                    else
                        { english = "Persistent varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'ivyimba kw'imitsi (cane cane ku maguru - kw'ifero) mu gihe c'imbanyi - Tanga inyigisho zerekeye amagara meza " ++ date
                        }

                EducationLegPainRedness ->
                    if isInitial then
                        { english = "Leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ububabare bw'amaguru mu gihe c'imbanyi - inyigisho ku vyerekeye amagara yatanzwe " ++ date
                        }

                    else
                        { english = "Persistent leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'ububabare bw'amaguru mu gihe c'imbanyi - inyigisho ku vyerekeye amagara yatanzwe " ++ date
                        }

                EducationPelvicPain ->
                    if isInitial then
                        { english = "Pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ububabare bwo mu nda yo hepfo mu gihe c'imbanyi - tanga inyigisho kuvyerekeye amagara " ++ date
                        }

                    else
                        { english = "Persistent pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Just <| "Ukubandanya kw'ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                -- Other signs do not reflect a diagnosis.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationLabel sign ->
            case sign of
                EducationNauseaVomiting ->
                    { english = "Nausea and Vomiting"
                    , kinyarwanda = Just "Iseseme no kuruka"
                    , kirundi = Just "Iseseme no kudahwa"
                    }

                EducationLegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    , kirundi = Just "Ibinyanya vy'amaguru"
                    }

                EducationLowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    , kirundi = Just "Ububabare bw'umugongo wo hasi"
                    }

                EducationConstipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    , kirundi = Just "Ukuzura inda"
                    }

                EducationHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Just "Ugusha k'umutima"
                    }

                EducationVaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    , kirundi = Just "Ivyimba ry'imitsi"
                    }

                EducationLegPainRedness ->
                    { english = "Leg Pain or Redness"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    , kirundi = Just "Ububabare no kubenjuka ku maguru"
                    }

                EducationPelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    , kirundi = Just "Ububabare bwo mu nda yo hepfo"
                    }

                EducationSaferSex ->
                    { english = "Safer Sex Practices"
                    , kinyarwanda = Just "Imibonano mpuzabitsina ikingiye"
                    , kirundi = Just "Imigirwa mpuzabitsina ikingiwe"
                    }

                EducationMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    , kirundi = Just "Ingwara yo mu mutwe ku muvyeyi ufite imbanyi"
                    }

                EducationEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Just "Iyuzura ry'amaberebere (Mastite précoce)"
                    }

                EducationMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Just "Ingwara y'imoko ituma amaberebere adasohoka"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationAppropriateProvided ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Just "Wahaye umubyeyi inyigisho zabugenewe ku buzima"
            , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara meza k'umugwayi"
            }

        PrenatalHealthEducationQuestion isChw sign ->
            case sign of
                EducationExpectations ->
                    { english = "Have you provided health education and anticipatory guidance on what to expect during the pregnancy"
                    , kinyarwanda = Just "Watanze inyigisho z'ubuzima k'umugore utwite unamusobanurira ibishobora kumubaho"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara hamwe no gutanga intumbero hakiri kare kuvyo umuntu yo kwitega mu gihe c'imbanyi"
                    }

                EducationVisitsReview ->
                    { english = "Have you reviewed anticipated visits by the CHW and to the health center with the mother"
                    , kinyarwanda = Just "Waba waganiriye n'umubyeyi ibyerekeye gusurwa n'umujyanama w'ubuzima cyangwa kujya ku kigonderabuzima"
                    , kirundi = Just "Mbega warihweje/warasubiyemwo/wararabiye hamwe/ ingene ingendo zitegekanijwe z'abaremeshakiyago hamwe n'ingendo zo kw'ivuriro kumwe n'umuvyeyi"
                    }

                EducationWarningSigns ->
                    { english = "Have you provided health education and anticipatory guidance on pregnancy warning signs"
                    , kinyarwanda = Just "Watanze inyigisho ku bimenyetso mpuruza k'umugore utwite nuko yakwitwara aramuste agize kimwe muribyo"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara hamwe no gutanga intumbero hakiri kare kuvy'imbanyi n'ibimenyetso bivuga vyayo"
                    }

                EducationHemorrhaging ->
                    { english = "Have you provided education on post-partum hemorrhaging"
                    , kinyarwanda = Just "Watanze inyigisho ku kimenyesto cyo kuva cyane nyuma yo kubyara"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye kuva amaraso umuntu ahejeje kuvyara"
                    }

                EducationFamilyPlanning ->
                    if isChw then
                        { english = "Have you provided education on family planning"
                        , kinyarwanda = Just "Watanze inyigisho zijyanye no kuboneza urubyaro"
                        , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye kuvyara k'urugero"
                        }

                    else
                        { english = "Have you counseled the patient on family planning options"
                        , kinyarwanda = Just "Waba wagiriye inama umurwayi (umubyeyi) uburyo bwo kuboneza urubyaro"
                        , kirundi = Just "Mbega warahanuye umugwayi kuvyerekeye uburyo bwo kuvyara k'urugero"
                        }

                EducationBreastfeeding ->
                    { english = "Have you provided education on breastfeeding"
                    , kinyarwanda = Just "Watanze inyigisho ku birebana no konsa"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye konsa"
                    }

                EducationImmunization ->
                    { english = "Have you provided education on immunizations"
                    , kinyarwanda = Just "Watanze inyigisho zijyanye na gahunda yo gukingiza"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye incanco"
                    }

                EducationHygiene ->
                    { english = "Have you provided education on hygiene"
                    , kinyarwanda = Just "Watanze inyigisho ku bijyanye n'isuku"
                    , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye isuku"
                    }

                EducationPositiveHIV ->
                    { english = "Have you counseled patient on positive HIV test meaning"
                    , kinyarwanda = Just "Waba wasobanuriye umurwayi (umubyeyi) icyo bisibanuye kugira ibisubizo biri positifu ku bwandu bw'agakoko gatera SIDA"
                    , kirundi = Just "Mbega warahanuye wongera urabwira umugwayi iciza c'igipimo c'umugera wa SIDA"
                    }

                EducationSaferSexHIV ->
                    { english = "Have you counseled patient on safer sex practices"
                    , kinyarwanda = Just "Wagiriye inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
                    , kirundi = Just "Mbega warahanuye  umugwayi kuvyerekeye iciza co kwikingira mu gihe c'imibonano mpuza ibitsina"
                    }

                EducationPartnerTesting ->
                    { english = "Have you encouraged the patient’s partner to get tested"
                    , kinyarwanda = Just "Waba washishikarije umubyueyi kubwira uwo babana kwipimisha"
                    , kirundi = Just "Mbega wateye inguvu umufasha w'umugwayi kugira yipimishe"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationDiabetesInform ->
            { english = "Counsel patient on healthy nutrition and exercise practices"
            , kinyarwanda = Just "Igisha umubyeyi ku mirire myiza no gukora imyitozo ngororamubiri"
            , kirundi = Just "Uguhanura umugwayi kugira afate ifunguro rizima/ryiza (ibifungugwa bizima) hamwe n'ikarashishi - ibikorwa vyo kugorora umubiri/imitsi"
            }

        PrenatalHealthEducationHivDetectableViralLoadInform ->
            { english = "Instruct the patient on the importance of strict adherence to their medication and the dangers of transmission to their child during labor and delivery"
            , kinyarwanda = Just "Igisha umubyeyi akamaro ku gufata imiti neza ndetse n'ingaruka zo kuba yakwanduza umwana mu gihe abyara"
            , kirundi = Just "Igisha umuvyeyi iciza co gukurikiza amategeko yogufata imiti n'ingaruka mbi zo kwandukiza umwana mu gihe c'ibise no kuvyara"
            }

        PrenatalHealthEducationNauseaAndVomitingAdvise ->
            { english = "Advise the patient that small amounts of chamomile tea, ginger, and Vitamin B6 can help relieve these symptoms if these are available to the patient"
            , kinyarwanda = Just "Gira umubyeyi inama ko gufata icyayi cya Chamomile, tangawizi na vitamini B6 byagabanya ibimenyetso afite igihe byaba bihari"
            , kirundi = Just "Kubwira umuvyeyi ko icayi ca Camomille gikeya, Tangawizi nkeya hamwe na Vitamine B6 bishobora gufasha mu kugabanya ivyo bimenyetso, mugihe uwo mugwayi yobironka"
            }

        PrenatalHealthEducationNauseaAndVomitingInform ->
            { english = "Inform the patient that the symptoms of nausea and vomiting usually resolve on their own in the second half of pregnancy"
            , kinyarwanda = Just "Menyesha umubyeyi ko ibimenyetso byo kugira iseseme no kuruka bigenda bigabanuka uko inda igenda ikura ( kumezi ane, atanu cyangwa atandatu)"
            , kirundi = Just "Bwira umuvyeyi ko ibimenyetso vy'iseseme no kudahwa vyiheza vyonyene mu gice ca kabiri c'imbanyi"
            }

        PrenatalHealthEducationLegCrampsInform ->
            { english = "Instruct the patient that the following may help relieve cramping in the legs"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira bishobora kugabanya ibinya mu maguru"
            , kirundi = Just "Bwira umuvyeyi ko ibi bikurikira bishobora gufasha kugabanya ibinyanya vy'amaguru"
            }

        PrenatalHealthEducationLowBackPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent lower back pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho mu gihe atwite igabanya uburibwe bw'umugongo wo hasi"
            , kirundi = Just "Bwira umuvyeyi ko imyimenyerezo yaburi munsi mu gihe c'imbanyi bifasha kwirinda ububabare bwo m'umugongo"
            }

        PrenatalHealthEducationConstipationInform ->
            { english = "Instruct the patient that increasing the intake of fruits, vegetables, high fiber foods, and water can help relieve constipation symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko kurya imbuto, imboga, ibiryo bisukura umubiri (fibre) no kunywa amazi birinda kunanairwa kwituma"
            , kirundi = Just "Bwira umuvyeyi ko gufungura ivyamwa, imboga, ibifungugwa birimwo inyuzinyuzi hamwa no kunywa amazi bishobora gufasha kugabanya ibimenyetso vyo kuzura inda"
            }

        PrenatalHealthEducationHeartburnInform ->
            { english = "Instruct the patient that the following may help relieve heartburn"
            , kinyarwanda = Just "Sobanurira umubyeyi ko ibi bikurikira bifasha mu kugabanya ikirungurira"
            , kirundi = Just "Bwira umuvyeyi ko ibi bikurikira bishobora gufasha kugabanya ugusha mu mushishito/mu mutima"
            }

        PrenatalHealthEducationVaricoseVeinsInform ->
            { english = "Instruct the patient that compression stockings (tight socks or leggings) and elevating their legs will help reduce varicose veins"
            , kinyarwanda = Just "Igisha umubyeyi ko kwambara ibintu bimufashe ku maguru (amasogisi,..) no gusegura amaguru igihe aryamye bizamurinda kubyimba kw'imitsi"
            , kirundi = Just "Bwira umuvyeyi ko amashesheti amufata cane hamwe no kuduza amaguru bitangabanya imitsi yavyimvye canke yarushe"
            }

        PrenatalHealthEducationLegPainRednessInform ->
            { english = "Instruct the patient that regular exercise and stretching can relieve leg pain or redness"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho izamurinda kuribwa amaguru ndetse no kuba yatukuara"
            , kirundi = Just "Bwira umuvyeyi ko kugira ikarashishi isanzwe no kugorora imitsi ko bishobora kugabanya ububabare bw'amaguru canke ugutukura"
            }

        PrenatalHealthEducationPelvicPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent pelvic pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngoraramubiri ihoraho izamurinda kuribwa mu kiziba cy'inda"
            , kirundi = Just "Bwira umuvyeyi ko imyimenyerezo yaburi munsi mu gihe c'imbanyi bifasha kwirinda ububabare bw'inda yo hepfo"
            }

        PrenatalHealthEducationSaferSexInform ->
            { english = "Counsel patient on safer sex practices"
            , kinyarwanda = Just "Gira inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
            , kirundi = Just "Uguhanira umuvyeyi gukora ibikorwa mpuzabitsina bitagira ingaruka mbi"
            }

        PrenatalHealthEducationEarlyMastitisOrEngorgmentInform ->
            { english = "Instruct the patient that the following may help relieve symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira byamufasha kugabanya uburibwe"
            , kirundi = Just "Bwira umuvyeyi ko ibi bikurikira bishobora gufasha kugabanya ibimenyetso"
            }

        PrenatalHealthEducationMentalHealthInform ->
            { english = "Provide information to support patients mental well being during pregnancy"
            , kinyarwanda = Just "Tanga inama zafasha umubyeyi utwite kubungabunga ubuzima bwo mu mutwe"
            , kirundi = Just "Tanga inyigisho kugira uremeshe abagore bafise imbanyi hama batekane mu mutwe, k'umutima no k'umubiri"
            }

        PrenatalNCDProgramHeaderPrefix ->
            { english = "This patient was diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwa"
            , kirundi = Just "Uyu mugwayi basanze afise"
            }

        PrenatalNCDProgramHeaderSuffix ->
            { english = "during her pregnancy"
            , kinyarwanda = Just "mu gihe yari atwite"
            , kirundi = Just "Mu gihe c'imbanyi yiwe"
            }

        PrenatalNCDProgramInstructions ->
            { english = "Refer patient to NCD services for further management"
            , kinyarwanda = Just "Ohereza umurwayi muri serivisi y'indwara zitandura bamwiteho byimbitse"
            , kirundi = Just "Rungika umugwayi mu gisata c'ingwara zitandukira kugira bagire ibindi bipimo vyiyongerako"
            }

        PrenatalUltrasoundHeader ->
            { english = "This patient is uncertain of LMP dating"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrenatalUltrasoundInstructions ->
            { english = "Refer patient to ultrasound for further evaluation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrenatalNextStepsTask isChw task ->
            case task of
                Pages.Prenatal.Activity.Types.NextStepsAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Just "Kwemeza itariki yo kugaruka"
                    , kirundi = Just "Kwemeza umubonano"
                    }

                Pages.Prenatal.Activity.Types.NextStepsFollowUp ->
                    { english = "CHW Follow Up"
                    , kinyarwanda = Just "Isura ry'umujyanama w'ubuzima"
                    , kirundi = Just "Ikurikiranwa rikozwe n'Abaremeshakiyago"
                    }

                Pages.Prenatal.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Just "Rungika kw'ivuriro"
                        }

                    else
                        { english = "Referral"
                        , kinyarwanda = Just "Kohereza"
                        , kirundi = Just "Kurungika"
                        }

                Pages.Prenatal.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                Pages.Prenatal.Activity.Types.NextStepsNewbornEnrolment ->
                    { english = "Newborn Enrollment"
                    , kinyarwanda = Just "Kwandika uruhinja"
                    , kirundi = Just "Ukwandika abana bavutse"
                    }

                Pages.Prenatal.Activity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.Activity.Types.NextStepsWait ->
                    { english = "Wait"
                    , kinyarwanda = Just "Tegereza"
                    , kirundi = Just "Rindira"
                    }

        PrenatalRecurrentNextStepsTask task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.NextStepsSendToHC ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Just "Kurungika"
                    }

                Pages.Prenatal.RecurrentActivity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.RecurrentActivity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

        PrenatalARVProgramInstructions forPostpartum ->
            if forPostpartum then
                { english = "Refer patient to ARV services for further management"
                , kinyarwanda = Just "Ohereza umurwayi muri serivisi itanga imiti igabanya ubukana bwa Virusi itera SIDA bamwiteho byimbiste"
                , kirundi = Nothing
                }

            else
                { english = "Refer patient to ARV services for assessment of ARVs"
                , kinyarwanda = Just "Ohereza umurwayi muri serivise itanga imiti igabanya ubukana kugirango hasuzumwe neza ibijyanye n'imiti igabanya ubukana bwa Virusi itera SIDA"
                , kirundi = Nothing
                }

        PrenatalHIVSignQuestion sign ->
            case sign of
                HIVProgramHC ->
                    { english = "Does the health center have a ARV services program"
                    , kinyarwanda = Just "Ikigonderabuzima cyaba gifite service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                PartnerHIVPositive ->
                    { english = "Is the patients partner HIV positive"
                    , kinyarwanda = Just "Umugabo we yaba afite virusi itera SIDA"
                    , kirundi = Just "Mbega umushingantahe wiwe afise umugera wa SIDA"
                    }

                PartnerTakingARV ->
                    { english = "Is the patients partner taking ARVs"
                    , kinyarwanda = Just "Umugabo we yaba fata imiti igabanya ubukana bwa virusi itera SIDA"
                    , kirundi = Nothing
                    }

                PartnerSurpressedViralLoad ->
                    { english = "Does the partner have a surpressed viral load"
                    , kinyarwanda = Just "Umugabo we yaba atakigaragaza ingano ya virusi mu maraso"
                    , kirundi = Nothing
                    }

                NoPrenatalHIVSign ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalImmunisationTask task ->
            case task of
                Pages.Prenatal.Activity.Types.TaskTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    , kirundi = Just "Rudadaza"
                    }

        PrenatalImmunisationDescription task ->
            case task of
                VaccineTetanus ->
                    { english = "The Tetanus vaccine prevents the patient from getting Tetanus which causes muscle spasms, fever, high blood pressure, and death."
                    , kinyarwanda = Just "Urukingo rw'agakwega rurinda umwana kwandura indwara y'agakwega (Tetanosi) itera kugagara kw'imitsi, umuriro, umuvuduko w'amaraso ndetse n'urupfu."
                    , kirundi = Just "Urucanco rwa Rudadaza rukingira umugwayi kwandura ingwara ya Rudadaza ariyo itera ukudadara kw'imitsi, ubushuhe, umuvuduko w'amaraso, hamwe n'urupfu"
                    }

        PrenatalImmunisationHeader task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    , kirundi = Just "Rudadaza"
                    }

        PrenatalImmunizationHistory task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus History"
                    , kinyarwanda = Just "Amakuru ku ndwara y'Agakwega"
                    , kirundi = Just "Akahise ka Rudadaza"
                    }

        PrenatalMentalHealthQuestion question ->
            case question of
                MentalHealthQuestion1 ->
                    { english = "I have been able to laugh and see the funny side of things"
                    , kinyarwanda = Just "Njya nshobora guseka kandi nkabona ibintu mu buryo bwiza"
                    , kirundi = Just "Nashoboye gutwenga no kubona uruhande rutwengeje kw'ibintu"
                    }

                MentalHealthQuestion2 ->
                    { english = "I have looked forward with enjoyment to things"
                    , kinyarwanda = Just "Nategereje ko ibintu nezerewe"
                    , kirundi = Just "Narindiranye ikineza ibintu"
                    }

                MentalHealthQuestion3 ->
                    { english = "I have blamed myself unnecessarily when things went wrong"
                    , kinyarwanda = Just "Njya niciraga urubanza iyo ibintu byabaga byagenze nabi"
                    , kirundi = Just "Nishwaniye ata mpamvu mugihe ibintu vyagenze nabi"
                    }

                MentalHealthQuestion4 ->
                    { english = "I have been anxious or worried for no good reason"
                    , kinyarwanda = Just "Njya mpangayika nta mpamvu igaragara"
                    , kirundi = Just "Nahagaritse umutima canke nagize ubwoba ata mpamvu ifatika"
                    }

                MentalHealthQuestion5 ->
                    { english = "I have felt scared or panicky for no very good reason"
                    , kinyarwanda = Just "Njya ngira ubwoba cyangwa nkakuka umutima nta mpamvu ifatika"
                    , kirundi = Just "Numvise ngize ubwoba canke mpangayitse ata mpamvu ifise insiguro"
                    }

                MentalHealthQuestion6 ->
                    { english = "Things have been getting on top of me"
                    , kinyarwanda = Just "Ibintu bijya bindenga "
                    , kirundi = Just "Ibintu vyagiye hejuru yanje/vyandemereye"
                    }

                MentalHealthQuestion7 ->
                    { english = "I have been so unhappy that I have had difficulty sleeping"
                    , kinyarwanda = Just "Njya numva mbabaye ku buryo ngira ikibazo cyo kudasinzira"
                    , kirundi = Just "Nababaye cane kuburyo vyanse ko nsinzira"
                    }

                MentalHealthQuestion8 ->
                    { english = "I have felt sad or miserable"
                    , kinyarwanda = Just "Njya numva mbabaye cyangwa mfite ishavu "
                    , kirundi = Just "Numvise mbabaye canke ngowe"
                    }

                MentalHealthQuestion9 ->
                    { english = "I have been so unhappy that I have been crying"
                    , kinyarwanda = Just "Njya numva mbabaye cyane ku buryo ndira"
                    , kirundi = Just "Nababaye cane kuburyo narize"
                    }

                MentalHealthQuestion10 ->
                    { english = "The thought of harming myself has occurred to me"
                    , kinyarwanda = Just "Ibitekerezo byo kwigirira nabi bijya binzamo"
                    , kirundi = Just "Iciyumviro co kwigirira nabi caranjemwo mu mutwe"
                    }

        PrenatalMentalHealthOptionForQuestion question option ->
            case question of
                MentalHealthQuestion1 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I always could"
                            , kinyarwanda = Just "Buri gihe"
                            , kirundi = Just "Nkuko nahora ndabishobora iminsi yose"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not quite so much now"
                            , kinyarwanda = Just "Ubu ntago ari cyane"
                            , kirundi = Just "Sivyinshi cane ubu"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely not so much now"
                            , kinyarwanda = Just "Ntago ari cyane na gato"
                            , kirundi = Just "Sivyinshi cane ubu"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Just "Nta na gato"
                            }

                MentalHealthQuestion2 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I ever did"
                            , kinyarwanda = Just "Nk'ibisanzwe"
                            , kirundi = Just "Nkuko nigeze kuba ndabikora"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Rather less than I used to"
                            , kinyarwanda = Just "Byaraganutse ugereranyije nuko byari bisanzwe"
                            , kirundi = Just "Ahubwo munsi yuko narinsanzwe nkora"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely less than I used to"
                            , kinyarwanda = Just "Gake cyane ugereranyije n‘uko byari bisanzwe"
                            , kirundi = Just "Munsi cane yivyo nahora nkora"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Hardly at all"
                            , kinyarwanda = Just "Habe na mba"
                            , kirundi = Just "Biragoye gose"
                            }

                MentalHealthQuestion3 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, nta na rimwe"
                            , kirundi = Just "Oya, Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Si cyane"
                            , kirundi = Just "Si kenshi cane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, some of the time"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Just "Ego, igihe kanaka"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, akenshi"
                            , kirundi = Just "Ego, igihe kinini"
                            }

                MentalHealthQuestion4 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, not at all"
                            , kinyarwanda = Just "No, nta na rimwe"
                            , kirundi = Just "Oya, nta na gato"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            , kirundi = Just "Biragoye bukebuke"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Just "Ego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, very often"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Just "Ego, kenshi cane"
                            }

                MentalHealthQuestion5 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Just "Nta na gato"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not much"
                            , kinyarwanda = Just "Oya, ntago ari cyane"
                            , kirundi = Just "Oya, si vyinshi"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Just "Ego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite a lot"
                            , kinyarwanda = Just "Yego, akenshi"
                            , kirundi = Just "Ego, vyinshi"
                            }

                MentalHealthQuestion6 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, I have been coping as well as ever"
                            , kinyarwanda = Just "Oya, ndabyakira nk'ibisanzwe"
                            , kirundi = Just "Oya, naragerageje kuvyakira uko bisanzwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, most of the time I have coped quite well"
                            , kinyarwanda = Just "Oya, akenshi njya nshoboraga kubyakira neza"
                            , kirundi = Just "Oya, igihe kinini navyigenjejemwo neza"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes I haven’t been coping as well as usual"
                            , kinyarwanda = Just "Yes, rimwe na rimwe ntago njya mbyakira neza"
                            , kirundi = Just "Ego, rimwe na rimwe sinigeze mpura navyo nkuko bisanzwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time I haven’t been able to cope"
                            , kinyarwanda = Just "Yego, kenshi na kenshi ntago njya mbyakira neza"
                            , kirundi = Just "Ego, igihe kinini ntivyankundiye ko menyera"
                            }

                MentalHealthQuestion7 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "nta na rimwe"
                            , kirundi = Just "Nta na gato"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not very often"
                            , kinyarwanda = Just "Oya, ntago ari kenshi"
                            , kirundi = Just "Oya, Si kenshi cane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Just "Ego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Just "Ego, igihe kinini"
                            }

                MentalHealthQuestion8 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Just "Nta na gato"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Ntago ari kenshi"
                            , kirundi = Just "Si kenshi cane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            , kirundi = Just "Ego, kenshi"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Just "Ego, igihe kinini"
                            }

                MentalHealthQuestion9 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, Nta na rimwe"
                            , kirundi = Just "Oya, Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Only occasionally"
                            , kinyarwanda = Just "Gisa rimwe na riwme"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "yego, kenshi"
                            , kirundi = Just "Ego, kenshi"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, Kemshi na kenshi"
                            , kirundi = Just "Ego, igihe kinini"
                            }

                MentalHealthQuestion10 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Never"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Just "Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            , kirundi = Just "Biragoye bukebuke"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Sometimes"
                            , kinyarwanda = Just "rimwe na rimwe"
                            , kirundi = Just "Rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            , kirundi = Just "Ego, kenshi"
                            }

        PrenatalMentalHealthSpecialistHelper ->
            { english = "Refer patient to mental health specialist for further evaluation"
            , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
            , kirundi = Just "Rungika umuvyeyi k'umuhinga w'ingwara zo mu mutwe kugira amupime kandi ibipimo vyiyongera"
            }

        PrenatalMentalHealthSpecialistQuestion ->
            { english = "Does this health center have a mental health specialist available"
            , kinyarwanda = Just "Iki kigo nderabuzima gifite umuganga w'inzobere ku buzima bwo mu mutwe"
            , kirundi = Nothing
            }

        PrenatalMentalHealthWarningPopupMessage ->
            { english = "Patient shows signs of being a suicide risk"
            , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
            , kirundi = Just "Umugwayi yerekana ibimenyetso yuko ashobora kwiyahura"
            }

        PrenatalMentalHealthWarningPopupInstructions ->
            { english = "Contact mental health specialist immediately"
            , kinyarwanda = Just "Ihutire kureba umuganga w'inzobere mu buzima bwo mu mutwe"
            , kirundi = Just "Ukuvugana n'umuhinga w'ingwara zo mu mutwe vuba bwango"
            }

        OutsideCareSignQuestion sign ->
            case sign of
                SeenAtAnotherFacility ->
                    { english = "Have you been seen at another facility since your last visit"
                    , kinyarwanda = Just "Waba hari irindi vuriro wagiyeho nyuma yo kuva hano"
                    , kirundi = Nothing
                    }

                GivenNewDiagnoses ->
                    { english = "Were you given a new diagnosis"
                    , kinyarwanda = Just "Haba hari ubundi burwayi bagusanzemo"
                    , kirundi = Just "Woba warahawe isuzumwa rishasha"
                    }

                GivenMedicine ->
                    { english = "Were you given medicine"
                    , kinyarwanda = Just "Waba warahawe imiti"
                    , kirundi = Just "Woba warahawe imiti"
                    }

                PlannedFollowUpCareWithSpecialist ->
                    { english = "Do you have follow up care planned with a specialist"
                    , kinyarwanda = Just "Waba ufite gahunda yo gukurikiranwa n'umuganga w'inzobere"
                    , kirundi = Just "Mbega warateguye ikurikiranwa rikozwe n'umuhinga mu kuvura abana"
                    }

                -- There's not question for this sign.
                NoOutsideCareSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        OutsideCareMedicationDosage medication ->
            case medication of
                OutsideCareMedicationQuinineSulphate ->
                    { english = "3 x a day for 7 days"
                    , kinyarwanda = Just "inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCoartem ->
                    { english = "4 tablets by mouth 2x a day for 7 days"
                    , kinyarwanda = Just "Kunywa ibinini 4 inshuro 2 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationPenecilin1 ->
                    { english = "IM x 1"
                    , kinyarwanda = Just "IM inshuro 1"
                    , kirundi = Just "IM Gucisha umuti mu mutsi incuro 1 gusa"
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 mu cyumweru mu byumweru 3"
                    , kirundi = Just "IM  Gucisha umuti mu mutsi incuro 1 mu ndwi mu kiringo c'indwi 3"
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "Kunywa inshuro 4 ku munsi mu minsi 14"
                    , kirundi = Just "Kumira incuro 4 ku munsi mu minsi 14"
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "Kunywa ibinini 4 ku munsi"
                    , kirundi = Just "Ibinini 4 vyo kumira ku munsi"
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    , kirundi = Just "IM buri munsi mu kiringo c'iminsi 10"
                    }

                OutsideCareMedicationMethyldopa2 ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Ikinini kimwe (1) co kumira kabiri (2) ku munsi"
                    }

                OutsideCareMedicationMethyldopa3 ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    , kirundi = Just "Kumira gatatu ku munsi ku munsi"
                    }

                OutsideCareMedicationMethyldopa4 ->
                    { english = "by mouth 4x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 4 ku munsi"
                    , kirundi = Just "Kumira incuro 4 ku munsi"
                    }

                OutsideCareMedicationCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Kumira incuro 2 ku munsi"
                    }

                OutsideCareMedicationAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 1 ku munsi"
                    , kirundi = Just "Kumira incuro 1 ku munsi"
                    }

                OutsideCareMedicationTDF3TC ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    , kirundi = Just "Kumira ikinini kimwe incuro 1 ku munsi"
                    }

                OutsideCareMedicationDolutegravir ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    , kirundi = Just "Kumira ikinini kimwe incuro 1 ku munsi"
                    }

                OutsideCareMedicationIron1 ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 1 ku munsi"
                    , kirundi = Just "Kumira ikinini kimwe incuro 1 ku munsi"
                    }

                OutsideCareMedicationIron2 ->
                    { english = "one tab by mouth 2x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 2 ku munsi"
                    , kirundi = Just "Kumira ikinini kimwe incuro 2 ku munsi"
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    , kirundi = Just "Kumira incuro 3 ku munsi"
                    }

                -- Dosage is not applicable for other options.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        OutsideCareMedicationLabel medication ->
            case medication of
                OutsideCareMedicationQuinineSulphate ->
                    { english = "Quinine Sulphate per os (10 mg/kg/dose)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCoartem ->
                    { english = "Coartem"
                    , kinyarwanda = Just "Kowaritemu"
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForMalaria ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                OutsideCareMedicationPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Just "Penisiline (udupimo tungana na miliyoni 2,4)"
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Just "Penisiline (udupimo tungana na miliyoni 2,4)"
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    , kirundi = Just "Erythromycine (500 mg)"
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    , kirundi = Just "azithromycine(2g)"
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "Ceftriaxone (1g)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForSyphilis ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                OutsideCareMedicationMethyldopa2 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationMethyldopa3 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationMethyldopa4 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCarvedilol ->
                    { english = "Carvedilol (6.25mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForHypertension ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta ns kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                OutsideCareMedicationTDF3TC ->
                    { english = "TDF+3TC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                OutsideCareMedicationDolutegravir ->
                    { english = "Dolutegravir (50mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForHIV ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                OutsideCareMedicationIron1 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationIron2 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    , kirundi = Just "Fer (60mg)"
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "Folic Acid (400IU)"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Acide Folique (400IU)"
                    }

                NoOutsideCareMedicationForAnemia ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalPhotoHelper ->
            { english = "Take a picture of the mother's belly. Then you and the mother will see how the belly has grown!"
            , kinyarwanda = Just "Fata ifoto y'inda y'umubyeyi hanyuma uyimwereke arebe uko yakuze/yiyongereye."
            , kirundi = Just "Fata ifoto y'inda y'umuvyeyi. Hama wewe n'umuvyeyi muzoza muraraba ingene inda ikura!"
            }

        PrenatalSymptom value ->
            case value of
                BurningWithUrination ->
                    { english = "Burning with Urination"
                    , kinyarwanda = Just "Kunyara Ukababara"
                    , kirundi = Just "Ugusha hamwe n'ugusoba"
                    }

                AbnormalVaginalDischarge ->
                    { english = "Abnormal Vaginal Discharge"
                    , kinyarwanda = Just "Gusohora ibintu bidasanzwe mu gitsina"
                    , kirundi = Just "Isohoka ridasanzwe ry'amazi canke ibintu bimeze nk'amazi mu gihimba c'irondoka"
                    }

                NauseaAndVomiting ->
                    { english = "Nausea and Vomiting"
                    , kinyarwanda = Just "Iseseme no kuruka"
                    , kirundi = Just "Iseseme no kudahwa"
                    }

                Heartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Just "Ugusha k'umutima"
                    }

                LegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    , kirundi = Just "Ibinyanya vy'amaguru"
                    }

                LowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    , kirundi = Just "Ububabare bw'umugongo wo hasi"
                    }

                CoughContinuous ->
                    { english = "Cough for >2 weeks"
                    , kinyarwanda = Just "Inkorora irengeje ibyumweru 2"
                    , kirundi = Just "Ugukorora ikiringo kirenga indwi 2"
                    }

                PelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    , kirundi = Just "Ububabare bwo mu nda yo hepfo"
                    }

                Constipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    , kirundi = Just "Ukuzura inda"
                    }

                VaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    , kirundi = Just "Ivyimba ry'imitsi"
                    }

                LegPainRedness ->
                    { english = "Leg Pain or Redness (One Leg)"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    , kirundi = Just "Ububabare no kubenjuka ku maguru (Ukuguru kumwe)"
                    }

                PostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    }

                PostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Just "Ukutihangana hageze gusoba"
                    }

                PostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                PostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Just "Uburuhe"
                    }

                PostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                PostpartumPerinealPainOrDischarge ->
                    { english = "Perineal pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Just "Ububabare bw'umugongo hepfo"
                    }

                NoPrenatalSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        PrenatalSymptomQuestion value ->
            case value of
                SymptomQuestionDizziness ->
                    { english = "Are you experiencing dizziness"
                    , kinyarwanda = Just "Waba ujya ugira isereri"
                    , kirundi = Just "Mbega urazungurirwa"
                    }

                SymptomQuestionLowUrineOutput ->
                    { english = "Are you experiencing low urine output"
                    , kinyarwanda = Just "Waba ujya unyara inkari nkeya"
                    , kirundi = Just "Mbega ubona usohora amasobwe make/umukoyo muke/umwanda mutoyi mukeya"
                    }

                SymptomQuestionDarkUrine ->
                    { english = "Are you experiencing dark urine"
                    , kinyarwanda = Just "Waba ujya unyara inkari zijimye"
                    , kirundi = Just "Mbega ubona uzana amasobwe usa n'uwirabura"
                    }

                SymptomQuestionPelvicPainHospitalization ->
                    { english = "Is there severe pain that requires referral to hospital"
                    , kinyarwanda = Just "Waba ufite ububabare bukabije busaba koherezwa ku bitaro"
                    , kirundi = Just "Mbega hariho ububabare bukomeye cane busaba kujanwa kwa Muganga"
                    }

                SymptomQuestionLegPainRednessLeft ->
                    { english = "On which side are you experiencing leg pain or redness"
                    , kinyarwanda = Just "Ni uruhe ruhande rw'ukuguru ruribwa cyangwa rutukuye"
                    , kirundi = Just "Ni kuruhande uruhe uriko urumva ububabare canke ugutukura k'ukuguru"
                    }

                SymptomQuestionLegPainful ->
                    { english = "Is the leg painful"
                    , kinyarwanda = Just "Ubabara ukuguru"
                    , kirundi = Just "Mbega ukuguru kurababara"
                    }

                SymptomQuestionLegSwollen ->
                    { english = "Is the leg swollen"
                    , kinyarwanda = Just "Ukuguru kurabyimbye"
                    , kirundi = Just "Mbega ukuguru kuravyimvye"
                    }

                SymptomQuestionLegWarm ->
                    { english = "Is the leg red or warm to the touch"
                    , kinyarwanda = Just "Ukuguru kuratukuye cyangwa kurashyushye iyo ukozeho"
                    , kirundi = Just "Mbega ukuguru kuratukurije canke kurashushe ugukozeko"
                    }

                SymptomQuestionNightSweats ->
                    { english = "Do you have night sweats"
                    , kinyarwanda = Just "Waba ubira ibyuya nijoro"
                    , kirundi = Just "Mbega urabira ivyuya mw'ijoro"
                    }

                SymptomQuestionBloodInSputum ->
                    { english = "Do you have blood in sputum"
                    , kinyarwanda = Just "Waba ugira ikororwa kirimo amaraso"
                    , kirundi = Just "Mbega urafise amaraso mu bikororwa"
                    }

                SymptomQuestionWeightLoss ->
                    { english = "Do you have weight loss"
                    , kinyarwanda = Just "Waba waratakaje ibiro"
                    , kirundi = Just "Mbega urata ibiro"
                    }

                SymptomQuestionSevereFatigue ->
                    { english = "Do you have severe fatigue"
                    , kinyarwanda = Just "Waba ugira umunaniro ukabije"
                    , kirundi = Just "Mbega urafise uburuhe burengeje"
                    }

                SymptomQuestionVaginalItching ->
                    { english = "Do you experience vaginal itching"
                    , kinyarwanda = Just "Waba ufite uburyaryate mu gitsina"
                    , kirundi = Just "Mbega uraribwaribwa mu gitsina c'irondoka"
                    }

                SymptomQuestionVaginalDischarge ->
                    { english = "Do you experience vaginal discharge"
                    , kinyarwanda = Just "Ujya ubona ibintu bidasanzwe biva mu gitsina"
                    , kirundi = Just "Mbega urasohora iriziri rivuye mu gitsina c'irondoka"
                    }

                SymptomQuestionFrequentUrination ->
                    { english = "Do you experience urinating frequently"
                    , kinyarwanda = Just "Waba ujya kunyara buri kanya"
                    , kirundi = Just "Mbega urumva ushaka gusoba/kuja k'umwanda mutoyi kenshi"
                    }

                SymptomQuestionFlankPain ->
                    { english = "Do you experience flank pain"
                    , kinyarwanda = Just "Waba ujya uribwa mu ibondo"
                    , kirundi = Just "Mbega urumva ububabare mu mbavu"
                    }

                SymptomQuestionPartnerUrethralDischarge ->
                    { english = "Does your partner have urethral discharge"
                    , kinyarwanda = Just "Umugabo wawe ajya agira ibintu bidasanzwe biva mu gitsina"
                    , kirundi = Just "Mbega umugabo wawe arafise gusohagura umukoyo"
                    }

                NoSymptomQuestions ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta na kimwe"
                    }

        PrenatalSymptomQuestionsHeader ->
            { english = "The patient has noted symptoms that require follow up questions"
            , kinyarwanda = Just "Umubyeyi yagaragaje ibimenyetso bisaba ibindi bibazo"
            , kirundi = Just "Umugwayi yerekanye ibimenyetso bisaba gukurikizako ibindi bibazo (kugira umuntu ategere)"
            }

        TestExecutionNote note ->
            case note of
                TestNoteRunToday ->
                    { english = "Run Today"
                    , kinyarwanda = Just "Ikizamini cyakozwe uyu munsi"
                    , kirundi = Just "Gikorwe uno munsi"
                    }

                TestNoteRunPreviously ->
                    { english = "Run Previously"
                    , kinyarwanda = Just "Ikizamimi cyakozwe ubushize"
                    , kirundi = Just "Cakozwe ubushize"
                    }

                TestNoteLackOfReagents ->
                    { english = "Lack of Reagents"
                    , kinyarwanda = Just "Kubura kw'ibikoresho byo gupima"
                    , kirundi = Just "Ibura ry'imiti ikoreshwa mu gupima"
                    }

                TestNoteLackOfOtherSupplies ->
                    { english = "Lack of Other Supplies"
                    , kinyarwanda = Just "Kubura kw'ibindi bikoresho bicyenerwa mu gupima"
                    , kirundi = Just "Ibura ry'ibikoresho bindi"
                    }

                TestNoteNoEquipment ->
                    { english = "No Equipment"
                    , kinyarwanda = Just "Nta gikoresho gihari"
                    , kirundi = Just "Nta bikoresho"
                    }

                TestNoteBrokenEquipment ->
                    { english = "Broken Equipment"
                    , kinyarwanda = Just "Igikoresho gipima cyarangiritse"
                    , kirundi = Just "Ibikoresho bimenetse"
                    }

                TestNoteNotIndicated ->
                    { english = "Not Indicated"
                    , kinyarwanda = Just "Ikizamini nticyasabwe"
                    , kirundi = Just "Ntivyerekanywe"
                    }

                TestNoteKnownAsPositive ->
                    { english = "Known as Positive"
                    , kinyarwanda = Just "Asanzwe afite ubwandu"
                    , kirundi = Just "Birazwi ko agwaye"
                    }

                TestNoteToBeDoneAtHospital ->
                    { english = "To be Done at Hospital"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Gukorerwa mu Bitaro"
                    }

        TestResult result ->
            case result of
                TestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Just "Afise ingwara"
                    }

                TestNegative ->
                    translationSet NegativeLabel

                TestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Just "kutamenyekana"
                    }

        PrenatalVaccineLabel value ->
            case value of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    , kirundi = Just "Rudadaza"
                    }

        PreTerm ->
            { english = "Pre Term"
            , kinyarwanda = Just "Inda itaragera igihe"
            , kirundi = Just "Imbere y'igihe"
            }

        PregnancyConcludedLabel ->
            { english = "or Pregnancy Concluded"
            , kinyarwanda = Just "Cyangwa Iherezo ry'inda"
            , kirundi = Just "Canke imbanyi yashitse ku gihe cayo"
            }

        PregnancyOutcomeLabel ->
            { english = "Pregnancy Outcome"
            , kinyarwanda = Just "Iherezo ry'inda"
            , kirundi = Just "Inyishu yerekeye imbanyi"
            }

        PregnancyOutcome outcome ->
            case outcome of
                OutcomeLiveAtTerm ->
                    { english = "Live Birth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Kubyara umwana muzima/Ushyitse (ku byumweru 38 kuzamura)"
                    , kirundi = Just "Imbanyi ivutse ikomeye ikiringo kigeze (Indwi 38 z'imbanyi canke zirenga)"
                    }

                OutcomeLivePreTerm ->
                    { english = "Live Birth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Kubyara mwana udashyitse (munsi y'ibyumweru 38)"
                    , kirundi = Just "Imbanyi ivutse imbere y'ikiringo (mbere y'indwi 38)"
                    }

                OutcomeStillAtTerm ->
                    { english = "Stillbirth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda bageze igihe cyo kuvuka (ku byumweru 38 kuzamura)"
                    , kirundi = Just "Kuvyarira ku gihe (Indwi 38 - AGE (z'Igihe co Kwibungenga Caharuwe ) canke zirenga)"
                    }

                OutcomeStillPreTerm ->
                    { english = "Stillbirth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda batagejeje igihe cyo kuvuka (munsi y'ibyumweru 38)"
                    , kirundi = Just "Kuvyara imbere yuko igihe kigera (imbere y'indwi 38 - AGE (Igihe co Kwibungenga Caharuwe)"
                    }

                OutcomeAbortions ->
                    { english = "Abortions (before 24 weeks EGA)"
                    , kinyarwanda = Just "Kuvanamo inda (mbere y'ibyumweru 24)"
                    , kirundi = Just "Ikugwamwo ry'imbanyi (imbere y'indwi 24)"
                    }

        PreviousCSectionScar ->
            { english = "Previous C-section scar"
            , kinyarwanda = Just "Inkovu yaho babaze ubushize"
            , kirundi = Just "Igikomere c'ugukorwa guheruka"
            }

        PreviousDelivery ->
            { english = "Previous Delivery"
            , kinyarwanda = Just "Kubyara guheruka"
            , kirundi = Just "Ukwibaruka guheruka"
            }

        PreviousDeliveryPeriods period ->
            case period of
                LessThan18Month ->
                    { english = "Less than 18 month ago"
                    , kinyarwanda = Just "Munsi y'amezi 18 ashize"
                    , kirundi = Just "Munsi y'amezi 18 aheze"
                    }

                MoreThan5Years ->
                    { english = "More than 5 years ago"
                    , kinyarwanda = Just "Hejuru y'imyaka itanu ishize"
                    , kirundi = Just "Imyaka irenga 5 iraheze"
                    }

                Neither ->
                    { english = "Neither"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta na kimwe"
                    }

        PreviousFloatMeasurement value ->
            { english = "Previous measurement: " ++ String.fromFloat value
            , kinyarwanda = Just <| "Ibipimo by'ubushize: " ++ String.fromFloat value
            , kirundi = Just <| "Igipimo catanguye: " ++ String.fromFloat value
            }

        PreviousMeasurementNotFound ->
            { english = "No previous measurement on record"
            , kinyarwanda = Just "Nta gipimo cy'ubushize cyanditswe"
            , kirundi = Just "Nta bipimo vyafashwe ubushize bihari"
            }

        PriorTreatmentTask task ->
            case task of
                TreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Just "Isubiramwo ry'imiti"
                    }

        Programs ->
            { english = "Programs"
            , kinyarwanda = Just "Porogaramu"
            , kirundi = Just "Imigambi"
            }

        ProgressPhotos ->
            { english = "Progress Photos"
            , kinyarwanda = Just "Uko amafoto agenda ahinduka"
            , kirundi = Just "Iterambere rya mafoto"
            }

        ProgressReport ->
            { english = "Progress Report"
            , kinyarwanda = Just "Raporo y’ibyakozwe"
            , kirundi = Just "Iterambere ry'icegeranyo"
            }

        ProgressReports ->
            { english = "Progress Reports"
            , kinyarwanda = Just "Raporo z’ibyakozwe"
            , kirundi = Just "Iterambere ry'ivyegeranyo"
            }

        ProgressTimeline ->
            { english = "Progress Timeline"
            , kinyarwanda = Just "Uko inda igenda ikura"
            , kirundi = Just "Ikiringo c'iterambere"
            }

        ProgressTrends ->
            { english = "Progress Trends"
            , kinyarwanda = Just "Uko ibipimo bigenda bizamuka"
            , kirundi = Just "Amayira y'iterambere"
            }

        ProvideHealthEducationAndInstructToIsolate ->
            { english = "Provide health education and instruct them to self isolate at home"
            , kinyarwanda = Nothing
            , kirundi = Just "Tanga inyigisho yerekeye amagara y'abantu kandi ubabwire kwiyugaranira ahantu hawenyene muhira "
            }

        PreTermPregnancy ->
            { english = "Number of Pre-term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima badashyitse"
            , kirundi = Just "Igitigiri c'imbanyi zavutse zitageze kw'itarike (Ariko abana bakaba bariho)"
            }

        TestDate ->
            { english = "Date of Test"
            , kinyarwanda = Just "Itariki y'Ikizamini"
            , kirundi = Just "Itarike y'igipimo"
            }

        TestName ->
            { english = "Test name"
            , kinyarwanda = Just "Izina ry'ikizamini"
            , kirundi = Just "Izina ry'igipimo"
            }

        TestPerformedQuestion ->
            { english = "Were you able to perform the test"
            , kinyarwanda = Just "Waba wakoze ikizamini"
            , kirundi = Just "Woba warashoboye gukora igipimo; gukora ikibazo"
            }

        TestPerformedTodayQuestion ->
            { english = "Did you perform this test today"
            , kinyarwanda = Just "Waba wakoze iki kizamini uyu munsi"
            , kirundi = Just "Mbega wagize ico gipimo uno munsi"
            }

        TestPrerequisiteQuestion value ->
            case value of
                PrerequisiteFastFor12h ->
                    { english = "Was this test performed before a meal"
                    , kinyarwanda = Just "Umurwayi yafatiwe iki kizamini mbere yo kurya"
                    , kirundi = Just "Mbega iki gipimo cabaye imbere yo gufungura"
                    }

                PrerequisiteImmediateResult ->
                    { english = "Where was this test performed"
                    , kinyarwanda = Just "Iki Kizamini cyakozwe"
                    , kirundi = Just "iki gipimo cabereye hehe"
                    }

                NoTestPrerequisites ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    }

        TestVariantUrineDipstickQuestion ->
            { english = "Which type of urine dipstick test was run"
            , kinyarwanda = Just "Ni ikihe kizamini cy'inkari cyakozwe"
            , kirundi = Just "Mbega ni ubuhe bwoko bw'igipimo c'umukoyo bwakozwe"
            }

        TestResultQuestion ->
            { english = "What was the result of the test"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni iyihe nyishu y'igipimo"
            }

        TestResultsQuestion ->
            { english = "What were the results of the test"
            , kinyarwanda = Just "Ibisubizo by'ikizamini byabaye ibihe"
            , kirundi = Just "Ni izihe nyishu z'ibipimo"
            }

        PriorDiagnosis ->
            { english = "Prior Diagnosis"
            , kinyarwanda = Just "Uburwayi yagize/yigeze kurwara"
            , kirundi = Just "Imbere y'isuzuma"
            }

        ProvidedHealthEducationAction ->
            { english = "Provided health education and anticipatory guidance"
            , kinyarwanda = Nothing
            , kirundi = Just "Tanga inyigisho yerekeye amagara y'abantu kandi utange intumbero hakiri kare"
            }

        ProvideHealthEducation ->
            { english = "Provide health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Just "Tanga inyigisho yerekeye amagara y'abantu kandi utange intumbero hakiri kare kuvyereke kwikingira"
            }

        ProvideHealthEducationShort ->
            { english = "Provide health education and anticipatory guidance"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Just "Tanga inyigisho yerekeye amagara y'abantu kandi utange intumbero hakiri kare"
            }

        ProvidedPreventionEducationQuestion ->
            { english = "Have you provided health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara hamwe no gutanga intumbero hakiri kare kugira umuntu yikingire"
            }

        ProvidedPreventionEducationQuestionShort ->
            { english = "Have you provided health education and anticipatory guidance"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho"
            , kirundi = Just "Mbega waratanze inyigisho kuvyerekeye amagara hamwe no gutanga intumbero hakiri kare"
            }

        ProvidedSymtomReliefGuidanceQuestion ->
            { english = "Have you provided the guidance for symptom relief"
            , kinyarwanda = Just "Wamusobanuriye ibijyanye n'imiti itangwa mukuvura ibimenyesto"
            , kirundi = Just "Mbega waratanze impanuro kuvyerekeye kwitezurira ibimenyetso"
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Just "Intara"
            , kirundi = Just "Intara"
            }

        ReadToggle isRead ->
            if isRead then
                { english = "Unread"
                , kinyarwanda = Nothing
                , kirundi = Just "Bitasomwe"
                }

            else
                { english = "Read"
                , kinyarwanda = Nothing
                , kirundi = Just "Ivyasomwe"
                }

        RandomBloodSugarResultNormalRange type_ ->
            case type_ of
                TestRunBeforeMeal _ ->
                    { english = "74-126 mg/dL (F)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TestRunAfterMeal _ ->
                    { english = "74-200 mg/dL (NF)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Read ->
            { english = "Read"
            , kinyarwanda = Nothing
            , kirundi = Just "Ivyasomwe"
            }

        ReasonForNotBreastfeeding reason ->
            case reason of
                NotBreastfeedingBreastPain ->
                    { english = "Breast pain"
                    , kinyarwanda = Just "Ububabare bw'amabere"
                    , kirundi = Just "Kubabara amabere"
                    }

                NotBreastfeedingBreastRedness ->
                    { english = "Breast redness"
                    , kinyarwanda = Just "Amabere aratukuye"
                    , kirundi = Just "Amabere atukura"
                    }

                NotBreastfeedingLowMilkProduction ->
                    { english = "Low milk production"
                    , kinyarwanda = Just "Amashereka adahagije"
                    , kirundi = Just "Umwimbu w'amata uri hasi"
                    }

                NotBreastfeedingProblemsLatching ->
                    { english = "Problems latching"
                    , kinyarwanda = Just "Ibibazo byo konka"
                    , kirundi = Just "Ingorane zo kugara"
                    }

                NotBreastfeedingMedicalProblems ->
                    { english = "Medical Problems"
                    , kinyarwanda = Just "Ibibazo by'uburwayi"
                    , kirundi = Just "Ingorane ziri m'ubuvuzi"
                    }

                NotBreastfeedingPersonalChoice ->
                    { english = "Personal Choice"
                    , kinyarwanda = Just "Amahitamo ye bwite"
                    , kirundi = Just "Ihitamwo ry'umuntu"
                    }

                NotBreastfeedingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReasonForNotIsolating reason ->
            case reason of
                NoSpace ->
                    { english = "No space available at home or clinic"
                    , kinyarwanda = Just "Nta mwanya uboneka mu rugo cyangwa mu ivuriro"
                    , kirundi = Just "Nta kibanza na kimwe kihari haba muhira canke kw'ivuriro/ku bitaro"
                    }

                TooIll ->
                    { english = "Too ill to leave alone"
                    , kinyarwanda = Just "Umurwayi ararembye ntagomba gusigara wenyine"
                    , kirundi = Just "Aragwaye cane kuburyo atogenda wenyene"
                    }

                CanNotSeparateFromFamily ->
                    { english = "Unable to separate from family"
                    , kinyarwanda = Just "Ntibishoboka kumutandukanya n'umuryango"
                    , kirundi = Just "Ntibishoboka kwitandukanya n'umuryango"
                    }

                OtherReason ->
                    { english = "Other"
                    , kinyarwanda = Just "Ikindi"
                    , kirundi = Just "Ibindi"
                    }

                IsolationReasonNotApplicable ->
                    translationSet NotApplicable

        ReasonForNotProvidingHealthEducation reason ->
            case reason of
                PatientNeedsEmergencyReferral ->
                    { english = "Patient needs an emergency referral"
                    , kinyarwanda = Just "Umurwayi akeneye kwoherezwa ku ivuriro byihutirwa"
                    , kirundi = Just "Umugwayi akeneye kujanwa kwa muganga vy'ihuta/kujanwa kubindi bitaro mu maguru masha"
                    }

                ReceivedEmergencyCase ->
                    { english = "Received an emergency case to treat"
                    , kinyarwanda = Just "Nakiriye undi murwayi ukeneye kuvurwa byihutirwa"
                    , kirundi = Just "Yaronse umugwayi yihutirwa kuvugwa"
                    }

                LackOfAppropriateEducationUserGuide ->
                    { english = "Lack of appropriate education user guide"
                    , kinyarwanda = Just "Nta mfashanyigisho yabugenewe ihari"
                    , kirundi = Just "Ukubura ry'indongozo nziza yo gukoresha ivyo wize"
                    }

                PatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Just "Umugwayi yanse"
                    }

                PatientTooIll ->
                    { english = "Patient too ill"
                    , kinyarwanda = Just "Umurwayi ararembye"
                    , kirundi = Just "Umugwaye aragwaye cane"
                    }

                NoReasonForNotProvidingHealthEducation ->
                    { english = "No reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    , kirundi = Just "Nta citwazo"
                    }

        ReasonForNotTaking reason ->
            case reason of
                NotTakingAdverseEvent ->
                    { english = "Adverse event"
                    , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                    , kirundi = Just "Icyabaye bibabaje"
                    }

                NotTakingNoMoney ->
                    { english = "No money for medication"
                    , kinyarwanda = Just "Nta mafaranga yo kwishyura imiti afite"
                    , kirundi = Just "Nta mafaranga yo kugura imiti"
                    }

                NotTakingMemoryProblems ->
                    { english = "Memory problems"
                    , kinyarwanda = Just "Ibibazo byo kwibagirwa"
                    , kirundi = Just "Ingorane zo kwibuka"
                    }

                NotTakingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoReasonForNotTakingSign ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Received ->
            { english = "Received"
            , kinyarwanda = Nothing
            , kirundi = Just "yaronse"
            }

        ReceivedFolicAcid ->
            { english = "Have you received Folic Acid"
            , kinyarwanda = Just "Wahawe ibinini bya Folic Acid"
            , kirundi = Just "Mbega wararonse Acide Folique"
            }

        ReceivedFrom ->
            { english = "Received From"
            , kinyarwanda = Nothing
            , kirundi = Just "Nakuye kwa"
            }

        ReceivedIronFolicAcid ->
            { english = "Has the mother received iron and folic acid supplement"
            , kinyarwanda = Just "Umubyeyi yahawe ibinini bya Fer cg Folic Acid byongera amaraso"
            , kirundi = Just "Mbega umuvyeyi yararonse ivyunyunyu vyiyongerako vya Fer na acide folique"
            }

        ReceivedMebendazole ->
            { english = "Has the mother received Mebendazole in the last 6 months"
            , kinyarwanda = Just "Ububyeyi yahawe umuti wa Mebendazole mu mezi 6 ashize"
            , kirundi = Just "Mbega nyina/umuvyeyi yararonse ibinini vyica inzoka arivyo mébendazole mu mezi 6 ashize"
            }

        ReceivedMosquitoNet ->
            { english = "Has the mother received a mosquito net"
            , kinyarwanda = Just "Umubyeyi yahawe inzitiramubu"
            , kirundi = Just "Mbega umuvyeyi yararonse umusegetera"
            }

        ReceivedVitaminA ->
            { english = "Have you received Vitamin A"
            , kinyarwanda = Just "Wahawe Vitamine A"
            , kirundi = Just "Mbega wararonse icunyunyu ca Vitamine A"
            }

        ReceiveOption option ->
            case option of
                OptionReceive ->
                    translationSet Yes

                OptionNotReceive ->
                    translationSet No

                OptionNotApplicable ->
                    translationSet NotApplicable

        Recommendation114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "Send Patient to the nearest health center"
                    , kinyarwanda = Just "Ohereza umurwayi ku kigo nderabuzima kikwegereye"
                    , kirundi = Just "Rungika umugwayi kw'ivuriro riri hagufi cane"
                    }

                SendToRRTCenter ->
                    { english = "Send patient to the Rapid Response Team center"
                    , kinyarwanda = Just "Ohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    , kirundi = Just "Rungika umugwayi mu kigo c'inyishu yihuta"
                    }

                SendToHospital ->
                    { english = "Send patient to the nearest hospital"
                    , kinyarwanda = Just "Ohereza umurwayi ku bitaro bikwegereye"
                    , kirundi = Just "Rungika umugwayi ku bitaro biri hagufu cane"
                    }

                OtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoneNoAnswer ->
                    { english = "No answer"
                    , kinyarwanda = Just "Nta Gisubizo cyabonetse"
                    , kirundi = Just "Nta nyishu"
                    }

                NoneBusySignal ->
                    { english = "Busy Signal"
                    , kinyarwanda = Just "Umurongo bawuvugiragaho"
                    , kirundi = Just "Ikimenyetso cafatiriwe"
                    }

                NoneOtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

        RecommendationSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Team will come to village"
                    , kinyarwanda = Just "Itsinda rizaza mu mudugudu"
                    , kirundi = Just "Umurwi uzoza ku musozi"
                    }

                SendToSiteWithForm ->
                    { english = "Advised to send patient to site with referral form"
                    , kinyarwanda = Just "Nagiriwe inama yo kohereza umurwayi ku rwego rubishinzwe yitwaje impapuro zimwohereza"
                    , kirundi = Just "Guhanura kungika umurwayi ku kigo afise urupapuro gw'irungikwa"
                    }

                OtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                NoneSentWithForm ->
                    { english = "No response. Sent patient with referral form."
                    , kinyarwanda = Just "Nta gisubizo. Nohereje umurwayi yitwaje impapuro zimwohereza."
                    , kirundi = Just " Nta nyishu. Rungika umugwayi wamuhaye n'urupapuro/Ifishi rumurungika ahandi"
                    }

                NonePatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Just "Umugwayi yanse"
                    }

                NoneOtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    }

                RecommendationSiteNotApplicable ->
                    translationSet NotApplicable

        Recommended ->
            { english = "Recommended"
            , kinyarwanda = Just "Imiti yemewe"
            , kirundi = Just "Bitegerezwa"
            }

        RecommendedButNotGivenDueTo ->
            { english = "recommended but not given due to"
            , kinyarwanda = Nothing
            , kirundi = Just "Vyategetswe ariko bitatanzwe kubera"
            }

        RecommendedSymptomRelief ->
            { english = "Recommended Symptom Relief"
            , kinyarwanda = Just "Imiti yemewe mukuvura ibimenyesto"
            , kirundi = Just "Icofasha kugabanya ibimenyetso categetswe"
            }

        RecommendedTreatmentSignDosage sign ->
            case sign of
                TreatmentPenecilin1 ->
                    { english = "IM x 1"
                    , kinyarwanda = Just "IM inshuro 1"
                    , kirundi = Just "IM Gucisha umuti mu mutsi incuro 1 gusa"
                    }

                TreatmentPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 buri cyumweru mu byumweru 3"
                    , kirundi = Just "IM incuro 1 mu ndwi mu kiringo c'indwi 3"
                    }

                TreatmentErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "mu kanwa inshuro enye ku munsi mu minsi 14"
                    , kirundi = Just "Kumira incuro 4 ku munsi mu minsi 14"
                    }

                TreatmentAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "ibinini 4 abinywe mu kanwa umunsi umwe"
                    , kirundi = Just "Ibinini 4 vyo kumira ku munsi"
                    }

                TreatmentCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    , kirundi = Just "IM buri munsi mu kiringo c'iminsi 10"
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "1 tablet by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "kunywa ikinini 1 inshuro ku munsi mu minsi 7"
                    , kirundi = Just "Ikinini 1 co kumira incuro 3 ku munsi, mu minsi 7"
                    }

                TreatmentNitrofurantoin ->
                    { english = "by mouth 2x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi mu minsi 7"
                    , kirundi = Just "Kumira incuro 2 ku munsi mu minsi 7"
                    }

                TreatmentAmoxicillin ->
                    { english = "by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 3 ku munsi mu minsi 7"
                    , kirundi = Just "Kumira incuro 3 ku munsi mu minsi 7"
                    }

                TreatmentClotrimaxazole200 ->
                    { english = "vaginally every night x 3 night"
                    , kinyarwanda = Just "mu gitsina buri joro mu majoro 3"
                    , kirundi = Just "Gucisha mu gitsina buri mugoroba hamwe n'amajoro atatu"
                    }

                TreatmentClotrimaxazole500 ->
                    { english = "vaginally one time"
                    , kinyarwanda = Just "inshuro imwe mu gitsina"
                    , kirundi = Just "Gucisha mu gitsina rimwe gusa"
                    }

                TreatmentMethyldopa2 ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Ikinini kimwe (1) co kumira kabiri (2) ku munsi"
                    }

                TreatmentMethyldopa3 ->
                    { english = "1 tablet by mouth three times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 3 ku munsi"
                    , kirundi = Just "Ikinini 1 co kumira 3 ku munsi"
                    }

                TreatmentMethyldopa4 ->
                    { english = "1 tablet by mouth four times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 4 ku munsi"
                    , kirundi = Just "Ikinini 1 co kumira 4 ku munsi"
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Kumira incuro 2 ku munsi"
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 1 ku munsi"
                    , kirundi = Just "Kumira incuro 1 ku munsi"
                    }

                TreatmentHydrochlorothiazide ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Just "Ikinini kimwe co kumira ku munsi buri munsi"
                    }

                TreatmentAmlodipine ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Just "Ikinini kimwe co kumira ku munsi buri munsi"
                    }

                TreatmentNifedipine ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Ikinini kimwe (1) co kumira kabiri (2) ku munsi"
                    }

                TreatmentCaptopril ->
                    { english = "1 tablet by mouth 3 times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 3 ku munsi"
                    , kirundi = Just "Ugufata ikinini 1, ukimize, 3 ku munsi mu minsi 5"
                    }

                TreatmentLisinopril ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Just "Ikinini kimwe co kumira ku munsi buri munsi"
                    }

                TreatmentAtenlol ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Just "Ikinini kimwe co kumira ku munsi buri munsi"
                    }

                TreatmentCloxacillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Just "Kumira ibinini 2 incuro 3 ku munsi mu minsi 7"
                    }

                TreatmentMastitisAmoxicillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Just "Kumira ibinini 2 incuro 3 ku munsi mu minsi 7"
                    }

                TreatmentPenecilinV ->
                    { english = "2 tablets by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "ibinini 2 mu kanwa inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Just "Kumira ibinini 2 incuro 3 ku munsi mu minsi 7"
                    }

                TreatmentParacetamol ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    , kirundi = Just "Kumira ikinini 1 incuro 3 ku munsi mu minsi 5"
                    }

                TreatmentIbuprofen ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    , kirundi = Just "Kumira ikinini 1 incuro 3 ku munsi mu minsi 5"
                    }

                TreatmentMetformin1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Ikinini kimwe (1) co kumira kabiri (2) ku munsi"
                    }

                TreatmentGlipenclamide1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Ikinini kimwe (1) co kumira kabiri (2) ku munsi"
                    }

                TreatmentMetformin2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Just "ibinini 2 mu kanwa mu gitondo n'ikinini kimwe mu kanwa nijoro"
                    , kirundi = Just "Kumira ibinini 2 vyo  mu gatondo n'ikinini 1 k'umugoroba"
                    }

                TreatmentGlipenclamide2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Just "ibinini 2 mu kanwa mu gitondo n'ikinini kimwe mu kanwa nijoro"
                    , kirundi = Just "Kumira ibinini 2 vyo  mu gatondo n'ikinini 1 k'umugoroba"
                    }

                TreatmentMetformin2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Just "ibinini bibiri mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Kumira ibinini 2 incuro 2 ku munsi"
                    }

                TreatmentGlipenclamide2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Just "ibinini bibiri mu kanwa inshuro 2 ku munsi"
                    , kirundi = Just "Kumira ibinini 2 incuro 2 ku munsi"
                    }

                -- Dosage is not applicable for other options.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        RecommendedTreatmentSignLabelForProgressReport sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "Quinine Sulphate - per os 10 mg/kg/dose, 3 times a day for 7 days"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Sulfate de quinine - per os 10 mg/kg/Idoze (igipimo),  Umuti wo gufata 3 k'umunsi mu kiringo c'iminsi indwi"
                    }

                TreatmentCoartem ->
                    { english = "Coartem - 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Coartem - ibinini 4 vyo kumira kabiri ku munsi mu minsi 3"
                    }

                _ ->
                    translationSet (RecommendedTreatmentSignLabel sign)

        RecommendedTreatmentSignLabel sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "Give quinine sulphate per os 10 mg/kg/dose, 3 times a day for 7 days"
                    , kinyarwanda = Just "Tanga umuti wa Kinini mu kanwa: 10mg ku kilo, gatatu ku munsi, mu minsi irindwi"
                    , kirundi = Just "Muhe umuti witwa sulfate de quinine per os 10 mg/kg/dose, azawufata 3 k'umunsi mu minsi 7"
                    }

                TreatmentCoartem ->
                    { english = "Give Coartem 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Just "Tanga AL (Kowaritemu) ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    , kirundi = Just "Muhe ibinini vya Coartem 4 vyo kumira 2 k'umunsi mu kiringo c'iminsi 3"
                    }

                TreatmentWrittenProtocols ->
                    { english = "GI complications: followed Written Protocols"
                    , kinyarwanda = Just "Afite ibibazo by'urwungano ngogozi: Kurikiza amabwiriza"
                    , kirundi = Just "Ingorane zijane na IG: Kurikiza Inyandiko Ntumberezo zivyerekeye"
                    }

                TreatmentReferToHospital ->
                    { english = "Severe Malaria: Stabilize and Refer to Hospital"
                    , kinyarwanda = Just "Afite Malaria y'Igikatu: Tanga umuti w'ibanze uhite umwoherza ku bitaro"
                    , kirundi = Just "Malariya ikaze: yicunge/yihagarike hama urungike umugwayi kwa muganga/ku bitaro"
                    }

                NoTreatmentForMalaria ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Just "Nta muti watanzwe"
                    }

                TreatmentPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Just "Penisiline (udupimo tungana na miliyoni 2,4)"
                    }

                TreatmentPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Just "Penisiline (udupimo tungana na miliyoni 2,4)"
                    }

                TreatmentErythromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    , kirundi = Just "Erythromycine (500 mg)"
                    }

                TreatmentAzithromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    , kirundi = Just "azithromycine(2g)"
                    }

                TreatmentCeftriaxon ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Ceftriaxone (1g)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoTreatmentForSyphilis ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Just "Nta muti watanzwe"
                    }

                TreatmentMethyldopa2 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa3 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa4 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "Carvedilol (6.25mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentHydrochlorothiazide ->
                    { english = "Hydrochlorothiazide (12.5mg)"
                    , kinyarwanda = Just "Idirokolotiyazide (12.5mg)"
                    , kirundi = Nothing
                    }

                TreatmentAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentNifedipine ->
                    { english = "Nifedipine (20mg)"
                    , kinyarwanda = Just "Nifedipine miligarama 20"
                    , kirundi = Nothing
                    }

                TreatmentCaptopril ->
                    { english = "Captopril (25mg)"
                    , kinyarwanda = Just "Kabutopulili miligaram 25"
                    , kirundi = Nothing
                    }

                TreatmentLisinopril ->
                    { english = "Lisinopril (5mg)"
                    , kinyarwanda = Just "Lizinopilili miligarama 5"
                    , kirundi = Nothing
                    }

                TreatmentAtenlol ->
                    { english = "Atenlol (12.5mg)"
                    , kinyarwanda = Just "Atenilolo miligarama 12.5"
                    , kirundi = Nothing
                    }

                NoTreatmentForHypertension ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Just "Nta muti watanzwe"
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "Aluminium Hydroxide (500mg)"
                    , kinyarwanda = Just "Idologiside d'Aluminiyumu miligarama 500"
                    , kirundi = Just "Hidroksidi ya Alimini (mg 500)"
                    }

                TreatmentHealthEducationForHeartburn ->
                    { english = "Not dispensing medicine. Follow health education protocols."
                    , kinyarwanda = Just "Witanga umuti. Kurikiza amabwiriza ajyanye n'inyigisho z'buzima."
                    , kirundi = Just "Ntihagire imiti itangwa. Kurikiza ivyanditswe ndongozi (protocoles) vy'igisho yerekeye amagara meza"
                    }

                TreatmentNitrofurantoin ->
                    { english = "Nitrofurantoin (100mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentAmoxicillin ->
                    { english = "Amoxicillin (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentClotrimaxazole200 ->
                    { english = "Clotrimaxazole (200mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentClotrimaxazole500 ->
                    { english = "Clotrimaxazole (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentCloxacillin ->
                    { english = "Cloxacillin (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMastitisAmoxicillin ->
                    { english = "Amoxicillin (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentPenecilinV ->
                    { english = "Penicillin V (250mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentParacetamol ->
                    { english = "Paracetamol (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentIbuprofen ->
                    { english = "Ibuprofen (400mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoTreatmentForMastitis ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Just "Nta muti watanzwe"
                    }

                TreatmentMetformin1m1e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide1m1e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMetformin2m1e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide2m1e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMetformin2m2e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide2m2e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentMetformin2m2eGlipenclamide1m1e ->
                    { english = "Metformin (500mg), Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide2m2eMetformin1m1e ->
                    { english = "Glipenclamide (5mg), Metformin (500mg)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoTreatmentForDiabetes ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Just "Nta muti watanzwe"
                    }

        RecordAcuteIllnessOutcome ->
            { english = "Record Acute Illness Outcome"
            , kinyarwanda = Just "Andika iherezo ry'indwara ifatiyeho"
            , kirundi = Just "Andika inyishu z'ingwara ikomeye"
            }

        RecordPregnancyOutcome ->
            { english = "Record Pregnancy Outcome"
            , kinyarwanda = Just "Andika iherezo ry'inda"
            , kirundi = Just "Andika inyishu zerekeye imbanyi"
            }

        RectalHemorrhoids ->
            { english = "Rectal Hemorrhoids"
            , kinyarwanda = Just "Kubyimba kw'imitsi y'ishyira(rectum)/Hemoroyide"
            , kirundi = Just "Ingwara y'ibivyimba m'umufutu/mu nyo"
            }

        RecurringHighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BloodPressure ->
                    { english = "Blood Pressure"
                    , kinyarwanda = Just "Umuvuduko w'amaraso"
                    , kirundi = Just "Umuvuduko w'amaraso"
                    }

        ReferredPatientToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Have you referred the patient to the health center"
                    , kinyarwanda = Just "Waba wohereje umurwayi ku kigo nderabuzima"
                    , kirundi = Just "Mbega wararungitse umugwayi kw'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Have you referred the patient to the hospital"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku bitaro"
                    , kirundi = Just "Mbega wararungitse umugwayi ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Have you referred the patient to the specialist"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku muganaga w'inzobere"
                    , kirundi = Just "Mbega wararungitse umugwayi k'umuhinga mu vy'ubuvuzi"
                    }

                FacilityARVProgram ->
                    { english = "Have you referred the patient to the ARV services"
                    , kinyarwanda = Just "Waba wohere umubyeyi muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Just "Mbega wararungitse umugwayi mu gisata kiraba ivy'ama ARV"
                    }

                FacilityNCDProgram ->
                    { english = "Have you referred the patient to NCD services"
                    , kinyarwanda = Just "Waba wohereje umubyeyi muri service y'indwara zitandura"
                    , kirundi = Just "Mbega wararungitse umugwayi mu gisata kiraba ivy'ingwara zitandukira"
                    }

                FacilityANCServices ->
                    { english = "Have you referred the patient to ANC services"
                    , kinyarwanda = Just "Wohereje umurwayi muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Just "Mbega wararungitse umugwayi mu gisata kiraba ivy'imbere yo kwibaruka"
                    }

                FacilityUltrasound ->
                    { english = "Have you referred the patient to ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Mbega wararungitse umugwayi mw'Iradiyo"
                    }

        ReferredToFacility facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Referred to health center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    , kirundi = Just "Yarungitswe kw'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Referred to hospital"
                    , kinyarwanda = Just "Yoherejwe ku bitaro"
                    , kirundi = Just "Yarungitswe ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Referred to mental health specialist"
                    , kinyarwanda = Just "Yoherejwe ku muganga w'inzobere w'ubuzima bwo mu mutwe"
                    , kirundi = Just "Yarungitswe k'umuhinga w'ingwara zo mu mutwe"
                    }

                FacilityARVProgram ->
                    { english = "Referred to ARV services"
                    , kinyarwanda = Just "Yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Just "Yarungitswe mu gisata kiraba ivya ma ARVs"
                    }

                FacilityNCDProgram ->
                    { english = "Referred to NCD services"
                    , kinyarwanda = Just "Yoherejwe muri service y'indwara zitandura"
                    , kirundi = Just "Yarungitswe mu gisata c'ingwara zitandukira"
                    }

                FacilityANCServices ->
                    { english = "Referred to ANC services"
                    , kinyarwanda = Just "Yoherejwe muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Just "Yarungitswe mu gisata kiraba ingwara zitandukira"
                    }

                FacilityUltrasound ->
                    { english = "Referred to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Yarungitswe mw'Iradiyo"
                    }

        ReferredToFacilityNot facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Not referred to health center"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku kigo nderabuzima"
                    , kirundi = Just "Yarungitswe kw'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Not referred to hospital"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku bitaro"
                    , kirundi = Just "Ntiyarungitswe ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Not referred to mental health specialist"
                    , kinyarwanda = Just "Ntabwo yoherejwe kwa muganga w'inzobere w'ubuzima bwo mu mutwe"
                    , kirundi = Just "Ntiyarungitswe k'umuhinga w'ingwara zo mu mutwe"
                    }

                FacilityARVProgram ->
                    { english = "Not referred to ARV services"
                    , kinyarwanda = Just "Ntago yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Just "Ntiyarungitswe mu gisata kiraba ivya ma ARVs"
                    }

                FacilityNCDProgram ->
                    { english = "Not referred to NCD services"
                    , kinyarwanda = Just "Ntabwo yoherejwe muri service y'indwara zitandura"
                    , kirundi = Just "Ntiyarungitswe mu gisata c'ingwara zitandukira"
                    }

                FacilityANCServices ->
                    { english = "Not referred to ANC services"
                    , kinyarwanda = Just "Ntabwo yoherejwe muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Just "Ntiyarungitswe mu gisata kiraba ingwara zitandukira"
                    }

                FacilityUltrasound ->
                    { english = "Not referred to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Nyiyarungitswe mw'Iradiyo"
                    }

        ReferredToFacilityPostpartum facility ->
            case facility of
                FacilityARVProgram ->
                    { english = "referred to ARV services for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise itanga imiti igabanya ubukana bwa Virusi itera SIDA kugirango akurikiranwe nyuma yo kubyara"
                    , kirundi = Just "Yarungitswe mu gisata kiraba ivya ma ARVs kugira bagire umwihwezo/ibipimo inyuma yo kwibaruka"
                    }

                FacilityNCDProgram ->
                    { english = "referred to NCD program for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise y'indwara zitandura kugirango akurikiranwe nyuma yo kubyara"
                    , kirundi = Just "Yarungitswe mu gisata c'ingwara zitandukira kugira habe umwihwezo wundi/ibipimo vyiyongerako inyuma yo kwibaruka"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReferToHospitalForFurtherEvaluation ->
            { english = "Refer patient to hospital for further evaluation"
            , kinyarwanda = Just "Ohereza umurwayi ku bitaro kugirango hakorwe isuzuma ryimbitse"
            , kirundi = Just "Rungika umugwayi/umuvyeyi ku bitaro kugira bagire isuzuma rindi"
            }

        ReferToHospitalForTesting ->
            { english = "Refer patient to hospital for testing"
            , kinyarwanda = Nothing
            , kirundi = Just "Rungika umugwayi/umuvyeyi ku bitaro kugira agirishe ibipimo"
            }

        ReferToProgramAction ->
            { english = "Refer patient to appropriate nutrition program"
            , kinyarwanda = Just "Ohereza umurwayi muri porogaramu y'imirire yabugenewe "
            , kirundi = Just "Rungika umugwayi/umuvyeyi mu gisata kiraba ivyo gufungura neza cabigenewe"
            }

        ReferToProgramQuestion ->
            { english = "Did you direct the patient to attend the next program session"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega warabwiye umugwayi ko yoja mu nyigisho ziteguwe ubutaha/inyigisho zizotangwa vuba"
            }

        RegisterContactHelper ->
            { english = "Not the contact you were looking for?"
            , kinyarwanda = Just "Ntabwo ari uwo washakishaga?"
            , kirundi = Just "Mbega siwa muntu wariko urarondera?"
            }

        RegisterParticipantHelper ->
            { english = "Not the participant you were looking for?"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega siwa muntu yaza mu nyigisho wariko urarondera?"
            }

        RegisterNewContact ->
            { english = "Register a new contact"
            , kinyarwanda = Just "Andika umuntu mushya wahuye n'umurwayi"
            , kirundi = Just "Andika inimero nshasha/umuntu wo kuvugana"
            }

        RegisterNewParticipant ->
            { english = "Register a new participant"
            , kinyarwanda = Just "Andika umurwayi mushya"
            , kirundi = Just "Andika uwitavye"
            }

        RegistratingHealthCenter ->
            { english = "Registrating Health Center"
            , kinyarwanda = Just "Izina ry'ikigo nderabuzima umugenerwabikorwa abarizwamo"
            , kirundi = Just "Ivuriro ryandika/kwiyandikisha kw'ivuriro"
            }

        RenalDisease ->
            { english = "Renal Disease"
            , kinyarwanda = Just "Indwara z'impyiko"
            , kirundi = Just "Ingwara yo mu mafyigo"
            }

        RemainingForDownloadLabel ->
            { english = "Remaining for Download"
            , kinyarwanda = Just "Ibisigaye gukurwa kuri seriveri"
            , kirundi = Just "Ibisigaye gupakurura"
            }

        RemainingForUploadLabel ->
            { english = "Remaining for Upload"
            , kinyarwanda = Just "Ibisigaye koherezwa kuri seriveri"
            , kirundi = Just "Ibisigaye kurungika"
            }

        RemindMe ->
            { english = "Remind Me"
            , kinyarwanda = Nothing
            , kirundi = Just "Nyibutsa"
            }

        RemindMePhrase ->
            { english = "Remind me of this message in:"
            , kinyarwanda = Nothing
            , kirundi = Just "Unyibutse ubu butumwa mu:"
            }

        ReportAge age ->
            { english = "Age: " ++ age
            , kinyarwanda = Just <| "Imyaka: " ++ age
            , kirundi = Just <| "Imyaka: " ++ age
            }

        ReportComponentAntenatal component ->
            case component of
                ComponentAntenatalRiskFactors ->
                    { english = "Risk Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Impamvu z'ingorane"
                    }

                ComponentAntenatalMedicalDiagnosis ->
                    { english = "Medical Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Isuzuma ryo kwa muganga"
                    }

                ComponentAntenatalObstetricalDiagnosis ->
                    { english = "Obstetrical Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Isuzuma ry'ivyara"
                    }

                ComponentAntenatalCHWActivity ->
                    { english = "CHW Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igikorwa c'Abaremeshakiyago"
                    }

                ComponentAntenatalPatientProgress ->
                    { english = "Patient Progress"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Iterambere ry'umugwayi"
                    }

                ComponentAntenatalLabsResults ->
                    { english = "Labs Results"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Inyishu z'ibipimo vy'ingwara"
                    }

                ComponentAntenatalProgressPhotos ->
                    { english = "Progress Photos"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Iterambere rya mafoto"
                    }

        ReportComponentNCD component ->
            case component of
                ComponentNCDRiskFactors ->
                    { english = "Risk Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Impamvu z'ingorane"
                    }

                ComponentNCDActiveDiagnosis ->
                    { english = "Active Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igisuzumo ciza/gusuzuma neza"
                    }

                ComponentNCDMedicalDiagnosis ->
                    { english = "Medical Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Isuzuma ryo kwa muganga"
                    }

                ComponentNCDPatientProgress ->
                    { english = "Patient Progress"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Iterambere ry'umugwayi"
                    }

                ComponentNCDLabsResults ->
                    { english = "Labs Results"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Inyishu z'ibipimo vy'ingwara"
                    }

        ReportComponentWellChild component ->
            case component of
                ComponentWellChildActiveDiagnoses ->
                    { english = "Acute Illness History"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Akahise k'ingwara ibabaza cane/ikaze"
                    }

                ComponentWellChildImmunizationHistory ->
                    translationSet ImmunizationHistory

                ComponentWellChildECD ->
                    { english = "Early Childhood Development"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Iterambere mu gukura ry'Umwana Mutoyi (DPE -IUM)"
                    }

                ComponentWellChildGrowth ->
                    { english = "Growth"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugukura"
                    }

                ComponentWellChildNextAppointment ->
                    { english = "Next Appointment"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umubonano uzokurikira"
                    }

        ReportDOB dob ->
            { english = "DOB: " ++ dob
            , kinyarwanda = Just <| "Itariki y'amavuko: " ++ dob
            , kirundi = Just <| "Itariki y'amavuko: " ++ dob
            }

        ReportResultsOfContactsSearch total ->
            case total of
                1 ->
                    { english = "There is 1 contract that matches your search."
                    , kinyarwanda = Just "Hagaragaye umuntu 1 uhuye nuwo washakaga."
                    , kirundi = Nothing
                    }

                _ ->
                    { english = "There are " ++ String.fromInt total ++ " contacts that match your search."
                    , kinyarwanda = Just <| "Hagaragaye abantu " ++ String.fromInt total ++ " bahuje nibyo ushakisha."
                    , kirundi = Just <| "Hano harimwo abo kuvugana " ++ String.fromInt total ++ " bahuye n'ukorondera kwawe"
                    }

        ReportResultsOfParticipantsSearch total ->
            case total of
                1 ->
                    { english = "There is 1 participant that matches your search."
                    , kinyarwanda = Just "Hari umujyenerwabikorwa 1 uhuye nuwo washatse"
                    , kirundi = Just "Hano hari uwitavye umwe ahuye n'ukurondera kwawe"
                    }

                _ ->
                    { english = "There are " ++ String.fromInt total ++ " participants that match your search."
                    , kinyarwanda = Just <| "Hari abagenerwabikorwa " ++ String.fromInt total ++ " bahuye nuwo ushaka mu ishakiro"
                    , kirundi = Just <| "Hano harimwo abitavye " ++ String.fromInt total ++ " bahuye n'ukurondera kwawe"
                    }

        ReportTab tab ->
            case tab of
                TabSPVReport ->
                    { english = "Standard Pediatric Report"
                    , kinyarwanda = Just "Raporo ku Isuzuma ry'Umwana"
                    , kirundi = Just "Icegeranyo gisanzwe c'igisata kiraba abana"
                    }

                TabNCDAScoreboard ->
                    translationSet ChildScorecard

        Reports ->
            { english = "Reports"
            , kinyarwanda = Just "Raporo"
            , kirundi = Just "Ivyegeranyo"
            }

        ReportCompleted { pending, completed } ->
            { english = String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Raporo irarangiye"
            , kirundi = Just <| String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Icegeranyo carangiye"
            }

        ResilienceCategory category ->
            case category of
                ResilienceCategoryIntroduction ->
                    { english = "Introduction"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceCategoryGrowth ->
                    { english = "Growth"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugukura"
                    }

                ResilienceCategoryStressManagement ->
                    { english = "Stress Management"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceCategoryMindfulness ->
                    { english = "Mindfulness"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceCategoryConnecting ->
                    { english = "Connecting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceCategorySelfCare ->
                    { english = "Self Care"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceCategoryEndOfPeriod ->
                    { english = "End Of Month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceMessage ->
            { english = "Resilience Message"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction1Title ->
            { english = "Welcome to the work based resilience messaging program."
            , kinyarwanda = Just "Murakaza neza muri gahunda yo kumenya kwiyitaho."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction1Paragraph1 name ->
            { english = "Hello " ++ name ++ ", Welcome to the work-based resilience program. As healthcare providers, we often face obstacles and challenges; for example: a lot of work, various responsibilities (including those at home), listening to patients with different problems, etc."
            , kinyarwanda = Just <| "Muraho neza " ++ name ++ ", Murakaza neza muri gahunda yo kumenya kwiyitaho bitewe n'akazi dukora. Nk'abakozi dutanga serivisi z'ubuvuzi, dukunze guhura n'imbogamizi; urugero: akazi kenshi, inshingano nyinshi harimo n'izo mu rugo, gutega amatwi abantu bafite ibibazo bitandukanye baba baje batugana, n'ibindi."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction1Paragraph2 ->
            { english = "You will be receiving short messages 3 times a week for six months. Those messages will help healthcare workers like us make progress in the quality of our work and learn how to cope with extreme fatigue and stress."
            , kinyarwanda = Just "Muzajya mwakira ubutumwa bugufi (messages) inshuro 3 mu cyumweru, mu gihe cy'amezi atandatu. Ubwo butumwa buzafasha abakozi batanga serivisi z'ubuvuzi nkatwe, gutera intambwe nziza mu kumenya uko dukora akazi kacu neza, no guhangana n'umunaniro ukabije."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction1Paragraph3 ->
            { english = "The aim of these messages is to help you as a healthcare worker feel better and more confident in your work and your daily life."
            , kinyarwanda = Just "Intego y'ubu butumwa ni ukugufasha nk'umukozi utanga serivisi z'ubuvuzi kumva umeze neza kandi wifitemo icyizere n'umutekano mu kazi ukora ndetse n'ubuzima bwawe bwa buri munsi."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Title ->
            { english = "More information about the program"
            , kinyarwanda = Just "Andi makuru ku byerekeye iyi gahunda"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Paragraph1 ->
            { english = "There are several things we face at work such as being given too many responsibilities and working overtime. However, we can at least learn how to deal with extreme fatigue and stress."
            , kinyarwanda = Just "Hari ibintu byinshi duhura nabyo mu kazi nko guhabwa inshingano nyinshi, gukora amasaha y'ikirenga,... Ariko byibuze dushobora kwiga uko twarwanya umunaniro ukabije."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Paragraph2 ->
            { english = "After this six-month journey, we hope that you will be able to:"
            , kinyarwanda = Just "Nyuma y'uru rugendo rw'amezi atandatu, twizera ko uzaba ushobora:"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Bullet1 ->
            { english = "Know what to do when you are under extreme fatigue or stress"
            , kinyarwanda = Just "Kumenya uko witwara mu gihe ufite umunaniro ukabije (stress)"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Bullet2 ->
            { english = "Recover after going through difficult times"
            , kinyarwanda = Just "Kongera kumererwa neza nyuma yo kunyura mu bihe bigoye"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Bullet3 ->
            { english = "Find time and learns ways to relax"
            , kinyarwanda = Just "Kubona akanya ko kuruhuka no kumenya uburyo bwo kuruhuka"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Bullet4 ->
            { english = "Know how to get along well with those we live with and work with"
            , kinyarwanda = Just "Kumenya kubanira neza abo tubana ndeste nabo dukorana"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction2Bullet5 ->
            { english = "Find happiness and enjoy life"
            , kinyarwanda = Just "Kunezerwa ndetse no kuryoherwa n'ubuzima"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction3Title ->
            { english = "Get a notebook ready!"
            , kinyarwanda = Just "Tegura ikaye yo kujya wandikamo"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction3Paragraph1 ->
            { english = "Prepare a special notebook that you will use for this program. Every message you will receive will help you think and act on a certain thing."
            , kinyarwanda = Just "Tegura ikaye yihariye uzifashisha muri iyi gahunda. Buri butumwa bugufi uzajya wakira buzajya bugufasha gutekereza no gukora ku kintu runaka."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction3Paragraph2 ->
            { english = "Read the messages in a timely manner and without distraction, think about them and act on them. Every prompt you will help you develop good habits."
            , kinyarwanda = Just "Soma ubutumwa bugufi mu gihe nyacyo/kitakubangamiye, butekerezeho hanyuma ubushyire mu bikorwa (ufate ingamba). Buri gikorwa cyose uzagerageza gukora kizagufasha kugira ingeso nziza."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction3Paragraph3 ->
            { english = "Keep notes in your notebook about your thoughts and actions."
            , kinyarwanda = Just "Reka twandike mu makayi yacu ibyo twatekereje n'ibyo twakoze."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Title ->
            { english = "Get the most out of the program"
            , kinyarwanda = Just "Ibizatuma iyi gahunda ikugirira akamaro"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Paragraph1 ->
            { english = "The more effort you put into this program, the more benefits you will get, you will learn how to take care of yourself, and how to be better at your job, as the days go by."
            , kinyarwanda = Just "Uko ushyira imbaraga muri iyi gahunda, uzungukiramo ibyiza byinshi, uzarushaho kumenya uko wakwiyitaho, n'uko wakora akazi kawe neza, uko iminsi izagenda ihita."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Paragraph2 ->
            { english = "In order for this program to benefit you, we encourage you to:"
            , kinyarwanda = Just "Kugira ngo iyi gahunda izakugirire akamaro, turagukangurira:"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Bullet1 ->
            { english = "Read all the messages you receive"
            , kinyarwanda = Just "Gusoma ubutumwa bwose wakiriye"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Bullet2 ->
            { english = "Try to follow the prompts"
            , kinyarwanda = Just "Kugerageza kubushyira mu bikorwa"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Bullet3 ->
            { english = "Assess what helped you and what did not"
            , kinyarwanda = Just "Gusesengura ibyagufashije n'ibitaragufashije"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction4Paragraph3 ->
            { english = "Share what you have learned with others who are in this program."
            , kinyarwanda = Just "Sangiza  ibyo wize abo muri kumwe muri iyi gahunda."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction5Title ->
            { english = "Pause for self-reflection"
            , kinyarwanda = Just "Gufata akanya ko kwisuzuma"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction5Paragraph1 ->
            { english = "Every month, you will have time for self reflection, see what you have done and how it is helping you. It will be time to look through your notebook and think about what worked and what did not."
            , kinyarwanda = Just "Buri kwezi, uzajya wisuzuma, urebe ibyo wakoze n'uko biri kugenda bigufasha. Azaba ari igihe cyo kureba mu ikaye yawe ugatekereza ku byagufashije n'ibitaragufashije."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction6Title ->
            { english = "A note on messages..."
            , kinyarwanda = Just "Gira icyo wandika ku butumwa bwagufashije"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction6Paragraph1 ->
            { english = "Some of these messages will be useful to you, and there are others which you might not find helpful. Keep writing in your notebook even though you do not fully understand the message. You might use it again depending on the situation or where you are."
            , kinyarwanda = Just "Ubu butumwa hari ubuzakugirira akamaro, hari n'ubundi uzumva ntacyo bugufashije. Komeza wandike mu ikayi yawe n'ubwo waba udasobanukiwe neza n'ubutumwa. Ushobora kongera kubwifashisha bitewe n'ibihe urimo cyangwa aho uri."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction7Title ->
            { english = "This a personal journey..."
            , kinyarwanda = Just "Uru ni urugendo rw'umuntu ku giti cye..."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction7Paragraph1 ->
            { english = "It is good to be in this six-month program because it will help you feel better, get stronger, and give you knowledge/skills that helps you deal with life  and professional problems. This is a personal journey. You are the one to choose when to allocate time for this program."
            , kinyarwanda = Just "Ni byiza kuba muri iyi gahunda y’amezi atandatu kuko izagufasha kurushaho kumererwa neza, ugakomera, no kuguha ubumenyi bugufasha guhangana n'ibibazo by'ubuzima muri rusange no mu kazi k’ubuvuzi. Uru ni urugendo rw'umuntu ku giti cye. Ni wowe uzihitiramo umwanya uzagenera iyi gahunda."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction7Paragraph2 ->
            { english = "Remember that the more effort you put into this program, the more successful you will be."
            , kinyarwanda = Just "Zirikana ko uko ushyira imbaraga muri iyi gahunda ari nako urushaho kugubwa neza."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction8Title ->
            { english = "We support you!"
            , kinyarwanda = Just "Urashyigikiwe!"
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction8Paragraph1 ->
            { english = "We know the effort you put into your work and how exhausting it is. That is why these short messages that you receive will help you take care of yourself."
            , kinyarwanda = Just "Tuzi imbaraga mushyira mu kazi kanyu ndetse n'uburyo kavunanye. Ni kubw' iyo mpamvu ubutumwa bugufi muzajya mwohererezwa buzajya bubafasha kwiyitaho."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction8Paragraph2 ->
            { english = "You will be sent messages that will help you cope with extreme fatigue and stress."
            , kinyarwanda = Just "Muzajya mwohererezwa ubutumwa buzabafasha guhangana n'imvune z'akazi kenshi."
            , kirundi = Nothing
            }

        ResilienceMessageIntroduction8Paragraph3 ->
            { english = "Remember: It is not the load that breaks you down. It’s the way you carry it."
            , kinyarwanda = Just "Intore ntiganya yishakira ibisubizo. Kwihangana bitera kunesha."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth1Title ->
            { english = "An Important reminder, make a list of things you are good at!"
            , kinyarwanda = Just "Kwibutsa! Kora urutonde rw'ibintu ukora neza!"
            , kirundi = Nothing
            }

        ResilienceMessageGrowth1Paragraph1 ->
            { english = "Think about what you are good at. Make a list of the things you do well, then ask yourself why they are important to you and why they matter."
            , kinyarwanda = Just "Tekereza kuby'uzi gukora neza. Kora urutonde rw'ibintu ukora neza, ubundi wibaze impamvu ari ingirakamaro kuri wowe."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth1Paragraph2 ->
            { english = "Remember: You don’t have to be great to start, but you do have to start to be great. So, let’s start!"
            , kinyarwanda = Just "Buhoro buhoro nirwo rugendo"
            , kirundi = Nothing
            }

        ResilienceMessageGrowth2Title ->
            { english = "Strive for progress not perfection"
            , kinyarwanda = Just "Akazi kanyu kagirira akamaro kanini ababagana"
            , kirundi = Nothing
            }

        ResilienceMessageGrowth2Paragraph1 ->
            { english = "It may feel like you are not doing well at your job, sometimes you work overtime. Think about how important your work is and how you help your clients. Before you go to bed at night, try to make a list of the good things you did today."
            , kinyarwanda = Just "Birashoboka ko wumva udakora neza akazi kawe, rimwe na rimwe ugakora n’amasaha y’ikirenga. Tekereza ukuntu akazi kawe ari ingezi n'uburyo ufasha abakugana. Mbere yo kuryama nijoro, gerageza gukora urutonde rw'ibintu byiza wakoze uyu umunsi."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth2Paragraph2 ->
            { english = "Your work is important and your clients appreciates the work you do."
            , kinyarwanda = Just "Akazi kawe ni ingenzi kandi abakugana bishimira ibikorwa ukora."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth2Paragraph3 ->
            { english = "Remember: Good things take time."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceMessageGrowth3Title ->
            { english = "Never underestimate yourself"
            , kinyarwanda = Just "Ufite imbaraga zo kwita ku buzima bwawe"
            , kirundi = Nothing
            }

        ResilienceMessageGrowth3Paragraph1 ->
            { english = "In our daily life, we encounter challenges that make us forget what is important to us. Try to find time to think about what matters; is it your family, people you care about, your work/profession, or is it your well-being. Those are your strengths. Think whether you live a life that serve your values."
            , kinyarwanda = Just "Mu buzima bwa buri munsi duhura n'ibitunaniza bituma twibagirwa icy'ingenzi kuri twe. Gerageza gufata umwanya utekereze kubigufitiye umumaro; ese n'umuryango wawe, n'abantu witaho, n'umwuga wawe, cyangwa n'ubuzima bwiza? Izo nizo mbaraga zawe. Tekereza niba ubaho ubuzima buguhesha agaciro."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth3Paragraph2 ->
            { english = "Remember: If it matters to you, you will find a way to do it."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageGrowth4Title ->
            { english = "Set your weekly objectives accordingly"
            , kinyarwanda = Just "Ihe intego za buri cyumweru"
            , kirundi = Nothing
            }

        ResilienceMessageGrowth4Paragraph1 ->
            { english = "Are there any changes you would like to make in your life? Set a goal of what you want to achieve each week. It does not have to be related to your work. Writing down your goal in a notebook will increase the chance of achieving it. Achieving a short goal will help you feel that you are the master of your life."
            , kinyarwanda = Just "Ese hari impinduka wifuza ku buzima bwawe? Buri cyumweru ihe intego y'icyo ushaka kugeraho. Singombwa kuba gifitanye isano n'akazi ukora. Andika iyo ntego mu ikayi ibi bizakongerera amahirwe yo kuyigeraho. Kugera ku ntego nto bizagufasha kumva ko uri umuyobozi w'ubuzima bwawe."
            , kirundi = Nothing
            }

        ResilienceMessageGrowth4Paragraph2 ->
            { english = "Remember: A goal without a plan is just a wish."
            , kinyarwanda = Just "Fata umwanya wo kongera kureba mu ikayi y’imihigo."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement1Title ->
            { english = "Hey! Do you know that you can help yourself"
            , kinyarwanda = Just "Ushobora kwifasha mu kurwanya umunaniro ukabje"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement1Paragraph1 ->
            { english = "Think of a time when you felt bad: angry, sad... Maybe you were tired, restless, had a headache, or felt short of breath. These are all signs of extreme fatigue or stress."
            , kinyarwanda = Just "Tekereza igihe wumvaga utameze neza: warakaye, ubabaye,... Birashoboka ko wari unaniwe,  utisanzuye, urwaye umutwe, cyangwa wumva udahumeka neza; ibyo byose ni ibimeyetso by'umunaniro ukabije (stress)."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement1Paragraph2 ->
            { english = "What did you do to feel better again? Did you know that you can help yourself? Follow the tips in the messages, you may find some that will help you."
            , kinyarwanda = Just "Ese ni iki wakoze kugira ngo wumve wongeye kumererwa neza? Wari uziko ushobora kwifasha burya!Kurikiza inama mu butumwa wohererezwa, ushobora gusangamo zimwe zagufasha."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement1Paragraph3 ->
            { english = "Remember: Sometimes you win, sometimes you learn."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Title ->
            { english = "Stress - Not always the bad thing!!"
            , kinyarwanda = Just "Waruziko hari igihe kugira umunaniro ukabije aba atari bibi!!"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Paragraph1 ->
            { english = "Research has shown that 86% of nurses experience extreme fatigue or stress which means that other workers can experience it too. This does not mean that being stressed or tired is always a bad thing. It can help you solve problems and achieve your goals."
            , kinyarwanda = Just "Ubushakashatsi bwagaragaje ko 86% by'abaforomo bahura n'umunaniro; bivuze ko n'abandi bakozi bishobora kubabaho. Ibi ntibisobanuye ko kuba wagira umunaniro ari bibi buri gihe. Bishobora kuba byagufasha gukemura ibibazo no kugera ku ntego zawe."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Paragraph2 ->
            { english = "When you are tired, you feel overwhelmed, and you do not get how to control it. During that time, you will feel scared, overloaded, unhappy, and weak."
            , kinyarwanda = Just "Iyo umunaniro ubaye mwishi wumva bikurenze, ukabura n'uburyo bwo kuwugenzura. Muri icyo gihe, uzumva ufite ubwoba, akazi kakurenze, utishimye,ufite n'intege nke."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Paragraph3 ->
            { english = "Here are some simple ways to help you:"
            , kinyarwanda = Just "Ubu ni bumwe mu buryo bworoshye bwagufasha:"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Paragraph4 ->
            { english = "Remember: No act of kindness is ever wasted."
            , kinyarwanda = Just "Giraneza wigendere, ineza uyisanga imbere."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Bullet1 ->
            { english = "Talk to a trusted friend, a family member, or a co-worker"
            , kinyarwanda = Just "Kuganira n’inshuti yawe wizeye, umwe mu bagize umuryango wawe, cyangwa mugenzi wawe mukorana"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Bullet2 ->
            { english = "Spend your time doing something you love."
            , kinyarwanda = Just "Gukoresha umwanya wawe ukora ikintu ukunda."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement2Bullet3 ->
            { english = "Be kind to others."
            , kinyarwanda = Just " Kugirira neza abandi."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement3Title ->
            { english = "Hello!!! You can turn your wounds into wisdom"
            , kinyarwanda = Just "Kumenya ibigutera umunaniro ukabije ni ingirakamaro mu kuwuhashya"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement3Paragraph1 ->
            { english = "Make a list of things that worries you. Now think about how those things make you feel. Are you feeling angry, frustrated, or stressed? Or it's all three! Do not hesitate to give names to your feelings because it helps you manage them better."
            , kinyarwanda = Just "Kora urutonde rw'ibintu bigutera impungenge. Noneho tekereza uburyo ibyo bintu bigutera kumva umerewe. Ese wumva urakaye, wacitse intege, cyangwa uhangayitse? Cyangwa ni byose uko ari bitatu! Ntutinde guha izina ibyiyumviro byawe kuko bigufasha kubicunga neza."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement3Paragraph2 ->
            { english = "Remember: One small positive thought in the morning can change your whole day."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement4Title ->
            { english = "Tune into your calm mood"
            , kinyarwanda = Just "Impinduka nto mu buzima bwa buri munsi zatuma wumva umerewe neza"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement4Paragraph1 ->
            { english = "Your work and family life can be challenging, and can make you feel depressed and incapable. If you make a small change in your life, you prepare yourself for difficult times."
            , kinyarwanda = Just "Akazi kawe n'ubuzima bw'umuryango wawe bishobora kuba ingorabahizi, bikagutera kumva uhangayitse kandi utanashoboye. Mu gihe ukoze impinduka nto mu buzima bwawe, ushobora kwitegura no guhangana n'ibihe bigoranye."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement4Paragraph2 ->
            { english = "These short messages give you advice and strategies for dealing with extreme fatigue and stress. So, choose what you can do and plan to do it in your life."
            , kinyarwanda = Just "Ubu butumwa buguha inama n'ingamba zo guhangana n'umunaniro ukabije. Rero hitamo ibyo wumva ushobora gukora hanyuma utegure kubikora mu buzima bwawe."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement4Paragraph3 ->
            { english = "Remember: Real change happens one step at a time."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement5Title name ->
            { english = "Hey " ++ name ++ " Relax, it's your time!!"
            , kinyarwanda = Just "Shakisha kandi unakore ibikorwa byagufasha kuruhuka"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement5Paragraph1 ->
            { english = "To relax is important for building ourselves up. Find something you like to do after work that will help you relax: for example reading books, listening to the radio, physical exercising, dancing,…"
            , kinyarwanda = Just "Kuruhuka ni ngombwa kugirango wiyubake. Shakisha ikintu ukunda gukora nyuma y’akazi cyagufasha kuruhuka neza: urugero gusoma ibitabo, kumva radio, gukora imyitozo ngororamubiri, kubyina,..."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement5Paragraph2 ->
            { english = "This will help you relax and do your work better the next day. Make it a habit to take care of yourself every day."
            , kinyarwanda = Just "Ibi bizagufasha kuruhuka no gukora akazi kawe neza umunsi ukurikira. Bigire akamenyero ko kwiyitaho buri munsi."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement5Paragraph3 ->
            { english = "Remember: Make time to rest and be thankful."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement6Title ->
            { english = "Why not making small changes? It might brighten up your week"
            , kinyarwanda = Just "Hari ibikorwa byinshi wakora byatuma umererwa neza"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement6Paragraph1 ->
            { english = "Making small changes in your daily life can help you cope with extreme fatigue and stress. Do a breathing exercise, talk to your friends, or drink water every once in a while. These small changes will help you feel better."
            , kinyarwanda = Just "Kugira impinduka nto mubuzima bwawe bwa buri munsi bishobora kugufasha kurwanya umunaniro ukabije. Gukora umwitozo wo guhumeka, kuganira n' inshuti zawe, cyangwa gusoma ku mazi buri kanya. Izi mpinduka nto zizagufasha kumererwa neza."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement6Paragraph2 ->
            { english = "Remember: If you see someone without a smile, give them one of yours."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Title ->
            { english = "You have strengths to face your challenges"
            , kinyarwanda = Just "Ibuka ko ufite imbaraga zo guhangana n'ibibazo uhura nabyo"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Paragraph1 ->
            { english = "Sometimes life can be challenging and it is easy to be discouraged. But you have strengths to face those challenges:"
            , kinyarwanda = Just "Rimwe na rimwe, ubuzima bushobora kuba ingorabahizi kandi biroroshye kuba wacika intege. Ariko ufite imbaraga zo guhangana nizo ngorane:"
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Paragraph2 ->
            { english = "Remember: When you help yourself, you help everyone around you."
            , kinyarwanda = Just "Buhoro buhoro nibwo buryo bw' umugenzi."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Bullet1 ->
            { english = "Find out what is causing your extreme fatigue and stress."
            , kinyarwanda = Just "Menya ikigutera umunaniro ukabije."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Bullet2 ->
            { english = "Change what you can change. Small changes will help you learn how to behave."
            , kinyarwanda = Just "Hindura ibyo ushobora guhindura. Impinduka nto zizagufasha kumenya uko witwara."
            , kirundi = Nothing
            }

        ResilienceMessageStressManagement7Bullet3 ->
            { english = "Accept what you cannot change. There are things that are beyond your control."
            , kinyarwanda = Just "Emera ibyo udashobora guhindura. Hari ibintu bimwe biba birenze ubushobozi bwawe."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness1Title ->
            { english = "It's time for a short break"
            , kinyarwanda = Just "Akanya k'ikiruhuko karageze"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness1Paragraph1 ->
            { english = "Did you have a lot of work today? Did you know that taking a break between work and doing breathing exercises can prevent you from getting tired quickly?"
            , kinyarwanda = Just "Uyu munsi wagize akazi kenshi? Ese wari uzi ko gufata akanya hagati mu kazi ugakora umwitozo wo guhumeka bikurinda kuruha vuba?"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness1Paragraph2 ->
            { english = "You can take regular breaks of a few minutes. Stretch your arms, take a moment to do a breathing exercise, and smile. Doing this on a regular basis can help you stay productive."
            , kinyarwanda = Just "Ushobora gufata akaruhuko k'iminota mike gahoraho. Ukananura amaboko yawe, ugafata akanya ko gukora umwitozo wo guhumeka igihe kirekire, ukaba wanaseka. Gukora ibi mu gihe gihoraho byagufasha gukomeza gukora neza."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness2Title ->
            { english = "Take a time and to what makes you happy"
            , kinyarwanda = Just "Ubu wafata umwanya ugakora ikikunezeza"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness2Paragraph1 ->
            { english = "Try to ignore all frustrations of the workplace. If you get home, relax and do what makes you happier; Such as praying, singing, listening to the radio, visiting farm activities, watering flowers, cooking, playing with children,…"
            , kinyarwanda = Just "Gerageza kwirengagiza ibikunaniza byo mu kazi aho ukorera. Niba ugeze mu rugo ruhuka ukore ibigushimisha bikuruhura kurusha ibindi. Nko gusenga, kuririmba, kumva radiyo, gusura ibikorwa mu murima, kuhira indabo, guteka, gukina n'abana,..."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness2Paragraph2 ->
            { english = "Research has shown that creative activities can also help you improve your work."
            , kinyarwanda = Just "Ubushakashatsi bwagaragaje ko ibikorwa wihangiye (creative activities) bishobora no kugufasha kunoza akazi kawe."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness2Paragraph3 ->
            { english = "Remember: A little progress each day can add up to big results."
            , kinyarwanda = Just "Ibyagushimisha birasendereye wibishakira kure."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness3Title ->
            { english = "Pay attention on purpose - the Goal is mindfulness"
            , kinyarwanda = Just "Ushobora kwifasha gutuza"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness3Paragraph1 ->
            { english = "Did you know that just paying attention to one thing you are doing can make you feel calmer? This is called mindfulness."
            , kinyarwanda = Just "Wari uzi ko gutekereza ku kintu kimwe gusa uri gukora bishobora gutuma wumva utuje? Nibyo bita mindfulness."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness3Paragraph2 ->
            { english = "Practice removing stressful thoughts from your mind, that will help you feel calmer. You can try this mindfulness activity while doing various daily activities such as eating, washing your hands, brushing your teeth, etc."
            , kinyarwanda = Just "Itoze gukura ibitekerezo bikunaniza mu ntekerezo zawe, maze bigufashe kumva utuje. Ushobora kugerageza uyu mwitozo wa mindfulness mu gihe uri gukora ibintu bitandukanye bya buri munsi nko kurya, gukaraba intoki, koza amenyo, n'ibindi."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness3Paragraph3 ->
            { english = "This mindfulness practice can be difficult for you when you are not used to it. Try doing it starting from today."
            , kinyarwanda = Just "Uyu mwitozo wa mindfulness, ushobora kugukomerera mu gihe utarawumenyera. Gerageza kuwukora uhereye uyu munsi."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness3Paragraph4 ->
            { english = "Remember: Mindfulness can be hard to do until we practice it. Give it a try today."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness4Title ->
            { english = "Make your body and mind in the same place and time"
            , kinyarwanda = Just "Gerageza umwitozo wa mindfluness"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness4Paragraph1 ->
            { english = "Try a mindfulness activity: washing your hands. Think of water splashing on your skin, feel the softness of soap, and pay attention as you wash it."
            , kinyarwanda = Just "Gerageza umwitozo wa mindfulness ukaraba intoki. Tekereza ku mazi ari kugwa ku kuruhu rwawe, wumve ubworohere bw'isabune mu gihe ukaraba, hanyuma urebe uruhu rwawe mu gihe urwoza."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness4Paragraph2 ->
            { english = "Look at how clean you skin is while you dry it. There is nothing more important in those few seconds than washing your hands."
            , kinyarwanda = Just "Reba uburyo uruhu rwawe rufite isuku mu gihe uri kurwumutsa. Muri ayo masegonda make ntakindi kintu cy' ingenzi uretse gukara intoki."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness4Paragraph3 ->
            { english = "Remember: Calmness is your weapon against challenges."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Title ->
            { english = "Breath easy, stress less!!"
            , kinyarwanda = Just "Guhumeka ni ingirakamaro ku buzima bwawe. Humeka"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Paragraph1 ->
            { english = "We breathe all the time. But when we have problems, it makes it hard to breath. Deep breathing exercises can help you relax."
            , kinyarwanda = Just "Duhumeka igihe cyose. Ariko iyo turi mubibazo duhumeka bitugoye. Umwitozo wo guhumeka cyane ushobora kugufasha kuruhuka."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Bullet1 ->
            { english = "Try to inhale through your nose for 4 seconds, this will make your stomach rise."
            , kinyarwanda = Just "Gerageza winjize umwuka ukoresheje amazuru yawe mu masegonda 4, urumva igifu cyawe kizamuka."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Bullet2 ->
            { english = "Exhale through your mouth for 4 seconds."
            , kinyarwanda = Just "Sohora umwuka ukoresheje umunwa wawe amasegonda 4."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Paragraph2 ->
            { english = "Just focus on how the breathing exercise is making your whole body feel."
            , kinyarwanda = Just "Tekereza gusa ukuntu umwitozo wo guhumeka uri gutuma wiyumva mu mubiri wose."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness5Paragraph3 ->
            { english = "Remember: Breathe deep to release negative energy."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Title ->
            { english = "Reminder: Breath in, breath out"
            , kinyarwanda = Just "Aka ni akanya keza ko kuba wakora umwitozo wo guhumeka"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Paragraph1 ->
            { english = "Are you feeling scared? Try this \"breathing exercise\"."
            , kinyarwanda = Just "Urumva ufite ubwoba? Gerageza \"umwitozo wo guhumeka\"."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Paragraph2 ->
            { english = "Say to yourself: when you breathe in \"2, 3, 4\", when you hold your breath \"2, 3, 4\", when you breathe out \"2, 3, 4\". Once you do this you will feel calm."
            , kinyarwanda = Just "Ibwire ubwawe: igihe winjije umwuka 2 3 4, igihe uhagaritse umwuka \"2, 3, 4\", igihe usohoye umwuka \"2, 3, 4\". Numara kubikora uzumva utuje."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Paragraph3 ->
            { english = "Practice doing it now so you know what to do the next time you feel scared."
            , kinyarwanda = Just "Itoze kubikora ubu kugira ngo umenye icyo gukora ubutaha wumvise ufite ubwoba."
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Paragraph4 ->
            { english = "Remember: Hope is the only thing stronger than fear."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Bullet1 ->
            { english = "Breath in and count to 4,"
            , kinyarwanda = Just "Injiza umwuka hanyuma ubare kugeza kuri 4,"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Bullet2 ->
            { english = "Hold your breath for a count of 4,"
            , kinyarwanda = Just "Hagarika umwuka ubare kugeza kuri 4,"
            , kirundi = Nothing
            }

        ResilienceMessageMindfulness6Bullet3 ->
            { english = "Then breath out for a count of 4."
            , kinyarwanda = Just "Hanyuma sohora umwuka ubara kugeza kuri 4."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting1Title ->
            { english = "Surround yourself with positive people."
            , kinyarwanda = Just "Girana ibihe byiza n'inshuti n'umuryango wawe."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting1Paragraph1 ->
            { english = "Support from people around you is important. It will help you know how to behave in your daily life and be healthy."
            , kinyarwanda = Just "Ubufasha buturutse ku bantu bakuba hafi aba ari ingenzi. Bwagufasha kumenya uko witwara mu buzima bwawe bwa buri munsi no kugira ubuzima bwiza."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting1Paragraph2 ->
            { english = "Try to get along well with your friends and family. Find time to connect with them and hang out with them after work."
            , kinyarwanda = Just "Gerageza kubana neza n'inshuti n'umuryango wawe. Shakisha umwanya wo gusabana/ kwishimana nabo nyuma y'akazi."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting1Paragraph3 ->
            { english = "Remember: Every day may not be good, but there will be something good in every day."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageConnecting2Title ->
            { english = "Happiness is being around people who love you and support you"
            , kinyarwanda = Just "Ni iby'umumaro kugirana ikiganiro kirambuye n'inshuti cyangwa abagize umuryango wawe."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting2Paragraph1 ->
            { english = "It is possible that it is been a while since you talked to someone important to you. If there is one, think of a special moment you shared together."
            , kinyarwanda = Just "Birashoboka ko haba hashize igihe kinini hari umuntu w'ingenzi mudaheruka kuvugana. Niba ahari, tekereza ibihe bidasanzwe mwagiranye."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting2Paragraph2 ->
            { english = "Calling, sending a short message, or visiting will let them know you have been thinking about them. It will make you feel better too."
            , kinyarwanda = Just "Kumuhamagara, kumwoherereza ubutumwa bugufi, cyangwa kumusura bizatuma amenya ko umaze igihe umuzirikana. Nawe bizatuma wumva umerewe neza."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting2Paragraph3 ->
            { english = "Remember: Life always offers you a second chance. It is called tomorrow."
            , kinyarwanda = Just "Ifuni ibagara ubucuti ni akarenge."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting3Title ->
            { english = "Maybe you should talk to someone \"A Therapist\""
            , kinyarwanda = Just "Tekerereza ibibazo byawe abagufasha kumererwa neza"
            , kirundi = Nothing
            }

        ResilienceMessageConnecting3Paragraph1 ->
            { english = "Everyone experiences good and bad times. If you are not feeling well, feeling unhappy or finding difficult to overcome and cope with problems, you may need someone to help you."
            , kinyarwanda = Just "Buri muntu ahura n'ibihe ibyiza n'ibibi. Niba utameze neza nkuko bisanzwe, utishimye, ubona hari ibikugoye guhangana nabyo, ushobora kuba ukeneye umuntu wagufasha."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting3Paragraph2 ->
            { english = "Try to find someone who can help you early, so that you feel good. Talk to a professional counsellor, your doctor or someone you trust. It is normal to feel uncomfortable in life, there is no problem in asking for help."
            , kinyarwanda = Just "Gerageza gushaka umuntu wagufasha hakiri kare, kugira ngo wumve umerewe neza. Ganiriza umujyanama wabigize umwuga, umuganga wawe cyangwa undi muntu wizeye. Mu buzima birasanzwe kumva utameze neza, ntacyo bitwaye gusaba ubufasha."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting3Paragraph3 ->
            { english = "Remember: It’s OK to not be OK. And it’s OK to ask for help."
            , kinyarwanda = Just "Umutwe umwe ntiwigira inama, wifasha gusara."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting4Title ->
            { english = "Support from your friends is valuable"
            , kinyarwanda = Just "Wibuke kugirana ibiganiro by'ubucuti n'abo ukorana nabo"
            , kirundi = Nothing
            }

        ResilienceMessageConnecting4Paragraph1 ->
            { english = "Did you have a lot of work today? Find time to connect with people you work with because their support is valuable."
            , kinyarwanda = Just "Uyu munsi wagize akazi kenshi? Shakisha umwanya wo gusabana nabo mukorana kuko ubufasha bwabo ari ingirakamaro."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting4Paragraph2 ->
            { english = "The regular work meetings keep everyone informed. But to meet with others regularly can be helpful too. If you work remotely (in the field or at the health post) talk to your co-worker on the phone when you can."
            , kinyarwanda = Just "Inama z'akazi zisanzwe zituma buri wese agezwaho amakuru. Ariko guhura bisanzwe n'abandi bishobora kugufasha nabyo. Niba ukorera kure (kuri terrain na health post (Ikigo ntanga buzima)) ganira n'umukozi mukorana kuri terefone mu gihe ubishoboye."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting4Paragraph3 ->
            { english = "Remember: Helping others is a way to help yourself."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageConnecting5Title ->
            { english = "The greatest healing therapy is friendship and love-but you can talk to a counselor"
            , kinyarwanda = Just "Menya igihe cyo gusaba ubufasha"
            , kirundi = Nothing
            }

        ResilienceMessageConnecting5Paragraph1 ->
            { english = "In everyday work everyone needs help, in case you are not feeling well or you feel despair. Who do you think can help you? Perhaps it is your friend or your leader (boss). Sometimes, you may need support from a professional counselor."
            , kinyarwanda = Just "Mukazi ka buri munsi umuntu wese akenera ubufasha, mu gihe atameze neza cyangwa yihebye. Urumva ari inde wizeye wagufasha? Ahari yaba ari inshuti yawe cyangwa umuyobozi wawe. Rimwe na rimwe, ushobora gukenera ubufasha buturutse k'umujyanama wabigize umwuga. Gerageza gutekereza kuwa gufasha kuruhuka."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting5Paragraph2 ->
            { english = "Try to think of those who can help you relax."
            , kinyarwanda = Just "Umutwe umwe wifasha gusara ntiwifasha gutekereza."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting6Title ->
            { english = "Sharing your feeling to a counselor is not a sign of weakness "
            , kinyarwanda = Just "Ni iby'umumaro kuganiriza umujyanama wabigize umwuga"
            , kirundi = Nothing
            }

        ResilienceMessageConnecting6Paragraph1 ->
            { english = "Healthcare workers can be confronted with a demanding task, especially in times of war and pandemics. If you feel tired, overwhelmed with problems all the time, reach out to a professional counselor. They are trained to help and keep it confidential."
            , kinyarwanda = Just "Abakozi batanga serivisi z'ubuvuzi bashobora kugira akazi kataboroheye cyane cyane mu bihe by'intambara n'ibyorezo. Niba wumva unaniwe, ukumva uremerewe n'ibibazo buri gihe, gana umujyanama wabigize umwuga. Baba baratojwe gutanga ubufasha no kubika ibanga."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting6Paragraph2 ->
            { english = "Research has shown that sharing your feelings with others can help you build ability to cope with extreme fatigue and stress. Everyone needs these things."
            , kinyarwanda = Just "Ubushakashatsi bwagaragaje ko gusangiza abandi uko wiyumva bigufasha kwiyubakamo ubushobozi bwo guhangana n'umunaniro ukabije. Ibi bintu buri wese arabikeneye."
            , kirundi = Nothing
            }

        ResilienceMessageConnecting6Paragraph3 ->
            { english = "Remember: Everyone falls down. Getting up is how you learn to walk."
            , kinyarwanda = Just "Ushaka gukira indwara arayirata."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Title ->
            { english = "Take care of yourself"
            , kinyarwanda = Just "Bimwe mu byagufasha kwiyitaho"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Paragraph1 ->
            { english = "Did you know that taking care of yourself can help reduce extreme fatigue and stress and make you happy? It allows you to deliver better healthcare services to your clients."
            , kinyarwanda = Just "Ese wari uzi ko kwiyitaho byagufasha kugabanya umunaniro ukabije bigatuma wishima? Bituma urushaho gutanga serivisi nziza z'ubuvuzi uha abakugana."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Paragraph2 ->
            { english = "These are some ways to help you take care of yourself:"
            , kinyarwanda = Just "Ubu ni bumwe mu buryo bwagufasha kwiyitaho:"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Paragraph3 ->
            { english = "It is good when an individual tries to do this every day."
            , kinyarwanda = Just "Nibyiza, iyo umuntu agerageje kubikora buri munsi."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Paragraph4 ->
            { english = "Remember: The challenge is not to be perfect - it is to be whole."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Bullet1 ->
            { english = "Sleep well/get enough sleep"
            , kinyarwanda = Just "Gusinzira neza/bihagije"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Bullet2 ->
            { english = "Eat a balanced diet"
            , kinyarwanda = Just "Kurya indyo yuzuye"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare1Bullet3 ->
            { english = "Take time to rest"
            , kinyarwanda = Just "Gufata umwanya wo kuruhuka"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare2Title ->
            { english = "Quiet your mind and relax your body. Do it!"
            , kinyarwanda = Just "Waruziko gusinzira bigira uruhare mu kumva wongeye kumererwa neza!"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare2Paragraph1 ->
            { english = "Having enough sleep is important, but when you work overtime or shift, it can be difficult for you to sleep well. Create your own relaxation routine before going to bed such as praying, a few minutes of breathing exercices, thinking about people who bring you joy,..."
            , kinyarwanda = Just "Gusinzira neza kandi bihagije ni ngombwa. Ariko mu gihe ukora amasaha y'ikirenga cyangwa ukora usimburwa (shift) bishobora kukugora gusinzira neza. Shyiraho gahunda yawe bwite yo kwisanzura mbere yo kuryama nko kuvuga isengesho, iminota mike yo gukora umwitozo wo guhumeka, gutekereza ku bantu bakuzanira umunezero,..."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare2Paragraph2 ->
            { english = "Try to do this every day so that your body get used to your bed time."
            , kinyarwanda = Just "Gerageza kubikora buri munsi kugira ngo umubiri wawe umenye igihe cyo gusinzira."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare2Paragraph3 ->
            { english = "Remember: A well-spent day brings happy sleep."
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare3Title ->
            { english = "Eat well, live well, and be well"
            , kinyarwanda = Just "Kurya neza ni bimwe mubituma umererwa neza"
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare3Paragraph1 ->
            { english = "Good nutrition is important for you and people you care about. What you eat and drink can effect your feelings, your ability to think, and how you cope with extreme fatigue and stress."
            , kinyarwanda = Just "Imirire myiza ni ingirakamaro kuri wowe no ku bantu witaho. Ibyo urya n'ibyo unywa bishobora kugira ingaruka ku byiyumviro byawe, kubushobozi bwawe bwo gutekereza, no kuburyo urwanya umunaniro ukabije."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare3Paragraph2 ->
            { english = "If you are tired and find it hard to pay attention to what you are doing these days, it would be better if you take a meal before leaving or take snacks to work with you."
            , kinyarwanda = Just "Niba unaniwe ukabona bikugoye kwita kubyo ukora muri iyi minsi. Byaba byiza ufashe amafunguro mbere yo kuva mu rugo cyangwa ukitwaza amafunguro mato ugiye mu kazi."
            , kirundi = Nothing
            }

        ResilienceMessageSelfCare3Paragraph3 ->
            { english = "Remember: Eat healthy, live healthy."
            , kinyarwanda = Just "Gahunda iyo zibaye nyinshi uhera kuyo kurya."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthTitle ->
            { english = "Congratulations! You have completed your first month!"
            , kinyarwanda = Just "Ishimire intsinzi yawe! Usoje ukwezi kwa mbere!"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthParagraph1 ->
            { english = "You have completed the first month of this journey. Now you have understood what stress is, what causes it, and know how you can control your feelings."
            , kinyarwanda = Just "Urangije ukwezi kwa mbere muri uru rugendo. Ubu noneho wamaze gusobanukirwa umunaniro ukabije icyo ari cyo, ikiwutera, unamenya uko ushobora kugenzura uko wiyumva."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthParagraph2 ->
            { english = "Now you have knowledge about:"
            , kinyarwanda = Just "Hari icyo wamenye kuri ibi bikurikira:"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthParagraph3 ->
            { english = "Think about how you felt when you tried these activities. Don't forget to write in your notebook."
            , kinyarwanda = Just "Tekereza uko wumvaga umeze igihe wageragezaga ibi bikorwa. Ntiwibagirwe kwandika mu ikaye yawe."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthBullet1 ->
            { english = "How to pay attention to one thing you are doing (Mindfulness)"
            , kinyarwanda = Just "Uburyo bwo gutekereza ku kintu kimwe uri gukora (Mindfulness)"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthBullet2 ->
            { english = "Breathing exercises"
            , kinyarwanda = Just "Umwitozo wo guhumeka (Breathing excercise)"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthBullet3 ->
            { english = "Self-care"
            , kinyarwanda = Just "Kwiyitaho"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfFirstMonthBullet4 ->
            { english = "How friends and family can help you feel better."
            , kinyarwanda = Just "Uburyo inshuti n'umuryango bashobora kugufasha kumererwa neza."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthTitle ->
            { english = "Congratulations! You have completed your second month."
            , kinyarwanda = Just "Ishimire intsinzi yawe! Usoje ukwezi kwa kabiri."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthParagraph1 ->
            { english = "You have completed the second month of this program. Now, we hope you are feeling better."
            , kinyarwanda = Just "Urangije ukwezi kwa kabiri muri iyi gahunda. Ubu turizera ko uri kurushaho kugenda umererwa neza."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthParagraph2 ->
            { english = "This month we learned the following things:"
            , kinyarwanda = Just "Muri uku kwezi twize uburyo bukurikira:"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthParagraph3 ->
            { english = "Have you ever tried any of these? Write in your notebook what helped you and what did not. Try to do it through the next month."
            , kinyarwanda = Just "Ese wigeze ugerageza kimwe muri ibi? Andika mu ikayi yawe ibyagufashije n'ibitaragufashije. Komeza ubigerageze kugeza ukwezi gutaha."
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthBullet1 ->
            { english = "Sleep well"
            , kinyarwanda = Just "Gusinzira neza"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthBullet2 ->
            { english = "Drink water"
            , kinyarwanda = Just "Kunywa amazi"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthBullet3 ->
            { english = "Take a break in the middle of work"
            , kinyarwanda = Just "Gufata akaruhuko hagati mu kazi"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthBullet4 ->
            { english = "Try a breathing exercise"
            , kinyarwanda = Just "Kugerageza umwitozo wo guhumeka"
            , kirundi = Nothing
            }

        ResilienceMessageEndOfSecondMonthBullet5 ->
            { english = "Get help when you need it"
            , kinyarwanda = Just "Gushaka ubufasha mu gihe ubukeneye"
            , kirundi = Nothing
            }

        ResilienceMonthlySurveyQuestion question ->
            case question of
                ResilienceSurveyQuestion1 ->
                    { english = "I look for creative ways to alter difficult situations"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestion2 ->
                    { english = "Regardless of what happens to me, I believe I can control my reaction to it"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestion3 ->
                    { english = "I believe I can grow in positive ways by dealing with difficult situations"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestion4 ->
                    { english = "I actively look for ways to replace the losses I encounter in life"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                _ ->
                    -- Not in use.
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceKickOffBirthDateQuestion ->
            { english = "What is your birth date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceKickOffEducationLevelQuestion ->
            { english = "What is your highest level of schooling"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceKickOffGenderQuestion ->
            { english = "What is your gender"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceKickOffMaritalStatusQuestion ->
            { english = "What is your marital status"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceKickOffRoleQuestion ->
            { english = "What is your role"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceKickOffUbudeheQuestion ->
            { english = "What is your Ubudehe category"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceNotificationHeader name ->
            { english = "Hello, " ++ name ++ "!"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceNotificationNumberOfUnread number ->
            if number == 1 then
                { english = "You have " ++ String.fromInt number ++ " unread message."
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "You have " ++ String.fromInt number ++ " unread messages."
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        ResilienceNotificationReadNowQuestion ->
            { english = "Would you like to read your messages now?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResilienceReminderHeader name reminderType ->
            case reminderType of
                ResilienceReminderDrinkWatter ->
                    { english = "Hello " ++ name ++ ", You should drink water!"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceReminderTakeBreak ->
                    { english = "You deserve it! Break time."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceReminderParagraph1 reminderType ->
            case reminderType of
                ResilienceReminderDrinkWatter ->
                    { english = "There are things that can help you manage extreme fatigue and stress. Feeling tired? Drink water! Drinking water helps every part of your body function properly. For example: your brain works better and you are more energized."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceReminderTakeBreak ->
                    { english = "A short break or time between work can boost your energy and help you do your job better."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceReminderParagraph2 reminderType ->
            case reminderType of
                ResilienceReminderDrinkWatter ->
                    { english = "Then, set a reminder in your phone and it will remind you to drink a glass of water every few hours."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceReminderTakeBreak ->
                    { english = "Try to take a break between tasks/short breaks. Do a breathing exercise, stretch your shoulders and arms. You will feel calm and ready to do well whatever comes next."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceReminderFooter reminderType ->
            case reminderType of
                ResilienceReminderDrinkWatter ->
                    { english = "Remember: Don’t count the days, make the days count."
                    , kinyarwanda = Just ""
                    , kirundi = Nothing
                    }

                ResilienceReminderTakeBreak ->
                    { english = "Remember: The stiller you are, the calmer life is. Take a moment to be still."
                    , kinyarwanda = Just ""
                    , kirundi = Nothing
                    }

        ResilienceRole role ->
            case role of
                ResilienceRoleCHW ->
                    { english = "CHW"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceRoleNurse ->
                    { english = "Health Care Worker (HC)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceRoleLineManager ->
                    { english = "Line Manager"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceRoleSupervisor ->
                    { english = "Supervisor"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceRoleDirector ->
                    { english = "Director General"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResilienceSurveyQuestionOption option ->
            case option of
                ResilienceSurveyQuestionOption0 ->
                    { english = "Does not describe me at all"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestionOption1 ->
                    { english = "Does not describe me"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestionOption2 ->
                    { english = "Neutral"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestionOption3 ->
                    { english = "Describes me"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ResilienceSurveyQuestionOption4 ->
                    { english = "Describes me very well"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ResolveMonth short month ->
            translateMonth month short

        ResolveMonthYY month year short ->
            translateMonthYY month year short

        RespiratoryDistress ->
            { english = "Respiratory Distress"
            , kinyarwanda = Just "Ahumeka bimugoye"
            , kirundi = Just "Ingorane yo guhema"
            }

        RespiratoryRate ->
            { english = "Respiratory Rate"
            , kinyarwanda = Just "Inshuro ahumeka"
            , kirundi = Just "Igipimo co guhema"
            }

        ResponsePeriod period ->
            case period of
                LessThan30Min ->
                    { english = "Less than 30 min"
                    , kinyarwanda = Just "Munsi y'iminota mirongo itatu"
                    , kirundi = Just "Munsi y'iminota 30"
                    }

                Between30min1Hour ->
                    { english = "30 min - 1 hour"
                    , kinyarwanda = Just "Hagati y’iminota mirongo itatu n’isaha"
                    , kirundi = Just "Iminota mirongo itatu (30) - Isaha imwe"
                    }

                Between1Hour2Hour ->
                    { english = "1 hour - 2 hours"
                    , kinyarwanda = Just "Hagati y'isaha n'amasaha abiri"
                    , kirundi = Just "Isaha imwe (1) - amasaha abiri (2)"
                    }

                Between2Hour1Day ->
                    { english = "2 hours - 1 day"
                    , kinyarwanda = Just "Hagati y'amasaha abiri n'umunsi"
                    , kirundi = Just "Amasaha abiri (2) - Umunsi umwe (1)"
                    }

                ResponsePeriodNotApplicable ->
                    translationSet NotApplicable

        Result ->
            { english = "Result"
            , kinyarwanda = Just "Igisubizo"
            , kirundi = Just "Inyishu"
            }

        ResultOfContacting114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "114 recommended to send patient to the nearest health center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku kigo nderabuzima kinyegereye"
                    , kirundi = Just "Ku 114 bemeje kurungika umugwayi mu Kigo c'amagara y'abantu kiri/ca hafi cane"
                    }

                SendToRRTCenter ->
                    { english = "114 recommended to send patient to Rapid Response Team center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    , kirundi = Just "Kuri 114 bemeje kurungika umugwayi mu kigo c'Umugwi w'Inyishu Yihuta"
                    }

                SendToHospital ->
                    { english = "114 recommended to send patient to the nearest hospital"
                    , kinyarwanda = Just "Ku 114 bangiriye inama yo kohereza umurwayi ku bitaro binyegereye"
                    , kirundi = Just "Ku 114 bemeje kurungika umugwayi ku bitaro biri/vya hafi cane"
                    }

                OtherRecommendation114 ->
                    { english = "114 did not recommended to send patient to site"
                    , kinyarwanda = Just "Ku 114 bansabye kutohereza umurwayi"
                    , kirundi = Just "Ku 114 ntibemeje kurungika umugwayi ku kibanza c'iyakiriro"
                    }

                NoneNoAnswer ->
                    { english = "Not able to talk to 114 - no answer"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- nta gisubizo"
                    , kirundi = Just "Ntivyakunze kuvugira kuri 114 - Nta nyishu"
                    }

                NoneBusySignal ->
                    { english = "Not able to talk to 114 - busy signal"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- umurongo bawuvugiragaho"
                    , kirundi = Nothing
                    }

                NoneOtherRecommendation114 ->
                    { english = "Not able to talk to 114 - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- Izindi mpamvu"
                    , kirundi = Just "Ntivyakunze kuvugira kuri 114 - Izindi mpavu"
                    }

        ResultOfContactingRecommendedSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Site recommendation: Team will come to village"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Itsinda rizaza mu mudugudu"
                    , kirundi = Just "Icifuzo c'urubuga: umurwi uzoza mu kw'ikgwati/ku musozi"
                    }

                SendToSiteWithForm ->
                    { english = "Site recommendation: Send patient to site with referral form"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Twohereze umurwayi yitwaje impapuro zimwohereza"
                    , kirundi = Just "Icifuzo c'urubuga: rungika umurwayi ku kigo afise urupapuro/ifishi y'irungikwa"
                    }

                OtherRecommendationSite ->
                    { english = "Site recommendation: Other"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Ibindi"
                    , kirundi = Just "Icifuzo c'urubuga: ibindi"
                    }

                NoneSentWithForm ->
                    { english = "Not able to talk to site due - no response. Sent patient with referral form"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera- nta gisubizo cyabonetse. Nohereje umurwayi yitwaje impapuro zimwohereza"
                    , kirundi = Just "Ntivyakunze kuvuganira k'urubuga/mu kigo kubera - nta nyishu. Rungika umugwayi wamuhaye n'urupapuro/Ifishi rumurungika ahandi"
                    }

                NonePatientRefused ->
                    { english = "Did not talk to site as patient has refused"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera umurwayi yanze"
                    , kirundi = Just "Ntiyashoboye kuvuga ashitse mu kigo/ku kibanza kuko umugwayi yanse"
                    }

                NoneOtherRecommendationSite ->
                    { english = "Not able to talk to site - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe- Izindi mpamvu"
                    , kirundi = Just "Ntivyakunze kuvuganira k'urubuga/mu kigo - Izindi mpavu"
                    }

                RecommendationSiteNotApplicable ->
                    translationSet NotApplicable

        ResultsMissing ->
            { english = "Results Missing"
            , kinyarwanda = Just "Ibisubizo Ntibihari"
            , kirundi = Just "Inyishu zabuze"
            }

        ResultsPending ->
            { english = "Results Pending"
            , kinyarwanda = Just "Ibisubizo birategerejwe"
            , kirundi = Just "Inyishu zirarindiriwe"
            }

        ReviewCaseWith144Respondent ->
            { english = "Review case with 114 Respondent"
            , kinyarwanda = Just "Ongera ukore isuzuma ufatanije n’ukwitabye kuri 114"
            , kirundi = Just "Subiramwo ikibazo hamwe n'uwishuye kuri 114"
            }

        Reviewed ->
            { english = "Reviewed"
            , kinyarwanda = Just "Byarebwe"
            , kirundi = Just "Vyasuzumwe"
            }

        ReviewPriorDiagnosis ->
            { english = "Review Prior Diagnosis"
            , kinyarwanda = Just "Kureba uburwayi yagize/yigeze kurwara"
            , kirundi = Just "Subiramwo isuzuma rya mbere"
            }

        RhNegative ->
            { english = "RH Negative"
            , kinyarwanda = Just "Ubwoko bw'amaraso ni Negatifu"
            , kirundi = Just "RH Négatif"
            }

        Right ->
            { english = "Right"
            , kinyarwanda = Just "Iburyo"
            , kirundi = Just "Iburyo"
            }

        RiskFactorAlert factor ->
            case factor of
                FactorNumberOfCSections number ->
                    if number == 1 then
                        { english = "1 previous C-section"
                        , kinyarwanda = Just "Yabazwe inshuro imwe ubushize"
                        , kirundi = Just "Ugukorwa rimwe vyabaye muri kahise kavuba"
                        }

                    else
                        { english = String.fromInt number ++ " previous C-sections"
                        , kinyarwanda = Just <| String.fromInt number ++ " ubushize yarabazwe"
                        , kirundi = Just <| String.fromInt number ++ " yakozwemo mu kahise "
                        }

                FactorCSectionInPreviousDelivery ->
                    { english = "C-section in previous delivery"
                    , kinyarwanda = Just "Yarabazwe ku nda ishize"
                    , kirundi = Just "Ugukorwa mu kwibaruka guheruka"
                    }

                FactorCSectionReason ->
                    { english = "C-section in previous delivery due to"
                    , kinyarwanda = Just "Ubushize yabazwe abyara kubera"
                    , kirundi = Just "Impamvu yatumye haba ugukorwa mu kwibaruka guheruka"
                    }

                FactorPreviousDeliveryPeriod ->
                    { english = "Previous delivery"
                    , kinyarwanda = Just "kubyara guheruka"
                    , kirundi = Just "Ukwibaruka guheruka"
                    }

                FactorSuccessiveAbortions ->
                    { english = "Patient experienced successive abortions"
                    , kinyarwanda = Just "Umubyeyi yavanyemo inda zikurikiranye"
                    , kirundi = Just "Umuvyeyi yagize imbanyi nyinshi zavuyeyo zikurikirana"
                    }

                FactorSuccessivePrematureDeliveries ->
                    { english = "Patient experienced successive preterm deliveries"
                    , kinyarwanda = Just "Umubyeyi yabyaye inda zidashyitse zikurikiranye"
                    , kirundi = Just "Umuvyeyi yagize imbanyi nyinshi zakurikiranye mu kuvuka hataragera"
                    }

                FactorStillbornPreviousDelivery ->
                    { english = "Stillbirth in previous delivery"
                    , kinyarwanda = Just "Ubushize yabyaye umwana upfuye(wapfiriye mu nda)"
                    , kirundi = Just "Kuvyarira ku gihe mu gihe co kwibaruka imbanyi iheruka"
                    }

                FactorBabyDiedOnDayOfBirthPreviousDelivery ->
                    { english = "Live Birth but the baby died the same day in previous delivery"
                    , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
                    , kirundi = Just "Umwana yavutse akomeye ariko yaciye apfa uwo munsi nyene avuka mu gihe c'ukwibaruka guheruka"
                    }

                FactorPartialPlacentaPreviousDelivery ->
                    { english = "Patient had partial placenta in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize iya nyuma ntiyavutse yose/yaje igice"
                    , kirundi = Just "Umuvyeyi yarafise isimbizo y'igice yo mu gihe c'imbanyi  iheruka"
                    }

                FactorSevereHemorrhagingPreviousDelivery ->
                    { english = "Patient experienced severe hemorrhage in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yaravuye cyane/bikabije ku nda y'ubushize"
                    , kirundi = Just "Umuvyeyi yaravuye amaraso cane mu gihe c'imbanyi iheruka"
                    }

                FactorPreeclampsiaPreviousPregnancy ->
                    { english = "Patient had preeclampsia in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso bibanziriza kugagara ku nda y'ubushize"
                    , kirundi = Just "Umuvyeyi yaragize umuvuduko w'amaraso udasanwze igihe c'imbanyi iheruka"
                    }

                FactorConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Just "Ubushize mubyeyi yagize ibimenyetso byo kugagara/Guhinda umushyitsi abyara"
                    , kirundi = Just "Umuvyeyi yaragize ibizunguzungu mu gihe co kwibaruka guheruka"
                    }

                FactorConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions and resulted in becoming unconscious after delivery"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso byo kugagara nyuma yo kubyara bimuviramo kutumva/guta ubwenge"
                    , kirundi = Just "Umuvyeyi yagize ibizunguzungu hama haziramwo uguta ubwenge mu gihe yarahejeje kuvyara"
                    }

                FactorIncompleteCervixPreviousPregnancy ->
                    { english = "Patient had an Incomplete Cervix in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize inkondo y'umura ntiyashoboye kwifunga neza"
                    , kirundi = Just "Umuvyeyi yagize ingorane y'umuringoti w'igitereko utari ukwiye mu gihe c'imbanyi iheruka"
                    }

                FactorVerticalCSectionScar ->
                    { english = "Vertical C-Section Scar"
                    , kinyarwanda = Just "Inkovu yo kubagwa irahagaze"
                    , kirundi = Just "Inkovu ihagaze y'uwakozwe"
                    }

                FactorGestationalDiabetesPreviousPregnancy ->
                    { english = "Patient had Gestational Diabetes in previous pregnancy"
                    , kinyarwanda = Just "Ubushize umubyeyi yagize indwara ya Diyabete itewe no gutwita"
                    , kirundi = Just "Umuvyeyi yagize Diyabete (Iduga ry'isukari mu mubiri) mu gihe c'imbanyi iheruka"
                    }

        RiskFactors ->
            { english = "Risk Factors"
            , kinyarwanda = Just "Abashobora kwibasirwa n'indwara runaka (kubera impamvu zitandukanye:kuba atwite..)"
            , kirundi = Just "Impamvu z'ingorane"
            }

        SachetsPerDayHelper weight recommendation ->
            { english = "The recommended amount for a " ++ String.fromFloat weight ++ " kg child is " ++ String.fromFloat recommendation ++ " sachets a day"
            , kinyarwanda = Just <| "Amasashe yemewe ku mwana w'ibiro " ++ String.fromFloat weight ++ " ni " ++ String.fromFloat recommendation ++ " ku munsi"
            , kirundi = Just <| "Igipimo gisabwa k'umana w'ibiro " ++ String.fromFloat weight ++ " ni " ++ String.fromFloat recommendation ++ " ku munsi"
            }

        SachetsPerDayQuestion ->
            { english = "How many sachets of supplement is given to the child per day"
            , kinyarwanda = Just "Ni amasashe angahe ahabwa umwana ku munsi"
            , kirundi = Just "Mbega umwana aronka udukarato/udusashe tw'imfungurwa zo kongereza - ivyunyunyu m'umubiri - tungahe k'umunsi"
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            , kirundi = Just "Kubika"
            }

        SaveAndNext ->
            { english = "Save & Next"
            , kinyarwanda = Just "Bika & ukomeze"
            , kirundi = Just "Bika hama ubandanye"
            }

        SaveAndRecordOutcome ->
            { english = "Save & Record Outcome"
            , kinyarwanda = Just "Bika & Andika iherezo ry'uburwayi"
            , kirundi = Just "Bika hama wandike inyishu"
            }

        SavedMoneyQuestion ->
            { english = "Have you saved money for use at the health center while you give birth"
            , kinyarwanda = Just "Wazigamye amafaranga yo gukoresha ku kigo nderabuzima igihe cyo kubyara"
            , kirundi = Just "Mbega warabitse amafaranga yo gukoresha kw'ivuriro/kwa muganga niwibaruka"
            }

        SaveError ->
            { english = "Save Error"
            , kinyarwanda = Just "Kubika error (ikosa mu kubika)"
            , kirundi = Just "Bika ikosa"
            }

        ScheduleFollowUp ->
            { english = "Schedule Follow Up"
            , kinyarwanda = Nothing
            , kirundi = Just "Tegura ibikurikira"
            }

        SearchEhezaForExistingParticipants ->
            { english = "Search E-Heza to see if the contact already exists"
            , kinyarwanda = Just "Reba muri E-heza niba abo bahuye basanzwe barimo"
            , kirundi = Just "Rondera ucuye muri E-Heza kugira urabe ko izina risanzwe ririmwo"
            }

        SearchExistingParticipants ->
            { english = "Search Existing Participants"
            , kinyarwanda = Just "Gushaka abagenerwabikorwa basanzwe muri sisiteme"
            , kirundi = Just "Rondera abitavye bahasanzwe/basanzwe barimwo"
            }

        SearchHelper ->
            { english = "Search to see if the participant already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Shakisha kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba atagaragara, mwandike nku mushya."
            , kirundi = Just "Rondera nimba uwitavye (inama canke inyigisho) arimwo muri E-Heza.Mu gihe uwo urondera atabonetse, ugurura/tegura iyindi fishe/urupapuro rw'abo."
            }

        SearchHelperFamilyMember ->
            { english = "Search to see if the additional family member already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Kanda ku Ishakiro kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba uwo muntu atagaragara mu ishakiro, mwandike nk'umugenerwabikorwa mushya."
            , kirundi = Just "Rondera uwundi muntu wo mu muryango ko yoba yaramaze kwinjizwa muri E-Heza. Mu gihe uwo urondera atabonetse, ugurura/tegura iyindi fishe/urupapuro rw'abo."
            }

        SecondName ->
            { english = "Second Name"
            , kinyarwanda = Just "Izina ry'umuryango"
            , kirundi = Just "Izina ry'umuryango"
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Just "Umurenge"
            , kirundi = Just "Umutumba"
            }

        SeeDosageScheduleByWeight ->
            { english = "See dosage schedule by Weight"
            , kinyarwanda = Nothing
            , kirundi = Just "Raba igipimo c'umuti (Doze) ingene ipanzwe/iteguye hakurikijwe ibiro"
            }

        SeeLabResults ->
            { english = "See Lab Results"
            , kinyarwanda = Just "Reba Ibisubizo by'Ibizamini Byafashwe"
            , kirundi = Just "Raba inyishu z'ibipimo"
            }

        SeeMore ->
            { english = "See More"
            , kinyarwanda = Just "Reba Ibindi"
            , kirundi = Just "Raba ibindi"
            }

        SelectAllDiagnoses ->
            { english = "Select all diagnoses"
            , kinyarwanda = Just "Hitamo uburwayi bwose bwagaragaye"
            , kirundi = Just "Hitamwo ugusuzuma kwose"
            }

        SelectAllSigns ->
            { english = "Select all signs that are present"
            , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite"
            , kirundi = Just "Hitamwo ibimenyetso vyose bihari"
            }

        SelectDangerSigns ->
            { english = "Please select one or more of the danger signs the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            , kirundi = Just "Hitamwo kimwe canke birenga mu bimenyetso vya hatari vyugarije umugwayi"
            }

        SelectDate ->
            { english = "Select Date"
            , kinyarwanda = Just "Hitamo Itariki"
            , kirundi = Just "Hitamwo itarike"
            }

        SelectedFamilyPlanningMethod ->
            { english = "Selected Family Planning Method"
            , kinyarwanda = Just "Uburyo bwo kuboneza urubyaro bwatoranijwe"
            , kirundi = Just "Hitamwo uburyo bwo kuvyara kurugero"
            }

        SelectIllnessSymptoms ->
            { english = "Please select one or more symptoms the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cyangwa byinshi mu bimenyetso umurwayi afite"
            , kirundi = Just "Hitamwo ikimenyetso c'ingwara canke birenga umugwayi ariko arumva"
            }

        SelectPostpartumChildDangerSigns ->
            { english = "Please select one or more of the danger signs the child is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umwana  yaba afite?"
            , kirundi = Just "Hitamwo kimwe canke birenga mu bimenyetso vya hatari vyugarije umwana"
            }

        SelectPostpartumMotherDangerSigns ->
            { english = "Please select one or more of the danger signs the mother is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            , kirundi = Just "Hitamwo kimwe canke birenga mu bimenyetso vya hatari vyugarije umuvyeyi"
            }

        SelectedProgram ->
            { english = "Selected Program"
            , kinyarwanda = Just "Porogaramu Yatoranyijwe"
            , kirundi = Just "Hitamwo umugambi"
            }

        SelectedVillage ->
            { english = "Selected Village"
            , kinyarwanda = Just "Umudugudu Watoranyijwe"
            , kirundi = Just "Hitamwo Ikigwati"
            }

        SelectEncounterType ->
            { english = "Select an encounter type"
            , kinyarwanda = Just "Hitamo ubwoko bw'icyiciro cyo gukorera"
            , kirundi = Just "Hitamwo ubwoko bw'umubonano"
            }

        SelectExistingAcuteIllness ->
            { english = "Select Existing Acute Illness"
            , kinyarwanda = Just "Hitamo Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Just "Hitamwo ingwara ikaze/ikomeye/igoye ihari"
            }

        SelectExistingAcuteIllnessToRecordOutcome ->
            { english = "Select Existing Acute Illness to Record Outcome"
            , kinyarwanda = Just "Hitamo indwara ifatiyeho iheruka kuvurwa kugira ngo wandike iherezo ryayo"
            , kirundi = Just "Hitamwo ingwara ikaze/ikomeye/igoye ihari yo kwinjiza/kwandi inyishu"
            }

        SelectProgram ->
            { english = "Select Program"
            , kinyarwanda = Just "Hitamo porogaramu"
            , kirundi = Just "Hitamwo umugambi"
            }

        SelectYourGroup ->
            { english = "Select your Group"
            , kinyarwanda = Just "Hitamo itsinda ryawe"
            , kirundi = Just "Hitamwo umurwi wawe"
            }

        SelectYourHealthCenter ->
            { english = "Select your Health Center"
            , kinyarwanda = Just "Hitamo ikigo nderabuzima"
            , kirundi = Just "Hitamwo ivuriro ryawe"
            }

        SelectYourVillage ->
            { english = "Select your village"
            , kinyarwanda = Just "Hitamo umudugudu wawe"
            , kirundi = Just "Hitamwo ikigwati cawe/umusozi canke umutumba wawe"
            }

        SelectedHCDownloading ->
            { english = "Downloading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectedHCNotSynced ->
            { english = "Data is not synced"
            , kinyarwanda = Nothing
            , kirundi = Just "Ivyegeranijwe ntibihujwe"
            }

        SelectedHCSyncing ->
            { english = "Data is syncing"
            , kinyarwanda = Nothing
            , kirundi = Just "Ivyegeranijwe birahujwe"
            }

        Send ->
            { english = "Send"
            , kinyarwanda = Nothing
            , kirundi = Just "Rungika"
            }

        ReportToWhatsApp ->
            { english = "Send via WhatsApp"
            , kinyarwanda = Nothing
            , kirundi = Just "Rungika ucishije kuri WhatsApp"
            }

        ReportToWhatsAppComponentsSelectionHeader reportType ->
            case reportType of
                Components.ReportToWhatsAppDialog.Model.ReportWellChild ->
                    { english = "Please select which sections of the Standard Pediatric Visit Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Hitamwo ibice vy'ivyegeranyo bisanzwe vyo kuramutsa/vyo kugendera abana ushaka kurungika:"
                    }

                Components.ReportToWhatsAppDialog.Model.ReportAntenatal ->
                    { english = "Please select which sections of the Antenatal Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Hitamwo ibice vy'ivyegeranyo vy'abibungenze canke vy'imvyaro wipfuza kurungika:"
                    }

                -- Not in use, because AcuteIllness does not allow
                -- components selection.
                Components.ReportToWhatsAppDialog.Model.ReportAcuteIllness ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Components.ReportToWhatsAppDialog.Model.ReportNCD ->
                    { english = "Please select which sections of the NCD Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Hitamwo ibice vy'ivyegeranyo vy'ingwara zitandukira ushaka kurungika:"
                    }

        ReportToWhatsAppConfirmationBeforeExecutingHeader ->
            { english = "By pressing send you are releasing the selected documents to:"
            , kinyarwanda = Nothing
            , kirundi = Just "Ufyonze aho kurungika, uba urekuye inyandiko zatowe/zahiswemwo kuri:"
            }

        ReportToWhatsAppConfirmationBeforeExecutingInstructions ->
            { english = "This action will take up to one minute to complete."
            , kinyarwanda = Nothing
            , kirundi = Just "Iki gikorwa kiraja gutwara umunota kugira gihere"
            }

        ReportToWhatsAppConfirmationBeforeExecutingQuestion ->
            { english = "Would you like to send?"
            , kinyarwanda = Nothing
            , kirundi = Just "Wipfuza kurungika?"
            }

        ReportToWhatsAppConsentQuestion ->
            { english = "Does the patient consent to having their medical records sent via WhatsApp?"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega umugwayi yaremeye kuronka ivyegeranyo vyo kwa muganga birungitswe kuri WhatsApp?"
            }

        ReportToWhatsAppExecutionResultFailure ->
            { english = "Action Failed. Please try again. If problem persists, please contact system administrator."
            , kinyarwanda = Nothing
            , kirundi = Just "Igikorwa canse. Gerageza kandi. Mugihe ingorane ibandanije, gerageza uvugane n'uwujejwe ubwo buhinga/umukuru w'ubuhinga ( umuhinga mu ma porogarama ya mudasobwa)"
            }

        ReportToWhatsAppExecutionResultSomethingWentWrong ->
            { english = "Something went wrong. Please contact system administrator."
            , kinyarwanda = Nothing
            , kirundi = Just "Hari ikintu kitagenze neza. Raba umuhinga/umurongozi w'ubu buryo bugezweho ( wiyi Sisiteme)"
            }

        ReportToWhatsAppExecutionResultSuccess ->
            { english = "Success. Report will be sent when device has internet conneciton."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReportToWhatsAppNoticeOfNonRespobsibility ->
            { english = "Please note that the medical professional and E-Heza will not be liable for what happens to these medical reports once released."
            , kinyarwanda = Nothing
            , kirundi = Just "Menya neza ko umuvuzi hamwe na E-Heza batazobazwa na kimwe mu gihe ivyegeranyo vyo kwa muganga bizoba bimaze gusohoka"
            }

        ReportToWhatsAppPhoneInputHeader ->
            { english = "Enter the correct phone number for the patient:"
            , kinyarwanda = Nothing
            , kirundi = Just "Injiza/andika inimero ya /umurongo wa terefone yo y'umugwayi"
            }

        ReportToWhatsAppPhoneVerificationHeader ->
            { english = "The phone number we have on file for this patient is:"
            , kinyarwanda = Nothing
            , kirundi = Just "Inimero za terefone dufise mu nyandiko z'uyu mugwayi ni:"
            }

        ReportToWhatsAppPhoneVerificationQuestion ->
            { english = "Is this the correct number for the patient's WhatsApp?"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega iyi nimero ya WhatsApp y'umugwayi/umuvyeyi niyo"
            }

        ReportToWhatsAppPhoneUpdateAtProfileQuestionPrefix ->
            { english = "Would you like to update the patient profile for"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega iyi nimero ya WhatsApp y'umugwayi/umuvyeyi niyo"
            }

        ReportToWhatsAppPhoneUpdateAtProfileQuestionSuffix ->
            { english = "with the number"
            , kinyarwanda = Nothing
            , kirundi = Just "Hamwe n'igiharuro"
            }

        ReportToWhatsAppPhoneUpdateConfirmationMessasge ->
            { english = "The patient record has been updated."
            , kinyarwanda = Nothing
            , kirundi = Just "Ivyanditswe vy'umugwayi vyavuguruwe/vyashizwe k'umunsi"
            }

        ServiceWorkerActive ->
            { english = "The app is installed on this device."
            , kinyarwanda = Just "Apulikasiyo muri icyi cyuma cy'inkoranabuhanga yinjijwe."
            , kirundi = Just "Porogarama yashizwe muri iki gikoresho"
            }

        ServiceWorkerCurrent ->
            { english = "You have the current version of the app."
            , kinyarwanda = Just "Ufite apulikasiyo nshya igezweho uyu munsi"
            , kirundi = Just "Ufise verisiyo y'ubu ya porogarama"
            }

        ServiceWorkerCheckForUpdates ->
            { english = "Check for updates"
            , kinyarwanda = Just "Kugenzura ibyavuguruwe"
            , kirundi = Just "Ukuraba ibishasha"
            }

        ServiceWorkerInstalling ->
            { english = "A new version of the app has been detected and is being downloaded. You can continue to work while this is in progress."
            , kinyarwanda = Nothing
            , kirundi = Just "A new version of the app has been detected and is being downloaded. You can continue to work while this is in progress."
            }

        ServiceWorkerInstalled ->
            { english = "A new version of the app has been downloaded."
            , kinyarwanda = Just "Gufungura verisio nshyashya byarangiye."
            , kirundi = Just "Uburyo bushasha bwo kwinjizamwo bwarazanywe/bwarashitse"
            }

        ServiceWorkerSkipWaiting ->
            { english = "Activate new version of the app"
            , kinyarwanda = Just "Gufungura verisio nshyashya"
            , kirundi = Just "Kora/ugurura uburyo bushasha bwo kwinjiza ivyengeranyo (porogarama nshasha)"
            }

        ServiceWorkerRestarting ->
            { english = "The app should reload momentarily with the new version."
            , kinyarwanda = Nothing
            , kirundi = Just "Porogarama itegerezwa gusubiramwo mu mwanya muto hamwe na verisiyo nshasha"
            }

        ServiceWorkerActivating ->
            { english = "A new version of the app is preparing itself for use."
            , kinyarwanda = Nothing
            , kirundi = Just "Uburyo bushasha buriko buritegura gukoreshwa"
            }

        ServiceWorkerActivated ->
            { english = "A new version of the app is ready for use."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerRedundant ->
            { english = "An error occurred installing a new version of the app."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerInactive ->
            { english = "The app is not yet installed on this device."
            , kinyarwanda = Nothing
            , kirundi = Just "Porogarama ntiyashizwe muri iki gikoresho"
            }

        ServiceWorkerRegNotAsked ->
            { english = "We have not yet attempted to install the app on this device."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerRegLoading ->
            { english = "Installation of the app on this device is progressing."
            , kinyarwanda = Nothing
            , kirundi = Just "Kwinjiza uburyo bushasha (porogarama nshasha) muri iki cuma/igikoresho buriko buratera imbere"
            }

        ServiceWorkerRegErr ->
            { english = "There was an error installing the app on this device. To try again, reload this page."
            , kinyarwanda = Nothing
            , kirundi = Just "Habayeho ikosa ryo kwinjiza porogarama muri iki gikoresho. Kugerageza kandi, subira gukorera aha nyene."
            }

        ServiceWorkerRegSuccess ->
            { english = "The app was successfully registered with this device."
            , kinyarwanda = Just "Igikorwa cyo gushyira apulikasiyo kuri iki gikoresho cy'ikoranabuhanga cyagenze neza."
            , kirundi = Just "Porogarama yashitse/yanditswe neza muri iki gikoresho"
            }

        ServiceWorkerStatus ->
            { english = "Deployment Status"
            , kinyarwanda = Just "Ibijyanye no kuvugurura no kongerera ubushobozi sisiteme"
            , kirundi = Just "Ingene irungika rimeze"
            }

        SevereAcuteMalnutrition ->
            { english = "Severe acute malnutrition"
            , kinyarwanda = Just "Imirire mibi ikabije imaze igihe gito"
            , kirundi = Just "Ingwara yo gufungura nabi ikaze"
            }

        SevereHemorrhagingPreviousDelivery ->
            { english = "Severe Hemorrhaging in previous delivery (>500 ml)"
            , kinyarwanda = Just "Ubushize yavuye cyane akimara kubyara hejuru ya Ml 500"
            , kirundi = Just "Ukuva amaraso gukaze mu gihe c'ivyara riheruka (>500ml)"
            }

        Shared ->
            { english = "Shared"
            , kinyarwanda = Just "Ayisangira n'abandi"
            , kirundi = Just "Yasangiye"
            }

        Signature ->
            { english = "Signature"
            , kinyarwanda = Nothing
            , kirundi = Just "Umukono"
            }

        SignOnDoorPostedQuestion ->
            { english = "Have you posted signs on the door indicating that the space is an isolation area"
            , kinyarwanda = Just "Waba washyize ibimenyetso ku rugi byerekana ko iki cyumba ari ikijyamo abantu bari mu kato"
            , kirundi = Just "Mbega warashize ku myango ibimenyetso vyerekana ko ikibaza ari ic'ubwiherero/ukuba wenyene"
            }

        SpecialityCareHeaderPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe uburwayi bwa"
            , kirundi = Just "Wasuzumwe"
            }

        SpecialityCareHeaderSuffix ->
            { english = "during your pregnancy"
            , kinyarwanda = Just "Mu gihe wari utwite"
            , kirundi = Just "Mu gihe c'imbanyi yawe"
            }

        SpecialityCareSignQuestion sign ->
            case sign of
                EnrolledToARVProgram ->
                    { english = "Are you currently enrolled in ARV services at the health center"
                    , kinyarwanda = Just "Waba wanditswe muri serivise itanaga imiti igabanya ubukana bwa Vurusi itera SIDA ku kigo nderabuzima"
                    , kirundi = Just "Ubu, mbega uri ku miti ya SIDA itangigwa kw'ivuriro (Ubu, mbega urimwo mubaronka ubufasha bwa ma ARV butangwa kw'ivuriro/kwa muganga)"
                    }

                EnrolledToNCDProgram ->
                    { english = "Are you currently enrolled in NCD services at the health center"
                    , kinyarwanda = Just "Waba usanzwe wanditse muri serivisi y'indwara zitandura ku kigo nderabusima"
                    , kirundi = Just "Ubu, mbega urimwo mubaronka ubufasha butangwa kubagwaye ingwara zitandukira butangwa kw'ivuriro/kwa muganga"
                    }

                NoSpecialityCareSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StillbornPreviousDelivery ->
            { english = "Stillborn in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana upfuye"
            , kirundi = Just "Yavutse yamaze gupfa mu gihe co kuvyara ku mbanyi iheruka"
            }

        StockCorrectionReason value ->
            case value of
                ReasonInputError ->
                    { english = "Error in input"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ikosa mu kwinjiza"
                    }

                ReasonExpiration ->
                    { english = "Expired stock"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ububiko bwataye igihe"
                    }

                ReasonMissing ->
                    { english = "Missing stock / unaccounted for"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ububiko bwabuze / Ibiri m'ububiko bidaharuye"
                    }

                ReasonOther ->
                    { english = "Other"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ibindi"
                    }

        StockManagement ->
            { english = "Stock Management"
            , kinyarwanda = Nothing
            , kirundi = Just "Ugucunga ububiko"
            }

        StockManagementMenu value ->
            case value of
                MenuReceiveStock ->
                    { english = "Receive Stock"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ukuronka ivyo gushira m'ububiko"
                    }

                MenuViewMonthDetails ->
                    { english = "View current month details"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Raba neza/ Subiramwo ivyanditse vyose vy'uku kwezi"
                    }

                MenuCorrectEntry ->
                    { english = "Correct entry"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kwinjiza neza"
                    }

        StockManagementBatchNumberQuestion ->
            { english = "What is the batch number"
            , kinyarwanda = Nothing
            , kirundi = Just "Inimero y'umurwi/ya karundo ni iyihe"
            }

        StockManagementCorrectionTypeLabel ->
            { english = "Please select the type of the correct"
            , kinyarwanda = Nothing
            , kirundi = Just "Hitamwo ico ubona arico"
            }

        StockManagementCorrectionEntryType value ->
            case value of
                EntryAddition ->
                    { english = "Addition"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Guteranya"
                    }

                EntrySubstraction ->
                    { english = "Substraction"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Gukuramwo"
                    }

        StockManagementCorrectionReasonLabel ->
            { english = "Please select the reason for the correct"
            , kinyarwanda = Nothing
            , kirundi = Just "Hitamwo impamvu y'ukuri"
            }

        StockManagementDateExpiresQuestion ->
            { english = "What is the expiration date"
            , kinyarwanda = Nothing
            , kirundi = Just "Itarike izorangirirako niyihe"
            }

        StockManagementEnterSignatureLabel ->
            { english = "Please enter your signature"
            , kinyarwanda = Nothing
            , kirundi = Just "Nimwinjize umukono wanyu"
            }

        StockManagementQuantityAddedQuestion ->
            { english = "How much stock is being received"
            , kinyarwanda = Nothing
            , kirundi = Just "Mbega aronka ivyo abika bingana gute"
            }

        StockManagementQuantityCorrectionLabel ->
            { english = "Please enter the quantity"
            , kinyarwanda = Nothing
            , kirundi = Just "Nimwinjize igitigiri"
            }

        StockManagementSelectDateLabel ->
            { english = "Select a date for this entry"
            , kinyarwanda = Nothing
            , kirundi = Just "Hitamwo itarike yuru ngwinjizo/itarike yo kwinjiza"
            }

        StockManagementSupplierQuestion ->
            { english = "Where was this received from"
            , kinyarwanda = Nothing
            , kirundi = Just "Iyi yakiririwe hehe"
            }

        StockSupplier value ->
            case value of
                SupplierMOH ->
                    { english = "MOH (Ministry of Health)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRBC ->
                    { english = "RBC (Rwanda Biomedical Center)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierUNICEF ->
                    { english = "UNICEF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRMSCentral ->
                    { english = "RWANDA MEDICAL SUPPLY (RMS)-Central Level"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRMSDistrict ->
                    { english = "RWANDA MEDICAL SUPPLY (RMS)-District Level"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierBUFMAR ->
                    { english = "BUFMAR (Le Bureau des Formations Médicales agréées du Rwanda)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StockSupplierAbbrev value ->
            case value of
                SupplierMOH ->
                    { english = "MOH"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRBC ->
                    { english = "RBC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierUNICEF ->
                    { english = "UNICEF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRMSCentral ->
                    { english = "RMS-Central"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierRMSDistrict ->
                    { english = "RMS-District"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SupplierBUFMAR ->
                    { english = "BUFMAR"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StuntingLevelLabel ->
            { english = "Level of stunting using child length mat"
            , kinyarwanda = Just "Ikigero cyo kugwingira hakoreshejwe agasambi"
            , kirundi = Nothing
            }

        StuntingLevel value ->
            case value of
                LevelGreen ->
                    translationSet ColorGreen

                LevelYellow ->
                    translationSet ColorYellow

                LevelRed ->
                    translationSet ColorRed

        SubsequentEncounter ->
            { english = "Subsequent Encounter"
            , kinyarwanda = Just "Igikorwa gikurikiyeho"
            , kirundi = Just "Uguhura kuri imbere/ umubonano uri imbere"
            }

        SubsequentEncounterReferral encounterType ->
            if encounterType == AcuteIllnessEncounterCHW then
                { english = "CHW Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku mujyanama w'ubuzima"
                , kirundi = Just "Ukurungika umugwayi kw'ivuriro bikozwe n'Abaremeshakiyago"
                }

            else
                { english = "Health Center Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku kigo nderabuzima"
                , kirundi = Just "Uzuza urupapuro rwo kurungika umurwayi kwa muganga rutangwa n'ivuriro"
                }

        SuccessiveAbortions ->
            { english = "Successive Abortions"
            , kinyarwanda = Just "Inda zavuyemo zikurikiranye"
            , kirundi = Just "Umuvyeyi yagize imbanyi nyinshi zavuyeyo zikurikirana"
            }

        SuccessivePrematureDeliveries ->
            { english = "Successive Premature Deliveries"
            , kinyarwanda = Just "Inda zavutse zidashyitse zikurikiranye"
            , kirundi = Just "Ukwibaruka hataragera kwakurikiranye"
            }

        SuspectedCovid19CaseAlert ->
            { english = "Suspected COVID-19 case"
            , kinyarwanda = Just "Acyekwaho kwandura COVID-19"
            , kirundi = Just "Hiketswe ingwara ya Korona-19 (COVID-19)"
            }

        SuspectedCovid19CaseAlertHelper ->
            { english = "Please isolate immediately from family and contact health center"
            , kinyarwanda = Just "Mutandukanye n'umuryango we byihuse uhite umenyesha Ikigo nderabuzima"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseIsolate ->
            { english = "Isolate immediately from family"
            , kinyarwanda = Just "Mutandukanye ako kanya n'umuryango we umushyire mu kato"
            , kirundi = Just "Itandukanye ubwo nyene n'umuryango wawe uje mu kibanza cawe wenyene"
            }

        SuspectedCovid19CaseContactHC ->
            { english = "Contact health center immediately"
            , kinyarwanda = Just "Menyesha ikigo nderabuzima ako kanya"
            , kirundi = Just "Ukuvugana n'ivuriro vuba bwango"
            }

        SuspectedCovid19CasePerformRapidTest ->
            { english = "Perform a rapid test immediately"
            , kinyarwanda = Just "Kora ikizamini nonaha"
            , kirundi = Just "Gukora igipimo cihuta aka kanya"
            }

        SuspectedCovid19CaseReferToHCForTesting ->
            { english = "Refer to Health Center for testing"
            , kinyarwanda = Nothing
            , kirundi = Just "Rungika kw'ivuriro kugirisha ibipimo"
            }

        SymptomRelief type_ ->
            case type_ of
                SymptomReliefParacetamol ->
                    { english = "Paracetamol for Fever"
                    , kinyarwanda = Just "Umuti wa Paracetamoro ugabanya umuriro"
                    , kirundi = Just "Paracétamol umuti w'ubushuhe"
                    }

                SymptomReliefVitaminC ->
                    { english = "Effervescent Vitamin C tablets"
                    , kinyarwanda = Just "Ibinini bya Vitamin C"
                    , kirundi = Just "Ibinini bibira vya Vitamine C"
                    }

                SymptomReliefPaidoterineSyrup ->
                    { english = "Paidoterin syrup as a decongestant"
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero"
                    , kirundi = Just "Sirop de Paidoterin nk'umuti wo kugabanya (kugurura nko mu mazuru - kugabanya ibiseru)"
                    }

                SymptomReliefCoughMixture ->
                    { english = "Cough mixtures such as Ascoril, Bronchalene, etc."
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero, n'indi"
                    , kirundi = Just "Ugukorora bivanze na Ascoril, Bronchalène, n'ibindi"
                    }

        Symptoms ->
            { english = "Symptoms"
            , kinyarwanda = Just "Ibimenyetso"
            , kirundi = Just "Ibimenyetso"
            }

        SymptomsGeneralSign sign ->
            case sign of
                BodyAches ->
                    { english = "Body Aches"
                    , kinyarwanda = Just "Ububabare bw'umubiri wose"
                    , kirundi = Just "Ukuvunagurika"
                    }

                Chills ->
                    { english = "Chills"
                    , kinyarwanda = Just "Gutengurwa"
                    , kirundi = Just "Gukanya"
                    }

                SymptomGeneralFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Just "Ubushuhe"
                    }

                Headache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kubabara umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    }

                NightSweats ->
                    { english = "Night Sweats"
                    , kinyarwanda = Just "Kubira ibyuya nijoro"
                    , kirundi = Just "Kubira ivyuya mw'ijoro"
                    }

                Lethargy ->
                    { english = "Lethargy"
                    , kinyarwanda = Just "Guhwera"
                    , kirundi = Just "Itiro rirenze"
                    }

                PoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Just "Ugukwega kutakwiye"
                    }

                UnableToDrink ->
                    { english = "Unable to Drink"
                    , kinyarwanda = Just "Ntashobora kunywa"
                    , kirundi = Just "Ntibishoboka kunywa"
                    }

                UnableToEat ->
                    { english = "Unable to Eat"
                    , kinyarwanda = Just "Ntashobora kurya"
                    , kirundi = Just "Ntibishoboka kurya"
                    }

                IncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Just "Afite inyota cyane"
                    , kirundi = Just "Kongereza inyota"
                    }

                DryMouth ->
                    { english = "Dry/Sticky Mouth"
                    , kinyarwanda = Just "Iminwa yumye"
                    , kirundi = Just "Umunwa wumye"
                    }

                SevereWeakness ->
                    { english = "Severe Weakness"
                    , kinyarwanda = Just "Yacitse intege cyane"
                    , kirundi = Just "Amagara make cane"
                    }

                YellowEyes ->
                    { english = "Yellow Eyes"
                    , kinyarwanda = Just "Amaso y'umuhondo"
                    , kirundi = Just "Amaso asa n'umutoto uhishiye"
                    }

                CokeColoredUrine ->
                    { english = "Coca-Cola Colored Urine"
                    , kinyarwanda = Just "Inkari zisa na kokakola"
                    , kirundi = Just "Umukoyo urimwo irangi rya coca-cola"
                    }

                SymptomsGeneralConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Just "Ukujugumira"
                    }

                SpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    , kirundi = Just "Ukuva amaraso aho nyene"
                    }

                NoSymptomsGeneral ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        SymptomsGISign sign ->
            case sign of
                SymptomGIAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    }

                BloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    , kirundi = Just "Uguhitwa bivanze n'amaraso"
                    }

                Nausea ->
                    { english = "Nausea"
                    , kinyarwanda = Just "Afite iseseme"
                    , kirundi = Just "Iseseme"
                    }

                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea - >3 liquid stools in the last 24 hours"
                    , kinyarwanda = Just "Nta maraso yituma- yituma ibyoroshye inshuro zirenze 3 mu masaha 24"
                    , kirundi = Just "Nta ntagucibwamo birimwo amaraso - > ugucibwamwo incuro zirenze 3 mu masaha 24"
                    }

                Vomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    , kirundi = Just "Ukudahwa"
                    }

                NoSymptomsGI ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        SymptomsGISignAbbrev sign ->
            case sign of
                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea"
                    , kinyarwanda = Just "Nta maraso yituma"
                    , kirundi = Just "Nta bihitwe birimwo amaraso"
                    }

                _ ->
                    translationSet (SymptomsGISign sign)

        SymptomsRespiratorySign sign ->
            case sign of
                BloodInSputum ->
                    { english = "Blood in Sputum"
                    , kinyarwanda = Just "Amaraso mu gikororwa"
                    , kirundi = Just "Amaraso mu bikororwa"
                    }

                Cough ->
                    { english = "Cough"
                    , kinyarwanda = Just "Inkorora"
                    , kirundi = Just "Inkorora"
                    }

                NasalCongestion ->
                    { english = "Nasal Congestion"
                    , kinyarwanda = Just "Gufungana mu mazuru"
                    , kirundi = Just "Ugupfungana amazuru"
                    }

                ShortnessOfBreath ->
                    { english = "Shortness of Breath"
                    , kinyarwanda = Just "Guhumeka nabi"
                    , kirundi = Just "Ibura ry'impwemu"
                    }

                SoreThroat ->
                    { english = "Sore Throat"
                    , kinyarwanda = Just "Kubabara mu muhogo"
                    , kirundi = Just ""
                    }

                LossOfSmell ->
                    { english = "Loss of Smell"
                    , kinyarwanda = Just "Kudahumurirwa"
                    , kirundi = Just "Ugutakaza kumoterwa"
                    }

                StabbingChestPain ->
                    { english = "Stabbing Chest Pain"
                    , kinyarwanda = Just "Kubabara mu gatuza"
                    , kirundi = Just "Ububabare nk'umusonga mu gikiriza (umengo bagucumise imbugita)"
                    }

                NoSymptomsRespiratory ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Just "Nta nimwe muri izo ziri hejuru"
                    }

        SymptomsTask task ->
            case task of
                SymptomsGeneral ->
                    { english = "General"
                    , kinyarwanda = Just "Ibimenyesto rusange"
                    , kirundi = Just "Rusangi"
                    }

                SymptomsRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Just "Ubuhumekero"
                    , kirundi = Just "Guhema"
                    }

                SymptomsGI ->
                    { english = "GI"
                    , kinyarwanda = Just "Urwungano ngogozi"
                    , kirundi = Nothing
                    }

        SyphilisRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Syphilis"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bwa Mburugu"
            , kirundi = Just "Uyu murwayi bamutoye Syphilis"
            }

        SyphilisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Just "Hitamo imiti n'igipimo/ibipimo (idoze) uzotanga k'umugwayi"
            }

        SyphilisRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            , kirundi = Just "Umenye neza ko umugwayi afashe imiti itamumerera nabi imbere yo kuyimwandikira"
            }

        SyphilisRecommendedTreatmentWarning ->
            { english = "If Erythromycin or Azithromycin used, must treat newborn immediately after delivery (does not cross into placenta)"
            , kinyarwanda = Just "Niba ari Erythromicine cg Azithromycine wakoresheje, ugomba kuvura uruhinja rukivuka (Uyu muti ntiwinjira mu ngobyi y'umwana)"
            , kirundi = Nothing
            }

        GroupEncounterClosed ->
            { english = "Group Encounter closed"
            , kinyarwanda = Nothing
            , kirundi = Just "Umubonano w'umurwi waheze"
            }

        GroupEncounterClosed2 sessionId ->
            { english =
                String.join " "
                    [ "Group Encounter"
                    , fromEntityUuid sessionId
                    , """is closed. If you need to make further modifications
            to it, please contact an administrator to have it
            re-opened."""
                    ]
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GroupEncounterLoading ->
            { english = "Loading Group Encounter"
            , kinyarwanda = Just "Gufungura icyiciro cyo gukorera"
            , kirundi = Just "kwegeranya ivyavuye mu nama z'imirwi"
            }

        GroupEncounterUnauthorized ->
            { english = "Group Encounter unauthorized"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GroupEncounterUnauthorized2 ->
            { english =
                """You are not authorized to view this health assessment.
        Please contact the Ihangane project for further
        instructions."""
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendPatientToFacility facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Send patient to the health center"
                    , kinyarwanda = Just "Ohereza umurwayi ku kigo nderabuzima"
                    , kirundi = Just "Rungika umugwayi kw'ivuriro"
                    }

                FacilityHospital ->
                    { english = "Send patient to the hospital"
                    , kinyarwanda = Just "Ohereza umurwayi kwa muganga"
                    , kirundi = Just "Rungika umugwayi ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Refer patient to mental health specialist for further evaluation"
                    , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
                    , kirundi = Just "Rungika umuvyeyi k'umuhinga w'ingwara zo mu mutwe kugira amupime kandi/agire ibipimo vyiyongera"
                    }

                FacilityARVProgram ->
                    { english = "Direct patient to the appropriate location"
                    , kinyarwanda = Just "Yobora umurwayi ahantu habugenewe"
                    , kirundi = Just "Ereka neza umugwayi aho atumbera"
                    }

                FacilityNCDProgram ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

                FacilityANCServices ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

                FacilityUltrasound ->
                    { english = "Send patient to ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Rungika umugwayi mw'Iradiyo"
                    }

        StartEncounter ->
            { english = "Start an encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            , kirundi = Just "Tangura umubonano"
            }

        StrartNewAcuteIllnessHelper ->
            { english = "If existing Acute Illness is not part of the list above, start a new encounter"
            , kinyarwanda = Just "Niba Indwara ifatiyeho iheruka kuvurwa itagaragara ku rutonde rwavuzwe haruguru , tangira isuzuma rishya"
            , kirundi = Just "Nimba ingwara ikaze/ikomeye ihari/iriho ubu itarimwo k'urutonde ruraha hejuru, tangura umubonano mushasha/guhura ubushasha"
            }

        StartDate ->
            { english = "Start Date"
            , kinyarwanda = Just "Itariki utangireyeho"
            , kirundi = Just "Itarike yo gutangura"
            }

        EndDate ->
            { english = "End Date"
            , kinyarwanda = Just "Itariki urangirijeho"
            , kirundi = Just "Itarike yo guherezako"
            }

        StartingStock ->
            { english = "Starting Stock"
            , kinyarwanda = Nothing
            , kirundi = Just "Ugutangura ububiko"
            }

        StartSyncing ->
            { english = "Start Syncing"
            , kinyarwanda = Just "Tangira uhuze amakuru kuri seriveri"
            , kirundi = Just "Tangura guhuza"
            }

        StatusLabel ->
            { english = "Status"
            , kinyarwanda = Just "Uko bihagaze kugeza ubu"
            , kirundi = Just "Indangakamere"
            }

        StopSyncing ->
            { english = "Stop Syncing"
            , kinyarwanda = Just "Tangira gukura amakuru kuri seriveri"
            , kirundi = Just "Hagarika guhuza"
            }

        Success ->
            { english = "Success"
            , kinyarwanda = Just "Byagezweho"
            , kirundi = Just "Kubigenza neza"
            }

        SyncGeneral ->
            { english = "Sync Status (General)"
            , kinyarwanda = Just "Ibijyanye no guhuza amakuru yafashwe n'igikoresho cy'ikoranabuhanga n'abitse kuri seriveri"
            , kirundi = Just "Indangakamere yo guhuza (muri rusangi)"
            }

        TabletSinglePlural value ->
            if value == "1" then
                { english = "1 tablet"
                , kinyarwanda = Just "Ikinini cyimwe"
                , kirundi = Just "Ikinini 1"
                }

            else
                { english = value ++ " tablets"
                , kinyarwanda = Just <| "ibinini " ++ value
                , kirundi = Just <| "ibinini " ++ value
                }

        TakingMedicationAsPrescribed taking ->
            if taking then
                { english = "Taking medication as prescribed"
                , kinyarwanda = Just "Yafashe imiti uko yayandikiwe"
                , kirundi = Just "Gufata imiti nkuko vyanditswe"
                }

            else
                { english = "Not taking medication as prescribed because of"
                , kinyarwanda = Just "Ntabwo yafashe imiti uko yayandikiwe kubera ko"
                , kirundi = Just "Kudafata imiti nkuko vyanditswe/vyategekanijwe kubera"
                }

        TasksCompleted completed total ->
            { english = String.fromInt completed ++ "/" ++ String.fromInt total ++ " Tasks Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ "/" ++ String.fromInt total ++ " Ibikorwa byarangiye"
            , kirundi = Just <| String.fromInt completed ++ "/" ++ String.fromInt total ++ " Ibikorgwa vyarangiye"
            }

        TargetedInterventions ->
            { english = "Targeted Interventions"
            , kinyarwanda = Just "Ibikorwa bifasha umwana mu buryo bwihariye"
            , kirundi = Just "Intabaro zerekejwe / ugutabara kwerekejwe"
            }

        TelephoneNumber ->
            { english = "Telephone Number"
            , kinyarwanda = Just "Numero ya telefoni"
            , kirundi = Just "Inimero ya Terefone"
            }

        Term ->
            { english = "Term"
            , kinyarwanda = Just "Inda igeze igihe"
            , kirundi = Just "Ikiringo"
            }

        TermPregnancy ->
            { english = "Number of Term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima bashyitse"
            , kirundi = Just "Igitigiri c'imbanyi zashitse ku gihe (abana bakavuka bakomeye)"
            }

        ThisGroupHasNoMothers ->
            { english = "This Group has no mothers assigned to it."
            , kinyarwanda = Just "Iki cyiciro nta mubyeyi cyagenewe."
            , kirundi = Just "Uyu mugwi nta muvyeyi/umu mama bawuhaye"
            }

        Time ->
            { english = "Time"
            , kinyarwanda = Just "igihe"
            , kirundi = Just "Umwanya"
            }

        To ->
            { english = "to"
            , kinyarwanda = Just "kuri"
            , kirundi = Just "kuri"
            }

        ToThePatient ->
            { english = "to the patient"
            , kinyarwanda = Just "ku murwayi"
            , kirundi = Just "Ku mugwayi"
            }

        TransportationPlanQuestion ->
            { english = "Have you planned for transportation to and from the health center to give birth"
            , kinyarwanda = Just "Waba warateganije uburyo uzagera ku kigo nderabuzima ugiye kubyara ndetse n'uburyo uzavayo nyuma yo kubyara"
            , kirundi = Just "Wigeze utegura ingene wogenda no kuva kw'ivuriro uhejeje kuvyara/kwibaruka"
            }

        TraveledToCOVID19CountryQuestion ->
            { english = "Have you traveled to any country or district known to have COVID-19 in the past 14 days"
            , kinyarwanda = Just "Waba waragiye mu gihugu cyangwa mu karere mu bizwi ko hagaragayemo ubwandu bwa Covid 19 mu minsi 14 ishize"
            , kirundi = Just "Mbega waratemberereye mu gihugu canke mu ntara y'Uburundi harimwo COVID-19 mu kiringo c'iminsi 14 iheze"
            }

        TravelHistory ->
            { english = "Travel History"
            , kinyarwanda = Just "Amukuru ku ngendo"
            , kirundi = Just "Akahise k'ingendo/urugendo"
            }

        TreatedWith ->
            { english = "Treated with"
            , kinyarwanda = Just "Bivurwa na"
            , kirundi = Just "Yavuwe na"
            }

        TreatedWithNot ->
            { english = "Not treated with"
            , kinyarwanda = Just "Ntibivurwa na"
            , kirundi = Just "Ntiyavuwe na"
            }

        Treatment ->
            { english = "Treatment"
            , kinyarwanda = Just "Ubuvuzi"
            , kirundi = Just "Ubuvuzi"
            }

        TreatmentDetailsAnemia ->
            { english = "At the previous visit you were given Iron (120mg), one 60mg tablet to be taken 2x a day for 3 months and Folic Acid (400 IU) to be taken daily for 3 months."
            , kinyarwanda = Just "Mu isura riheruka wahawe umuti (Ubutare or Feri) wongera amaraso(120mg), miligarama 60 inshuro ebyiri ku munsi mu mezi atatu na Acide folike(400 UI)inshuro imwe ku munsi mu miezi atatu."
            , kirundi = Just "Aho uherukira kuza, wahawe fer (120 mg) - icunyunyu c'icuma), ikini ca 60 mg co gufata kabiri (2) ku munsi mu kiringo c'amezi atatu (3)  hamwe na acide folique (400 UI) wo gufata iminsi yose mu mezi atatu (3)."
            }

        TreatmentDetailsHIV dolutegravir arvs ->
            if dolutegravir && arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day and Doltegravir (50mg) to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine ikinini kimwe ku munsi na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                , kirundi = Just "Aho uherukira kuza, wahawe FTD + 3TC (ikinini 1), umuti/ikinini wo/ gufata (kumira) rimwe (1) ku munsi hamwe na Doltégravir (50 mg) wo kumira rimwe (1) ku munsi."
                }

            else if dolutegravir then
                { english = "At the previous visit you were given Doltegravir (50mg), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura rishize wahawe ikinini cya Dulutogaraviri(50mg), ikinini kimwe ku munsi."
                , kirundi = Just "Aho uherukira kuza, wahawe doltégravir (50 mg), umuti wo kumira rimwe (1) k'umunsi."
                }

            else if arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                , kirundi = Just "Aho uherukira kuza, wahawe FTD + 3TC (ikinini 1), umuti/ikinini wo/co gufata (kumira) rimwe (1) ku munsi."
                }

            else
                { english = ""
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        TreatmentDetailsHypertension forModeratePreeclamsia sign ->
            let
                diagnosis =
                    if forModeratePreeclamsia then
                        "Moderate Preeclampsia"

                    else
                        "Hypertension"
            in
            case sign of
                TreatmentMethyldopa2 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 2x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro ebyri ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Just <| "Aho uherukira kuza, wahawe méthyldopa (250 mg), umuti wo kumira kabiri (2) ku munsi ya" ++ diagnosis ++ "."
                    }

                TreatmentMethyldopa3 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 3x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro eshatu ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Just <| "Aho uherukira kuza, wahawe méthyldopa (250 mg), umuti wo kumira gatatu (3) ku munsi ku ngwara ya" ++ diagnosis ++ "."
                    }

                TreatmentMethyldopa4 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Just <| "Aho uherukira kuza, wahawe méthyldopa (250 mg), umuti wo kumira kane (4) ku munsi ku ngwara ya" ++ diagnosis ++ "."
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Just <| "Aho uherukira kuza, wahawe méthyldopa (250 mg), umuti wo kumira kane (4) ku munsi hamwe na carvédilol (6,25 mg) wo kumira 2 ku munsi ku ngwara ya" ++ diagnosis ++ "."
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day and Amlodipine (5mg), by mouth 1x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi na Amlodipine (5mg), mu kanwa inshuro imwe ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Just <| "Aho uherukira kuza, wahawe méthyldopa (250 mg), umuti wo kumira kane (4) ku munsi hamwe na carvédilol (6,25 mg) wo kumira 2 ku munsi na amlodipine (5 mg) wo kumira 1 ku munsi ku ngwara ya" ++ diagnosis ++ "."
                    }

                -- All others are not Hypertension treatments.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TreatmentDetailsMalaria sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "At the previous visit you were given Quinine Sulphate per os 10 mg/kg/dose, to be taken 3 times a day for 7 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe umuti wa Kinini 10mg ku kilo, gatatu ku munsi mu minsi irindwi."
                    , kirundi = Just "Aho uherukira kuza, wahawe sulfate de quinine per os 10 mg/kg/dose, umuti wo gufata incuro zitatu (3) ku munsi mu kiringo c'iminsi indwi (7)."
                    }

                TreatmentCoartem ->
                    { english = "At the previous visit you were given Coartem, 4 tablets to be taken by mouth twice per day x 3 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe AL (Kowaritemu), ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    , kirundi = Just "Aho uherukira kuza, wahawe Coartem, ibinini bine (4) vyo kumira kabiri (2) k'umunsi mu kiringo c'iminsi itatu (3)."
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TreatmentDetailsSyphilis sign ->
            case sign of
                TreatmentPenecilin1 ->
                    { english = "At the previous visit you were given Penicillin (2.4 million units), IM x 1."
                    , kinyarwanda = Just "Mu isura rishize wahawe Penisilini (inite Miliyoni 2.4 ), IM inshuro 1."
                    , kirundi = Just "Aho uherukira kuza, wahawe pénicilline (ingero zingana na miliyoni zibiri n'ibice bine 2,4), umuti wo guterwa bacishije mu mutsi (IM) rimwe (1)."
                    }

                TreatmentPenecilin3 ->
                    { english = "At the previous visit you were given Penicillin (2.4 million units), IM 1x a week for 3 weeks."
                    , kinyarwanda = Just "Mu isura rishize wahawe Penisilini (inite Miliyoni 2.4 ), IM inshuro 1, IM inshuro 1 buri cyumweru mu byumweru 3."
                    , kirundi = Just "Aho uherukira kuza, wahawe pénicilline (ingero zingana na miliyoni zibiri n'ibice bine 2,4), umuti wo guterwa bacishije mu mutsi (IM) rimwe (1) mu ndwi mu kiringo c'indwi zitatu (3)."
                    }

                TreatmentErythromycin ->
                    { english = "At the previous visit you were given Erythromycin (500mg), by mouth 4x a day for 14 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Erythromicine (500mg), mu kanwa inshuro enye ku munsi mu minsi 14."
                    , kirundi = Just "Aho uherukira kuza, wahawe érythromycine (500 mg), umuti wo kumira kane (4) k'umunsi mu kiringo c'iminsi cumi nine (14)."
                    }

                TreatmentAzithromycin ->
                    { english = "At the previous visit you were given Azithromycin (2g), 4 tabs by mouth x one day."
                    , kinyarwanda = Just "Mu isura rishize wahawe Azithromycine (2g), Ibinini 4 abinywe mu kanwa umunsi umwe."
                    , kirundi = Just "Aho uherukira kuza, wahawe azithromycine (2 g), ibini bine (4) vyokumirira rimwe k'umunsi."
                    }

                TreatmentCeftriaxon ->
                    { english = "At the previous visit you were given Ceftriaxone (1g), IM daily x 10 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Ceftriaxone (1g), IM buri munsi mu minsi 10."
                    , kirundi = Just "Aho uherukira kuza, wahawe Ceftriaxone (1g), imiti yo mu mitsi (intramusculaire) iminsi yose mu kiringo c'iminsi cumi (10). "
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TreatmentReviewQuestionAdverseEvents ->
            { english = "Have you experienced any adverse events"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            , kirundi = Just "Wigeze uhura n'ibintu bibi bitandukanye"
            }

        TreatmentReviewQuestionAdverseEventsHospitalization ->
            { english = "The patient had an adverse reaction to the medication. Would you like to refer them to the hospital as next step"
            , kinyarwanda = Just "Umurwayi yabonye ibintu bidasanzwe byatewe n'imiti yahawe. Waba ushaka kumwhoreza ku bitaro nk'igikorwa gikurikiyeho"
            , kirundi = Just "Umugwayi yagize ingaruka mbi kuri uyo muti. Wumva ushaka kumurungika ku bitoro nk'intambwe ikurikira"
            }

        TreatmentReviewQuestionMedicationByPMTCT ->
            { english = "Did you receive medicine from PMTCT"
            , kinyarwanda = Just "Wahawe imiti muri PMTCT"
            , kirundi = Just "Mbega wararonse imiti ivuye muri PTME"
            }

        TreatmentReviewQuestionMissedDoses ->
            { english = "Have you missed any doses"
            , kinyarwanda = Just "Haba hari imiti wasimbutse gufata"
            , kirundi = Just "Wigeze wibagira gufata igipimo c'umuti/Idoze y'umuti"
            }

        TreatmentReviewQuestionStillTaking ->
            { english = "Are you still taking this medication"
            , kinyarwanda = Just "Uracyari gufata imiti"
            , kirundi = Just "Uracari kuri uyu muti/kuri iyi miti"
            }

        TreatmentReviewQuestionStillTakingForHIV ->
            { english = "Are you still taking ARVs"
            , kinyarwanda = Just "Uracyari gufata imiti igabanya ubukana bwa virusi itera SIDA"
            , kirundi = Just "Uracafata ama ARV"
            }

        TreatmentReviewTask forModeratePreeclamsia task ->
            case task of
                TreatmentReviewPrenatalMedication ->
                    { english = "Prenatal Medication"
                    , kinyarwanda = Just "Imiti yo gufata mu gihe utwite"
                    , kirundi = Just "Imiti yo gufat amu gihe cy'imbanyi"
                    }

                TreatmentReviewHIV ->
                    { english = "HIV Medication"
                    , kinyarwanda = Just "Imiti ya Virusi Itera SIDA"
                    , kirundi = Just "Imiti ya Umugera wa SIDA"
                    }

                TreatmentReviewHypertension ->
                    if forModeratePreeclamsia then
                        { english = "Moderate Preeclamsia Medication"
                        , kinyarwanda = Just "Imiti Preklampusi Yoroheje"
                        , kirundi = Just "Imiti y'ingorane y'imbanyi bisanzwe"
                        }

                    else
                        { english = "Hypertension Medication"
                        , kinyarwanda = Just "Imiti y'Umuvuduko w'Amaraso"
                        , kirundi = Just "Imiti y'umuvuduko urenze w'amaraso"
                        }

                TreatmentReviewMalaria ->
                    { english = "Malaria Medication"
                    , kinyarwanda = Just "Imiti ya Malariya"
                    , kirundi = Just "Imiti ya Malariya"
                    }

                TreatmentReviewAnemia ->
                    { english = "Anemia Medication"
                    , kinyarwanda = Just "Imiti ivura indwara y'Amaraso make"
                    , kirundi = Just "Imiti ivura igabanuka ry'amaraso"
                    }

                TreatmentReviewSyphilis ->
                    { english = "Syphilis Medication"
                    , kinyarwanda = Just "Imiti ya Mburugu"
                    , kirundi = Just "Imiti ya Syphilis"
                    }

        TreatmentReviewWarningPopupMessage ->
            { english = "Patient non-adherent"
            , kinyarwanda = Just "Uyu murwayi ntabwo yubahiriza gahunda yo kunywa imiti uko bisabwa"
            , kirundi = Just "Umugwayi atubahiriza ivyo bamubwiye"
            }

        TreatmentReviewWarningPopupInstructions ->
            { english = "Further evaluation necessary"
            , kinyarwanda = Just "Gusuzuma byimbitse"
            , kirundi = Just "Umugwayi atubahiriza ivyo bamubwiye"
            }

        TrySyncing ->
            { english = "Try syncing with backend"
            , kinyarwanda = Just "Gerageza guhuza amakuru y'iki gikoresho cy'ikoranabuhanga n'abakoze E-Heza"
            , kirundi = Nothing
            }

        TuberculosisPast ->
            { english = "Tuberculosis in the past"
            , kinyarwanda = Just "Yigeze kurwara igituntu"
            , kirundi = Just "Akahise k'igituntu"
            }

        TuberculosisPresent ->
            { english = "Tuberculosis in the present"
            , kinyarwanda = Just "Arwaye igituntu"
            , kirundi = Just "Akubu k'igituntu"
            }

        TuberculosisInstructions ->
            { english = "Follow TB protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kuvura igitintu"
            , kirundi = Just "Gukurikiza Inyandiko Ntumberezo zerekeye Igituntu"
            }

        TuberculosisInstructionsFollowed ->
            { english = "followed TB protocols"
            , kinyarwanda = Just "Hakurikijwe amabwiriza yo kuvura igitintu"
            , kirundi = Just "Inyandiko Ntumberezo zerekeye Igituntu zakurikijwe"
            }

        TuberculosisWarning ->
            { english = "Patient is high risk for active Tuberculosis"
            , kinyarwanda = Just "Umubyeyi afite ibyago byinshi byo kuba afite igituntu"
            , kirundi = Just "Umugwayi afise ingorane iri hejuru y'ingwara y'igituntu"
            }

        TwoVisits ->
            { english = "Two visits"
            , kinyarwanda = Just "Inshuro ebyiri"
            , kirundi = Just "Ingendo zibiri"
            }

        Type ->
            { english = "Type"
            , kinyarwanda = Just "Ubwoko bw'Urukingo"
            , kirundi = Just "Ubwoko"
            }

        UbudeheLabel ->
            { english = "Ubudehe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UbudeheNumber ubudehe ->
            case ubudehe of
                Ubudehe1 ->
                    { english = "1"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ubudehe2 ->
                    { english = "2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ubudehe3 ->
                    { english = "3"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Ubudehe4 ->
                    { english = "4"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoUbudehe ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        UndeterminedDiagnoses ->
            { english = "Undetermined Diagnoses"
            , kinyarwanda = Just "Uburwayi ntibusobanutse"
            , kirundi = Just "Isuzuma ritamenyekana"
            }

        UndeterminedDiagnosisMessage ->
            { english = "undetermined diagnosis - followed Post-Partum Protocols"
            , kinyarwanda = Just "Uburwayi ntibusobanutse - hakurikijwe mabwiriza yo kwita ku mubyeyi wabyaye"
            , kirundi = Just "Isuzuma ritamenyekana - Kurikiza inyandikondongozi zerekeye inyuma y'ivyara"
            }

        UnitCopiesPerMM3 ->
            { english = "copies/mm3"
            , kinyarwanda = Just "Kopi/mm3"
            , kirundi = Just "Ikopi/mm3"
            }

        UnitGramsPerDeciliter ->
            { english = "g/dL"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnitInternationalUnitsPerLiter ->
            { english = "IU/L"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnitCentimeter ->
            { english = "cm"
            , kinyarwanda = Just "cm"
            , kirundi = Just "cm"
            }

        UnitMilliGramsPerDeciliter ->
            { english = "mg/dL"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnitMillimeter ->
            { english = "mm"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnitMillimolesPerLiter ->
            { english = "mmol/L"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnitOfMeasurement unit ->
            case unit of
                UnitMmolL ->
                    translationSet UnitMillimolesPerLiter

                UnitMgdL ->
                    translationSet UnitMilliGramsPerDeciliter

        UniversalInterventions ->
            { english = "Universal Interventions"
            , kinyarwanda = Just "Ibikorwa bifasha umwana muri rusange"
            , kirundi = Just "Intabaro zizwi hose"
            }

        Update ->
            { english = "Update"
            , kinyarwanda = Just "Kuvugurura"
            , kirundi = Just "Gushira kugihe"
            }

        UpdateError ->
            { english = "Update Error"
            , kinyarwanda = Just "ikosa mwivugurura"
            , kirundi = Just "Gushira kugihe ikosa"
            }

        Uploading ->
            { english = "Uploading"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UrineDipstickTestLabel variant ->
            case variant of
                VariantShortTest ->
                    { english = "Urine Dipstick Short"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo bike"
                    , kirundi = Just "Ugupima umukoyo na Dipstick ngufi"
                    }

                VariantLongTest ->
                    { english = "Urine Dipstick Long"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    , kirundi = Just "Ugupima umukoyo na Dipstick ndende"
                    }

        UrineDipstickTestVariant variant ->
            case variant of
                VariantShortTest ->
                    { english = "Short Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo bike"
                    , kirundi = Just "Ugupima umukoyo na Dipstick ngufi"
                    }

                VariantLongTest ->
                    { english = "Long Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    , kirundi = Just "Ugupima umukoyo na Dipstick ndende"
                    }

        UrinaryTractInfectionRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Urinary Tract Infection"
            , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
            , kirundi = Just "Uyu mugwayi yerekana ibimenyetso vy'ingwara y'imiringoti y'umukoyo"
            }

        UrinaryTractInfectionRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Just "Hitamo imiti n'igipimo/ibipimo (idoze) uzotanga k'umugwayi"
            }

        UrinaryTractInfectionRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            , kirundi = Just "Umenye neza ko umugwayi afashe imiti itamumerera nabi imbere yo kuyimwandikira"
            }

        UterineMyoma ->
            { english = "Uterine Myoma"
            , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
            , kirundi = Just "Ibivyimba vyo mu gitereko"
            }

        VaccinationStatus status ->
            case status of
                StatusBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Ntibyakozwe"
                    , kirundi = Just "Inyuma"
                    }

                StatusCompleted ->
                    { english = "Completed"
                    , kinyarwanda = Just "Byarakozwe"
                    , kirundi = Just "Vyujujwe"
                    }

                StatusUpToDate ->
                    { english = "Up To Date"
                    , kinyarwanda = Just "Biri ku gihe"
                    , kirundi = Just "Kugeza kuri iyi tarike"
                    }

        VaccinationNoDosesAdministered ->
            { english = "There are no recorded immunizations for this patient"
            , kinyarwanda = Just "Nta makuru ku nkigo agaragara"
            , kirundi = Just "Hano nta ncanco zanditswe kuri uyu mugwayi"
            }

        VaccineDoseAdministeredPreviouslyPrenatalQuestion vaccineType ->
            { english = "Did the patient receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Uyu mubyeyi yaba yarabonye urukingo rw'" ++ vaccineType ++ "bakaba batarabyanditse"
            , kirundi = Just <| "Mbega umugwayi yararonse incanco y' " ++ vaccineType ++ " yuyu munsi, zitanditswe aho hejuru."
            }

        VaccineDoseAdministeredPreviouslyWellChildQuestion vaccineType ->
            { english = "Did the child receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Umwana yaba yarabonye " ++ vaccineType ++ " bakaba batarabyanditse"
            , kirundi = Just <| "Mbega umwana yararonse incanco " ++ vaccineType ++ " zitanditswe aho hejuru"
            }

        VaccineDoseAdministeredTodayPrenatalQuestion vaccineType ->
            { english = "Will the patient receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umubyeyi arahabwa urukingo rw'" ++ vaccineType ++ " uyu munsi"
            , kirundi = Just <| "Mbega umuvyeyi araronka urucanco " ++ vaccineType ++ " uno munsi"
            }

        VaccineDoseAdministeredTodayWellChildQuestion vaccineType ->
            { english = "Will the child receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umwana arahabwa " ++ vaccineType ++ " uyu munsi"
            , kirundi = Just <| "Mbega umwana araronka urucanco " ++ vaccineType ++ " uno munsi"
            }

        VaccineType site vaccineType ->
            case vaccineType of
                WellChildVaccine wellChildVaccineType ->
                    case wellChildVaccineType of
                        VaccineBCG ->
                            { english = "BCG Bacilius Calmette - Guérin Vaccine (BCG)"
                            , kinyarwanda = Just "Urukingo rw'igituntu"
                            , kirundi = Just "Urucanco rwa BCG (Bacilius Calmette - Guérin)"
                            }

                        VaccineOPV ->
                            { english = "Oral Polio Vaccine (OPV)"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                            , kirundi = Just "Uraconco rw'ubukangwe mu kanwa"
                            }

                        VaccineDTP ->
                            case site of
                                SiteBurundi ->
                                    { english = "Pentavalent Vaccine"
                                    , kinyarwanda = Nothing
                                    , kirundi = Just "Urucanco rwa DTC (Diphtérie-Tétanos-Coqueluche)- HepB - Hib"
                                    }

                                _ ->
                                    { english = "DTP - HepB - Hib Vaccine"
                                    , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                                    , kirundi = Nothing
                                    }

                        VaccineDTPStandalone ->
                            { english = "DTP Vaccine (4-th dose)"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        VaccinePCV13 ->
                            { english = "Pneumoccocal Vaccine (PCV 13)"
                            , kinyarwanda = Just "Urukingo rw'umusonga"
                            , kirundi = Just "Urucanco rw'umusonga (PCV 13)"
                            }

                        VaccineRotarix ->
                            { english = "Rotavirus (Rotarix) Vaccine"
                            , kinyarwanda = Just "Urukingo rw'impiswi"
                            , kirundi = Just "Urucanco rwa rotavirus (Rotarix)"
                            }

                        VaccineIPV ->
                            { english = "Inactivated Polio Vaccine"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                            , kirundi = Just "Urucanco rw'ubukangwe rudakora"
                            }

                        VaccineMR ->
                            { english = "Measles-Rubella Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                            , kirundi = Just "Urucanco gw'Agasama"
                            }

                        VaccineHPV ->
                            { english = "HPV Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Inkondo y'umura"
                            , kirundi = Just "Urucanco rwa VHP"
                            }

                PrenatalVaccine prenatalVaccineType ->
                    case prenatalVaccineType of
                        VaccineTetanus ->
                            { english = "Tetanus"
                            , kinyarwanda = Just "Agakwega"
                            , kirundi = Just "Rudadaza"
                            }

        VaginalExamination ->
            { english = "Vaginal Examination"
            , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina"
            , kirundi = Just "Igipimo c'igitsina"
            }

        VaginalExamSign sign ->
            case sign of
                FoulSmellingLochia ->
                    { english = "Foul Smelling Lochia"
                    , kinyarwanda = Just "Ibisanza binuka"
                    , kirundi = Just "Uguhema nabi kwa Lochia"
                    }

                ExcessiveVaginalBleeding ->
                    { english = "Bleeding"
                    , kinyarwanda = Just "Kuva"
                    , kirundi = Just "Ukuva amaraso"
                    }

                NormalVaginalExam ->
                    { english = "Normal"
                    , kinyarwanda = Just "Bisanzwe"
                    , kirundi = Just "Bisanzwe"
                    }

        ValidationErrors ->
            { english = "Validation Errors"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        -- As in, the version the app
        Version ->
            { english = "Version"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        View ->
            { english = "View"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ViewProgressReport ->
            { english = "View Progress Report"
            , kinyarwanda = Just "Raporo y’ibyakozwe"
            , kirundi = Just "Raba neza iterambere ry'ivyegeranyo"
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Just "Umudugudu"
            , kirundi = Just "Ikigwati"
            }

        VitaminAWarningPopupMessage ->
            { english = "Patient did not recieve Vitamin A"
            , kinyarwanda = Nothing
            , kirundi = Just "Umugwayi ntiyaronse Vitamine A"
            }

        WaitForVitalsRecheckHelper ->
            { english = "Patient needs to return in 2 hours to confirm blood pressure. Instruct the patient to wait until called for further testing."
            , kinyarwanda = Just "Umurwayi agomba kugaruka mu masaha 2 kugira ngo twemeze neza umuvuduko w'amaraso. Saba umurwayi kwihangana kugeza umuhamagaye kugira ngo yongere asuzumwe."
            , kirundi = Just "Umugwayi arategerezwa kugaruka mu masaha 2 kugira amenye ivyerekeye Ivuduka ry'amaraso. Bwira umugwayi arindire gushika bamuhamagaye kugira agirishe ibindi bipimo birengako."
            }

        WaitForLabsResultsHelper ->
            { english = "Patient has labs pending. Instruct the patient to wait until called for lab results and further diagnoses."
            , kinyarwanda = Just "Umurwayi afite ibisubizo ategereje bya Laboratwari. Musabe gutegereza kugeza umuhamagaye ngo afate ibisubizo anamenye indwara afite."
            , kirundi = Just "Umugwayi afise ibipimo bikiriko birihwezwa. Bwira umugwayi wawe yihanganye gushika bamuhamagaye aze gutora inyishu z'ibipimo hamwe n'ibindi basuzumye"
            }

        WaitInstructions ->
            { english = "To proceed with more encounters while you wait for test results or a vitals recheck, touch \"Pause Encounter\" below to leave this encounter. You can return to it from the case management screen."
            , kinyarwanda = Just "Kugira ngo ukomeze ufate andi masuzuma menshi mu gihe utegereje ibisubizo cyangwa se gusubiramo ibipimo by'ubuzima, kanda mu nsi hano ahanditse ngo \" Ba uhagaritse igikorwa\" kugira ngo usohoke kuri iri suzuma. Ushobora kurigarukaho unyuze ku kibaho cyo Kuvura uburwayi."
            , kirundi = Just "Kugira ubandanye n'iyindi mibonano mu gihe ukirindiriye inyishu z'ibipimo canke ibindi basuzuma vy'amagara, fyonda ahari \"hagarika umubonano\" aha hasi kugira uwuvemwo. Urashobora kuhagaruka uravye ahari ama dosiye"
            }

        Warning ->
            { english = "Warning"
            , kinyarwanda = Just "Impuruza"
            , kirundi = Just "Impanuro"
            }

        WasFbfDistirbuted activity ->
            case activity of
                ChildActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    , kirundi = Just "Nimba igipimo catanzwe kidahuye n'intumbero zanditse zico gikorwa, hitamwo impamvu"
                    }

                MotherActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    , kirundi = Just "Nimba igipimo catanzwe kidahuye n'intumbero zanditse zico gikorwa, hitamwo impamvu"
                    }

        WeekSinglePlural value ->
            if value == 1 then
                { english = "1 Week"
                , kinyarwanda = Just "1 Icyumweru"
                , kirundi = Just "1 Indwi"
                }

            else
                { english = String.fromInt value ++ " Weeks"
                , kinyarwanda = Just <| String.fromInt value ++ " Ibyumweru"
                , kirundi = Just <| String.fromInt value ++ " Indwi"
                }

        Weight ->
            { english = "Weight"
            , kinyarwanda = Just "Ibiro"
            , kirundi = Just "Uburemere"
            }

        WelcomeUser name ->
            { english = "Welcome " ++ name
            , kinyarwanda = Just <| "Murakaza neza " ++ name
            , kirundi = Just <| "Ikaze " ++ name
            }

        Wellbeing ->
            { english = "Wellbeing"
            , kinyarwanda = Nothing
            , kirundi = Just "Imibereho myiza"
            }

        WellChildActivityTitle activity ->
            case activity of
                WellChildDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    , kirundi = Just "Ibimenyetso vy'akaga"
                    }

                WellChildNutritionAssessment ->
                    translationSet NutritionAssessmentLabel

                WellChildECD ->
                    { english = "ECD"
                    , kinyarwanda = Just "Kwita ku mikurire y'abana bato"
                    , kirundi = Just "Iterambere mu gukura ry'Umwana Mutoyi (DPE -IUM)"
                    }

                WellChildMedication ->
                    { english = "Medication"
                    , kinyarwanda = Just "Gufata imiti"
                    , kirundi = Just "Gufata Imiti"
                    }

                WellChildPregnancySummary ->
                    { english = "Birth History"
                    , kinyarwanda = Just "Amakuru y'uko yavutse"
                    , kirundi = Just "Akahise k'amavuka"
                    }

                WellChildImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Just "Incanco"
                    }

                WellChildNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Just "Intambwe zikurkira"
                    }

                WellChildPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Just "Ifoto"
                    }

                WellChildNCDA ->
                    translationSet ChildScorecard

                WellChildHomeVisit ->
                    translationSet HomeVisit

        WellChildDangerSignsTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskSymptomsReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Just "Isubiramwo ry'ikimenyetso"
                    }

                Pages.WellChild.Activity.Types.TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    , kirundi = Just "Ivyangombwa"
                    }

        WellChildEncounterPopup popupType ->
            case popupType of
                PopupDangerSigns ->
                    { english = "Child shows signs of acute illness. Please close this encounter and continue in an “Acute Illness” encounter immediately."
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umwana afise ibimenyetso vy'ingwara ikomeye. Hagarika ivyerekeye ukubonana canke ukuganira ahubwo ihutire kuvura iyoingwara ikomeye ako kanya nyene."
                    }

                PopupECD ecdPopupType ->
                    case ecdPopupType of
                        ChildBehind ->
                            { english = "Child is behind on ECD milestones. Continue to monitor the child and provide anticipatory guidance to the caregiver."
                            , kinyarwanda = Nothing
                            , kirundi = Just "Umwana ari inyuma y'ibice bikomakomeye vya DPE (Ugutera imbere mu gukura k'umwana). Bandanya ucungera umwana kandi utanga impanuro zibereye k'umuvyeyi."
                            }

                        ReferToSpecialist ->
                            { english = "Child is behind on ECD milestones. Refer the child to a specialist."
                            , kinyarwanda = Nothing
                            , kirundi = Just "Umwana yacerewe canke ari inyuma y'ibice bikomakomeye vya DPE (Ugutera imbere mu gukura k'umwana). Rungika umwana k'umuganga yanonosoye ivy'abana."
                            }

        WellChildECDMilestoneForDiagnosisPane encounterType ->
            case encounterType of
                Milestone6Weeks ->
                    { english = "1.5 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone14Weeks ->
                    { english = "3.5 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone6Months ->
                    { english = "6 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone9Months ->
                    { english = "9 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone12Months ->
                    { english = "12 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone15Months ->
                    { english = "15 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone18Months ->
                    { english = "18 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone2Years ->
                    { english = "24 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone3Years ->
                    { english = "36 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Milestone4Years ->
                    { english = "48 Mo"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        WellChildMacrocephalyWarning ->
            { english = "Child shows signs of macrocephaly, follow hydrocephalus protocol. Please refer to a specialist if concerned for genetic syndrome or other problems."
            , kinyarwanda = Nothing
            , kirundi = Just "Umwana afise ibimenyetso vyo kugira umutwe munini, kurikiza ivyanditswe bitanga intumbero (Protocole) yo gukura amazi mu mutwe. Rungika umwana k'umuhinga nimba ubona izindi ngorane z'amavukiro."
            }

        WellChildMicrocephalyWarning ->
            { english = "Child shows signs of microcephaly.c, nutritional, and genetic problems.  Please refer to a specialist if concerned for genetic syndrome or other problems."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WellChildImmunisationDescription site task ->
            case task of
                VaccineBCG ->
                    { english = "BCG protects your child from getting the worst complications of tuberculosis, which can affect the lungs and could be deadly for young children."
                    , kinyarwanda = Just "Urukingo rw'igituntu rurinda umwana ibyago byo kuba yakwandura igituntu, ndeste nibyago byashamikiraho bishobora kwibasira ibihaha, ibi bikaba byanahitana umwana akiri muto."
                    , kirundi = Just "Urucanco rwa BCG rukinga umwana wawe ingorane/inkurikizi zikomeye z'Igituntu, ingwara ishobora gufata/guhitana amahaha kandi ishobora kwica abana bakiri bato"
                    }

                VaccineDTP ->
                    { english = "Prevents the child from getting lockjaw (Tetanus), whooping cough (Pertussis), liver failure (Hepatitis B), breathing problems and fever (Diptheria)."
                    , kinyarwanda = Just "Rurinda umwana indwara ya agakwega, kokolishe, umwijima wo mubwoko bwa B, n'ibibazo, ibibazo byo guhumeka n'umuriro (Akaniga)."
                    , kirundi = Just "Gukingira umwana ko yandura Tetanusi, Kokerishe, Ingwara y'Igitigu (Hépatite B), Ingorane zo guhema nabi hamwe n'ubushuhe bita Diphtérie."
                    }

                VaccineDTPStandalone ->
                    { english = "Prevents the child from getting lockjaw (Tetanus), whooping cough (Pertussis), breathing problems and fever (Diptheria)."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV prevents certain types of cancer from developing in your child."
                    , kinyarwanda = Just "Rurinda umwana kurwara zimwe muri kanseri"
                    , kirundi = Just "VHP irahagarika ubwoko bumwe bumwe bwa Kansere gukwiragira mu mubiri w'umwana"
                    }

                VaccineIPV ->
                    { english = "Is the final vaccine to prevent Polio in children. IPV boosts the effects of the previous polio vaccines your child received."
                    , kinyarwanda = Just "Ni urukingo rwa nyuma rw'imbasa ku bana, rwongerera imbaraga / rushimangira inkingo z'imbasa yabonye mbere."
                    , kirundi = Just "Ni urucanco rwanyuma go gukinga ubukangwe ku bana. Urucanco rwa VPI rurongereza inguvu ku ncanco zaheze/zatanzwe zirwanya ubukangwe abana baronse."
                    }

                VaccineMR ->
                    { english = "Prevents the child from contracting a highly contagious viral infection that causes a fever, lesions, and diarrhea. MR is very dangerous for pregnant women, causing miscarriage or birth defects. Vaccinating your child prevents the spread of the disease in the community."
                    , kinyarwanda = Nothing
                    , kirundi = Just "Gukingira umwana ko yandura indwara zimutera umushuhe, impinduka z'umubiri, uguhitwa/ugicibwamwo. MR nimbi cane ku bavyeyi bibungenze, kuko ituma imbanyi zikoroka canke abana babumbwa ukundi. Gucandagisha umwana bimukingira ingwara kandi zigakingwa no mu karere abamwo"
                    }

                VaccineOPV ->
                    { english = "OPV prevents the child from contracting the Polio Virus, which affects the spinal cord and can cause paralysis."
                    , kinyarwanda = Just "Uru rukingo rurinda umwana kwandura Virusi itera indwara y'imbasa, iyo virusi ifata ururenda ruba mu ruti rw'umugongo bigatera umwana ubumuga bw'ingingo (Amaguru cg amaboko)."
                    , kirundi = Just "Urucanco rw'ubukangwe rubuza ko umwana yandura umugera w'ubukangwe, uwo mugera utera ingwara mu giti/ruti rw'umugongo hama hagashobora kuziramwo ubumuga (Paralysie)"
                    }

                VaccinePCV13 ->
                    { english = "Protects against any disease caused by a specific bacteria that can lead to lung infections."
                    , kinyarwanda = Just "Rurinda umwana indwara ziterwa n'udukoko twangiza ibihaha."
                    , kirundi = Just "Rurakinga ingwara iyariyo yose itewe n'umugera kanaka utera ingwara zitandukanye zo mu mahaha"
                    }

                VaccineRotarix ->
                    case site of
                        SiteRwanda ->
                            { english = "Protects against diarrhea caused by the Rotavirus. Diarrhea is the 3rd leading cause of death of children in Rwanda."
                            , kinyarwanda = Just "Rurinda umwana impiswi ziterwa n'udukoko twa rotavirusi. Impiswi ni impamvu ya gatatu itera imfu z'abana mu Rwanda."
                            , kirundi = Nothing
                            }

                        _ ->
                            { english = "Protects against diarrhea caused by the Rotavirus."
                            , kinyarwanda = Just "Rurinda umwana impiswi ziterwa n'udukoko twa rotavirusi."
                            , kirundi = Just "Rurakinga ugucibwamwo/uguhitwa bitewe na Rotavirus."
                            }

        WellChildImmunisationDosage site task ->
            case task of
                VaccineBCG ->
                    { english = "There is one dose of BCG and it is given at birth."
                    , kinyarwanda = Just "Urukingo rw'igituntu rutangwa inshuro imwe umwana akimara kuvuka."
                    , kirundi = Just "Hari idoze 1 y'urucanco rwa BCG (rukinga Igituntu) rukaba rutangwa umwana akivuka"
                    }

                VaccineDTP ->
                    case site of
                        SiteBurundi ->
                            { english = "There are 3 doses of Pentavalent - 6 weeks, 10 weeks, and 14 weeks."
                            , kinyarwanda = Nothing
                            , kirundi = Just "Hari idoze 3 vy'urucanco rwa DTC-HepB-Hib: indwi 6, indwi 10 hamwe n'indwi 14"
                            }

                        _ ->
                            { english = "There are 3 doses of DTP-HepB-Hib - 6 weeks, 10 weeks, and 14 weeks."
                            , kinyarwanda = Just "Umwana ahabwa inshuro eshatu inkingo zikurikira:(urukingo rw'agakwega, Hepatite yo mubwoko bwa B, nigihuka) yujuje ibyumweru 6, ibyumweru 10, no ku byumweru 14."
                            , kirundi = Nothing
                            }

                VaccineDTPStandalone ->
                    { english = "This is the 4-th dose of DTP - 18 months."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "There are 2 doses of HPV - at 12 years and 12.5 years."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'inkondo y'umura inshuro 2 - ku myaka 12 n'imyaka 12.5."
                    , kirundi = Just "Hari idoze 2 vy'urucanco rwa HPV-ku myaka 12 hamwe no ku myaka 12,5"
                    }

                VaccineIPV ->
                    { english = "There is only one dose of the inactivated vaccine at 14 weeks."
                    , kinyarwanda = Just "Uru rukingo aruhabwa inshuro imwe gusa ku byumweru 14."
                    , kirundi = Just "Hano hari igipimo kimwe gusa/Idoze imwe gusa y'urucanco itegeze ikora mu gihe c'indwi 14"
                    }

                VaccineMR ->
                    case site of
                        SiteBurundi ->
                            { english = "There are 2 doses of Measles-Rubella - at 9 months and 18 months."
                            , kinyarwanda = Nothing
                            , kirundi = Just "Hari idoze 2 vy'urucanco rw'Agasama-ku mezi 9 hamwe no ku mezi 18"
                            }

                        _ ->
                            { english = "There are 2 doses of Measles-Rubella - at 9 months and 15 months."
                            , kinyarwanda = Just "Umwana ahabwa urukingo rw'Iseru na Rubeyole inshuro 2: Afite Amezi 9, n'amezi 15."
                            , kirundi = Nothing
                            }

                VaccineOPV ->
                    { english = "There are 4 doses of OPV - at birth, 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'imbasa inshuro 4:Akivuka, ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    , kirundi = Just "Hari idoze 4 vy'urucanco rwa VPO: avutse,  indwi 6, indwi 10 hamwe n'indwi 14"
                    }

                VaccinePCV13 ->
                    { english = "There are 3 doses of PCV 13 - 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'umusonga inshuro 3:Ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    , kirundi = Just "Hari idoze 3 vy'urucanco rwa PCV 13 - indwi 6, indwi 10 hamwe n'indwi 14"
                    }

                VaccineRotarix ->
                    { english = "There are 2 doses of Rotarix - 6 weeks and 10 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'impiswi inshuro 2:Ku byumweru 6, no ku byumweru 10."
                    , kirundi = Just "Hari idoze 2 vy'urucanco rwa Rotarix-ku ndwi 6 hamwe no ku ndwi 10"
                    }

        WellChildImmunisationHeader task ->
            case task of
                VaccineBCG ->
                    { english = "Bacillus Calmette - Guérin (BCG)"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Just "Urucanco rwa BCG (Bacilius Calmette - Guérin)"
                    }

                VaccineDTP ->
                    { english = "Diptheria, Hepatitis B, Tetanus, and Pertussis"
                    , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Urucanco rwa DTC (Diphtérie-Tétanos-Coqueluche)- HepB - Hib"
                    }

                VaccineDTPStandalone ->
                    { english = "Diptheria, Tetanus and Pertussis"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "Human Papillomavirus (HPV)"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    , kirundi = Just "Urucanco rwa VHP"
                    }

                VaccineIPV ->
                    { english = "Inactivated Polio Vaccine (IPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Just "Urucanco rw'ubukangwe rudakora"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella (MR)"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Just "Urucanco gw'Agasama"
                    }

                VaccineOPV ->
                    { english = "Oral Polio Vaccine (OPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                    , kirundi = Just "Uraconco rw'ubukangwe mu kanwa"
                    }

                VaccinePCV13 ->
                    { english = "Pneumococcal Vaccine (PCV 13)"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Just "Urucanco rw'umusonga (PCV 13)"
                    }

                VaccineRotarix ->
                    { english = "Rotavirus Vaccine (Rotarix)"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Just "Urucanco rwa rotavirus (Rotarix)"
                    }

        WellChildImmunizationHistory site task ->
            case task of
                VaccineBCG ->
                    { english = "BCG History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'igituntu"
                    , kirundi = Just "Akahise ka BCG"
                    }

                VaccineDTP ->
                    case site of
                        SiteBurundi ->
                            { english = "Pentavalent History"
                            , kinyarwanda = Nothing
                            , kirundi = Just "Akahise ka DTC - HepB - Hib"
                            }

                        _ ->
                            { english = "DTP - HepB - Hib History"
                            , kinyarwanda = Just "Amakuru kuri DTP - HepB - Hib"
                            , kirundi = Nothing
                            }

                VaccineDTPStandalone ->
                    { english = "DTP History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV History"
                    , kinyarwanda = Just "Amakuru ku rukingo rw'inkondo y'umura"
                    , kirundi = Just "Akahise ka VHP"
                    }

                VaccineIPV ->
                    { english = "IPV History"
                    , kinyarwanda = Just "Amakuru k' Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Just "Akahise k'uracanco rw'Ubukangwe"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella History"
                    , kinyarwanda = Just "amakuru k'Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Just "Akahise k'Agasama"
                    }

                VaccineOPV ->
                    { english = "OPV History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'imbasa rutangwa mu kanwa"
                    , kirundi = Just "Akahise k'Urucanco gw'Ubukangwe"
                    }

                VaccinePCV13 ->
                    { english = "PCV 13 History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'umusonga"
                    , kirundi = Just "Akahise ka PCV 13 Urucanco rw'umusonga"
                    }

                VaccineRotarix ->
                    { english = "Rotarix History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'impiswi"
                    , kirundi = Just "Akahise ka Rotrarix"
                    }

        WellChildHomeVisitTask task ->
            case task of
                TaskFeeding ->
                    translationSet Feeding

                TaskCaring ->
                    translationSet Caring

                TaskHygiene ->
                    translationSet Hygiene

                TaskFoodSecurity ->
                    translationSet FoodSecurity

        WellChildImmunisationTask site task ->
            case task of
                Measurement.Model.TaskBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskDTP ->
                    case site of
                        SiteBurundi ->
                            { english = "Pentavalent"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        _ ->
                            { english = "DTP - HepB - Hib"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                Measurement.Model.TaskDTPStandalone ->
                    { english = "DTP "
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    , kirundi = Just "VHP"
                    }

                Measurement.Model.TaskIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Just "VPI"
                    }

                Measurement.Model.TaskMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Just "Agasama"
                    }

                Measurement.Model.TaskOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    , kirundi = Just "Urucanco gw'Ubukangwe"
                    }

                Measurement.Model.TaskPCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Just "Urucanco rw'umusonga"
                    }

                Measurement.Model.TaskRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskOverview ->
                    { english = "Overview"
                    , kinyarwanda = Just "Ishusho Rusange"
                    , kirundi = Just "Incamake"
                    }

        WellChildMedicationTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskAlbendazole ->
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskMebendezole ->
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskVitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        WellChildNextStepsTask isChw task ->
            case task of
                TaskContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Just "Ivyazanye intererano"
                    }

                TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Just "Inyigisho z'amagara"
                    }

                TaskSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Just "Rungika kw'ivuriro"
                        }

                    else
                        { english = "Refer to Program"
                        , kinyarwanda = Nothing
                        , kirundi = Nothing
                        }

                TaskFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Just "Kurikirana"
                    }

                TaskNextVisit ->
                    { english = "Next Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        WellChildSymptom symptom ->
            case symptom of
                SymptomBreathingProblems ->
                    { english = "Breathing problems"
                    , kinyarwanda = Just "Ibibazo bijyanye no guhumeka"
                    , kirundi = Just "Ingorane zo guhema nabi"
                    }

                SymptomConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Just "Ukujugumira"
                    }

                SymptomLethargyOrUnresponsiveness ->
                    { english = "Lethargy or unresponsiveness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    , kirundi = Just "Itiro rirenze canke ukuraba/uguta ubwenge"
                    }

                SymptomDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    , kirundi = Just "Uguhitwa"
                    }

                SymptomVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    , kirundi = Just "Ukudahwa"
                    }

                SymptomUmbilicalCordRedness ->
                    { english = "Umbilical Cord Redness"
                    , kinyarwanda = Just "Guhisha k'umukondo"
                    , kirundi = Nothing
                    }

                SymptomStiffNeckOrBulgingFontanelle ->
                    { english = "Stiff neck or bulging fontanelle"
                    , kinyarwanda = Just "Kugagara ibikanu cyangwa igihorihori kibyimbye"
                    , kirundi = Nothing
                    }

                SymptomSevereEdema ->
                    { english = "Severe Edema"
                    , kinyarwanda = Just "Kubyimba bikabije (Cyane ibirenge cg intonki)"
                    , kirundi = Just "Kuvyimba cane"
                    }

                SymptomPalmoplantarPallor ->
                    { english = "Palmoplantar pallor"
                    , kinyarwanda = Just "Kweruruka mu biganza no mu bworo bw'ibirenge"
                    , kirundi = Nothing
                    }

                SymptomHistoryOfFever ->
                    { english = "History of fever"
                    , kinyarwanda = Just "Amakuru yerekeye umuriro yagize mu bihe byashize"
                    , kirundi = Just "Akahise k'ubushuhe"
                    }

                SymptomBabyTiresQuicklyWhenFeeding ->
                    { english = "Baby tires quickly when feeding"
                    , kinyarwanda = Just "Umwana ahita ananirwa iyo atangiye kurya"
                    , kirundi = Nothing
                    }

                SymptomCoughingOrTearingWhileFeeding ->
                    { english = "Coughing/tearing while feeding (<6 months)"
                    , kinyarwanda = Just "Gukorora/Kwiriza iyo atangiye kurya (munsi y'amezi 6)"
                    , kirundi = Nothing
                    }

                SymptomRigidMusclesOrJawClenchingPreventingFeeding ->
                    { english = "Rigid muscles/jaw clenching that prevents feeding"
                    , kinyarwanda = Just "Imikaya ireze/amatama afunganye bikamubuza kurya"
                    , kirundi = Nothing
                    }

                ExcessiveSweatingWhenFeeding ->
                    { english = "Excessive sweating when feeding"
                    , kinyarwanda = Just "Kubira ibyuya byinshi iyo ari kurya"
                    , kirundi = Just "Kubira ivyuya vyinshi iyo ari konka"
                    }

                NoWellChildSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Just "Nta nimwe muri izi"
                    }

        WellChildVaccineLabel site vaccineType ->
            case vaccineType of
                VaccineBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    case site of
                        SiteBurundi ->
                            { english = "Pentavalent"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        _ ->
                            { english = "DTP - HepB - Hib"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                VaccineDTPStandalone ->
                    { english = "DTP - HepB - Hib"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Unkondo y'Umura"
                    , kirundi = Just "VHP"
                    }

                VaccineIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Just "VPI"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Just "Agasama"
                    }

                VaccineOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    , kirundi = Just "Urucanco gw'Ubukangwe"
                    }

                VaccinePCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Just "Urucanco rw'umusonga"
                    }

                VaccineRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

        WhatDoYouWantToDo ->
            { english = "What do you want to do?"
            , kinyarwanda = Just "Urashaka gukora iki?"
            , kirundi = Just "Ushaka gukora iki?"
            }

        WhatType ->
            { english = "What type"
            , kinyarwanda = Just "Ubuhe bwoko"
            , kirundi = Just "Ubuhe bwoko"
            }

        WhatWasTheirResponse ->
            { english = "What was their response"
            , kinyarwanda = Just "Ni iki bagusubije"
            , kirundi = Just "Batanze inyishu iyihe"
            }

        WhoCaresForTheChildDuringTheDay ->
            { english = "Who cares for the child during the day"
            , kinyarwanda = Just "Ni inde wita ku mwana ku manywa"
            , kirundi = Just "Ninde araba umwana k'umurango"
            }

        WhoInFamilyHasCondition ->
            { english = "Who in the family has this condition"
            , kinyarwanda = Just "Ni inde mu muryango ufite iki kibazo"
            , kirundi = Just "Ninde mu muryango iwanyu ameze uku/afise ingorane nk'iyi/afise ikibazo nk'iki"
            }

        WhyNot ->
            { english = "Why not"
            , kinyarwanda = Just "Kubera iki"
            , kirundi = Just "Kubera iki"
            }

        WrittenProtocolsFollowed ->
            { english = "Written protocols followed"
            , kinyarwanda = Just "Amabwiriza yanditse yakurikijwe"
            , kirundi = Just "Gukurikiza inyandikondongozi zanditse"
            }

        Year ->
            { english = "Year"
            , kinyarwanda = Just "Umwaka"
            , kirundi = Just "Umwaka"
            }

        YearsOld int ->
            { english = String.fromInt int ++ " years old"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt int
            , kirundi = Just <| "Imyaka " ++ String.fromInt int
            }

        Yes ->
            { english = "Yes"
            , kinyarwanda = Just "Yego"
            , kirundi = Just "Ego"
            }

        Zone ->
            { english = "Zone"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ZScoreHeadCircumferenceForAge ->
            { english = "Z-Score Head Circumference for Age: "
            , kinyarwanda = Just "Z-score ku muzenguruko w'umutwe ugereranije n'imyaka afite: "
            , kirundi = Just "Umuzunguruko w'umutwa wa Score-Z ku myaka: "
            }

        ZScoreHeightForAge ->
            { english = "Z-Score Height for Age: "
            , kinyarwanda = Just "Z-score Uburebure ku myaka: "
            , kirundi = Just "Uburebure bwa Score-Z ku myaka: "
            }

        ZScoreMuacForAge ->
            { english = "MUAC for Age: "
            , kinyarwanda = Just "MUAC ku myaka: "
            , kirundi = Just "CMB ku myaka: "
            }

        ZScoreWeightForAge ->
            { english = "Z-Score Weight for Age: "
            , kinyarwanda = Just "Z-score Ibiro ku myaka: "
            , kirundi = Just "Ibiro vya score z ku myaka: "
            }

        ZScoreWeightForHeight ->
            { english = "Z-Score Weight for Height: "
            , kinyarwanda = Just "Z-score Ibiro ku uburebure: "
            , kirundi = Just "Ibiro vya score z ku burebure: "
            }


translateMyRelatedBy : MyRelatedBy -> TranslationSet String
translateMyRelatedBy relationship =
    case relationship of
        MyChild ->
            { english = "Child"
            , kinyarwanda = Just "Umwana"
            , kirundi = Just "Child"
            }

        MyParent ->
            { english = "Parent"
            , kinyarwanda = Nothing
            , kirundi = Just "Umuvyeyi"
            }

        MyCaregiven ->
            { english = "Care given"
            , kinyarwanda = Nothing
            , kirundi = Just "Urerwa"
            }

        MyCaregiver ->
            { english = "Caregiver"
            , kinyarwanda = Nothing
            , kirundi = Just "umurezi"
            }


{-| Basically, this is backwards. Our data is showing what the second
person is from the first person's point of view, but we want to
ask the question the opposite way.
-}
translateMyRelatedByQuestion : MyRelatedBy -> TranslationSet String
translateMyRelatedByQuestion relationship =
    case relationship of
        MyChild ->
            { english = "is the parent of"
            , kinyarwanda = Just "ni umubyeyi wa"
            , kirundi = Just "Ni umuvyeyi wa"
            }

        MyParent ->
            { english = "is the child of"
            , kinyarwanda = Just "ni umwana wa"
            , kirundi = Just "Ni umwana wa"
            }

        MyCaregiven ->
            { english = "is the caregiver for"
            , kinyarwanda = Just "ni umurezi wa"
            , kirundi = Just "Ni umurezi wa"
            }

        MyCaregiver ->
            { english = "is given care by"
            , kinyarwanda = Just "arerwa na"
            , kirundi = Just "aregwa na"
            }


translateActivePage : Page -> TranslationSet String
translateActivePage page =
    case page of
        DevicePage ->
            { english = "Device Status"
            , kinyarwanda = Just "Uko igikoresho cy'ikoranabuhanga gihagaze"
            , kirundi = Just "Ingene igikoresho kimeze"
            }

        PinCodePage ->
            { english = "PIN Code"
            , kinyarwanda = Just "Umubare w'ibanga"
            , kirundi = Just "Inimero kabanga"
            }

        PageNotFound _ ->
            { english = "Missing"
            , kinyarwanda = Just "Ibibura"
            , kirundi = Just "Ibibura"
            }

        ServiceWorkerPage ->
            { english = "Deployment"
            , kinyarwanda = Nothing
            , kirundi = Just "Irungika"
            }

        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    { english = "Clinical"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ivyo kwa muganga"
                    }

                ClinicsPage ->
                    { english = "Groups"
                    , kinyarwanda = Just "Itsinda"
                    , kirundi = Just "Imirwi"
                    }

                ClinicalProgressReportPage _ _ ->
                    { english = "Clinical Progress Report"
                    , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
                    , kirundi = Just "Icegeranyo c'iterambera mu kuvugwa co kwa muganga"
                    }

                CreatePersonPage _ _ ->
                    { english = "Create Person"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Shiraho umuntu"
                    }

                DashboardPage _ ->
                    { english = "Dashboards"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Imbaho"
                    }

                GlobalCaseManagementPage ->
                    { english = "Case Management"
                    , kinyarwanda = Just "Gukurikirana Umurwayi"
                    , kirundi = Just "Ugucungera ingwara"
                    }

                DemographicsReportPage _ _ ->
                    { english = "Demographics Report"
                    , kinyarwanda = Just "Raporo y'umwirondoro"
                    , kirundi = Just "Icegeranyo c'ibiharuro vy'abantu"
                    }

                EditPersonPage _ ->
                    { english = "Edit Person"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MyAccountPage ->
                    { english = "My Account"
                    , kinyarwanda = Just "Compte"
                    , kirundi = Nothing
                    }

                PersonPage _ _ ->
                    { english = "Person"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PersonsPage _ _ ->
                    { english = "Participant Directory"
                    , kinyarwanda = Just "Ububiko bw'amakuru y'umurwayi"
                    , kirundi = Nothing
                    }

                PrenatalParticipantPage _ _ ->
                    { english = "Antenatal Participant"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                IndividualEncounterParticipantsPage encounterType ->
                    case encounterType of
                        AcuteIllnessEncounter ->
                            { english = "Acute Illness Participants"
                            , kinyarwanda = Just "Abagaragweho n'uburwayi butunguranye"
                            , kirundi = Nothing
                            }

                        AntenatalEncounter ->
                            { english = "Antenatal Participants"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        ChildScoreboardEncounter ->
                            { english = "Child Scorecard Participants"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        HomeVisitEncounter ->
                            { english = "Home Visit Participants"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        InmmunizationEncounter ->
                            { english = "Inmmunization Participants"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        NCDEncounter ->
                            { english = "NCD Participants"
                            , kinyarwanda = Just "Abitabiriye muri Serivise y'indwara zitandura"
                            , kirundi = Nothing
                            }

                        NutritionEncounter ->
                            { english = "Nutrition Participants"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        WellChildEncounter ->
                            { english = "Standard Pediatric Visit Participant"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                RelationshipPage _ _ _ ->
                    { english = "Relationship"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SessionPage _ sessionPage ->
                    case sessionPage of
                        ActivitiesPage ->
                            { english = "Activities"
                            , kinyarwanda = Just "Ibikorwa"
                            , kirundi = Nothing
                            }

                        ActivityPage _ ->
                            { english = "Activity"
                            , kinyarwanda = Just "Igikorwa"
                            , kirundi = Nothing
                            }

                        AttendancePage ->
                            { english = "Attendance"
                            , kinyarwanda = Just "Ubwitabire"
                            , kirundi = Nothing
                            }

                        ParticipantsPage ->
                            { english = "Participants"
                            , kinyarwanda = Just "Abagenerwabikorwa"
                            , kirundi = Nothing
                            }

                        ChildPage _ ->
                            { english = "Child"
                            , kinyarwanda = Just "Umwana"
                            , kirundi = Nothing
                            }

                        MotherPage _ ->
                            { english = "Mother"
                            , kinyarwanda = Just "Umubyeyi"
                            , kirundi = Nothing
                            }

                        NextStepsPage _ _ ->
                            { english = "Next Steps"
                            , kinyarwanda = Just "Ibikurikiyeho"
                            , kirundi = Just "Intambwe zikurkira"
                            }

                        ProgressReportPage _ ->
                            { english = "Progress Report"
                            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
                            , kirundi = Nothing
                            }

                PrenatalEncounterPage _ ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    , kirundi = Just "Umubonano imbere yo kuvyara"
                    }

                PrenatalActivityPage _ _ ->
                    { english = "Antenatal Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalRecurrentEncounterPage _ ->
                    { english = "Antenatal Recurrent Encounter"
                    , kinyarwanda = Just "Kwipimisha inda bigaruka"
                    , kirundi = Nothing
                    }

                PrenatalRecurrentActivityPage _ _ ->
                    { english = "Antenatal Recurrent Activity"
                    , kinyarwanda = Just "Igikorwa cyo kwipimisha inda bigaruka"
                    , kirundi = Nothing
                    }

                IndividualEncounterTypesPage ->
                    { english = "Encounter Types"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PregnancyOutcomePage _ _ ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Inyishu yerekeye imbanyi"
                    }

                NutritionParticipantPage _ _ ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    , kirundi = Nothing
                    }

                NutritionEncounterPage _ ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    , kirundi = Nothing
                    }

                NutritionActivityPage _ _ ->
                    { english = "Nutrition Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionProgressReportPage _ ->
                    { english = "Nutrition Progress Report"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessParticipantPage _ _ ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Isuzuma  ry'uburwayi butunguranye"
                    , kirundi = Just "Guhura n'ingwara ibabaza cane/ikaze"
                    }

                AcuteIllnessEncounterPage _ ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Isuzuma  ry'uburwayi butunguranye"
                    , kirundi = Just "Guhura n'ingwara ibabaza cane/ikaze"
                    }

                AcuteIllnessActivityPage _ _ ->
                    { english = "Acute Illness Activity"
                    , kinyarwanda = Just "Igikorwa cyo kuvura uburwayi butunguranye"
                    , kirundi = Nothing
                    }

                AcuteIllnessProgressReportPage _ _ ->
                    { english = "Acute Illness Progress Report"
                    , kinyarwanda = Just "Raporo y’ibyakozwe ku ndwara zifatiyeho"
                    , kirundi = Nothing
                    }

                AcuteIllnessOutcomePage _ ->
                    { english = "Acute Illness Outcome"
                    , kinyarwanda = Just "Iherezo ry'indwara ifatiyeho"
                    , kirundi = Nothing
                    }

                HomeVisitEncounterPage _ ->
                    { english = "Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo"
                    , kirundi = Just "Umubonano mu gihe co kugendera muhira"
                    }

                HomeVisitActivityPage _ _ ->
                    { english = "Home Visit Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildParticipantPage _ _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    , kirundi = Just "Inama mu gihe c'urugendo rusanzwe go mu gisata kiraba abana"
                    }

                WellChildEncounterPage _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    , kirundi = Just "Inama mu gihe c'urugendo rusanzwe go mu gisata kiraba abana"
                    }

                WellChildActivityPage _ _ ->
                    { english = "Standard Pediatric Visit Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildProgressReportPage _ ->
                    { english = "Progress Report"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDParticipantPage _ _ ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Just "Isuzuma ku Burwayi Butandura"
                    , kirundi = Just "Umubonano werekeye ingwara zitandukira"
                    }

                NCDEncounterPage _ ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Just "Isuzuma ku Burwayi Butandura"
                    , kirundi = Just "Umubonano werekeye ingwara zitandukira"
                    }

                NCDActivityPage _ _ ->
                    { english = "NCD Activity"
                    , kinyarwanda = Just "Igikorwa ku Burwayi Butandura"
                    , kirundi = Just "Igikorwa c'ingwara zitandukira"
                    }

                NCDRecurrentEncounterPage _ ->
                    { english = "NCD Recurrent Encounter"
                    , kinyarwanda = Just "Isuzuma Rigaruka ku Burwayi Butandura"
                    , kirundi = Just "Umubonano ugaruka canke wa minsi yose w'ingwara zandukira"
                    }

                NCDRecurrentActivityPage _ _ ->
                    { english = "NCD Recurrent Activity"
                    , kinyarwanda = Just "Igikorwa Kigaruka ku Burwayi Butandura"
                    , kirundi = Just "Igikorwa kigaruka canke ca minsi c'ingwara zitandukira"
                    }

                NCDProgressReportPage _ ->
                    { english = "NCD Progress Report"
                    , kinyarwanda = Just "Raporo ku Burwayi Butandura"
                    , kirundi = Just "Uguterintambwe mu cegeranyo c'ingwara zitandukira"
                    }

                TraceContactPage _ ->
                    { english = "Trace Contact"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PatientRecordPage _ _ ->
                    { english = "Patient Record"
                    , kinyarwanda = Just "Amakuru y'Umurwayi"
                    , kirundi = Just "Icegeranyo c'umugwayi"
                    }

                PrenatalLabsHistoryPage _ _ _ ->
                    { english = "Labs History"
                    , kinyarwanda = Just "Amakuru ku Bizamini byafashwe"
                    , kirundi = Just "Akahise k'ibipimo vy'ingwara"
                    }

                MessagingCenterPage ->
                    { english = "Messaging Center"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Igihande c'ubutumwa"
                    }

                WellbeingPage ->
                    { english = "Wellbeing"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Imibereho myiza"
                    }

                StockManagementPage ->
                    { english = "Stock Management"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ugucunga ububiko"
                    }

                ChildScoreboardParticipantPage _ ->
                    { english = "Child Scoreboard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifishi y'Imikurire y'Umwana"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounterPage _ ->
                    { english = "Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifishi y'Imikurire y'Umwana"
                    , kirundi = Just "Ukubonana ubwa mbere kw'ikarata y'ikurikiranwa ry'umwana"
                    }

                ChildScoreboardActivityPage _ _ ->
                    { english = "Child Scorecard Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildScoreboardReportPage _ ->
                    translationSet ChildScorecard


translateChartPhrase : ChartPhrase -> TranslationSet String
translateChartPhrase phrase =
    case phrase of
        AgeCompletedMonthsYears ->
            { english = "Age (completed months and years)"
            , kinyarwanda = Just "Imyaka uzuza amezi n'imyaka"
            , kirundi = Just "Imyaka (Amezi n'Imyaka bikwiye)"
            }

        AgeWeeks ->
            { english = "Age (weeks)"
            , kinyarwanda = Nothing
            , kirundi = Just "Imyaka (Indwi)"
            }

        ChartAgeRange range ->
            case range of
                RangeBirthToThirteenWeeks ->
                    { english = "Birth to 13-weeks (z-scores)"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Kuva kw'ivuka gushika k'undwi 13 (z-scores)"
                    }

                RangeBirthToTwoYears ->
                    { english = "Birth to 2-years (z-scores)"
                    , kinyarwanda = Just "kuvuka (Kuva avutse)  kugeza ku myaka 2 Z-score"
                    , kirundi = Just "Kuva kw'ivuka gushika ku myaka 2 (z-scores)"
                    }

                RangeBirthToFiveYears ->
                    { english = "Birth to 5-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 0-5"
                    , kirundi = Just "Kuva kw'ivuka gushika ku myaka 5 (z-scores)"
                    }

                RangeFiveToTenYears ->
                    { english = "5 to 10-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-10"
                    , kirundi = Just "Kuva ku myaka 5-10 (z-scores)"
                    }

                RangeFiveToNineteenYears ->
                    { english = "5 to 19-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-19"
                    , kirundi = Just "Imyaka 5-19 (z-scores)"
                    }

        HeadCircumferenceCm ->
            { english = "Head Circumference (cm)"
            , kinyarwanda = Nothing
            , kirundi = Just "Umuzingi w'umutwe (cm)"
            }

        HeadCircumferenceForAge gender ->
            case gender of
                Male ->
                    { english = "Head Circumference Boys"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umuzingi w'umutwe ku bahungu"
                    }

                Female ->
                    { english = "Head Circumference Girls"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umuzingi w'umutwe ku bakobwa"
                    }

        HeightForAge gender ->
            case gender of
                Male ->
                    { english = "Height-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    , kirundi = Just "Uburebure ku myaka/ abahungu"
                    }

                Female ->
                    { english = "Height-For-Age Girls"
                    , kinyarwanda = Just "Uburebure ku myaka/ umukobwa"
                    , kirundi = Just "Uburebure ku myaka/ abakobwa"
                    }

        LengthCm ->
            { english = "Length (cm)"
            , kinyarwanda = Just "Uburebure cm"
            , kirundi = Just "Uburebure (cm)"
            }

        LengthForAge gender ->
            case gender of
                Male ->
                    { english = "Length-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    , kirundi = Just "Uburebure ku myaka/ abahungu"
                    }

                Female ->
                    { english = "Length-For-Age Girls"
                    , kinyarwanda = Just "uburebure ku myaka UMUKOBWA"
                    , kirundi = Just "Uburebure ku myaka/ abakobwa"
                    }

        Months ->
            { english = "Months"
            , kinyarwanda = Just "Amezi"
            , kirundi = Just "Amezi"
            }

        WeightForAge gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Age Boys"
                    , kinyarwanda = Just "Ibiro ku myaka umuhungu"
                    , kirundi = Just "Ibiro-ku-myaka abahungu"
                    }

                Female ->
                    { english = "Weight-For-Age Girls"
                    , kinyarwanda = Just "ibiro ku myaka umukobwa"
                    , kirundi = Just "Ibiro-ku-myaka abakobwa"
                    }

        WeightForLength gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Height Boys"
                    , kinyarwanda = Just "Ibiro ku Uburebure umuhungu"
                    , kirundi = Just "Ibiro-ku-uburebure abahungu"
                    }

                Female ->
                    { english = "Weight-For-Height Girls"
                    , kinyarwanda = Just "ibiro ku uburebure umukobwa"
                    , kirundi = Just "Ibiro-ku-uburebure abakobwa"
                    }

        WeightKg ->
            { english = "Weight (kg)"
            , kinyarwanda = Just "Ibiro kg"
            , kirundi = Just "Uburemere kg"
            }


translateDashboard : Dashboard -> TranslationSet String
translateDashboard trans =
    case trans of
        BeneficiariesLabel ->
            { english = "FBF Beneficiaries"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AcuteIllnessDiagnosed ->
            { english = "Acute Illness Diagnosed"
            , kinyarwanda = Just "Uburwayi bufatiyeho bwasuzumwe"
            , kirundi = Nothing
            }

        BeneficiariesTableColumnLabel label ->
            case label of
                New ->
                    { english = "New beneficiaries to program"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Missed ->
                    { english = "Missed session by beneficiaries"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Malnourished ->
                    { english = "Malnourished beneficiaries"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Total ->
                    { english = "Total beneficiaries in program"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        BeneficiariesTableLabel ->
            { english = "Grouped by age (Months)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BoysFilterLabel ->
            { english = "Boys"
            , kinyarwanda = Just "Umuhungu"
            , kirundi = Just "Abahungu"
            }

        CallsTo114 ->
            { english = "Calls to 114"
            , kinyarwanda = Just "Inshuro bahamagaye 114"
            , kirundi = Just "Incuro bahamagaye 114"
            }

        CaseManagementFirstWordHelper ->
            { english = "Review"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CaseManagementHelper ->
            { english = "list of malnourished children"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CaseManagementLabel ->
            { english = "Case Management"
            , kinyarwanda = Just "Gukurikirana Umurwayi"
            , kirundi = Just "Ugucungera ingwara"
            }

        CompletedProgramLabel ->
            { english = "Completed Program"
            , kinyarwanda = Nothing
            , kirundi = Just "Igikorwa carangiy"
            }

        CurrentPregnancies ->
            { english = "Currently Pregnant Women"
            , kinyarwanda = Just "Abagore basanzwe batwite"
            , kirundi = Just "Abagore bafise imbanyi ubu"
            }

        CommunityLevelCases ->
            { english = "Community Level Cases"
            , kinyarwanda = Just "Umubare w'ababonetse ku rwego rw'umudugudu"
            , kirundi = Just "ingorane zishika mu kibano"
            }

        ComplicatedMalariaReferredToHC ->
            { english = "Complicated Malaria Referred to HC"
            , kinyarwanda = Just "Abarwaye Malariya y'ikigatu boherejwe ku Kigo Nderabuzima"
            , kirundi = Just "Malariya ikomeye yarungitswe kw'Ivuriro"
            }

        ComplicatedGIInfectionsReferredToHc ->
            { english = "Complicated GI Infections Referred to Health Center"
            , kinyarwanda = Just "Uburwayi bwo munda bukomeye bwoherejwe ku kigo nderabuzima"
            , kirundi = Just "Ingwara zandukiye zo mu mara kandi zigoye zarungitswe kw'Ivuriro"
            }

        DiagnosisUndetermined ->
            { english = "Diagnosis Undetermined"
            , kinyarwanda = Just "Uburwayi budasobanutse"
            , kirundi = Just "Isuzuma ntiryaheze"
            }

        DiagnosedCases ->
            { english = "Diagnosed Cases"
            , kinyarwanda = Just "Umubare w'indwara zavuwe"
            , kirundi = Just "Ivyasuzumwe"
            }

        FamilyPlanningLabel ->
            { english = "Family Planning"
            , kinyarwanda = Just "Kuboneza Urubyaro"
            , kirundi = Just "Kuvyara k'urugero"
            }

        FamilyPlanningOutOfWomen { total, useFamilyPlanning } ->
            { english = String.fromInt useFamilyPlanning ++ " out of " ++ String.fromInt total ++ " women"
            , kinyarwanda = Nothing
            , kirundi = Just <| String.fromInt useFamilyPlanning ++ " mu bagore " ++ String.fromInt total
            }

        FeversByCause ->
            { english = "Fevers by Cause"
            , kinyarwanda = Just "Impamvu zateye umuriro"
            , kirundi = Just "Ubushuhe n'icabuteye"
            }

        FeverCause cause ->
            case cause of
                FeverCauseCovid19 ->
                    { english = "COVID-19"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FeverCauseMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    }

                FeverCauseRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Guhema"
                    }

                FeverCauseGI ->
                    { english = "Gastrointeritis"
                    , kinyarwanda = Just "Indwara yo mu nda"
                    , kirundi = Just "Ingwara zo mu nda"
                    }

                FeverCauseUnknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    , kirundi = Just "Ntibizwi"
                    }

        FeverOfUnknownOrigin ->
            { english = " Fever of Unknown Origin"
            , kinyarwanda = Just "Umuriro utazwi icyawuteye"
            , kirundi = Just "Ubushuhe bitazwi iyo bwazananye"
            }

        Filter filter ->
            case filter of
                Stunting ->
                    { english = "Stunting"
                    , kinyarwanda = Just "Igwingira"
                    , kirundi = Just "Ugucererwa mu gukura"
                    }

                Underweight ->
                    { english = "Underweight"
                    , kinyarwanda = Just "Ibiro bidahagije"
                    , kirundi = Just "Ibiro bikeya cane"
                    }

                Wasting ->
                    { english = "Wasting"
                    , kinyarwanda = Just "Kunanuka Bikabije"
                    , kirundi = Just "Gupfisha ubusa"
                    }

                Dashboard.MUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Just "(CMB) Uburyo bwo gupima vuba na vuba ingwara yo gufungura nabi:"
                    }

                MissedSession ->
                    { english = "Missed Sessions"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ivyigwa vyabuze"
                    }

        FilterProgramType filterProgramType ->
            case filterProgramType of
                FilterAllPrograms ->
                    { english = "All"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Vyose"
                    }

                FilterProgramAchi ->
                    { english = "ACHI"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterProgramFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterProgramPmtct ->
                    { english = "PMTCT"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterProgramSorwathe ->
                    { english = "Sorwathe"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterProgramCommunity ->
                    { english = "Community"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ikibano"
                    }

        Filters ->
            { english = "Filters"
            , kinyarwanda = Just "Guhitamo"
            , kirundi = Just "Gucagura"
            }

        GirlsFilterLabel ->
            { english = "Girls"
            , kinyarwanda = Just "Umukobwa"
            , kirundi = Just "Abakobwa"
            }

        GoodNutritionLabel ->
            { english = "% Good nutrition"
            , kinyarwanda = Just "% Abafite imirire myiza"
            , kirundi = Just "% vyo Gufungura neza"
            }

        HomeDeliveries ->
            { english = "Home Deliveries"
            , kinyarwanda = Just "Ababyariye mu Rugo"
            , kirundi = Just "Imvyaro zabereye muhira"
            }

        HealthFacilityDeliveries ->
            { english = "Health Facility Deliveries"
            , kinyarwanda = Just "Ababyariye ku Ivuriro"
            , kirundi = Just "Imvyaro zabereye kw'ivuriro"
            }

        HealthCenterReferrals ->
            { english = "Health Center Referrals"
            , kinyarwanda = Just "Aboherejwe ku kigo nderabuzima"
            , kirundi = Just "Kurungika abarwayi kw'ivuriro"
            }

        IncidenceOf ->
            { english = "Incidence of"
            , kinyarwanda = Just "Umubare w'abana bashya bafite"
            , kirundi = Just "icabaye"
            }

        LastUpdated ->
            { english = "Last updated"
            , kinyarwanda = Just "Ivugurura riheruka"
            , kirundi = Just "Ivyashizwe ku gihe ubwanyuma"
            }

        Moderate ->
            { english = "Moderate"
            , kinyarwanda = Nothing
            , kirundi = Just "Hagati na hagati"
            }

        MissedSessionsLabel ->
            { english = "Missed Session"
            , kinyarwanda = Nothing
            , kirundi = Just "Icigwa cabuze"
            }

        ModeratelyMalnourished ->
            { english = "Moderately Malnourished"
            , kinyarwanda = Nothing
            , kirundi = Just "Ugufungura nabi bisanzwe"
            }

        MothersInANC ->
            { english = "Mothers in ANC"
            , kinyarwanda = Just "Ababyeyi bari muri serivisi ikurikirana abagore batwite"
            , kirundi = Just "Abavyeyi bari mu ncanco z'imbanyi "
            }

        NewCasesLabel ->
            { english = "New Cases"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewCasesPerMonth ->
            { english = "New cases per month"
            , kinyarwanda = Just "Abashya bagaragaye mu kwezi"
            , kirundi = Just "Izindi ngwara nshasha"
            }

        NewPregnancy ->
            { english = "New Identified Pregnancies"
            , kinyarwanda = Just "Abagore bashya batwite"
            , kirundi = Just "Imbanyi nshasha zamenyekanye"
            }

        NewBeneficiaries ->
            { english = "New Beneficiaries"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewbornsInCare ->
            { english = "Newborns in Care"
            , kinyarwanda = Just "Impinja zikurikiranwa"
            , kirundi = Just "Abana bavutse baravugwa"
            }

        NoDataForPeriod ->
            { english = "No data for the selected period."
            , kinyarwanda = Just "Nta bipimo bigaragara muri iki gihe wahisemo"
            , kirundi = Just "Nta makuru y'igihe catowe."
            }

        PatientsManagedAtHome ->
            { english = "Managed at Home"
            , kinyarwanda = Just "Abavuriwe mu Rugo"
            , kirundi = Just "Vyakorewe muhira"
            }

        PatientCurrentlyUnderCare ->
            { english = "Currently Under Care"
            , kinyarwanda = Just "Abacyitabwaho"
            , kirundi = Just "Abariko baravugwa ubu"
            }

        PercentageLabel period ->
            case period of
                Dashboard.OneYear ->
                    { english = "from last year"
                    , kinyarwanda = Just "Guhera umwaka ushize"
                    , kirundi = Just "guhera mu mwaka uheze"
                    }

                Dashboard.ThisMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Just "guhera mu kwezi guheze"
                    }

                Dashboard.LastMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Just "guhera mu kwezi guheze"
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Just "guhera mu kwezi guheze"
                    }

        PeriodFilter period ->
            case period of
                Dashboard.OneYear ->
                    { english = "1 year"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Umwaka 1"
                    }

                Dashboard.ThisMonth ->
                    { english = "This month"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Uku kwezi"
                    }

                Dashboard.LastMonth ->
                    { english = "Last month"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Ukwezi gushize"
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "Three months"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Amezi atatu"
                    }

        ProgramType ->
            { english = "Program Type"
            , kinyarwanda = Nothing
            , kirundi = Just "Ubwoko bwa Porogarama"
            }

        ResolvedCases ->
            { english = " Resolved Cases: Currently in Care"
            , kinyarwanda = Just "Abavuwe: Bacyitabwaho"
            , kirundi = Just "Ingwara zaronse inyishu: Kugeza ubu ariko aravugwa"
            }

        Severe ->
            { english = "Severe"
            , kinyarwanda = Nothing
            , kirundi = Just "Bikaze"
            }

        SeverelyMalnourished ->
            { english = "Severely Malnourished"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StatisticsFirstWordHelper ->
            { english = "See"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StatisticsHelper ->
            { english = "statistics for this month"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SubFilter filter ->
            case filter of
                FilterTotal ->
                    { english = "Total"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterModerate ->
                    { english = "Moderate"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FilterSevere ->
                    { english = "Severe"
                    , kinyarwanda = Nothing
                    , kirundi = Just "Bikaze"
                    }

        TotalBeneficiaries ->
            { english = "Total Beneficiaries"
            , kinyarwanda = Just "Umubare w'abana bose bafite"
            , kirundi = Just "Abagenerwabikorwa bose"
            }

        TotalMalnourished ->
            { english = "Total Malnourished"
            , kinyarwanda = Nothing
            , kirundi = Just "Abagwaye ingwara yo gufungura nabi bose"
            }

        TotalEncountersLabel ->
            { english = "Total encounters completed"
            , kinyarwanda = Just "Ibikorwa byose byarangiye"
            , kirundi = Just "Icegeranyo c'imibonano yose yaheze"
            }

        TotalAssessment ->
            { english = "Total # of Assessments"
            , kinyarwanda = Just "Umubare wose w'Amasuzuma Yakozwe"
            , kirundi = Just "igitigiri cy'Ivyasuzumwe vyose hamwe"
            }

        UncomplicatedMalariaByChws ->
            { english = "Uncomplicated Malaria Managed by CHWs"
            , kinyarwanda = Just "Abarwaye Malariya yorohejwe yavuwe n'abajyanama b'ubuzima"
            , kirundi = Just "Malariya yoroshe yacungerewe n'abaremeshakiyago"
            }

        UncomplicatedMalariaInPregnancyReferredToHc ->
            { english = "Uncomplicated Malaria in Pregnancy Referred to HC"
            , kinyarwanda = Just "Ababyeyi batwite bafite Malariya yoroheje boherejwe ku kigo nderabuzima"
            , kirundi = Just "Malariya yoroshe mu gihe c'imbanyi yarungutswe kw'ivuriro"
            }

        UncomplicatedGIInfectionByCHWS ->
            { english = "Uncomplicated GI Infections Managed by CHWs"
            , kinyarwanda = Just "Uburwayi bwo mu nda bworoheje bwavuwe n'abajyanama w'ubuzima"
            , kirundi = Just "Ingwara zo mu nda zoroshe zirashobora gucungegwa n'abaremeshakiyago"
            }

        UseFamilyPlanning ->
            { english = "use family planning"
            , kinyarwanda = Nothing
            , kirundi = Just "Koresha kuvyara k'urugero"
            }

        Within4MonthsOfDueDate ->
            { english = "Within 4 Months of Due Date"
            , kinyarwanda = Just "Inda ibura amezi 4 ngo ivuke"
            , kirundi = Just "Mu kiringo c'amezi 4 imbere y'uko ushikira itarike yagenywe"
            }

        WithDangerSigns ->
            { english = "With Danger Signs"
            , kinyarwanda = Just "Abafite Ibimenyetso Mpuruza"
            , kirundi = Just "Hamwe n'ibimenyetso bikomeye cane"
            }


translateLoginPhrase : LoginPhrase -> TranslationSet String
translateLoginPhrase phrase =
    case phrase of
        ForgotPassword1 ->
            { english = "Forgot your password?"
            , kinyarwanda = Just "Wibagiwe ijambo ry'ibanga?"
            , kirundi = Nothing
            }

        ForgotPassword2 ->
            { english = "Call The Ihangane Project at +250 788 817 542"
            , kinyarwanda = Just "Hamagara The Ihangane Project kuri +250 788 817 542(Hamagara kumushinga wa ihangane"
            , kirundi = Just "Hamagara kuri +250 788 817 542"
            }

        LoggedInAs ->
            { english = "Logged in as"
            , kinyarwanda = Just "Kwinjira nka"
            , kirundi = Just "Winjiye nka"
            }

        LoginToSyncHealthCenters ->
            { english = "Please log in before syncing health centers"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PinCode ->
            { english = "PIN code"
            , kinyarwanda = Nothing
            , kirundi = Just "Inimero kabanga"
            }

        PinCodeRejected ->
            { english = "Your PIN code was not recognized."
            , kinyarwanda = Just "Umubare wawe w'ibanga ntabwo uzwi."
            , kirundi = Just "Inimero kabanga ntago izwi"
            }

        SignIn ->
            { english = "Sign In"
            , kinyarwanda = Just "Kwinjira"
            , kirundi = Just "Kwinjira"
            }

        SignOut ->
            { english = "Sign Out"
            , kinyarwanda = Just "Gusohoka muri sisiteme"
            , kirundi = Nothing
            }

        YouMustLoginBefore ->
            { english = "You must sign in before you can access the"
            , kinyarwanda = Just "Ugomba kubanza kwinjira muri sisitemi mbere yuko ubona"
            , kirundi = Nothing
            }


translateMonth : Month -> Bool -> TranslationSet String
translateMonth month short =
    case month of
        Jan ->
            if short then
                { english = "Jan"
                , kinyarwanda = Just "Mut"
                , kirundi = Just "Nze"
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                , kirundi = Just "Nzero"
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                , kirundi = Just "Ruh"
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                , kirundi = Just "Ruhuhuma"
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                , kirundi = Just "Ntw"
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                , kirundi = Just "Ntwarante"
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                , kirundi = Just "Nda"
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                , kirundi = Just "Ndamukiza"
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                , kirundi = Just "Rus"
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                , kirundi = Just "Rusama"
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                , kirundi = Just "Ruh"
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                , kirundi = Just "Ruheshi"
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                , kirundi = Just "Muk"
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                , kirundi = Just "Mukakaro"
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                , kirundi = Just "Mya"
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                , kirundi = Just "Myandagaro"
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                , kirundi = Just "Nya"
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                , kirundi = Just "Nyakanga"
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Git"
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                , kirundi = Just "Gitugutu"
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Muny"
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                , kirundi = Just "Munyonyo"
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                , kirundi = Just "Kig"
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                , kirundi = Just "Kigarama"
                }


translateMonthYY : Month -> Int -> Bool -> TranslationSet String
translateMonthYY month year short =
    translateMonth month short
        |> (\set ->
                { english = set.english ++ " " ++ String.fromInt year
                , kinyarwanda = Maybe.map (\kinyarwanda -> kinyarwanda ++ " " ++ String.fromInt year) set.kinyarwanda
                , kirundi = Maybe.map (\kirundi -> kirundi ++ " " ++ String.fromInt year) set.kirundi
                }
           )


translateHttpError : Http.Error -> TranslationSet String
translateHttpError error =
    case error of
        Http.NetworkError ->
            { english = "Something went wrong. Please refresh the page and try again. If problem persisits, please contact system administrator."
            , kinyarwanda = Just "Hari ikitagenze neza. Ongera ugerageze ukoraho, niba ikibazo gikomeje hamagara umuyobozi wa sisiteme."
            , kirundi = Just "Hari ikintu kitagenze neza. Gerageza ukagure ugwo ruhande uriko urakoremwo hama usubire ugerageze. Mu gihe ingorane ibandanije, hamagara umuhinga/umurongozi w'ubu buryo bugezwewo (wiyi Sisiteme)."
            }

        Http.Timeout ->
            { english = "The request to the server timed out."
            , kinyarwanda = Just "Ibyo wasabye kuri seriveri byarengeje igihe."
            , kirundi = Just "Umwanya w'igisabo kwa Serivere waheze."
            }

        Http.BadUrl url ->
            { english = "URL is not valid: " ++ url
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Http.BadStatus _ ->
            { english = "The server indicated the following error:"
            , kinyarwanda = Just "Aya makosa yagaragaye hamagara kuri seriveri:"
            , kirundi = Nothing
            }

        Http.BadPayload _ _ ->
            { english = "The server responded with data of an unexpected type."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }


translateValidationError : ValidationError -> TranslationSet String
translateValidationError id =
    case id of
        DigitsOnly ->
            { english = "should contain only digit characters"
            , kinyarwanda = Nothing
            , kirundi = Just <| "Itegerezwa kuba irimwo ibiharuro gusa"
            }

        InvalidBirthDate ->
            { english = "is invalid"
            , kinyarwanda = Nothing
            , kirundi = Just "Ntivyemewe"
            }

        InvalidBirthDateForAdult ->
            { english = "is invalid - adult should at least 13 years old"
            , kinyarwanda = Nothing
            , kirundi = Just "Ntivyemewe - umuntu akuze ategerezwa kuba afise ni miburiburi imyaka 13"
            }

        InvalidBirthDateForChild ->
            { english = "is invalid - child should be below the age of 13"
            , kinyarwanda = Nothing
            , kirundi = Just "Ntivyemewe - umwana ategerezwa kuba ari munsi y'imyaka 13"
            }

        InvalidHmisNumber ->
            { english = "is invalid - child should be between 1 and 15"
            , kinyarwanda = Nothing
            , kirundi = Just "Ntivyemewe- umwana ategerezwa kuba ari hagati y'umwaka 1 n'imyaka 15"
            }

        LengthError correctLength ->
            { english = "should contain " ++ String.fromInt correctLength ++ " characters"
            , kinyarwanda = Nothing
            , kirundi = Just <| "Itegerezwa kuba irimwo " ++ String.fromInt correctLength ++ " indome"
            }

        RequiredField ->
            { english = "is a required field"
            , kinyarwanda = Just "ni ngombwa kuhuzuza"
            , kirundi = Just "Ahantu ari ngombwa"
            }

        UnknownProvince ->
            { english = "is not a known province"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni intara itazwi"
            }

        UnknownDistrict ->
            { english = "is not a known district"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni akarere katazwi"
            }

        UnknownSector ->
            { english = "is not a known sector"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni umutumba utazwi"
            }

        UnknownCell ->
            { english = "is not a known cell"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni ahantu hatazwi"
            }

        UnknownVillage ->
            { english = "is not a known village"
            , kinyarwanda = Nothing
            , kirundi = Just "Ni ikigwati kitazwi"
            }

        DecoderError err ->
            { english = "Decoder error: " ++ err
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }


translateFormError : ErrorValue ValidationError -> TranslationSet String
translateFormError error =
    case error of
        Empty ->
            { english = "should not be empty"
            , kinyarwanda = Just "igomba kuzuzwa"
            , kirundi = Just "Ntitegerezwa kuba atakirimwo"
            }

        InvalidString ->
            { english = "is not a valid string"
            , kinyarwanda = Just "Ntibyemewe kwandikama inyuguti"
            , kirundi = Just "Ni urukurikirane rutazwi"
            }

        InvalidEmail ->
            { english = "is not a valid email"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidFormat ->
            { english = "is not a valid format"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidInt ->
            { english = "is not a valid integer"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidFloat ->
            { english = "is not a valid number"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidBool ->
            { english = "is not a valid boolean"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SmallerIntThan int ->
            { english = "must be smaller than " ++ String.fromInt int
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GreaterIntThan int ->
            { english = "must be larger than " ++ String.fromInt int
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SmallerFloatThan float ->
            { english = "must be smaller than " ++ String.fromFloat float
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GreaterFloatThan float ->
            { english = "must be larger than " ++ String.fromFloat float
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ShorterStringThan int ->
            { english = "must have fewer than " ++ String.fromInt int ++ " characters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LongerStringThan int ->
            { english = "must have more than " ++ String.fromInt int ++ " characters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NotIncludedIn ->
            { english = "was not among the valid options"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CustomError e ->
            translateValidationError e


{-| This one is hampered by the fact that the field names in etaque/elm-form
are untyped strings, but we do our best.
-}
translateFormField : String -> TranslationSet String
translateFormField field =
    case field of
        "clinic_id" ->
            translationSet Group

        "closed" ->
            translationSet Closed

        "training" ->
            translationSet Group

        "scheduled_date.start" ->
            translationSet StartDate

        "scheduled_date.end" ->
            translationSet EndDate

        _ ->
            { english = field
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }
