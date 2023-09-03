module Translate exposing
    ( Adherence(..)
    , ChartPhrase(..)
    , Dashboard(..)
    , Language
    , LoginPhrase(..)
    , TranslationId(..)
    , ValidationError(..)
    , translate
    , translateActivePage
    , translateAdherence
    , translateChartPhrase
    , translateCounselingTimingHeading
    , translateFormError
    , translateFormField
    , translateHttpError
    , translateLoginPhrase
    , translateText
    , translateValidationError
    , translationSet
    )

{-| This module has just the translations ... for types and
general utilities, see `Translate.Model` and `Translate.Utils`.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTiming(..), CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..), IndividualEncounterType(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (..)
import Backend.NCDActivity.Model exposing (NCDActivity(..), NCDRecurrentActivity(..))
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.Nurse.Model exposing (ResilienceRole(..))
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
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
        , HighSeverityAlert(..)
        , MedicalDiagnosis(..)
        , ObstetricalDiagnosis(..)
        , PregnancyTrimester(..)
        , PrenatalActivity(..)
        , PrenatalRecurrentActivity(..)
        , RecurringHighSeverityAlert(..)
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
import Components.SendViaWhatsAppDialog.Model
    exposing
        ( ReportComponentAntenatal(..)
        , ReportComponentNCD(..)
        , ReportComponentWellChild(..)
        )
import Date exposing (Month)
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
        , FilterPeriod(..)
        , FilterProgramType(..)
        )
import Pages.GlobalCaseManagement.Model exposing (CaseManagementFilter(..), FollowUpDueOption(..), LabsEntryState(..))
import Pages.MessagingCenter.Model exposing (MessagingTab(..))
import Pages.NCD.Activity.Types exposing (ExaminationTask(..), MedicalHistoryTask(..))
import Pages.NCD.ProgressReport.Model exposing (NCDRiskFactor(..))
import Pages.NCD.RecurrentActivity.Types
import Pages.Nutrition.Activity.Model
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
import Pages.WellChild.Activity.Types exposing (NextStepsTask(..), NutritionAssessmentTask(..), VaccinationStatus(..))
import Pages.WellChild.Encounter.Model exposing (ECDPopupType(..), WarningPopupType(..))
import Pages.WellChild.ProgressReport.Model
    exposing
        ( ECDStatus(..)
        , NCDAANCNewbornItem(..)
        , NCDAFillTheBlanksItem(..)
        , NCDAInfrastructureEnvironmentWashItem(..)
        , NCDANutritionBehaviorItem(..)
        , NCDATargetedInterventionsItem(..)
        , NCDAUniversalInterventionsItem(..)
        )
import Restful.Endpoint exposing (fromEntityUuid)
import Restful.Login exposing (LoginError(..), LoginMethod(..))
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
    = CheckingCachedCredentials
    | ForgotPassword1
    | ForgotPassword2
    | LoggedInAs
    | LoginError Http.Error
    | LoginRejected LoginMethod
    | LoginToSyncHealthCenters
    | Logout
    | LogoutInProgress
    | LogoutFailed
    | Password
    | PinCode
    | PinCodeRejected
    | SignIn
    | SignOut
    | Username
    | WorkOffline
    | YouMustLoginBefore


type ChartPhrase
    = AgeCompletedMonthsYears
    | AgeWeeks
    | Birth
    | ChartAgeRange ChartAgeRange
    | HeadCircumferenceCm
    | HeadCircumferenceForAge Gender
    | HeightCm
    | HeightForAge Gender
    | LengthCm
    | LengthForAge Gender
    | Months
    | OneYear
    | WeightForAge Gender
    | WeightForLength Gender
    | WeightKg
    | YearsPlural Int
    | ZScoreChartsAvailableAt


type ValidationError
    = DigitsOnly
    | InvalidBirthDate
    | InvalidBirthDateForAdult
    | InvalidBirthDateForChild
    | InvalidHmisNumber
    | LengthError Int
    | LettersOnly
    | RequiredField
    | UnknownGroup
    | UnknownProvince
    | UnknownDistrict
    | UnknownSector
    | UnknownCell
    | UnknownVillage
    | DecoderError String


type Adherence
    = PrescribedAVRs
    | CorrectDosage
    | TimeOfDay
    | Adhering


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
    | ChildrenWhoDied
    | CompletedProgramLabel
    | CommunityLevelCases
    | ComplicatedMalariaReferredToHC
    | ComplicatedGIInfectionsReferredToHc
    | CurrentPregnancies
    | DiagnosisUndetermined
    | DiagnosedCases
    | FamilyPlanningLabel
    | FamilyPlanningOutOfWomen { total : Int, useFamilyPlanning : Int }
    | FamilyThatMoved
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
    | LoadingDataGeneral
    | MissedSessionsLabel
    | Moderate
    | ModeratelyMalnourished
    | MothersInANC
    | NewBeneficiaries
    | NewbornsInCare
    | NewCasesLabel
    | NewCasesPerMonth
    | NewPregnancy
    | NoDataGeneral
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
    | SyncNotice
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
    | AccessDenied
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
    | ActivitityTitleAchi
    | ActivitiesToComplete Int
    | ActivitityLabelAchi
    | ActivityProgressReport Activity
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
    | Admin
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
    | Adherence Adherence
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
    | AgeSingleDayWithoutMonth Int Int
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
    | BabyName String
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
    | BloodPressureElevatedOcassions
    | BloodPressureDiaLabel
    | BloodPressureSysLabel
    | BloodSmearQuestion
    | BloodSmearLabel
    | BloodSmearResult BloodSmearResult
    | BMI
    | BMIHelper
    | BodyTemperature
    | Born
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
    | CandidiasisRecommendedTreatmentInstructions
    | CannotStartEncounterLabel
    | CardiacDisease
    | CaregiverAccompanyQuestion
    | CaregiverName
    | CaregiverNationalId
    | CaseManagement
    | CaseManagementFilterLabel CaseManagementFilter
    | CaseManagementPaneHeader CaseManagementFilter
    | CentimeterShorthand
    | Celsius
    | CelsiusAbbrev
    | Cell
    | ChartPhrase ChartPhrase
    | CheckAllThatApply
    | CheckIn
    | ChildCleanQuestion
    | ChildHmisNumber
    | ChildDemographicInformation
    | ChildIdentification
    | ChildNutritionSignLabel ChildNutritionSign
    | ChildNutritionSignReport ChildNutritionSign
    | ChildOf
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
    | ColorAlertIndication ColorAlertIndication
    | Completed
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
    | CurrentIllnessBegan
    | CSectionScar CSectionScar
    | Dashboard Dashboard
    | GroupNotFound
    | Group
    | Groups
    | GroupUnauthorized
    | Close
    | Closed
    | ConditionImproving Bool
    | ConditionImprovingQuestion
    | ConfirmationRequired
    | ConfirmDeleteTrainingGroupEncounters
    | Connected
    | ContactExposure
    | ContactInformation
    | Continue
    | CounselingTimingHeading CounselingTiming
    | CounselingTopic CounselingTopic
    | CounselorReviewed
    | CovidContactTracing
    | CovidTestingInstructions
    | CounselorSignature
    | CSectionInPreviousDelivery
    | CSectionReason
    | CSectionReasons CSectionReason
    | CreateGroupEncounter
    | CreateRelationship
    | CreateTrainingGroupEncounters
    | ChwDashboardLabel
    | CurrentlyPregnant
    | CurrentlyPregnantQuestion
    | CurrentStock
    | DangerSign DangerSign
    | DangerSignsLabelForChw
    | DangerSignsLabelForNurse
    | Date
    | DateConcludedEstimatedQuestion
    | DateOfContact
    | DateOfLastAssessment
    | DatePregnancyConcluded
    | DashboardLabel
    | DateReceived
    | DateOfBirth
    | Day
    | DayAbbrev
    | DaySinglePlural Int
    | Days
    | DaysAbbrev
    | DaysPresent
    | DaysSinglePlural Int
    | Delete
    | DeleteTrainingGroupEncounters
    | DeliveryComplication DeliveryComplication
    | DeliveryComplicationsPresentQuestion
    | DeliveryComplicationsSelectionLabel
    | DeliveryLocation
    | DeliveryOutcome
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
    | Done
    | DoTheFollowing
    | Downloading
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
    | EditRelationship
    | Ega
    | EgaHeader
    | EgaWeeks
    | ElevatedRespiratoryRate
    | EmergencyReferralHelperReferToHC
    | EmergencyReferralHelperReferToHospitalForEvaluation
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
    | EnterAmountDistributed
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
    | Failure
    | FamilyInformation
    | FamilyMembers
    | FamilyPlanningCurentlyQuestion
    | FamilyPlanningInFutureQuestion
    | FamilyPlanningSignLabel FamilyPlanningSign
    | FamilyUbudehe
    | FatherOrChiefId
    | FatherOrChiefName
    | FatherNationalId
    | FavoriteToggle Bool
    | FbfDistribution ClinicType
    | FbfToReceive Activity Float
    | FetalHeartRate
    | FetalMovement
    | FetalPresentationLabel
    | FetalPresentation FetalPresentation
    | Fetch
    | FillTheBlanks
    | FilterByName
    | Finish
    | FirstAntenatalVisit
    | FirstName
    | FiveVisits
    | FoodGroup FoodGroup
    | FollowPostpartumProtocols
    | FollowUpWithPatientIn
    | FollowUpWithPatientOn
    | FollowUpByChwLabel
    | FollowUpLabel
    | FollowUpWithMotherLabel
    | FollowUpOption FollowUpOption
    | FollowUpDueOption FollowUpDueOption
    | FoodSupplementationConsumedQuestion
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
    | GroupEncounter
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
    | HouseholdSize
    | HowManyDoses
    | HaveAnyOfTheFollowingQuestion
    | HttpError Http.Error
    | HoursSinglePlural Int
    | HowManyPerWeek
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
    | ImmunisationHistory
    | IncompleteCervixPreviousPregnancy
    | IndexPatient
    | IndividualEncounter
    | IndividualEncounterFirstVisit IndividualEncounterType
    | IndividualEncounterLabel IndividualEncounterType Bool
    | IndividualEncounterSelectVisit IndividualEncounterType Bool
    | IndividualEncounterSubsequentVisit IndividualEncounterType
    | IndividualEncounterType IndividualEncounterType Bool
    | IndividualEncounterTypes
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
    | KilogramsPerMonth
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
    | LabsHistoryCompletedInstructions
    | LabsHistoryCompletedLabel
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
    | LinkToMother
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
    | MakeSureYouAreConnected
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
    | MeasurementNoChange
    | MeasurementGained Float
    | MeasurementLost Float
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
    | MessagingCenter
    | MessagingTab MessagingTab
    | MMHGUnit
    | MiddleName
    | Minutes Int
    | MinutesAgo Int
    | MissedDosesOfMedicatgion Int
    | ModeOfDelivery ModeOfDelivery
    | ModeOfDeliveryLabel
    | ModeratelyUnderweight
    | ModeratePreeclampsia
    | Month
    | MonthAbbrev
    | MonthSinglePlural Int
    | MonthsOfStock
    | MonthsOld
    | Mother
    | MotherDemographicInformation
    | MotherId
    | MotherName String
    | MotherNameLabel
    | MotherNationalId
    | Mothers
    | MTDIn
    | MTDOut
    | MUAC
    | MuacHelper
    | MyAccount
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
    | NCDANumberOfANCVisitsQuestion
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
    | NCDSocialHistorySignQuestion NCDSocialHistorySign
    | Neck
    | NeckCPESign NeckCPESign
    | NegativeLabel
    | Never
    | Next
    | NextAppointment
    | NextDue
    | NextDoseDue
    | NextImmunisationVisit
    | NextPediatricVisit
    | NextSteps
    | NextStepsTask Bool Pages.AcuteIllness.Activity.Types.NextStepsTask
    | No
    | NoActivitiesCompleted
    | NoActivitiesCompletedForThisParticipant
    | NoActivitiesPending
    | NoActivitiesPendingForThisParticipant
    | NoContactReason NoContactReason
    | NoGroupsFound
    | NoMatchesFound
    | NormalRange
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
    | NoParticipantsFound
    | NotAvailable
    | NotConnected
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
    | Old
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
    | Page404
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
    | ParticipantSummary
    | ParticipantDemographicInformation
    | ParticipantInformation
    | PartnerHivTestResult
    | PartnerReceivedHivCounseling
    | PartnerReceivedHivTesting
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
    | PleaseSelectGroup
    | PleaseSync
    | PointOfCare
    | PositiveLabel
    | PostpartumEncounter
    | PostpartumHealingProblem PostpartumHealingProblem
    | PostpartumHealingProblemQuestion
    | PostpartumChildDangerSign PostpartumChildDangerSign
    | PostpartumMotherDangerSign PostpartumMotherDangerSign
    | Predecessor Predecessor
    | PreeclampsiaPreviousPregnancy
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
    | PrenatalImmunisationHistory PrenatalVaccineType
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
    | Profession
    | Programs
    | ProgressPhotos
    | ProgressReport
    | ProgressReports
    | ProgressTimeline
    | ProgressTrends
    | ProvideHealthEducationAndInstructToIsolate
    | PrenatalParticipant
    | PrenatalParticipants
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
    | ReasonForCSection
    | ReasonForNotBreastfeeding BreastfeedingSign
    | ReasonForNotIsolating ReasonForNotIsolating
    | ReasonForNotTaking ReasonForNotTaking
    | ReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | Received
    | ReceivedDewormingPill
    | ReceivedFolicAcid
    | ReceivedFrom
    | ReceivedIronFolicAcid
    | ReceivedMebendazole
    | ReceivedMosquitoNet
    | ReceivedVitaminA
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
    | Register
    | RegisterContactHelper
    | RegisterParticipantHelper
    | RegisterNewContact
    | RegisterNewParticipant
    | RegistratingHealthCenter
    | RegistrationSuccessful
    | RegistrationSuccessfulParticipantAdded
    | RegistrationSuccessfulSuggestAddingChild
    | RegistrationSuccessfulSuggestAddingMother
    | RelationSuccessful
    | RelationSuccessfulChildWithMother
    | RelationSuccessfulMotherWithChild
    | RemainingForDownloadLabel
    | RemainingForUploadLabel
    | RemainingTotalToUpload
    | RemindMe
    | RemindMePhrase
    | RenalDisease
    | ReportAge String
    | ReportComponentAntenatal ReportComponentAntenatal
    | ReportComponentNCD ReportComponentNCD
    | ReportComponentWellChild ReportComponentWellChild
    | ReportDOB String
    | ReportRemaining Int
    | ReportResultsOfContactsSearch Int
    | ReportResultsOfParticipantsSearch Int
    | ReportTab ReportTab
    | Reports
    | RecentAndUpcomingGroupEncounters
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
    | Retry
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
    | Search
    | SearchByName
    | SearchEhezaForExistingParticipants
    | SearchExistingParticipants
    | SearchHelper
    | SearchHelperFamilyMember
    | SecondName
    | Sector
    | SeeDosageScheduleByWeight
    | SeeLabResults
    | SeeMore
    | SelectAntenatalVisit
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
    | SelectGroup
    | SelectProgram
    | SelectLanguage
    | SelectYourGroup
    | SelectYourHealthCenter
    | SelectYourVillage
    | SelectedHCDownloading
    | SelectedHCNotSynced
    | SelectedHCSyncing
    | Send
    | SendViaWhatsApp
    | SendViaWhatsAppComponentsSelectionHeader Components.SendViaWhatsAppDialog.Model.ReportType
    | SendViaWhatsAppConfirmationBeforeExecutingHeader
    | SendViaWhatsAppConfirmationBeforeExecutingInstructions
    | SendViaWhatsAppConfirmationBeforeExecutingQuestion
    | SendViaWhatsAppConsentQuestion
    | SendViaWhatsAppExecutionResultFailure
    | SendViaWhatsAppExecutionResultSomethingWentWrong
    | SendViaWhatsAppExecutionResultSuccess
    | SendViaWhatsAppNoticeOfNonRespobsibility
    | SendViaWhatsAppPhoneInputHeader
    | SendViaWhatsAppPhoneVerificationHeader
    | SendViaWhatsAppPhoneVerificationQuestion
    | SendViaWhatsAppPhoneUpdateAtProfileQuestionPrefix
    | SendViaWhatsAppPhoneUpdateAtProfileQuestionSuffix
    | SendViaWhatsAppPhoneUpdateConfirmationMessasge
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
    | SubsequentAntenatalVisit
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
    | SymptomsAtFirstEncounter
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
    | ShowAll
    | StartEncounter
    | StartEndDate
    | StrartNewAcuteIllnessHelper
    | StartDate
    | EndDate
    | StartingStock
    | StartSyncing
    | StatusLabel
    | StopSyncing
    | StorageQuota { usage : Int, quota : Int }
    | Submit
    | SubmitPairingCode
    | Success
    | SyncGeneral
    | TabletSinglePlural String
    | TakenCareOfBy
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
    | ThisActionCannotBeUndone
    | ThisGroupHasNoMothers
    | Time
    | To
    | ToThePatient
    | Training
    | TrainingGroupEncounterCreateSuccessMessage
    | TrainingGroupEncounterDeleteSuccessMessage
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
    | UnitMilliGramsPerDeciliter
    | UnitMillimolesPerLiter
    | UnitOfMeasurement UnitOfMeasurement
    | UniversalInterventions
    | Unknown
    | Update
    | UpdateError
    | Uploading
    | UrineDipstickTestLabel TestVariant
    | UrineDipstickTestVariant TestVariant
    | UrinaryTractInfectionRecommendedTreatmentHeader
    | UrinaryTractInfectionRecommendedTreatmentHelper
    | UrinaryTractInfectionRecommendedTreatmentInstructions
    | UterineMyoma
    | VaccinationCatchUpRequiredQuestion
    | VaccinationStatus VaccinationStatus
    | VaccinationNoDosesAdministered
    | VaccineDoseAdministeredPreviouslyPrenatalQuestion String
    | VaccineDoseAdministeredPreviouslyWellChildQuestion String
    | VaccineDoseAdministeredTodayPrenatalQuestion String
    | VaccineDoseAdministeredTodayWellChildQuestion String
    | VaccineType VaccineType
    | VaginalExamination
    | VaginalExamSign VaginalExamSign
    | ValidationErrors
    | Version
    | View
    | ViewProgressReport
    | Village
    | Visits
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
    | WellChildImmunisationDescription WellChildVaccineType
    | WellChildImmunisationDosage WellChildVaccineType
    | WellChildImmunisationHeader WellChildVaccineType
    | WellChildImmunisationHistory WellChildVaccineType
    | WellChildImmunisationTask Measurement.Model.ImmunisationTask
    | WellChildMedicationTask Pages.WellChild.Activity.Types.MedicationTask
    | WellChildNextStepsTask Bool Pages.WellChild.Activity.Types.NextStepsTask
    | WellChildSymptom WellChildSymptom
    | WellChildVaccineLabel WellChildVaccineType
    | WhatDoYouWantToDo
    | WhatType
    | WhatWasTheirResponse
    | WhoCaresForTheChildDuringTheDay
    | WhoInFamilyHasCondition
    | WhyNot
    | WhyDifferentFbfAmount Activity
    | WrittenProtocolsFollowed
    | Year
    | YearsOld Int
    | Yes
    | YouAreNotAnAdmin
    | YourGroupEncounterHasBeenSaved
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
            , kirundi = Nothing
            }

        AbdomenCPESign option ->
            case option of
                Hepatomegaly ->
                    { english = "Hepatomegaly"
                    , kinyarwanda = Just "Kubyimba umwijima"
                    , kirundi = Nothing
                    }

                Splenomegaly ->
                    { english = "Splenomegaly"
                    , kinyarwanda = Just "Kubyimba urwangashya"
                    , kirundi = Nothing
                    }

                TPRightUpper ->
                    { english = "Tender to Palpation right upper"
                    , kinyarwanda = Just "Igice cyo hejuru iburyo kirababara  iyo ugikanze"
                    , kirundi = Nothing
                    }

                TPRightLower ->
                    { english = "Tender to Palpation right lower"
                    , kinyarwanda = Just "Igice cyo hasi iburyo kirababara  iyo ugikanze"
                    , kirundi = Nothing
                    }

                TPLeftUpper ->
                    { english = "Tender to Palpation left upper"
                    , kinyarwanda = Just "Igice cyo hejuru ibumoso kirababara  iyo ugikanze"
                    , kirundi = Nothing
                    }

                TPLeftLower ->
                    { english = "Tender to Palpation left lower"
                    , kinyarwanda = Just "Igice cyo hasi ibumoso kirababara  iyo ugikanze"
                    , kirundi = Nothing
                    }

                Hernia ->
                    { english = "Hernia"
                    , kinyarwanda = Just "Urugingo ruyobera cg rwinjira mu rundi"
                    , kirundi = Nothing
                    }

                NormalAbdomen ->
                    translationSet Normal

        Abnormal ->
            { english = "Abnormal"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Abortions ->
            { english = "Abortions"
            , kinyarwanda = Just "Inda yavuyemo"
            , kirundi = Nothing
            }

        Accept ->
            { english = "Accept"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AccompaniedByPartner ->
            { english = "Was the patient accompanied by partner during the assessment"
            , kinyarwanda = Just "Umubyeyi yaherekejwe n'umugabo we mu gihe yaje kwipimisha"
            , kirundi = Nothing
            }

        AccompanyToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Will you accompany the patient to the health center"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku kigonderabuzima"
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Will you accompany the patient to the hospital"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku bitaro"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Will you accompany the patient to mental health specialist"
                    , kinyarwanda = Just "Uzaherekeza umurwayi ku muganaga winzobere k'ubuzima bwo mu mutwe"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Will you accompany the patient to ARV services"
                    , kinyarwanda = Just "Uraherekeza umubyei muri erivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "Will you accompany the patient to NCD services"
                    , kinyarwanda = Just "Uzaherekeza umurwayi muri serivisi y'indwara zitandura"
                    , kirundi = Nothing
                    }

                FacilityANCServices ->
                    { english = "Will you accompany the patient to ANC services"
                    , kinyarwanda = Just "Uzaherekeza umubyeyi muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FacilityUltrasound ->
                    { english = "Will you accompany the patient to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        AccessDenied ->
            { english = "Access denied"
            , kinyarwanda = Just "Kwinjira ntibyemera"
            , kirundi = Nothing
            }

        Actions ->
            { english = "Actions"
            , kinyarwanda = Just "Ibikorwa"
            , kirundi = Nothing
            }

        ActionsTaken ->
            { english = "Actions Taken"
            , kinyarwanda = Just "Ibyakozwe"
            , kirundi = Nothing
            }

        ActionsToTake ->
            { english = "Actions To Take"
            , kinyarwanda = Just "Ibigomba gukorwa"
            , kirundi = Nothing
            }

        AcuteFindingsGeneralSign sign ->
            case sign of
                LethargicOrUnconscious ->
                    { english = "Lethargic Or Unconscious"
                    , kinyarwanda = Just "Yahwereye cyangwa yataye ubwenge"
                    , kirundi = Nothing
                    }

                AcuteFindingsPoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Nothing
                    }

                SunkenEyes ->
                    { english = "Sunken Eyes"
                    , kinyarwanda = Just "Amaso yahenengeye"
                    , kirundi = Nothing
                    }

                PoorSkinTurgor ->
                    { english = "Poor Skin Turgor"
                    , kinyarwanda = Just "Uruhu rwumye"
                    , kirundi = Nothing
                    }

                Jaundice ->
                    { english = "Jaundice"
                    , kinyarwanda = Just "Umuhondo/umubiri wahindutse umuhondo"
                    , kirundi = Nothing
                    }

                NoAcuteFindingsGeneralSigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        AcuteFindingsRespiratorySign sign ->
            case sign of
                Stridor ->
                    { english = "Stridor"
                    , kinyarwanda = Just "Guhumeka ajwigira"
                    , kirundi = Nothing
                    }

                NasalFlaring ->
                    { english = "Nasal Flaring"
                    , kinyarwanda = Just "Amazuru abyina igihe ahumeka"
                    , kirundi = Nothing
                    }

                SevereWheezing ->
                    { english = "Severe Wheezing"
                    , kinyarwanda = Just "Guhumeka nabi cyane ajwigira"
                    , kirundi = Nothing
                    }

                SubCostalRetractions ->
                    { english = "Sub-Costal Retractions"
                    , kinyarwanda = Just "Icyena mu mbavu"
                    , kirundi = Nothing
                    }

                NoAcuteFindingsRespiratorySigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        AcuteIllnessAdverseEvent event ->
            case event of
                AdverseEventRashOrItching ->
                    { english = "Rash or Itching"
                    , kinyarwanda = Just "Kwishima cyangwa gusesa uduheri (turyaryata)"
                    , kirundi = Nothing
                    }

                AdverseEventFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Nothing
                    }

                AdverseEventDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    , kirundi = Nothing
                    }

                AdverseEventVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    , kirundi = Nothing
                    }

                AdverseEventFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "umunaniro"
                    , kirundi = Nothing
                    }

                AdverseEventOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoAdverseEvent ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        AcuteIllnessAdverseEventKindsQuestion ->
            { english = "What kind of adverse events"
            , kinyarwanda = Just "Ni ibihe bintu wabonye bidasanzwe (bitewe n'imiti wafashe)"
            , kirundi = Nothing
            }

        AcuteIllnessDangerSign sign ->
            case sign of
                DangerSignConditionNotImproving ->
                    { english = "Condition not improving"
                    , kinyarwanda = Just "Yanyoye imiti ariko ntiyoroherwa"
                    , kirundi = Nothing
                    }

                DangerSignUnableDrinkSuck ->
                    { english = "Unable to Drink/Suck"
                    , kinyarwanda = Just "Ntashoboye kunywa/konka"
                    , kirundi = Nothing
                    }

                DangerSignVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    , kirundi = Nothing
                    }

                DangerSignConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Nothing
                    }

                DangerSignLethargyUnconsciousness ->
                    { english = "Lethargy or Unconsciousness"
                    , kinyarwanda = Just "Yahwereye cyangwa ntiyumva"
                    , kirundi = Nothing
                    }

                DangerSignRespiratoryDistress ->
                    { english = "Respiratory Distress"
                    , kinyarwanda = Just "Ahumeka bimugoye"
                    , kirundi = Nothing
                    }

                DangerSignSpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    , kirundi = Nothing
                    }

                DangerSignBloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    , kirundi = Nothing
                    }

                DangerSignNewSkinRash ->
                    { english = "New Skin Rash"
                    , kinyarwanda = Just "Yasheshe uduheri dushya"
                    , kirundi = Nothing
                    }

                NoAcuteIllnessDangerSign ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        AcuteIllnessDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Nothing
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Nothing
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Nothing
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Complicated Malaria"
                    , kinyarwanda = Just "Malariya y'igikatu"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Gastrointestinal Infection with Complications"
                    , kinyarwanda = Just "Indwara yo mu nda ikabije"
                    , kirundi = Nothing
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Gastrointestinal Infection without Complications"
                    , kinyarwanda = Just "Indwara yo mu nda yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Ibicurane n'inkorora byoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Acute Respiratory Infection with Complications"
                    , kinyarwanda = Just "Indwara y'ubuhumekero ikabije"
                    , kirundi = Nothing
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Umusonga woroheje"
                    , kirundi = Nothing
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Nothing
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Nothing
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        AcuteIllnessDiagnosisWarning diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19 case"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Nothing
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Nothing
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Nothing
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Malaria with Complications"
                    , kinyarwanda = Just "Afite Malariya y'igikatu"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Suspected Gastrointestinal Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda ikabije"
                    , kirundi = Nothing
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Suspected Gastrointestinal Infection (without Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda yoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Inkorora n'ibicurane byoroheje "
                    , kirundi = Nothing
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Suspected Acute Respiratory Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara y'ubuhumekero ikabije"
                    , kirundi = Nothing
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Suspected Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Aracyekwaho umusonga woroheje"
                    , kirundi = Nothing
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Nothing
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Nothing
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        AcuteIllnessExisting ->
            { english = "Existing Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Nothing
            }

        AcuteIllnessHistory ->
            { english = "Acute Illness History"
            , kinyarwanda = Just "Amakuru ku ndwara ifatiyeho"
            , kirundi = Nothing
            }

        AcuteIllnessLowRiskCaseHelper ->
            { english = "This patient is a low risk case and should be sent home to be monitored by a CHW"
            , kinyarwanda = Just "Uyu murwayi nta byago afite byo kuba yaranduye Covid-19, agomba koherezwa mu rugo agakurikiranwa n'umujyanama w'ubuzima"
            , kirundi = Nothing
            }

        AcuteIllnessNew ->
            { english = "New Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho nshyashya"
            , kirundi = Nothing
            }

        AcuteIllnessOutcomeLabel ->
            { english = "Acute Illness Outcome"
            , kinyarwanda = Just "Iherezo ry'indwara ifatiyeho\n"
            , kirundi = Nothing
            }

        AcuteIllnessStatus status ->
            case status of
                AcuteIllnessBegan ->
                    { english = "Began"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessUpdated ->
                    { english = "Updated"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ActiveDiagnosis ->
            { english = "Active Diagnosis"
            , kinyarwanda = Just "Uburwayi Bwasuzumwe"
            , kirundi = Nothing
            }

        AcuteIllnessOutcome outcome ->
            case outcome of
                OutcomeIllnessResolved ->
                    { english = "Illness Resolved"
                    , kinyarwanda = Just "Indwara Yarakize"
                    , kirundi = Nothing
                    }

                OutcomeLostToFollowUp ->
                    { english = "Lost to Follow Up"
                    , kinyarwanda = Just "Umurwayi yaburiwe irengero"
                    , kirundi = Nothing
                    }

                OutcomeMovedOutsideCA ->
                    { english = "Moved outside the catchment area"
                    , kinyarwanda = Just "Umurwayi yimukiye ahandi"
                    , kirundi = Nothing
                    }

                OutcomePatientDied ->
                    { english = "Patient Died"
                    , kinyarwanda = Just "Umurwayi yarapfuye"
                    , kirundi = Nothing
                    }

                Backend.IndividualEncounterParticipant.Model.OutcomeReferredToHC ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                OutcomeOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

        AddChild ->
            { english = "Add Child"
            , kinyarwanda = Just "Ongeraho umwana"
            , kirundi = Nothing
            }

        AddContact ->
            { english = "Add Contact"
            , kinyarwanda = Just "Ongeraho uwo bahuye"
            , kirundi = Nothing
            }

        AddedToPatientRecordOn ->
            { english = "Added to patient record on"
            , kinyarwanda = Just "Yongewe ku makuru y'umurwayi kuwa"
            , kirundi = Nothing
            }

        AddFamilyMember ->
            { english = "Add Family Member"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AddFamilyMemberFor name ->
            { english = "Add Family Member for " ++ name
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AddNewParticipant ->
            { english = "Add new participant"
            , kinyarwanda = Just "Ongeramo Umugenerwabikorwa musha"
            , kirundi = Nothing
            }

        AddParentOrCaregiver ->
            { english = "Add Parent or Caregiver"
            , kinyarwanda = Just "Ongeraho umubyeyi cyangwa umurezi"
            , kirundi = Nothing
            }

        AddToGroup ->
            { english = "Add to Group..."
            , kinyarwanda = Just "Ongeraho itsinda..."
            , kirundi = Nothing
            }

        Admin ->
            { english = "Administration"
            , kinyarwanda = Just "Abakuriye"
            , kirundi = Nothing
            }

        Administer ->
            { english = "Administer"
            , kinyarwanda = Just "Tanga umuti"
            , kirundi = Nothing
            }

        AdministerAzithromycinHelper ->
            { english = "By mouth 1x"
            , kinyarwanda = Just "Inshuro imwe mu kanwa"
            , kirundi = Nothing
            }

        AdministerCeftriaxoneHelper ->
            { english = "IM once"
            , kinyarwanda = Just "Urushinge mu mikaya inshuro imwe"
            , kirundi = Nothing
            }

        AdministerMebendezoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            , kirundi = Nothing
            }

        AdministerMetronidazoleHelper ->
            { english = "By mouth twice a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini inshuro ebyiri ku munsi mu minsi irindwi"
            , kirundi = Nothing
            }

        AdministerAlbendazoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            , kirundi = Nothing
            }

        AdministerPrenatalMebendezoleHelper ->
            { english = "1 dose once a day for one day"
            , kinyarwanda = Just "Ikinini kimwe inshuro imwe ku munsi mu munsi umwe"
            , kirundi = Nothing
            }

        AdministerFolicAcidHelper ->
            { english = "Take daily for 3 months"
            , kinyarwanda = Just "Fata imiti buri munsi mu gihe cy'amexi 3"
            , kirundi = Nothing
            }

        AdministerHIVARVHelper ->
            { english = "Take 1x a day by mouth"
            , kinyarwanda = Just "Fata ikinini 1 ku munsi mu kanwa"
            , kirundi = Nothing
            }

        AdministerIronHelper ->
            { english = "Take 1 60 mg tabs 2x a day x 3 months"
            , kinyarwanda = Just "Fata mg 1 60 inshuro 2 ku munsi mu mezi atatu"
            , kirundi = Nothing
            }

        AdministerParacetamolHelper ->
            { english = "Take 1 tablet by mouth 3 times a day for 5 days"
            , kinyarwanda = Just "Fata ikinini 1 mu kanwa inshuro 3 ku munsi mu minsi 5"
            , kirundi = Nothing
            }

        AdministerVitaminAHelperPrenatal ->
            { english = "Vitamin A is given once"
            , kinyarwanda = Just "Vitamine A itangwa inshuro 1"
            , kirundi = Nothing
            }

        AdministerVitaminAHelperWellChild ->
            { english = "Put the correct number of drops directly into the mouth of the child"
            , kinyarwanda = Just "Shyira mu kanwa k'umwana ibitonyanga bigenwe"
            , kirundi = Nothing
            }

        Administered ->
            { english = "Administered"
            , kinyarwanda = Just "Umuti watanzwe"
            , kirundi = Nothing
            }

        AdministeredMedicationQuestion ->
            { english = "Have you administered"
            , kinyarwanda = Just "Watanze umuti"
            , kirundi = Nothing
            }

        AdministeredOneOfAboveMedicinesQuestion ->
            { english = "Have you administered one of the above medicines to the patient"
            , kinyarwanda = Just "Waba wahaye umurwyayi umwe mu miti yavuzwe haruguru"
            , kirundi = Nothing
            }

        AddressInformation ->
            { english = "Address Information"
            , kinyarwanda = Just "Aho atuye/Aho abarizwa"
            , kirundi = Nothing
            }

        AfterEachLiquidStool ->
            { english = "after each liquid stool"
            , kinyarwanda = Just "buri uko amaze kwituma ibyoroshye"
            , kirundi = Nothing
            }

        AgeWord ->
            { english = "Age"
            , kinyarwanda = Just "Imyaka"
            , kirundi = Nothing
            }

        Activities ->
            { english = "Activities"
            , kinyarwanda = Just "Ibikorwa"
            , kirundi = Nothing
            }

        ActivitiesCompleted count ->
            { english = "Completed (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibyarangiye (" ++ String.fromInt count ++ ")"
            , kirundi = Nothing
            }

        ActivitiesHelp activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                    , kinyarwanda = Just "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                    , kirundi = Nothing
                    }

                MotherActivity Lactation ->
                    { english = "Ideally a mother exclusively breastfeeds her infant for at least 6 months. Every mother should be asked about how she is feeding her infant each month."
                    , kinyarwanda = Just "Ni byiza ko umubyeyi yonsa umwana we byibuze amezi 6 nta kindi amuvangiye. Buri mubyeyi agomba kubazwa uko agaburira umwana we buri kwezi."
                    , kirundi = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "If a mother is breastfeeding, she should receive FBF every month. If she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Please review the following forms with the participant."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "Every child should receive FBF every month. If he/she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NCDA ->
                    translationSet ChildScorecard

        ActivitiesLabel activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Which, if any, of the following methods do you use?"
                    , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                    , kirundi = Nothing
                    }

                MotherActivity Lactation ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    , kirundi = Nothing
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms:"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height:"
                    , kinyarwanda = Just "Uburebure:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC:"
                    , kinyarwanda = Just "Ikizigira cy'akaboko:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Select all signs that are present:"
                    , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo:"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight:"
                    , kinyarwanda = Just "Ibiro:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors:"
                    , kinyarwanda = Just "Impamvu zateye uburwayi:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up:"
                    , kinyarwanda = Just "Gukurikirana umurwayi:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education:"
                    , kinyarwanda = Just "Inyigisho ku buzima:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center:"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima:"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NCDA ->
                    { english = "Child Scorecard:"
                    , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana:"
                    , kirundi = Nothing
                    }

        ActivitiesTitle activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro?"
                    , kirundi = Nothing
                    }

                MotherActivity Lactation ->
                    { english = "Lactation"
                    , kinyarwanda = Just "Konsa"
                    , kirundi = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "FBF Mother"
                    , kinyarwanda = Just "FBF y'umubyeyi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NCDA ->
                    translationSet ChildScorecard

        ActivitityTitleAchi ->
            { english = "Aheza Child"
            , kinyarwanda = Just "Aheza igenewe umwana"
            , kirundi = Nothing
            }

        ActivityProgressReport activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    , kirundi = Nothing
                    }

                MotherActivity Lactation ->
                    { english = "Lactation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Counseling"
                   , kinyarwanda = Nothing
                   , kirundi = Nothing }
                -}
                ChildActivity ChildFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition Signs"
                    , kinyarwanda = Just "Ibimenyetso by'imirire"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                ChildActivity Activity.Model.NCDA ->
                    translationSet ChildScorecard

        ActivitiesToComplete count ->
            { english = "To Do (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibisabwa gukora (" ++ String.fromInt count ++ ")"
            , kirundi = Nothing
            }

        ActivitityLabelAchi ->
            { english = "Enter the amount of Aheza distributed below."
            , kinyarwanda = Just "Uzuza hano ingano ya Aheza utanze"
            , kirundi = Nothing
            }

        ActivePage page ->
            translateActivePage page

        AcuteIllnessActivityTitle activity ->
            case activity of
                AcuteIllnessSymptoms ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kongera kureba ibimenyetso"
                    , kirundi = Nothing
                    }

                AcuteIllnessPhysicalExam ->
                    { english = "Physical Exam"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Nothing
                    }

                AcuteIllnessPriorTreatment ->
                    { english = "Prior Treatment History"
                    , kinyarwanda = Just "Amakuru ku miti yafashe"
                    , kirundi = Nothing
                    }

                AcuteIllnessLaboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Nothing
                    }

                AcuteIllnessExposure ->
                    { english = "Exposure / Travel History"
                    , kinyarwanda = Just "Afite ibyago byo kwandura/amakuru ku ngendo yakoze"
                    , kirundi = Nothing
                    }

                AcuteIllnessNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

                AcuteIllnessOngoingTreatment ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Nothing
                    }

                AcuteIllnessDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    , kirundi = Nothing
                    }

        Adherence adherence ->
            translateAdherence adherence

        AdverseEventSinglePlural val ->
            if val == 1 then
                { english = "Adverse event"
                , kinyarwanda = Just "Ikintu kidasanzwe (bitewe n'imiti wafashe)"
                , kirundi = Nothing
                }

            else
                { english = "Adverse events"
                , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                , kirundi = Nothing
                }

        Age months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " iminsi"
            , kirundi = Nothing
            }

        AgeDays days ->
            { english = String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt days ++ " Iminsi"
            , kirundi = Nothing
            }

        AgeMonthsWithoutDay months ->
            { english = String.fromInt months ++ " months"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi"
            , kirundi = Nothing
            }

        AgeSingleBoth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Umunsi"
            , kirundi = Nothing
            }

        AgeSingleMonth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Iminsi"
            , kirundi = Nothing
            }

        AgeSingleDayWithMonth months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " Umunsi"
            , kirundi = Nothing
            }

        AgeSingleDayWithoutMonth months days ->
            { english = String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt days ++ " Umunsi"
            , kirundi = Nothing
            }

        AlertChwToFollowUp ->
            { english = "Alert CHW to follow up with patient"
            , kinyarwanda = Just "Menyesha umujyanama w'ubuzima gukurikirana umurwayi"
            , kirundi = Nothing
            }

        AgeOneYearOld ->
            { english = "One year old"
            , kinyarwanda = Just "Umwaka umwe"
            , kirundi = Nothing
            }

        AgeOneYearAndOneMonth ->
            { english = "One year and one month"
            , kinyarwanda = Just "Umwaka n'ukwezi kumwe"
            , kirundi = Nothing
            }

        AgeOneYearWithMonths months ->
            { english = "One year and " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| "Umwaka n'amezi " ++ String.fromInt months
            , kirundi = Nothing
            }

        AgeYearsWithSingleMonth years month ->
            { english = String.fromInt years ++ " years " ++ String.fromInt month ++ " month"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt years ++ " Ukwezi " ++ String.fromInt month
            , kirundi = Nothing
            }

        AgeYearsAndMonths years months ->
            { english = String.fromInt years ++ " years " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt years ++ " Amezi " ++ String.fromInt months
            , kirundi = Nothing
            }

        AILaboratoryTask task ->
            case task of
                LaboratoryMalariaTesting ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Nothing
                    }

                LaboratoryCovidTesting ->
                    { english = "Covid Rapid Test"
                    , kinyarwanda = Just "Ikizamini cya Covid-19 cyihuse"
                    , kirundi = Nothing
                    }

        And ->
            { english = "and"
            , kinyarwanda = Just "na"
            , kirundi = Nothing
            }

        AndSentence ->
            { english = "and"
            , kinyarwanda = Just "maze"
            , kirundi = Nothing
            }

        AntenatalCare ->
            { english = "Antenatal Care"
            , kinyarwanda = Just "Isuzuma ku mugore utwite"
            , kirundi = Nothing
            }

        AntenatalProgressReport ->
            { english = "Antenatal Progress Report"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AntenatalVisistsHistory ->
            { english = "Antenatal Visits History"
            , kinyarwanda = Just "Amakuru ku isurwa ry'umugore utwite"
            , kirundi = Nothing
            }

        AmbulancArrivalPeriodQuestion ->
            { english = "How long did it take the ambulance to arrive"
            , kinyarwanda = Just "Bitwara igihe kingana gute ngo imbangukiragutabara ihagere"
            , kirundi = Nothing
            }

        ANCEncountersNotRecordedQuestion ->
            { english = "Were there any ANC encounters that are not recorded above"
            , kinyarwanda = Just "Haba hari ipimishawa ry'inda ryakozwe bakaba batarabyanditse"
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
            , kirundi = Nothing
            }

        AppointmentConfirmation ->
            { english = "Appointment Confirmation"
            , kinyarwanda = Just "Kwemeza itariki yo kugaruka"
            , kirundi = Nothing
            }

        AppointmentConfirmationInstrunction ->
            { english = "The patient should visit the health center on the following date"
            , kinyarwanda = Just "Umubyeyi agomba kujya ku kigo nderabuzima ku itariki ikurikira"
            , kirundi = Nothing
            }

        All ->
            { english = "All"
            , kinyarwanda = Just "Uburwayi bwose"
            , kirundi = Nothing
            }

        AllowedValuesRangeHelper constraints ->
            { english = "Allowed values are between " ++ String.fromFloat constraints.minVal ++ " and " ++ String.fromFloat constraints.maxVal ++ "."
            , kinyarwanda = Just <| "Imibare yemewe iri hagati ya " ++ String.fromFloat constraints.minVal ++ " na " ++ String.fromFloat constraints.maxVal ++ "."
            , kirundi = Nothing
            }

        AreYouSure ->
            { english = "Are you sure?"
            , kinyarwanda = Just "Urabyizeye?"
            , kirundi = Nothing
            }

        Assessment ->
            { english = "Assessment"
            , kinyarwanda = Just "Ipimwa"
            , kirundi = Nothing
            }

        Asthma ->
            { english = "Asthma"
            , kinyarwanda = Just "Asthma (Agahema)"
            , kirundi = Nothing
            }

        At ->
            { english = "at"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Attendance ->
            { english = "Attendance"
            , kinyarwanda = Just "Ubwitabire"
            , kirundi = Nothing
            }

        AvoidingGuidanceReason value ->
            case value of
                AvoidingGuidanceHypertensionLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Nothing
                    }

                AvoidingGuidanceHypertensionKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    , kirundi = Nothing
                    }

                AvoidingGuidanceHypertensionPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Nothing
                    }

                AvoidingGuidanceHypertensionPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Nothing
                    }

                AvoidingGuidanceHypertensionReinforceAdherence ->
                    { english = "Reinforce adherence of existing dosage"
                    , kinyarwanda = Just "Shimangira umwigisha akamaro ko kubahiriza gufata imiti asanganwe"
                    , kirundi = Nothing
                    }

                AvoidingGuidanceHypertensionOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

        Baby ->
            { english = "Baby"
            , kinyarwanda = Just "Umwana"
            , kirundi = Nothing
            }

        BabyDiedOnDayOfBirthPreviousDelivery ->
            { english = "Live Birth but the baby died the same day in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
            , kirundi = Nothing
            }

        BabyName name ->
            { english = "Baby: " ++ name
            , kinyarwanda = Just <| "Umwana: " ++ name
            , kirundi = Nothing
            }

        Back ->
            { english = "Back"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BackendError ->
            { english = "Error contacting backend"
            , kinyarwanda = Just "Seriveri yerekanye amakosa akurikira"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                BreastPain ->
                    { english = "Are you experiencing breast pain"
                    , kinyarwanda = Just "Waba ubabara amabere"
                    , kirundi = Nothing
                    }

                BreastRedness ->
                    { english = "Are you experiencing breast redness"
                    , kinyarwanda = Just "Amabere yawe yaba atukuye"
                    , kirundi = Nothing
                    }

                EnoughMilk ->
                    { english = "Do you have enough milk for your baby to breastfeed at least 8 times per day"
                    , kinyarwanda = Just "Waba ufite amashereka ahagije yo konsa umwana wawe nibura inshuro 8 kumunsi"
                    , kirundi = Nothing
                    }

                LatchingWell ->
                    { english = "Is the baby latching well"
                    , kinyarwanda = Just "Umwana aronka neza"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        BeatsPerMinuteUnitLabel ->
            { english = "Beats per minute"
            , kinyarwanda = Just "Inshuro umutima utera ku munota"
            , kirundi = Nothing
            }

        BeginNewEncounter ->
            { english = "Begin a New Encounter"
            , kinyarwanda = Just "Tangira igikorwa gishya"
            , kirundi = Nothing
            }

        BirthDefect defect ->
            case defect of
                DefectBirthInjury ->
                    { english = "Birth Injury"
                    , kinyarwanda = Just "Impanuka zo mu kuvuka"
                    , kirundi = Nothing
                    }

                DefectCleftLipWithCleftPalate ->
                    { english = "Cleft Lip with Cleft Palate"
                    , kinyarwanda = Just "Ibibari k'umunwa n'urusenge rw'akanwa"
                    , kirundi = Nothing
                    }

                DefectCleftPalate ->
                    { english = "Cleft Palate"
                    , kinyarwanda = Just "Ibibari ku rusenge rw'akanwa"
                    , kirundi = Nothing
                    }

                DefectClubFoot ->
                    { english = "ClubFoot"
                    , kinyarwanda = Just "Ibirenge bitameze neza"
                    , kirundi = Nothing
                    }

                DefectMacrocephaly ->
                    { english = "Macrocephaly"
                    , kinyarwanda = Just "Umutwe munini cyane"
                    , kirundi = Nothing
                    }

                DefectGastroschisis ->
                    { english = "Gastroschisis"
                    , kinyarwanda = Just "Umwobo ku nda bituma imyanya yo mu nda iba hanze"
                    , kirundi = Nothing
                    }

                DefectHearingLoss ->
                    { english = "Hearing Loss"
                    , kinyarwanda = Just "Ubumuga bwo kutumva"
                    , kirundi = Nothing
                    }

                DefectUndescendedTestes ->
                    { english = "Undescended Testes"
                    , kinyarwanda = Just "Udusabo tw'itanga tutari mu mwanya watwo"
                    , kirundi = Nothing
                    }

                DefectHypospadias ->
                    { english = "Hypospadias"
                    , kinyarwanda = Just "Umwenge unyuramo inkari ku gice cyo hasi cy'imboro"
                    , kirundi = Nothing
                    }

                DefectInguinalHernia ->
                    { english = "Inguinal Hernia"
                    , kinyarwanda = Just "Urura rwamanutse ruva mu gice cyarwo"
                    , kirundi = Nothing
                    }

                DefectMicrocephaly ->
                    { english = "Microcephaly"
                    , kinyarwanda = Just "Umutwe muto cyane"
                    , kirundi = Nothing
                    }

                DefectNeuralTubes ->
                    { english = "Neural Tubes Defects"
                    , kinyarwanda = Just "Urutirigongo rudafunze neza"
                    , kirundi = Nothing
                    }

                DefectDownSyndrome ->
                    { english = "Down Syndrome"
                    , kinyarwanda = Just "Ikibazo giterwa no kuvukana uturangamuntu(Chromosomes) turenze utwangomwa"
                    , kirundi = Nothing
                    }

                DefectCongenitalHeart ->
                    { english = "CongenitalHeart Defects (CHD)"
                    , kinyarwanda = Just "Yavukanye ibibazo by'umutima"
                    , kirundi = Nothing
                    }

                DefectVentricalSeptal ->
                    { english = "Ventrical Septal Defect"
                    , kinyarwanda = Just "Ibibazo by'umutima"
                    , kirundi = Nothing
                    }

                DefectPulmonaryValveAtresiaAndStenosis ->
                    { english = "Pulmonary Valve Atresia and Stenosis"
                    , kinyarwanda = Just "Ibibazo by'umutima n'ibihaha"
                    , kirundi = Nothing
                    }

                NoBirthDefects ->
                    { english = "None"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        BirthDefectLabel ->
            { english = "Birth Defect"
            , kinyarwanda = Just "Yavukanye ubumuga"
            , kirundi = Nothing
            }

        BirthDefectsPresentQuestion ->
            { english = "Does the child have any birth defects"
            , kinyarwanda = Just "Hari ubumuga/bibazo umwana yaba yaravukanye"
            , kirundi = Nothing
            }

        BirthDefectsSelectionLabel ->
            { english = "Which of the following"
            , kinyarwanda = Just "Ni ubuhe muri ubu bukurikira"
            , kirundi = Nothing
            }

        BloodGlucose ->
            { english = "Blood Glucose"
            , kinyarwanda = Just "Ingano y'Isukari mu Maraso"
            , kirundi = Nothing
            }

        BloodPressure ->
            { english = "Blood Pressure"
            , kinyarwanda = Just "Umuvuduko w'amaraso"
            , kirundi = Nothing
            }

        BloodPressureElevatedOcassions ->
            { english = "Blood Pressure Elevated occasions"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BloodPressureDiaLabel ->
            { english = "Diastolic"
            , kinyarwanda = Just "Umuvuduko w'amaraso wo hasi"
            , kirundi = Nothing
            }

        BloodPressureSysLabel ->
            { english = "Systolic"
            , kinyarwanda = Just "Umubare w'umuvuduko w'amaraso wo hejuru"
            , kirundi = Nothing
            }

        BloodSmearQuestion ->
            { english = "Did you perform a blood smear"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BloodSmearLabel ->
            { english = "Malaria Blood Smear"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        BMI ->
            { english = "BMI"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        BMIHelper ->
            { english = "Calculated based on Height and Weight"
            , kinyarwanda = Just "Byabazwe hashingiwe ku burebure n'ibiro"
            , kirundi = Nothing
            }

        BodyTemperature ->
            { english = "Body Temperature"
            , kinyarwanda = Just "Ubushyuhe bw'umubiri"
            , kirundi = Nothing
            }

        Born ->
            { english = "Born"
            , kinyarwanda = Just "Kuvuka/ itariki y'amavuko"
            , kirundi = Nothing
            }

        BornUnderweight ->
            { english = "Born Underweight"
            , kinyarwanda = Just "Yavukanye ibiro bidashyitse"
            , kirundi = Nothing
            }

        BoughtClothesQuestion ->
            { english = "Have you bought clothes and other essential items for the child"
            , kinyarwanda = Just "Waba waraguze imyenda n'ibindi bikoresho by'ibanze bikenewe ku mwana"
            , kirundi = Nothing
            }

        BowedLegs ->
            { english = "Bowed Legs"
            , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
            , kirundi = Nothing
            }

        BpmUnit respiratoryRate ->
            { english = String.fromInt respiratoryRate ++ " bpm"
            , kinyarwanda = Just <| "Inshuro ahumeka ku munota " ++ String.fromInt respiratoryRate
            , kirundi = Nothing
            }

        BreathsPerMinuteUnitLabel ->
            { english = "Breaths per minute"
            , kinyarwanda = Just "Inshuro ahumeka ku munota"
            , kirundi = Nothing
            }

        BreastExam ->
            { english = "Breast Exam"
            , kinyarwanda = Just "Gusuzuma amabere"
            , kirundi = Nothing
            }

        BreastExamDischargeQuestion ->
            { english = "What kind of discharge"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                DischargeBrownOrBloody ->
                    { english = "Brown or Bloody"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                DischargeYellow ->
                    { english = "Yellow"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                DischargeGreen ->
                    { english = "Green"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        BreastExamQuestion ->
            { english = "Did you show the patient how to perform a self breast exam"
            , kinyarwanda = Just "Weretse umubyeyi uko yakwisuzuma amabere?"
            , kirundi = Nothing
            }

        BreastExamSign option ->
            case option of
                Mass ->
                    { english = "Mass"
                    , kinyarwanda = Just "Utubyimba mu Ibere"
                    , kirundi = Nothing
                    }

                Discharge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Gusohoka kw'ibintu bidasanzwe"
                    , kirundi = Nothing
                    }

                Infection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Nothing
                    }

                NormalBreast ->
                    translationSet Normal

                Warmth ->
                    { english = "Warmth"
                    , kinyarwanda = Just "Ubushyuhe"
                    , kirundi = Nothing
                    }

        BrittleHair ->
            { english = "Brittle Hair"
            , kinyarwanda = Just "Gucurama no guhindura ibara ku misatsi"
            , kirundi = Nothing
            }

        ByMouthDaylyForXDays days ->
            { english = "by mouth daily x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "ku munsi / mu  minsi " ++ String.fromInt days
            , kirundi = Nothing
            }

        ByMouthTwiceADayForXDays days ->
            { english = "by mouth twice per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            , kirundi = Nothing
            }

        ByMouthThreeTimesADayForXDays days ->
            { english = "by mouth three times per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            , kirundi = Nothing
            }

        Call114 ->
            { english = "Call 114"
            , kinyarwanda = Just "Hamagara 114"
            , kirundi = Nothing
            }

        Called114Question ->
            { english = "Were you able to talk with 114"
            , kinyarwanda = Just "Wabashije kuvugana n’abantu bo kuri 114"
            , kirundi = Nothing
            }

        Cancel ->
            { english = "Cancel"
            , kinyarwanda = Just "Guhagarika"
            , kirundi = Nothing
            }

        CandidiasisRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Candidiasis"
            , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso bya Kandidoze"
            , kirundi = Nothing
            }

        CandidiasisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Nothing
            }

        CandidiasisRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            , kirundi = Nothing
            }

        CannotStartEncounterLabel ->
            { english = "You cannot open a new encounter, as there's already a completed encounter today for"
            , kinyarwanda = Just "Ntago bishoboka gutangira isuzuma rishya, kuko hari isuzuma ryarangiye uyu munsi rya"
            , kirundi = Nothing
            }

        CardiacDisease ->
            { english = "Cardiac Disease"
            , kinyarwanda = Just "Indwara z'umutima"
            , kirundi = Nothing
            }

        CaregiverAccompanyQuestion ->
            { english = "Do you have a caregiver to accompany you to the health center when you give birth"
            , kinyarwanda = Just "Ufite umuntu wo kuguherekeza ku kigo nderabuzima igihe ugiye kubyara"
            , kirundi = Nothing
            }

        CaregiverName ->
            { english = "Caregiver's Name"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CaregiverNationalId ->
            { english = "Caregiver's National ID"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Just "Akagali"
            , kirundi = Nothing
            }

        CaseManagement ->
            { english = "Case Management"
            , kinyarwanda = Just "Gukurikirana Umurwayi"
            , kirundi = Nothing
            }

        CaseManagementFilterLabel filter ->
            case filter of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    , kirundi = Nothing
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterNutrition ->
                    { english = "Home Visit"
                    , kinyarwanda = Just "Gusura Umurwayi"
                    , kirundi = Nothing
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Nothing
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa ufite indwara zitandura"
                    , kirundi = Nothing
                    }

        CaseManagementPaneHeader encounterType ->
            case encounterType of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi wavuwe indwara zifatiyeho"
                    , kirundi = Nothing
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterNutrition ->
                    { english = "Child Nutrition Follow Up"
                    , kinyarwanda = Just "Gukurikirana imirire y'umwana"
                    , kirundi = Nothing
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Nothing
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Just "Ibizamini bikorerwa ufite indwara zitandura"
                    , kirundi = Nothing
                    }

        CentimeterShorthand ->
            { english = "cm"
            , kinyarwanda = Just "cm"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        CheckIn ->
            { english = "Check in:"
            , kinyarwanda = Just "Kureba abaje"
            , kirundi = Nothing
            }

        ChildCleanQuestion ->
            { english = "Is the sick child clean"
            , kinyarwanda = Just "Ese umwana urwaye afite isuku"
            , kirundi = Nothing
            }

        ChildHmisNumber ->
            { english = "Child HMIS Number"
            , kinyarwanda = Just "Numero y'umwana muri HMIS"
            , kirundi = Nothing
            }

        ChildDemographicInformation ->
            { english = "Child Demographic Information"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChildIdentification ->
            { english = "Child Identification"
            , kinyarwanda = Just "Umwirondoro w'Umwana"
            , kirundi = Nothing
            }

        ChildNutritionSignLabel sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    , kirundi = Nothing
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso"
                    , kirundi = Nothing
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "Kubura apeti /kunanirwa kurya"
                    , kirundi = Nothing
                    }

        ChildNutritionSignReport sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    , kirundi = Nothing
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None"
                    , kinyarwanda = Just "Nta bimenyetso"
                    , kirundi = Nothing
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "kubura apeti (kunanirwa kurya)"
                    , kirundi = Nothing
                    }

        Children ->
            { english = "Children"
            , kinyarwanda = Just "Abana"
            , kirundi = Nothing
            }

        ChildrenNames ->
            { english = "Children's names"
            , kinyarwanda = Just "Amazina y'umwana"
            , kirundi = Nothing
            }

        ChildrenNationalId ->
            { english = "Children's National ID"
            , kinyarwanda = Just "Indangamuntu y'umwana"
            , kirundi = Nothing
            }

        ChildScoreboardActivityTitle activity ->
            case activity of
                ChildScoreboardNCDA ->
                    { english = "Child Scorecard"
                    , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana"
                    , kirundi = Nothing
                    }

                ChildScoreboardVaccinationHistory ->
                    { english = "Vaccination History"
                    , kinyarwanda = Just "Amakuru ku Nkingo"
                    , kirundi = Nothing
                    }

        ChildScorecard ->
            { english = "Child Scorecard"
            , kinyarwanda = Just "Ifishi y’Imikurire y’Umwana"
            , kirundi = Nothing
            }

        ChooseOne ->
            { english = "Choose one"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CHWAction value ->
            case value of
                ActionPregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    , kirundi = Nothing
                    }

                ActionLabs ->
                    { english = "Labs"
                    , kinyarwanda = Just "Ibizamini byafashwe"
                    , kirundi = Nothing
                    }

                ActionDangerSignsPresent ->
                    { english = "Danger Signs Present"
                    , kinyarwanda = Just "Hagaragaye Ibimenyetso Mpuruza"
                    , kirundi = Nothing
                    }

                ActionReferredToHealthCenter ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe Ku Kigonderbuzima"
                    , kirundi = Nothing
                    }

                ActionAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Just "Kwemeza Itariki yo Kugarukaho"
                    , kirundi = Nothing
                    }

                ActionHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku Buzima"
                    , kirundi = Nothing
                    }

                ActionBirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    , kirundi = Nothing
                    }

        ChwActivity ->
            { english = "Chw Activity"
            , kinyarwanda = Just "Igikorwa cy'Umujyana w'Ubuzima"
            , kirundi = Nothing
            }

        ChildOf ->
            { english = "Child of"
            , kinyarwanda = Just "Umwana wa"
            , kirundi = Nothing
            }

        ChildName ->
            { english = "Child Name"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Clear ->
            { english = "Clear"
            , kinyarwanda = Just "Gukuraho"
            , kirundi = Nothing
            }

        ClickTheCheckMark ->
            { english = "Click the check mark if the mother / caregiver is in attendance. The check mark will appear green when a mother / caregiver has been signed in."
            , kinyarwanda = Just "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        Dashboard dashboard ->
            translateDashboard dashboard

        ClinicalProgressReport ->
            { english = "Clinical Progress Report"
            , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
            , kirundi = Nothing
            }

        CloseAcuteIllnessLabel ->
            { english = "or Close an Acute Illness"
            , kinyarwanda = Just "Cyangwa Ufunge Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Nothing
            }

        CloseAndContinue ->
            { english = "Close & Continue"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ColorAlertIndication indication ->
            case indication of
                ColorAlertRed ->
                    { english = "Red"
                    , kinyarwanda = Just "Umutuku"
                    , kirundi = Nothing
                    }

                ColorAlertYellow ->
                    { english = "Yellow"
                    , kinyarwanda = Just "Umuhondo"
                    , kirundi = Nothing
                    }

                ColorAlertGreen ->
                    { english = "Green"
                    , kinyarwanda = Just "Icyatsi"
                    , kirundi = Nothing
                    }

        Completed ->
            { english = "Completed"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CompleteFacilityReferralForm facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Complete a health center referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi ku kigo Nderabuzima"
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Complete a hospital referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza ku bitaro"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Complete a referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo koherza umurwayi"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Complete an ARV services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza muri serivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "Complete a NCD services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi muri service y'indwara zitandura"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        ContactedHC ->
            { english = "Contacted health center"
            , kinyarwanda = Just "Yamenyesheje ikigo nderabuzima"
            , kirundi = Nothing
            }

        ContactedHCQuestion ->
            { english = "Have you contacted the health center"
            , kinyarwanda = Just "Wamenyesheje ikigo nderabuzima"
            , kirundi = Nothing
            }

        ContactedRecommendedSiteQuestion ->
            { english = "Did you contact the recommended site"
            , kinyarwanda = Just "Wamenyesheje urwego rushinzwe gukurikirana umurwayi"
            , kirundi = Nothing
            }

        ContactInitiatedQuestion ->
            { english = "Where you able to speak with the contact"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ContactName ->
            { english = "Contact Name"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ContactsTracingCompleteDetails ->
            { english = "Please fill in contact details"
            , kinyarwanda = Just "Uzuza amakuru arambuye y'umuntu wahuye n'umurwayi"
            , kirundi = Nothing
            }

        ContactsTracingHelper ->
            { english = "Please record everyone that the patient has come into contact within 2 days of their symptoms beginning"
            , kinyarwanda = Just "Andika umuntu wese wahuye n'umurwayi mu minshi 2 ishize ibimenyetso bigaragaye"
            , kirundi = Nothing
            }

        ContactWithCOVID19SymptomsHelper ->
            { english = "Symptoms include:!!!! fever, dry cough, and shortness of breath"
            , kinyarwanda = Just "Ibimenyetso birimo: umuriro, inkorora y'akayi no guhumeka nabi"
            , kirundi = Nothing
            }

        ContactWithCOVID19SymptomsQuestion ->
            { english = "Have you had contacts with others who exhibit symptoms or have been exposed to COVID-19"
            , kinyarwanda = Just "Waba warigeze uhura n'abantu bagaragaje ibimenyetso bya covid-19 cyangwa n'abari bafite ibyago byo kuyandura"
            , kirundi = Nothing
            }

        Continued ->
            { english = "Continued"
            , kinyarwanda = Just "Yakomeje"
            , kirundi = Nothing
            }

        ContributingFactor factor ->
            case factor of
                FactorLackOfBreastMilk ->
                    { english = "Lack of breast milk (for children < 6 months)"
                    , kinyarwanda = Just "Kubura kw'amashereka (ku mwana uri munsi y'amezi atandatu)"
                    , kirundi = Nothing
                    }

                FactorMaternalMastitis ->
                    { english = "Maternal mastitis (for children < 6 months)"
                    , kinyarwanda = Just "Umubyeyi yabyimbye amabere (ku mwana uri munsi y'amezi atandatu)"
                    , kirundi = Nothing
                    }

                FactorPoorSuck ->
                    { english = "Poor suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Nothing
                    }

                FactorDiarrheaOrVomiting ->
                    { english = "Diarrhea or vomiting"
                    , kinyarwanda = Just "Impiswi cyangwa kuruka"
                    , kirundi = Nothing
                    }

                NoContributingFactorsSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Nothing
                    }

        ContributingFactorsQuestion ->
            { english = "Has patient or patient’s mother experienced any of the following"
            , kinyarwanda = Just "Umurwayi cyangwa umubyeyi we bagaragaje ibimenyetso bikurikira"
            , kirundi = Nothing
            }

        ConvulsionsAndUnconsciousPreviousDelivery ->
            { english = "Experienced convulsions and resulted in becoming unconscious after delivery"
            , kinyarwanda = Just "Ubushize yaragagaye bimuviramo kutumva akimara kubyara"
            , kirundi = Nothing
            }

        ConvulsionsPreviousDelivery ->
            { english = "Experienced convulsions in previous delivery"
            , kinyarwanda = Just "Ubushize yaragagaye abyara"
            , kirundi = Nothing
            }

        CurrentIllnessBegan ->
            { english = "Current illness began"
            , kinyarwanda = Just "Igihe ubu burwayi afite bwatangiriye"
            , kirundi = Nothing
            }

        CSectionScar scar ->
            case scar of
                Vertical ->
                    { english = "Vertical"
                    , kinyarwanda = Just "Irahagaze"
                    , kirundi = Nothing
                    }

                Horizontal ->
                    { english = "Horizontal"
                    , kinyarwanda = Just "Iratambitse"
                    , kirundi = Nothing
                    }

                NoScar ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        GroupNotFound ->
            { english = "Group not found"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Group ->
            { english = "Group"
            , kinyarwanda = Just "Itsinda"
            , kirundi = Nothing
            }

        Groups ->
            { english = "Groups"
            , kinyarwanda = Just "Itsinda"
            , kirundi = Nothing
            }

        Close ->
            { english = "Close"
            , kinyarwanda = Just "Gufunga"
            , kirundi = Nothing
            }

        Closed ->
            { english = "Closed"
            , kinyarwanda = Just "Gufunga"
            , kirundi = Nothing
            }

        GroupUnauthorized ->
            { english = "You are not authorized to work with this Group."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ConfirmDeleteTrainingGroupEncounters ->
            { english = "Are you sure you want to delete all training Group Encounters?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DeliveryComplication complication ->
            case complication of
                ComplicationGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yaje umugore utwite"
                    , kirundi = Nothing
                    }

                ComplicationEmergencyCSection ->
                    { english = "Emergency C-Section"
                    , kinyarwanda = Just "Kubagwa bitewe n'impamvu zihutirwa"
                    , kirundi = Nothing
                    }

                ComplicationPreclampsia ->
                    { english = "Preeclampsia"
                    , kinyarwanda = Just "Umuvuduko w'amaraso uza uje k'umugore twite (Preclampsia)"
                    , kirundi = Nothing
                    }

                ComplicationMaternalHemmorhage ->
                    { english = "Maternal Hemorrhage"
                    , kinyarwanda = Just "Kuva amaraso ku mubyeyi utwite cyangwa nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                ComplicationHiv ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                ComplicationMaternalDeath ->
                    { english = "Maternal Death"
                    , kinyarwanda = Just "Urupfu rw'umubyeyi"
                    , kirundi = Nothing
                    }

                ComplicationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoDeliveryComplications ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        DeliveryComplicationsPresentQuestion ->
            { english = "Were there any complications with the delivery"
            , kinyarwanda = Just "Haba hari ibibazo umubyeyi yagize abyara"
            , kirundi = Nothing
            }

        DeliveryComplicationsSelectionLabel ->
            { english = "Which of the following were present"
            , kinyarwanda = Just "Ni ibiki byagaragaye muri ibi bikurikira"
            , kirundi = Nothing
            }

        ConditionImproving isImproving ->
            if isImproving then
                { english = "Improving"
                , kinyarwanda = Just "Ari koroherwa"
                , kirundi = Nothing
                }

            else
                { english = "Not improving"
                , kinyarwanda = Just "Ntabwo ari koroherwa"
                , kirundi = Nothing
                }

        ConditionImprovingQuestion ->
            { english = "Is your condition improving"
            , kinyarwanda = Just "Urumva uri koroherwa"
            , kirundi = Nothing
            }

        ConfirmationRequired ->
            { english = "Please confirm:"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Connected ->
            { english = "Connected"
            , kinyarwanda = Just "Ufite interineti (murandasi)"
            , kirundi = Nothing
            }

        ContactExposure ->
            { english = "Contact Exposure"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ContactInformation ->
            { english = "Contact Information"
            , kinyarwanda = Just "Uburyo bwakwifashishwa mu kugera ku mugenerwabikorwa"
            , kirundi = Nothing
            }

        Continue ->
            { english = "Continue"
            , kinyarwanda = Just "Gukomeza"
            , kirundi = Nothing
            }

        CounselingTimingHeading timing ->
            translateCounselingTimingHeading timing

        CounselingTopic topic ->
            { english = topic.english
            , kinyarwanda = topic.kinyarwanda
            , kirundi = Nothing
            }

        CounselorReviewed ->
            { english = "I have reviewed the above with the participant."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CovidContactTracing ->
            { english = "Covid Contact Tracing"
            , kinyarwanda = Just "Gushakisha abahuye n'uwanduye Covid-19"
            , kirundi = Nothing
            }

        CovidTestingInstructions ->
            { english = "Perform a COVID-19 Rapid Test to confirm patient’s diagnosis"
            , kinyarwanda = Just "Kora ikizamini cyihuse cya Covid-19 kugira ngo hemezwe icyo umurwayi arwaye"
            , kirundi = Nothing
            }

        CounselorSignature ->
            { english = "Entry Counselor Signature"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CSectionInPreviousDelivery ->
            { english = "C-section in previous delivery"
            , kinyarwanda = Just "Yarabazwe ku nda ishize"
            , kirundi = Nothing
            }

        CSectionReason ->
            { english = "Reason for C-section"
            , kinyarwanda = Just "Impamvu yo kubagwa"
            , kirundi = Nothing
            }

        CSectionReasons reason ->
            case reason of
                Breech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Nothing
                    }

                Emergency ->
                    { english = "Emergency"
                    , kinyarwanda = Just "Ibyihutirwa"
                    , kirundi = Nothing
                    }

                FailureToProgress ->
                    { english = "Failure to Progress"
                    , kinyarwanda = Just "Ntibyiyongera"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.None ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

                Other ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                PreviousCSection ->
                    { english = "Previous c-section"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        CreateGroupEncounter ->
            { english = "Create Group Encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            , kirundi = Nothing
            }

        CreateRelationship ->
            { english = "Create Relationship"
            , kinyarwanda = Just "Ibijyanye no guhuza amasano"
            , kirundi = Nothing
            }

        CreateTrainingGroupEncounters ->
            { english = "Create All Training Group Encounters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CurrentlyPregnant ->
            { english = "Currently Pregnant"
            , kinyarwanda = Just "Aratwite"
            , kirundi = Nothing
            }

        CurrentlyPregnantQuestion ->
            { english = "Is the patient currently pregnant"
            , kinyarwanda = Just "Umurwayi aratwite"
            , kirundi = Nothing
            }

        CurrentStock ->
            { english = "Current Stock"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChwDashboardLabel ->
            { english = "CHW Snapshot"
            , kinyarwanda = Just "Ishusho y'ibyagezweho"
            , kirundi = Nothing
            }

        DeleteTrainingGroupEncounters ->
            { english = "Delete All Training Group Encounters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DashboardLabel ->
            { english = "Dashboard"
            , kinyarwanda = Just "Ikibaho cy’amakuru y’ingenzi"
            , kirundi = Nothing
            }

        DateReceived ->
            { english = "Date Received"
            , kinyarwanda = Just "Italiki yakiriweho"
            , kirundi = Nothing
            }

        DeliveryLocation ->
            { english = "Delivery Location"
            , kinyarwanda = Just "Aho yabyariye"
            , kirundi = Nothing
            }

        DeliveryOutcome ->
            { english = "Delivery Outcome"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DangerSign sign ->
            case sign of
                VaginalBleeding ->
                    { english = "Vaginal bleeding"
                    , kinyarwanda = Just "Kuva"
                    , kirundi = Nothing
                    }

                HeadacheBlurredVision ->
                    { english = "Severe headaches with blurred vision"
                    , kinyarwanda = Just "Kuribwa umutwe bidasanzwe ukareba ibikezikezi"
                    , kirundi = Nothing
                    }

                Convulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Nothing
                    }

                AbdominalPain ->
                    { english = "Severe Abdominal pain"
                    , kinyarwanda = Just "Kuribwa mu nda bikabije"
                    , kirundi = Nothing
                    }

                DifficultyBreathing ->
                    { english = "Difficulty breathing"
                    , kinyarwanda = Just "Guhumeka nabi"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.Fever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Nothing
                    }

                ExtremeWeakness ->
                    { english = "Extreme weakness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    , kirundi = Nothing
                    }

                ImminentDelivery ->
                    { english = "Imminent delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Nothing
                    }

                Labor ->
                    { english = "Labor"
                    , kinyarwanda = Just "Kujya ku nda"
                    , kirundi = Nothing
                    }

                LooksVeryIll ->
                    { english = "Looks very ill"
                    , kinyarwanda = Just "Ararembye cyane"
                    , kirundi = Nothing
                    }

                SevereVomiting ->
                    { english = "Severe vomiting"
                    , kinyarwanda = Just "Araruka bikabije"
                    , kirundi = Nothing
                    }

                Unconscious ->
                    { english = "Unconscious"
                    , kinyarwanda = Just "Yataye ubwenge"
                    , kirundi = Nothing
                    }

                GushLeakingVaginalFluid ->
                    { english = "Gush or leaking of vaginal fluid"
                    , kinyarwanda = Just "Ibintu biva mu gitsina by'uruzi"
                    , kirundi = Nothing
                    }

                PrematureOnsetContractions ->
                    { english = "Premature Onset of Contractions"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoDangerSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso/nta na kimwe"
                    , kirundi = Nothing
                    }

        DangerSignsLabelForChw ->
            { english = "Danger Signs"
            , kinyarwanda = Just "Ibimenyetso Mpuruza"
            , kirundi = Nothing
            }

        DangerSignsLabelForNurse ->
            { english = "Patient shows signs of"
            , kinyarwanda = Just "Umurwayi aragaragaza ibimenyetso bya"
            , kirundi = Nothing
            }

        DangerSignsTask task ->
            case task of
                ReviewDangerSigns ->
                    { english = "Review Danger Signs"
                    , kinyarwanda = Just "Kureba ibimenyetso mpuruza"
                    , kirundi = Nothing
                    }

        Date ->
            { english = "Date"
            , kinyarwanda = Just "Itariki"
            , kirundi = Nothing
            }

        DateConcludedEstimatedQuestion ->
            { english = "What was the estimated due date for the child"
            , kinyarwanda = Just "Ni iyihe taliki yari iteganyijwe ko umubyeyi azabyariraho"
            , kirundi = Nothing
            }

        DateOfContact ->
            { english = "Date of Contact"
            , kinyarwanda = Just "Itariki bahuriyeho"
            , kirundi = Nothing
            }

        DateOfLastAssessment ->
            { english = "Date of last Assessment"
            , kinyarwanda = Just "Amakuru y'ipimwa rirangiye"
            , kirundi = Nothing
            }

        DatePregnancyConcluded ->
            { english = "Date Pregnancy Concluded"
            , kinyarwanda = Just "Itariki y'iherezo ryo gutwita"
            , kirundi = Nothing
            }

        Day ->
            { english = "Day"
            , kinyarwanda = Just "Umunsi"
            , kirundi = Nothing
            }

        DayAbbrev ->
            { english = "Day"
            , kinyarwanda = Just "Umu"
            , kirundi = Nothing
            }

        DaySinglePlural value ->
            if value == 1 then
                { english = "1 Day"
                , kinyarwanda = Just "1 Umunsi"
                , kirundi = Nothing
                }

            else
                { english = String.fromInt value ++ " Days"
                , kinyarwanda = Just <| String.fromInt value ++ " Iminsi"
                , kirundi = Nothing
                }

        DateOfBirth ->
            { english = "Date of Birth"
            , kinyarwanda = Just "Itariki y'amavuko"
            , kirundi = Nothing
            }

        Days ->
            { english = "days"
            , kinyarwanda = Just "Iminsi"
            , kirundi = Nothing
            }

        DaysAbbrev ->
            { english = "days"
            , kinyarwanda = Just "Imi"
            , kirundi = Nothing
            }

        DaysPresent ->
            { english = "Days present"
            , kinyarwanda = Just "Igihe gishize"
            , kirundi = Nothing
            }

        DaysSinglePlural value ->
            if value == 1 then
                { english = "1 day"
                , kinyarwanda = Just "Umunsi 1"
                , kirundi = Nothing
                }

            else
                { english = String.fromInt value ++ " days"
                , kinyarwanda = Just <| "Iminsi " ++ String.fromInt value
                , kirundi = Nothing
                }

        Delete ->
            { english = "Delete"
            , kinyarwanda = Just "Gusiba"
            , kirundi = Nothing
            }

        DemographicInformation ->
            { english = "Demographic Information"
            , kinyarwanda = Just "Umwirondoro"
            , kirundi = Nothing
            }

        DemographicsReport ->
            { english = "Demographics Report"
            , kinyarwanda = Just "Raporo y'umwirondoro"
            , kirundi = Nothing
            }

        Details ->
            { english = "Details"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DetectableViralLoad ->
            { english = "Detectable Viral Load"
            , kinyarwanda = Just "Ingano ya virusi itera SIDA iracyagaragara mu maraso"
            , kirundi = Nothing
            }

        Device ->
            { english = "Device"
            , kinyarwanda = Just "Igikoresho"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        Diabetes ->
            { english = "Diabetes"
            , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
            , kirundi = Nothing
            }

        DiagnosedAtAnotherFacilityPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe"
            , kirundi = Nothing
            }

        DiagnosedAtAnotherFacilitySuffix ->
            { english = "at another facility and were given medication. Which medication was given?"
            , kinyarwanda = Just "Ku rindi vuriro wagiyeho ugahabwa imiti. Ni iyihe miti wahawe?"
            , kirundi = Nothing
            }

        DiagnosedByOutsideCare ->
            { english = "Diagnosed by outside care"
            , kinyarwanda = Just "Yasuzumiwe ku rindi vuriro"
            , kirundi = Nothing
            }

        DiagnosedOn ->
            { english = "Diagnosed on"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Diagnosis ->
            { english = "Diagnosis"
            , kinyarwanda = Just "Uburwayi bwabonetse"
            , kirundi = Nothing
            }

        DiagnosisDate ->
            { english = "Diagnosis Date"
            , kinyarwanda = Just "Itariki y'Isuzuma"
            , kirundi = Nothing
            }

        DifferenceBetweenDueAndDeliveryDates ->
            { english = "Difference between due date and delivery date"
            , kinyarwanda = Just "Ikinyuranyo kiri hagati y'amatariki"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                DistributedPartiallyOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Izindi mpamvu"
                    , kirundi = Nothing
                    }

        District ->
            { english = "District"
            , kinyarwanda = Just "Akarere"
            , kirundi = Nothing
            }

        DOB ->
            { english = "DOB"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Done ->
            { english = "Done"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DoTheFollowing ->
            { english = "Do the Following"
            , kinyarwanda = Just "Kora ibi bikurikira"
            , kirundi = Nothing
            }

        Downloading ->
            { english = "Downloading"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DropzoneDefaultMessage ->
            { english = "Touch here to take a photo, or drop a photo file here."
            , kinyarwanda = Just "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
            , kirundi = Nothing
            }

        DueDate ->
            { english = "Due Date"
            , kinyarwanda = Just "Itariki azabyariraho"
            , kirundi = Nothing
            }

        DueTo ->
            { english = "Due to"
            , kinyarwanda = Just "Kubera"
            , kirundi = Nothing
            }

        EarlyChildhoodDevelopment ->
            { english = "Early Childhood Development"
            , kinyarwanda = Just "Gahunda ikomatanije y'imikurire"
            , kirundi = Nothing
            }

        EarlyMastitisOrEngorgmentReliefMethod method ->
            case method of
                ReliefMethodBreastMassage ->
                    { english = "Massage"
                    , kinyarwanda = Just "Korera amabere masage"
                    , kirundi = Nothing
                    }

                ReliefMethodIncreaseFluid ->
                    { english = "Increase fluid"
                    , kinyarwanda = Just "Ongera ibyo kunywa"
                    , kirundi = Nothing
                    }

                ReliefMethodBreastfeedingOrHandExpression ->
                    { english = "continue breastfeeding or use hand expression"
                    , kinyarwanda = Just "komeza konsa cyangwa ukoreshe ikiganza wikame"
                    , kirundi = Nothing
                    }

        ECDSignQuestion sign ->
            case sign of
                FollowMothersEyes ->
                    { english = "Does the child follow their mothers eyes"
                    , kinyarwanda = Just "Umwana akurikiza amaso nyina"
                    , kirundi = Nothing
                    }

                MoveArmsAndLegs ->
                    { english = "Does the child move their arms and legs"
                    , kinyarwanda = Just "Umwana anyeganyeza amaboko n'amaguru"
                    , kirundi = Nothing
                    }

                RaiseHandsUp ->
                    { english = "Does the child raise their hands up"
                    , kinyarwanda = Just "Umwana azamura ibiganza"
                    , kirundi = Nothing
                    }

                Smile ->
                    { english = "Does the child smile"
                    , kinyarwanda = Just "Umwana araseka"
                    , kirundi = Nothing
                    }

                RollSideways ->
                    { english = "Does the child roll from left to right and right to left"
                    , kinyarwanda = Just "Umwana yihindukiza ku mpande, iburyo ni’ibumoso ikindi gihe akagana ibumoso n'iburyo"
                    , kirundi = Nothing
                    }

                BringHandsToMouth ->
                    { english = "Does the child bring their hands to their mouth"
                    , kinyarwanda = Just "Umwana akoza ibiganza bye ku murwa"
                    , kirundi = Nothing
                    }

                HoldHeadWithoutSupport ->
                    { english = "Does the child hold their head steady without support"
                    , kinyarwanda = Just "Umwana abasha kwemesha umutwe we ubwe ntawumufashe"
                    , kirundi = Nothing
                    }

                HoldAndShakeToys ->
                    { english = "Does the child hold and shake toys and swing at dangling toys"
                    , kinyarwanda = Just "Umwana ashobora gufata akanazunguza ibikinisho ndetse akabinyeganyeza iyo afite ibikinisho bivuga"
                    , kirundi = Nothing
                    }

                ReactToSuddenSounds ->
                    { english = "Does the child react to sudden noises or sounds"
                    , kinyarwanda = Just "Umwana agaragaza ko yumvise amajwi cg urusaku bitunguranye"
                    , kirundi = Nothing
                    }

                UseConsonantSounds ->
                    { english = "Is the child using consonant sounds in babbling, for example “da, da, da”"
                    , kinyarwanda = Just "Umwana akoresha amajwi yumvikanamo inyuguti igihe yivugisha, urugero:da,da,da,.."
                    , kirundi = Nothing
                    }

                RespondToSoundWithSound ->
                    { english = "Does the child respond to sound by making sound"
                    , kinyarwanda = Just "Umwana asubirisha ijwi igihe yumvise irindi ijwi"
                    , kirundi = Nothing
                    }

                TurnHeadWhenCalled ->
                    { english = "Does the child turn their head when their name is called"
                    , kinyarwanda = Just "Umwana ahindukiza umutwe iyo hari uhamagaye izina rye"
                    , kirundi = Nothing
                    }

                SitWithoutSupport ->
                    { english = "Can the child sit without support for a short while, for example sit on the floor on their own"
                    , kinyarwanda = Just "Umwana ashobora kwicara akanya gato nta kintu cyangwa umuntu umufashe"
                    , kirundi = Nothing
                    }

                SmileBack ->
                    { english = "Does the child smile back at you"
                    , kinyarwanda = Just "Umwana yaba agusekera iyo umusekeye"
                    , kirundi = Nothing
                    }

                RollTummyToBack ->
                    { english = "Can the child roll from their tummy to their back on their own"
                    , kinyarwanda = Just "Umwana ashobora kubura inda akagarama nta muntu umufashije"
                    , kirundi = Nothing
                    }

                ReachForToys ->
                    { english = "Does the child reach for nearby toys on their own"
                    , kinyarwanda = Just "Umwana ashobora gufata ibikinisho bimwegereye"
                    , kirundi = Nothing
                    }

                UseSimpleGestures ->
                    { english = "Does the child use simple gestures such as waving “bye-bye”"
                    , kinyarwanda = Just "Umwana akoresha ibimenyetso byoroheje nko gupepera iyo musezeranaho"
                    , kirundi = Nothing
                    }

                StandOnTheirOwn ->
                    { english = "Can the child stand on their own"
                    , kinyarwanda = Just "Umwana ashobora guhagarara nta muntu umufashe"
                    , kirundi = Nothing
                    }

                CopyDuringPlay ->
                    { english = "Does the child copy you during play"
                    , kinyarwanda = Just "Umwana yigana ibyo urimo gukora igihe mukina"
                    , kirundi = Nothing
                    }

                SayMamaDada ->
                    { english = "Does the child say “mama” and “dada”"
                    , kinyarwanda = Just "Umwana ashobora kuvuga “mama” cyangwa “dada”"
                    , kirundi = Nothing
                    }

                CanHoldSmallObjects ->
                    { english = "Can the child hold small objects that fit inside their hands"
                    , kinyarwanda = Just "Umwana ashobora gufata ibintu bito bikwiye mu kiganza cye"
                    , kirundi = Nothing
                    }

                LooksWhenPointedAt ->
                    { english = "Does the child look at something when you point to it and say “look”"
                    , kinyarwanda = Just "Umwana ashobora kwerekeza amaso ku kintu cyose umweretse"
                    , kirundi = Nothing
                    }

                UseSingleWords ->
                    { english = "Does the child use several single words to get what they want"
                    , kinyarwanda = Just "Umwana akoresha amagambo mato mato kandi yungikanye ashaka kugira icyo akubwira /agusaba"
                    , kirundi = Nothing
                    }

                WalkWithoutHelp ->
                    { english = "Does the child walk without help"
                    , kinyarwanda = Just "Umwana ashobora kugenda nta muntu umufashije"
                    , kirundi = Nothing
                    }

                PlayPretend ->
                    { english = "Does the child play pretend - like talking on a toy phone"
                    , kinyarwanda = Just "Umwana ajya akina asa nk'uvugira kuri telefoni"
                    , kirundi = Nothing
                    }

                PointToThingsOfInterest ->
                    { english = "Does the child point to interesting things"
                    , kinyarwanda = Just "Umwana atunga intoki ibintu bimunejeje"
                    , kirundi = Nothing
                    }

                UseShortPhrases ->
                    { english = "Does the child use 2-4 word phrases"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro igizwe n'amagambo 2 kugera kuri 4"
                    , kirundi = Nothing
                    }

                InterestedInOtherChildren ->
                    { english = "Does the child show interest in other children"
                    , kinyarwanda = Just "Umwana agaragaza ko yishimiye abandi bana"
                    , kirundi = Nothing
                    }

                FollowSimpleInstructions ->
                    { english = "Does the child follow simple instructions"
                    , kinyarwanda = Just "Umwana akurikiza amabwiriza yoroheje ahawe"
                    , kirundi = Nothing
                    }

                KickBall ->
                    { english = "Can the child kick a ball"
                    , kinyarwanda = Just "Umwana ashobora gutera umupira"
                    , kirundi = Nothing
                    }

                PointAtNamedObjects ->
                    { english = "Does the child point to something - like a toy or a picture - when you name it"
                    , kinyarwanda = Just "Umwana ashobora kukwereka ikintu agitunga urutoki iyo uvuze izina ryacyo, Urugero:Igikinisho cg ifoto"
                    , kirundi = Nothing
                    }

                DressThemselves ->
                    { english = "Can the child dress themselves"
                    , kinyarwanda = Just "Umwana ashobora kwiyambika"
                    , kirundi = Nothing
                    }

                WashHandsGoToToiled ->
                    { english = "Can the child wash their hands on their own and go to the toilet in the designated area on their own"
                    , kinyarwanda = Just "Umwana ashobora kwikarabya intoki, akanijyana mu bwiherero ahateganijwe wenyine"
                    , kirundi = Nothing
                    }

                KnowsColorsAndNumbers ->
                    { english = "Does the child know basic colors and numbers"
                    , kinyarwanda = Just "Umwana azi amabara n'imibare by'ibanze"
                    , kirundi = Nothing
                    }

                UseMediumPhrases ->
                    { english = "Does the child use 4-5 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo 2 kugera kuri 4"
                    , kirundi = Nothing
                    }

                PlayMakeBelieve ->
                    { english = "Does the child play make-believe"
                    , kinyarwanda = Just "Umwana akunda gukina yigana"
                    , kirundi = Nothing
                    }

                FollowThreeStepInstructions ->
                    { english = "Does the child follow 3-step commands - like “get dressed, comb your hair, and wash your face“"
                    , kinyarwanda = Just "Umwana ashobora gukurikiza amabwiriza nka 3 aherewe icyarimwe, Urugero:Ambara, Sokoza umusatsi unakarabe mu maso"
                    , kirundi = Nothing
                    }

                StandOnOneFootFiveSeconds ->
                    { english = "Can the child hop and stand on one foot for up to 5 seconds"
                    , kinyarwanda = Just "Umwana ashobora guhagarara ku kaguru kamwe akandi kanenetse mu gihe cy'amasegonda 5"
                    , kirundi = Nothing
                    }

                UseLongPhrases ->
                    { english = "Does the child use 5-6 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo atanu cyangwa atandatu"
                    , kirundi = Nothing
                    }

                ShareWithOtherChildren ->
                    { english = "Does the child share and take turns with other children"
                    , kinyarwanda = Just "Umwana ashobora gusangira no kujya ahererekanya ibintu n'abandi bana"
                    , kirundi = Nothing
                    }

                CountToTen ->
                    { english = "Can the child count to 10"
                    , kinyarwanda = Just "Umwana ashobora kubara kugeza ku 10"
                    , kirundi = Nothing
                    }

                NoECDSigns ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        ECDStatus status ->
            case status of
                StatusOnTrack ->
                    { english = "On Track"
                    , kinyarwanda = Just "Biri ku gihe"
                    , kirundi = Nothing
                    }

                StatusECDBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Biri inyuma"
                    , kirundi = Nothing
                    }

                StatusOffTrack ->
                    { english = "Off Track"
                    , kinyarwanda = Just "Ntibyakozwe"
                    , kirundi = Nothing
                    }

                NoECDStatus ->
                    { english = "No Status"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Edd ->
            { english = "EDD"
            , kinyarwanda = Just "Itariki y'agateganyo yo kubyara"
            , kirundi = Nothing
            }

        EddHeader ->
            { english = "Estimated Date of Delivery"
            , kinyarwanda = Just "Itariki y'agateganyo azabyariraho"
            , kirundi = Nothing
            }

        Edema ->
            { english = "Edema"
            , kinyarwanda = Just "Kubyimba"
            , kirundi = Nothing
            }

        EditRelationship ->
            { english = "Edit Relationship"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Ega ->
            { english = "EGA"
            , kinyarwanda = Just "Ibyumweru inda imaze"
            , kirundi = Nothing
            }

        EgaHeader ->
            { english = "Estimated Gestational Age"
            , kinyarwanda = Just "Amezi y'agateganyo y'inda"
            , kirundi = Nothing
            }

        EgaWeeks ->
            { english = "EGA (Weeks)"
            , kinyarwanda = Just "EGA (Ibyumweru)"
            , kirundi = Nothing
            }

        ElevatedRespiratoryRate ->
            { english = "Elevated respiratory rate"
            , kinyarwanda = Just "Inshuro ahumeka zazamutse"
            , kirundi = Nothing
            }

        EmergencyReferralHelperReferToHC ->
            { english = "Refer patient to health center immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku kigonderabuzima byihuse"
            , kirundi = Nothing
            }

        EmergencyReferralHelperReferToHospitalForEvaluation ->
            translationSet ReferToHospitalForFurtherEvaluation

        EmergencyReferralHelperReferToHospitalForImmediateDelivery ->
            { english = "Refer patient to hospital for immediate delivery"
            , kinyarwanda = Just "Ohereza umubyeyi aka kanya ku bitaro abyarireyo"
            , kirundi = Nothing
            }

        EmergencyReferralHelperReferToHospitalImmediately ->
            { english = "Refer patient to hospital immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku bitaro byihuse"
            , kirundi = Nothing
            }

        EmergencyReferralHelperReferToMaternityWard ->
            { english = "Refer to Maternity Ward Immediately"
            , kinyarwanda = Just "Ihutire kohereza umubyeyi aho babyarira"
            , kirundi = Nothing
            }

        EmergencyReferralHelperReferToEmergencyObstetricCareServices ->
            { english = "Stabilize and Refer to Emergency Obstetric Care Services"
            , kinyarwanda = Just "Tanga umuti w'ibanze uhite wohereza umubyeyi muri serivice zita ku babyeyi"
            , kirundi = Nothing
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Just ""
            , kirundi = Nothing
            }

        EncounterDate ->
            { english = "Encounter Date"
            , kinyarwanda = Just "Itariki igikorwa cyakoreweho"
            , kirundi = Nothing
            }

        EncounterTypeFollowUpQuestion encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Do you want to start a subsequent Acute Illness encounter for"
                    , kinyarwanda = Just "Urashaka Gutangira Ibikorwa bikurikiyeho ku burwayi bwa"
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    { english = "What type of Antenatal encounter would you like to start for"
                    , kinyarwanda = Just "Ni irihe suzuma ku mugore utwite ushaka gutangira kuri"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "Do you want to start a Home Visit assessment for"
                    , kinyarwanda = Just "Urashaka gutangira igikorwa cyo gusura mu rugo"
                    , kirundi = Nothing
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
                            , kirundi = Nothing
                            }

                        Covid19Page ->
                            { english = "COVID-19"
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        MalariaPage ->
                            { english = "Malaria"
                            , kinyarwanda = Just "Malariya"
                            , kirundi = Nothing
                            }

                        GastroPage ->
                            { english = "Gastro"
                            , kinyarwanda = Just "Indwara yo mu nda"
                            , kirundi = Nothing
                            }

                NutritionPage ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'Umwana"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        EndEncounterQuestion ->
            { english = "End Encounter?"
            , kinyarwanda = Just "Gusoza igikorwa?"
            , kirundi = Nothing
            }

        EndGroupEncounter ->
            { english = "End Group Encounter"
            , kinyarwanda = Just "Gusoza igikorwa"
            , kirundi = Nothing
            }

        EnrolNewborn ->
            { english = "Enroll Newborn"
            , kinyarwanda = Just "Andika Uruhinja"
            , kirundi = Nothing
            }

        EnrolNewbornHelper enrolled ->
            if enrolled then
                { english = "Newborn is already enrolled"
                , kinyarwanda = Just "Uruhinja rusanzwe rwanditse"
                , kirundi = Nothing
                }

            else
                { english = "Click on 'Enroll Newborn' button to perform enrollment"
                , kinyarwanda = Just "Kanda kuri 'Andika Uruhinja' kugira ngo urwandike"
                , kirundi = Nothing
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

        EnterAmountDistributed ->
            { english = "Enter amount distributed"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        EnterPairingCode ->
            { english = "Enter pairing code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        EntryStatusDiagnosis status ->
            case status of
                StatusOngoing ->
                    { english = "Ongoing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                StatusResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        MemoryQuota quota ->
            { english = "Memory used " ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya wa memori (ushobora kubika amakuru igihe gito) ungana na MB" ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " kuri MB" ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024))
            , kirundi = Nothing
            }

        StorageQuota quota ->
            { english = "Storage used " ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.quota // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya ungana na MB" ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " umwanya wose ungana na MB" ++ String.fromInt (quota.quota // (1024 * 1024))
            , kirundi = Nothing
            }

        SubmitPairingCode ->
            { english = "Submit Pairing Code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            , kirundi = Nothing
            }

        EPDSPreformedOn ->
            { english = "EPDS performed on"
            , kinyarwanda = Just "Igipimo cy'ukuntu yiyumva nyuma yo kubyara cyakozwe kuwa"
            , kirundi = Nothing
            }

        EpisiotomyOrPerinealTearQuestion ->
            { english = "Did the patient have an episiotomy or a perineal tear"
            , kinyarwanda = Just "Umubyeyi baramwongereye cg yaracitse abyara"
            , kirundi = Nothing
            }

        EpisiotomyOrPerinealTearHealingQuestion ->
            { english = "Is it healing normally"
            , kinyarwanda = Just "Igisebe kiri gukira neza"
            , kirundi = Nothing
            }

        ErrorCheckLocalConfig ->
            { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorConfigurationError ->
            { english = "Configuration error"
            , kinyarwanda = Just "Ikosa mu igena miterere"
            , kirundi = Nothing
            }

        Estimated ->
            { english = "Estimated"
            , kinyarwanda = Just "Itariki y'amavuko igenekerejwe"
            , kirundi = Nothing
            }

        ExaminationTask task ->
            case task of
                Vitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.NutritionAssessment ->
                    { english = "Nutrition Assessment"
                    , kinyarwanda = Just "Gusuzuma imirire"
                    , kirundi = Nothing
                    }

                CorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Just "Isuzuma ryimbitse"
                    , kirundi = Nothing
                    }

                ObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Just "Ibipimo by'inda"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.BreastExam ->
                    translationSet BreastExam

                GUExam ->
                    { english = "GU Exam"
                    , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina n'inzira z'inkari"
                    , kirundi = Nothing
                    }

        ExaminationTaskRecurrent task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.ExaminationVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                ExposureExposure ->
                    { english = "Contact Exposure"
                    , kinyarwanda = Just "Abantu mwahuye"
                    , kirundi = Nothing
                    }

        Failure ->
            { english = "Failure"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Extremities ->
            { english = "Extremities"
            , kinyarwanda = Just "Ku mpera z'ibice by'umubiri (ibiganza,ibirenge)"
            , kirundi = Nothing
            }

        Eyes ->
            { english = "Eyes"
            , kinyarwanda = Just "Amaso"
            , kirundi = Nothing
            }

        Facility ->
            { english = "Facility"
            , kinyarwanda = Just "Ivuriro"
            , kirundi = Nothing
            }

        FamilyInformation ->
            { english = "Family Information"
            , kinyarwanda = Just "Amakuru ku muryango"
            , kirundi = Nothing
            }

        FamilyMembers ->
            { english = "Family Members"
            , kinyarwanda = Just "Abagize umuryango"
            , kirundi = Nothing
            }

        FamilyPlanningCurentlyQuestion ->
            { english = "Which, if any, of the following methods do you use"
            , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha"
            , kirundi = Nothing
            }

        FamilyPlanningInFutureQuestion ->
            { english = "Which, if any, of these methods will you use after your pregnancy"
            , kinyarwanda = Just "Niba buhari, ni ubuhe buryo uzakoresha nyuma yo kubyara?"
            , kirundi = Nothing
            }

        FamilyPlanningSignLabel sign ->
            case sign of
                AutoObservation ->
                    { english = "Auto-observation"
                    , kinyarwanda = Just "Kwigenzura ururenda"
                    , kirundi = Nothing
                    }

                Condoms ->
                    { english = "Condoms"
                    , kinyarwanda = Just "Udukingirizo"
                    , kirundi = Nothing
                    }

                CycleBeads ->
                    { english = "Cycle beads"
                    , kinyarwanda = Just "Urunigi"
                    , kirundi = Nothing
                    }

                CycleCounting ->
                    { english = "Cycle counting"
                    , kinyarwanda = Just "Kubara "
                    , kirundi = Nothing
                    }

                Hysterectomy ->
                    { english = "Hysterectomy"
                    , kinyarwanda = Just "Bakuyemo nyababyeyi"
                    , kirundi = Nothing
                    }

                Implants ->
                    { english = "Implants"
                    , kinyarwanda = Just "Akapira ko mu kaboko"
                    , kirundi = Nothing
                    }

                Injectables ->
                    { english = "Injectables"
                    , kinyarwanda = Just "Urushinge"
                    , kirundi = Nothing
                    }

                IUD ->
                    { english = "IUD"
                    , kinyarwanda = Just "Akapira ko mu mura (agapira ko munda ibyara)"
                    , kirundi = Nothing
                    }

                LactationAmenorrhea ->
                    { english = "Lactation amenorrhea"
                    , kinyarwanda = Just "Uburyo bwo konsa"
                    , kirundi = Nothing
                    }

                NoFamilyPlanning ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta buryo bwo kuboneza urubyaro yahisemo"
                    , kirundi = Nothing
                    }

                OralContraceptives ->
                    { english = "Oral contraceptives"
                    , kinyarwanda = Just "Ibinini"
                    , kirundi = Nothing
                    }

                Spermicide ->
                    { english = "Spermicide"
                    , kinyarwanda = Just "Ibinini byica intangangabo bicishwa mu gitsina"
                    , kirundi = Nothing
                    }

                TubalLigatures ->
                    { english = "Tubal ligatures"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagore"
                    , kirundi = Nothing
                    }

                Vasectomy ->
                    { english = "Vasectomy"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagabo"
                    , kirundi = Nothing
                    }

        FamilyUbudehe ->
            { english = "Family Ubudehe"
            , kinyarwanda = Just "Icyiciro cy'ubudehe umuryango uherereyemo"
            , kirundi = Nothing
            }

        FbfDistribution clinicType ->
            case clinicType of
                Achi ->
                    { english = "Aheza Distribution"
                    , kinyarwanda = Just "Gutanga Aheza"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = "FBF Distribution"
                    , kinyarwanda = Just "Gutanga FBF (Shishakibondo)"
                    , kirundi = Nothing
                    }

        FbfToReceive activity amount ->
            case activity of
                MotherActivity _ ->
                    { english = "Mother should receive: " ++ String.fromFloat amount ++ " kgs of CSB++ (FBF)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildActivity _ ->
                    { english = "Child should receive: " ++ String.fromFloat amount ++ " kgs of CSB++ (FBF)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        FatherOrChiefId ->
            { english = "Father or Chief of Family ID"
            , kinyarwanda = Just "Indangamuntu y'Umukuru w'Umuryango"
            , kirundi = Nothing
            }

        FatherOrChiefName ->
            { english = "Fathers or Chief of Family Name"
            , kinyarwanda = Just "Amazina y'Umukuru w'muryango"
            , kirundi = Nothing
            }

        FatherNationalId ->
            { english = "Father's National ID"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        FetalMovement ->
            { english = "Fetal Movement"
            , kinyarwanda = Just "Uko umwana akina mu nda"
            , kirundi = Nothing
            }

        FetalPresentationLabel ->
            { english = "Fetal Presentation"
            , kinyarwanda = Just "Uko umwana ameze mu nda"
            , kirundi = Nothing
            }

        FetalPresentation option ->
            case option of
                FetalBreech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Nothing
                    }

                Cephalic ->
                    { english = "Cephalic"
                    , kinyarwanda = Just "Umwana abanje umutwe"
                    , kirundi = Nothing
                    }

                Transverse ->
                    { english = "Transverse"
                    , kinyarwanda = Just "Gitambitse (Umwana aritambitse)"
                    , kirundi = Nothing
                    }

                Twins ->
                    { english = "Twins"
                    , kinyarwanda = Just "Impanga"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    , kirundi = Nothing
                    }

        Fetch ->
            { english = "Fetch"
            , kinyarwanda = Just "Gushakisha"
            , kirundi = Nothing
            }

        FillTheBlanks ->
            { english = "Fill in the Blanks: Cyatsi, Hondo, Tuku & Ibipimo"
            , kinyarwanda = Just "Uzuza ukoresheje: Cyatsi, Hondo, Tuku & Ibipimo"
            , kirundi = Nothing
            }

        FilterByName ->
            { english = "Filter by name"
            , kinyarwanda = Just "Hitamo izina ryuwo ushaka"
            , kirundi = Nothing
            }

        Finish ->
            { english = "Finish"
            , kinyarwanda = Just "Soza igikorwa"
            , kirundi = Nothing
            }

        FirstAntenatalVisit ->
            { english = "First Antenatal Visit"
            , kinyarwanda = Just "Kwipimisha inda bwa mbere"
            , kirundi = Nothing
            }

        FirstName ->
            { english = "First Name"
            , kinyarwanda = Just "Izina ry'idini"
            , kirundi = Nothing
            }

        FiveVisits ->
            { english = "Five visits"
            , kinyarwanda = Just "Inshuro eshanu"
            , kirundi = Nothing
            }

        FoodGroup group ->
            case group of
                FoodGroupVegetables ->
                    { english = "Vegetables"
                    , kinyarwanda = Just "Imboga"
                    , kirundi = Nothing
                    }

                FoodGroupCarbohydrates ->
                    { english = "Carbohydrates"
                    , kinyarwanda = Just "Ibinyamasukari"
                    , kirundi = Nothing
                    }

                FoodGroupProtein ->
                    { english = "Protein"
                    , kinyarwanda = Just "Ibyubakumubiri"
                    , kirundi = Nothing
                    }

        FollowPostpartumProtocols ->
            { english = "Follow Postpartum Protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kwita ku mubyeyi wabyaye"
            , kirundi = Nothing
            }

        FollowUpWithPatientIn ->
            { english = "Follow up with patient in"
            , kinyarwanda = Just "Kurikirana umurwayi uri mu bitaro"
            , kirundi = Nothing
            }

        FollowUpWithPatientOn ->
            { english = "Follow up with patient on"
            , kinyarwanda = Just "Gukurikirana Umurwayi Ku itariki"
            , kirundi = Nothing
            }

        FollowUpByChwLabel ->
            { english = "CHW should follow up with patient in"
            , kinyarwanda = Just "Umujyanama w'ubuzima agomba gukurikirana umurwayi mu"
            , kirundi = Nothing
            }

        FollowUpLabel ->
            { english = "Follow up with the patient in"
            , kinyarwanda = Just "Gukurikirana umurwayi mu"
            , kirundi = Nothing
            }

        FollowUpWithMotherLabel ->
            { english = "Follow up with the mother in"
            , kinyarwanda = Just "Gukurikirana umubyeyi mu"
            , kirundi = Nothing
            }

        FollowUpOption option ->
            case option of
                OneDay ->
                    { english = "1 Day"
                    , kinyarwanda = Just "Umunsi 1"
                    , kirundi = Nothing
                    }

                ThreeDays ->
                    { english = "3 Days"
                    , kinyarwanda = Just "Iminsi 3"
                    , kirundi = Nothing
                    }

                OneWeek ->
                    { english = "1 Week"
                    , kinyarwanda = Just "Icyumweru 1"
                    , kirundi = Nothing
                    }

                TwoWeeks ->
                    { english = "2 Weeks"
                    , kinyarwanda = Just "Ibyumweru 2"
                    , kirundi = Nothing
                    }

                OneMonth ->
                    { english = "1 Month"
                    , kinyarwanda = Just "Ukwezi 1"
                    , kirundi = Nothing
                    }

                TwoMonths ->
                    { english = "2 Months"
                    , kinyarwanda = Just "Amezi 2"
                    , kirundi = Nothing
                    }

                ThreeMonths ->
                    { english = "3 Months"
                    , kinyarwanda = Just "Amezi 3"
                    , kirundi = Nothing
                    }

        FollowUpDueOption option ->
            case option of
                OverDue ->
                    { english = "Past Due"
                    , kinyarwanda = Just "Itariki yarenze"
                    , kirundi = Nothing
                    }

                DueToday ->
                    { english = "Due Today"
                    , kinyarwanda = Just "Itariki yageze uyu munsi"
                    , kirundi = Nothing
                    }

                DueThisWeek ->
                    { english = "This week"
                    , kinyarwanda = Just "Iki cyumweru"
                    , kirundi = Nothing
                    }

                DueThisMonth ->
                    { english = "This Month"
                    , kinyarwanda = Just "Uku kwezi"
                    , kirundi = Nothing
                    }

                DueNextMonth ->
                    { english = "Next Month"
                    , kinyarwanda = Just "Ukwezi gutaha"
                    , kirundi = Nothing
                    }

        FoodSupplementationConsumedQuestion ->
            { english = "Is the food supplementation being consumed"
            , kinyarwanda = Just "Inyongeramirire ihabwa umwana nkuko bikwiriye"
            , kirundi = Nothing
            }

        ForIllustrativePurposesOnly ->
            { english = "For illustrative purposes only"
            , kinyarwanda = Just "Ku mpamvu zumvikana gusa"
            , kirundi = Nothing
            }

        FormError errorValue ->
            translateFormError errorValue

        FormField field ->
            translateFormField field

        FundalHeight ->
            { english = "Fundal Height"
            , kinyarwanda = Just "Uburebure bwa Nyababyeyi"
            , kirundi = Nothing
            }

        FundalPalpableQuestion ->
            { english = "Is fundal palpable"
            , kinyarwanda = Just "Ese nyababyeyi irumvikana igihe usuzuma umubyeyi"
            , kirundi = Nothing
            }

        FundalPalpableWarning ->
            { english = "Inconsistent with documented gestational age, recommended ultrasound."
            , kinyarwanda = Just "Ntibihura n'ibyumweru by'inda byanditswe, urasabwa guca mu cyuma gisuzuma ababyeyi batwite."
            , kirundi = Nothing
            }

        Gender gender ->
            case gender of
                Male ->
                    { english = "Male"
                    , kinyarwanda = Just "Gabo"
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Female"
                    , kinyarwanda = Just "Gore"
                    , kirundi = Nothing
                    }

        GenderLabel ->
            { english = "Gender"
            , kinyarwanda = Just "Igitsina"
            , kirundi = Nothing
            }

        GestationalDiabetesPreviousPregnancy ->
            { english = "Gestational Diabetes in previous pregnancy"
            , kinyarwanda = Just "Ubushize yarwaye Diyabete itewe no gutwita"
            , kirundi = Nothing
            }

        Glass value ->
            { english = value ++ " Glass"
            , kinyarwanda = Just <| "Ikirahuri " ++ value
            , kirundi = Nothing
            }

        GoHome ->
            { english = "Go to main page"
            , kinyarwanda = Just "Kujya ahabanza"
            , kirundi = Nothing
            }

        GotResultsPreviouslyQuestion ->
            { english = "Has patient previously performed HBA1C test and got results"
            , kinyarwanda = Just "Umurwayi yaba yarakorewe ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize abona n'ibisubizo"
            , kirundi = Nothing
            }

        GroupAssessment ->
            { english = "Group Encounter"
            , kinyarwanda = Just "Gukorera itsinda"
            , kirundi = Nothing
            }

        Grams ->
            { english = "grams"
            , kinyarwanda = Just "Amagarama"
            , kirundi = Nothing
            }

        GroupEncounter ->
            { english = "Group Encounter"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GroupOfFoods value ->
            case value of
                Staples ->
                    { english = "Staples (grains, roots and tubers)"
                    , kinyarwanda = Just "Ibinyabijumba/Ibitera imbaraga"
                    , kirundi = Nothing
                    }

                Legumes ->
                    { english = "Legumes (beans, peas, cereals)"
                    , kinyarwanda = Just "Ibibyamisogwe (Ibishyimbo, amashyaza, ibinyampeke)"
                    , kirundi = Nothing
                    }

                DairyProducts ->
                    { english = "Dairy products"
                    , kinyarwanda = Just "Ibikomoka ku mata"
                    , kirundi = Nothing
                    }

                AnimalSourceFoods ->
                    { english = "Animal-source foods (flesh meats, eggs, fish, small fish (indagara))"
                    , kinyarwanda = Just "Ibikomoka ku matungo (inyama, amagi, amafi, indagara)"
                    , kirundi = Nothing
                    }

                Eggs ->
                    { english = "Eggs"
                    , kinyarwanda = Just "Amagi"
                    , kirundi = Nothing
                    }

                FruitsVegetables ->
                    { english = "Fruits and vegetables"
                    , kinyarwanda = Just "Imbuto n'Imboga"
                    , kirundi = Nothing
                    }

                BreastMilk ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Just "Konka"
                    , kirundi = Nothing
                    }

                MealsWithEdibleOil ->
                    { english = "Meals with added edible oil"
                    , kinyarwanda = Just "Ifunguro ryongewemo amavuta"
                    , kirundi = Nothing
                    }

        Growth ->
            { english = "Growth"
            , kinyarwanda = Just "Imikurire"
            , kirundi = Nothing
            }

        Gravida ->
            { english = "Gravida"
            , kinyarwanda = Just "Inda zose watwise"
            , kirundi = Nothing
            }

        HalfOfDosage dosage ->
            { english = "half of " ++ dosage ++ " dosage"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HandedReferralFormQuestion ->
            { english = "Did you hand the referral form to the patient"
            , kinyarwanda = Just "Wahaye umurwayi urupapuro rumwohereza"
            , kirundi = Nothing
            }

        HandPallor ->
            { english = "Hand Pallor"
            , kinyarwanda = Just "Ikiganza cyerurutse"
            , kirundi = Nothing
            }

        Hands ->
            { english = "Hands"
            , kinyarwanda = Just "Ibiganza"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        HbA1cPercentage ->
            { english = "Percentage (%)"
            , kinyarwanda = Just "Ku ijana (%)"
            , kirundi = Nothing
            }

        HbA1cMostRecentTestResultInstruction ->
            { english = "Please input the most recent HBA1C test result"
            , kinyarwanda = Just "Injiza ibisubizo biheruka ku kizamini gipima ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
            , kirundi = Nothing
            }

        HCRecommendation recommendation ->
            case recommendation of
                SendAmbulance ->
                    { english = "agreed to call the District Hospital to send an ambulance"
                    , kinyarwanda = Just "bemeranya guhamagara ibitaro ngo byohereze imbangukiragutabara"
                    , kirundi = Nothing
                    }

                HomeIsolation ->
                    { english = "advised patient to stay home in isolation"
                    , kinyarwanda = Just "bagira inama umurwayi yo kuguma mu rugo mu kato"
                    , kirundi = Nothing
                    }

                ComeToHealthCenter ->
                    { english = "advised patient to go to the health center for further evaluation"
                    , kinyarwanda = Just "kimugira inama yo kujya ku kigo nderabuzima gukoresha isuzuma ryimbitse"
                    , kirundi = Nothing
                    }

                ChwMonitoring ->
                    { english = "CHW should continue to monitor"
                    , kinyarwanda = Just "cyemeza ko umujyanama w’ubuzima agomba gukomeza gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

                HCRecommendationNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    , kirundi = Nothing
                    }

        HCResponseQuestion ->
            { english = "What was the Health Center's response"
            , kinyarwanda = Just "Ni ikihe gisubizo cyavuye ku kigo nderabuzima"
            , kirundi = Nothing
            }

        HCResponsePeriodQuestion ->
            { english = "How long did it take the Health Center to respond"
            , kinyarwanda = Just "Byatwaye igihe kingana gute ngo ikigo nderabuzima gisubize"
            , kirundi = Nothing
            }

        HeadCircumferenceHelper ->
            { english = "Using a tape measure, wrap the tape around the widest possible circumference; above the ears and midway between the eyebrows and the hairline to the occipital prominence on the back of the head."
            , kinyarwanda = Just "Wifashishije metero bushumi kandi umwana aryamye agaramye, zengurutsa iyo metero ku mutwe w'umwana hejuru y'amatwi uhereye inyuma, izenguruke ku gahanga  kugeza ugeze aho watangiriye."
            , kirundi = Nothing
            }

        HeadCircumferenceNotTakenLabel ->
            { english = "Please check if the head circumference was not taken today"
            , kinyarwanda = Just "Reba niba ibipimo by'umuzenguruko w'umutwe bitafashwe uyu munsi"
            , kirundi = Nothing
            }

        HeadHair ->
            { english = "Head/Hair"
            , kinyarwanda = Just "Umutwe/Umusatsi"
            , kirundi = Nothing
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Just "Ikigo Nderabuzima"
            , kirundi = Nothing
            }

        HealthCenterDetermined ->
            { english = "Health center determined this is a"
            , kinyarwanda = Just "Ikigo nderabuzima cyagaragaje ko"
            , kirundi = Nothing
            }

        HealthEducationNotProvided ->
            { english = "No health education provided"
            , kinyarwanda = Just "Nta nyigisho ku buzima zatanzwe"
            , kirundi = Nothing
            }

        HealthEducationProvided ->
            { english = "Health education provided"
            , kinyarwanda = Just "Hatanzwe inyigisho ku buzima"
            , kirundi = Nothing
            }

        HealthEducationProvidedQuestion ->
            { english = "Have you provided health education (or anticipatory guidance)"
            , kinyarwanda = Just "Watanze ikiganiro ku buzima (Cyangwa ubujyanama bw'ibanze)"
            , kirundi = Nothing
            }

        HealthInsuranceQuestion ->
            { english = "Do you have health insurance"
            , kinyarwanda = Just "Ufite ubwishingizi bwo kwivuza"
            , kirundi = Nothing
            }

        Heart ->
            { english = "Heart"
            , kinyarwanda = Just "Umutima"
            , kirundi = Nothing
            }

        HeartburnReliefMethod method ->
            case method of
                ReliefMethodAvoidLargeMeals ->
                    { english = "Avoid large, fatty meals"
                    , kinyarwanda = Just "Irinde ibiribwa byinshi, byongera ibinure"
                    , kirundi = Nothing
                    }

                ReliefMethodCeaseSmoking ->
                    { english = "Cease smoking "
                    , kinyarwanda = Just "Hagarika kunywa itabi"
                    , kirundi = Nothing
                    }

                ReliefMethodAvoidAlcohom ->
                    { english = "Avoid alcohol consumption "
                    , kinyarwanda = Just "Irinde kunywa ibisindisha"
                    , kirundi = Nothing
                    }

                ReliefMethodSleepWithHeadRaised ->
                    { english = "Sleep with their head raised in the bed"
                    , kinyarwanda = Just "Gerageza kuryama umutwe wegutse/useguye"
                    , kirundi = Nothing
                    }

        HeartburnRecommendedTreatmentHeader ->
            { english = "This patient has signs of persistent heartburn"
            , kinyarwanda = Just "Umubyeyi afite ikirungurira gihoraho"
            , kirundi = Nothing
            }

        HeartburnRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye wo guha uyu murwayi"
            , kirundi = Nothing
            }

        HeartMurmur ->
            { english = "Heart Murmur"
            , kinyarwanda = Just "Ijwi ry'umutima igihe utera"
            , kirundi = Nothing
            }

        HeartCPESign sign ->
            case sign of
                IrregularRhythm ->
                    { english = "Irregular Rhythm"
                    , kinyarwanda = Just "Injyana ihindagurika"
                    , kirundi = Nothing
                    }

                NormalRateAndRhythm ->
                    { english = "Normal Rate And Rhythm"
                    , kinyarwanda = Just "Bimeze neza/Injyana imeze neza"
                    , kirundi = Nothing
                    }

                SinusTachycardia ->
                    { english = "Sinus Tachycardia"
                    , kinyarwanda = Just "Gutera k'umutima birenze cyane igipimo gisanzwe"
                    , kirundi = Nothing
                    }

        HeartRate ->
            { english = "Heart Rate"
            , kinyarwanda = Just "Gutera k'umutima (inshuro umutima utera)"
            , kirundi = Nothing
            }

        Height ->
            { english = "Height"
            , kinyarwanda = Just "Uburebure"
            , kirundi = Nothing
            }

        High ->
            { english = "High"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HighRiskCase ->
            { english = "high-risk case"
            , kinyarwanda = Just "afite ibyago byinshi byo kuba yaranduye"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                HighRiskConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        HighRiskFactors ->
            { english = "High Risk Factors"
            , kinyarwanda = Just "Abafite ibyago byinshi byo"
            , kirundi = Nothing
            }

        HighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BodyTemperature ->
                    { english = "Body Temperature"
                    , kinyarwanda = Just "Ubushyuhe bw'umubiri"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.FetalHeartRate ->
                    { english = "No fetal heart rate noted"
                    , kinyarwanda = Just "Umutima w'umwana ntutera"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.FetalMovement ->
                    { english = "No fetal movement noted"
                    , kinyarwanda = Just "Umwana ntakina mu nda"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.HeartRate ->
                    { english = "Heart Rate"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.RespiratoryRate ->
                    { english = "Respiratory Rate"
                    , kinyarwanda = Just "Inshuro ahumeka"
                    , kirundi = Nothing
                    }

        HighSeverityAlerts ->
            { english = "High Severity Alerts"
            , kinyarwanda = Just "Bimenyetso mpuruza bikabije"
            , kirundi = Nothing
            }

        History ->
            { english = "History"
            , kinyarwanda = Just "Amakuru"
            , kirundi = Nothing
            }

        HistoryTask task ->
            case task of
                Obstetric ->
                    { english = "Obstetric History"
                    , kinyarwanda = Just "Amateka y'inda zibanza (ku nda yatwise)"
                    , kirundi = Nothing
                    }

                Medical ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    , kirundi = Nothing
                    }

                Social ->
                    { english = "Partner Information"
                    , kinyarwanda = Just "Amakuru y'uwo bashakanye (umugabo)"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.OutsideCare ->
                    translationSet OutsideCareLabel

        HIV ->
            { english = "HIV"
            , kinyarwanda = Just "Virusi itera SIDA"
            , kirundi = Nothing
            }

        HIVPCRResult result ->
            case result of
                ResultSuppressedViralLoad ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                Negative ->
                    translationSet NegativeLabel

                NegativeDiscordantCouple ->
                    { english = "Negative - discordant couple"
                    , kinyarwanda = Just "Nta bwandu afite ariko abana n'ubufite"
                    , kirundi = Nothing
                    }

                Positive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Nothing
                    }

                Backend.Person.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntabizi"
                    , kirundi = Nothing
                    }

        HIVStatusLabel ->
            { english = "HIV Status"
            , kinyarwanda = Just "Uko ahagaze ku bijyanye n'ubwandu bwa virusi ya SIDA"
            , kirundi = Nothing
            }

        HIVTreatmentSign sign ->
            case sign of
                HIVTreatmentNoMedicineNotSeenAtPMTCT ->
                    { english = "Never seen at PMTCT"
                    , kinyarwanda = Just "Ntiyigeze agera muri PMTCT"
                    , kirundi = Nothing
                    }

                HIVTreatmentNoMedicineOutOfStock ->
                    { english = "Stock Out"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Nothing
                    }

                HIVTreatmentNoMedicinePatientRefused ->
                    { english = "Patient Refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Nothing
                    }

                HIVTreatmentNoMedicineOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        HomeVisitActivityTitle activity ->
            case activity of
                Feeding ->
                    { english = "Feeding"
                    , kinyarwanda = Just "Kugaburira umwana"
                    , kirundi = Nothing
                    }

                Caring ->
                    { english = "Caring"
                    , kinyarwanda = Just "Kwita ku mwana"
                    , kirundi = Nothing
                    }

                Hygiene ->
                    { english = "Hygiene"
                    , kinyarwanda = Just "Isuku"
                    , kirundi = Nothing
                    }

                FoodSecurity ->
                    { english = "Food Security"
                    , kinyarwanda = Just "Kwihaza ku biribwa"
                    , kirundi = Nothing
                    }

        HouseholdSize ->
            { english = "Household Size"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HowManyDoses ->
            { english = "How many doses"
            , kinyarwanda = Just "Ingahe"
            , kirundi = Nothing
            }

        HaveAnyOfTheFollowingQuestion ->
            { english = "Do you have any of the following"
            , kinyarwanda = Just "Waba wagize ibi bikurikira?"
            , kirundi = Nothing
            }

        HttpError error ->
            translateHttpError error

        HoursSinglePlural value ->
            if value == 1 then
                { english = "1 Hour"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = String.fromInt value ++ " Hours"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        HowManyPerWeek ->
            { english = "How many per week"
            , kinyarwanda = Just "Unywa imiti y'itabi ingahe ku cyumweru"
            , kirundi = Nothing
            }

        Hypertension ->
            { english = "Hypertension"
            , kinyarwanda = Just "Indwara y'umuvuduko w'amaraso"
            , kirundi = Nothing
            }

        HypertensionAndPregnantHeader ->
            { english = "This patient has Hypertension and is pregnant"
            , kinyarwanda = Just "Uyu murwayi afite indwara y'umuvuduko w'amaraso kandi aratwite"
            , kirundi = Nothing
            }

        HypertensionBeforePregnancy ->
            { english = "Hypertension before pregnancy"
            , kinyarwanda = Just "Umuvuduko w'amaraso mbere yo gutwita"
            , kirundi = Nothing
            }

        HypertensionRecommendedTreatmentHeader isChronic ->
            if isChronic then
                { english = "This patient shows signs of Chronic hypertension"
                , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                , kirundi = Nothing
                }

            else
                { english = "This patient shows signs of Pregnancy-Induced hypertension"
                , kinyarwanda = Just "Uyu Murwayi agaragaza ibimenyetso by'Umuvuduko w'amaraso watewe no gutwita"
                , kirundi = Nothing
                }

        HypertensionRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            , kirundi = Nothing
            }

        HypertensionRecommendedTreatmentUpdateHeader forModeratePreeclamsia ->
            if forModeratePreeclamsia then
                { english = "This patient was previously diagnosed with Moderate Preeclamsia"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize preekalampusi"
                , kirundi = Nothing
                }

            else
                { english = "This patient was previously diagnosed with Hypertension"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize umuvuduko w'amaraso"
                , kirundi = Nothing
                }

        HypertensionRecommendedTreatmentUpdateBPLabel ->
            { english = "The patients current BP is"
            , kinyarwanda = Just "Ubu umubyeyi afite umuvuduko w'amaraso ungana na"
            , kirundi = Nothing
            }

        HypertensionRecommendedTreatmentUpdateCurrentTreatment ->
            { english = "The patient is currently prescribed"
            , kinyarwanda = Just "Ubu umubyeyi afata imiti ikurikira"
            , kirundi = Nothing
            }

        HypertensionRecommendedTreatmentUpdateNewTreatment value ->
            case value of
                TreatementUpdateMaintainCurrentDoasage ->
                    { english = "It is recommended that the medication remain unchanged -"
                    , kinyarwanda = Just "Birasabwa ko imiti idahinduka -"
                    , kirundi = Nothing
                    }

                TreatementUpdateIncreaseOneDose ->
                    { english = "It is recommended that the medication is increased one dosage level to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze imwe kuri"
                    , kirundi = Nothing
                    }

                TreatementUpdateIncreaseTwoDoses ->
                    { english = "It is recommended that the medication is increased two dosage levels to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze ebyiri kuri"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        HypertensionRecommendedTreatmentUpdateStartTreatment ->
            { english = "It is recommended to start treatment with"
            , kinyarwanda = Just "Arasabwa gutangirira kuri iyi miti"
            , kirundi = Nothing
            }

        HypertensionStageAndRenalComplicationsHeader renalComplications diagnosis ->
            case diagnosis of
                DiagnosisHypertensionStage1 ->
                    if renalComplications then
                        { english = "This patient has Stage One Hypertension with Renal Complications"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri ku rwego rwa mbere n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Nothing
                        }

                    else
                        { english = "This patient has Stage One Hypertension"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri ku rwego rwa mbere"
                        , kirundi = Nothing
                        }

                DiagnosisHypertensionStage2 ->
                    if renalComplications then
                        { english = "This patient has Stage Two Hypertension with Renal Complications"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Nothing
                        }

                    else
                        { english = "This patient has Stage Two Hypertension"
                        , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri"
                        , kirundi = Nothing
                        }

                DiagnosisHypertensionStage3 ->
                    { english = "This patient has Stage Three Hypertension"
                    , kinyarwanda = Just "Uyu murwayi afite umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                IllnessSymptomVisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Just "Uko areba byahindutse"
                    , kirundi = Nothing
                    }

                IllnessSymptomRash ->
                    { english = "Rash on body, feet or hands"
                    , kinyarwanda = Just "Ari kwishimagura ku mubiri: ku birenge cg ibiganza"
                    , kirundi = Nothing
                    }

                IllnessSymptomPainlessUlcerMouth ->
                    { english = "Painless ulcer in mouth"
                    , kinyarwanda = Just "Agasebe kataryana mu kanwa"
                    , kirundi = Nothing
                    }

                IllnessSymptomPainlessUlcerGenitals ->
                    { english = "Painless ulcer in genital area"
                    , kinyarwanda = Just "Agasebe kataryana mu myanya ndangagitsina"
                    , kirundi = Nothing
                    }

                NoIllnessSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        Immunisation ->
            { english = "Immunization"
            , kinyarwanda = Just "Inkingo"
            , kirundi = Nothing
            }

        ImmunisationHistory ->
            { english = "Immunization History"
            , kinyarwanda = Just "Amakuru ku nkingo yafashe"
            , kirundi = Nothing
            }

        IncompleteCervixPreviousPregnancy ->
            { english = "Incomplete Cervix in previous pregnancy"
            , kinyarwanda = Just "Ubushize inkondo y'umura ntiyashoboye kwifunga neza"
            , kirundi = Nothing
            }

        IndexPatient ->
            { english = "Index Patient"
            , kinyarwanda = Just "Umubare w'umurwayi"
            , kirundi = Nothing
            }

        IndividualEncounter ->
            { english = "Individual Encounter"
            , kinyarwanda = Just "Gukorera umuntu umwe"
            , kirundi = Nothing
            }

        IndividualEncounterFirstVisit encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "First Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa cya mbere ku burwayi"
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    { english = "First Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mugore utwite"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounter ->
                    { english = "First Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku ifishi y'imikurire y'umwana"
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "First Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo bwambere"
                    , kirundi = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "First Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "First NCD Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere kuburwayi butandura"
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = "First Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mirire"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    { english = "First Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura rya mbere ku mwana"
                    , kirundi = Nothing
                    }

        IndividualEncounterLabel encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa ku burwayi butunguranye"
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounter ->
                    { english = "Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifish y'Imikurire y'Umwana"
                    , kirundi = Nothing
                    }

                HomeVisitEncounter ->
                    { english = "Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo"
                    , kirundi = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Just "Isuzuma ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Newborn Exam"
                        , kinyarwanda = Just "Isuzuma ry'uruhinja"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Standard Pediatric Visit Encounter"
                        , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                        , kirundi = Nothing
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
                        { english = "Select Newborn Exam Visit"
                        , kinyarwanda = Just "Hitamo isuzuma ry'Uruhinja"
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
                    , kirundi = Nothing
                    }

                AntenatalEncounter ->
                    translationSet AntenatalCare

                ChildScoreboardEncounter ->
                    translationSet ChildScorecard

                HomeVisitEncounter ->
                    { english = "Home Visit"
                    , kinyarwanda = Just "Gusura Umurwayi"
                    , kirundi = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDEncounter ->
                    { english = "Noncommunicable Diseases"
                    , kinyarwanda = Just "Indwara Zitandura"
                    , kirundi = Nothing
                    }

                NutritionEncounter ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'umwana"
                    , kirundi = Nothing
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Newborn Exam"
                        , kinyarwanda = Just "Isuzuma ry'uruhinja"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Standard Pediatric Visit"
                        , kinyarwanda = Just "Kujyana Umwana mu Isuzumiro"
                        , kirundi = Nothing
                        }

        IndividualEncounterTypes ->
            { english = "Individual Encounter Types"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InfrastructureEnvironment ->
            { english = "Infrastructure, Environment"
            , kinyarwanda = Just "Ibikorwa remezo n’ibidukikije"
            , kirundi = Nothing
            }

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Just "Ibikorwaremezo, Ibidukikije n'Amazi"
            , kirundi = Nothing
            }

        InitialResultsDisplay display ->
            case display of
                InitialResultsHidden ->
                    { english = "Display all mothers / caregivers"
                    , kinyarwanda = Just "Kugaragaza ababyeyi bose / abarezi"
                    , kirundi = Nothing
                    }

                InitialResultsShown ->
                    { english = "Hide all mothers / caregivers"
                    , kinyarwanda = Just "Hisha ababyeyi bose / abarezi"
                    , kirundi = Nothing
                    }

        IntractableVomiting isIntractable ->
            if isIntractable then
                { english = "Intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bikabije"
                , kirundi = Nothing
                }

            else
                { english = "Non-intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bidakabije"
                , kirundi = Nothing
                }

        IntractableVomitingQuestion ->
            { english = "Is Vomiting Intractable"
            , kinyarwanda = Just "Kuruka bikabije"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        IsThisYouQuestion ->
            { english = "Is this you"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        KilogramsPerMonth ->
            { english = "kgs / month"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        KnownAsPositiveQuestion task ->
            case task of
                TaskHIVTest ->
                    { english = "Is this patient known to be HIV positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite ubwandu bwa virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskSyphilisTest ->
                    { english = "Is this patient known to be Syphilis - RPR positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite uburwayi bwa Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Is this patient known to be Hepatitis B positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara y'umwijima yo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                TaskMalariaTest ->
                    { english = "Is this patient known to be Malaria positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara ya Malariya"
                    , kirundi = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Is this patient known to be pregnant"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko atwite?"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        KnownPositiveHepatitisB ->
            { english = "Known Hepatitis B positive"
            , kinyarwanda = Just "Asanzwe afite indwara y'Umwijima wo mu bwoko bwa B"
            , kirundi = Nothing
            }

        KnownPositiveHIV ->
            { english = "Known HIV positive"
            , kinyarwanda = Just "Asanzwe afite Ubwandu bw'agakoko gatera SIDA"
            , kirundi = Nothing
            }

        LabelOnePregnancyEpisodeOpen ->
            { english = "There is one pregnancy episode that is open"
            , kinyarwanda = Just "Hari isuzuma rigifunguye ku mugore utwite"
            , kirundi = Nothing
            }

        LabelSeenHealthcareProviderForPregnancy ->
            { english = "Have you seen a healthcare provider for current pregnancy"
            , kinyarwanda = Just "Waba warigeze usuzumwa n'umuganga kuri iyinda utwite"
            , kirundi = Nothing
            }

        LabelDocumentPregnancyOutcome ->
            { english = "No - document pregnancy outcome"
            , kinyarwanda = Just "Ntabwo iherezo ry'inda ryanditswe"
            , kirundi = Nothing
            }

        Lab ->
            { english = "Lab"
            , kinyarwanda = Just "Ibizamini"
            , kirundi = Nothing
            }

        LabHistory ->
            { english = "Lab History"
            , kinyarwanda = Just "Amakuru ku bizamini byakozwe"
            , kirundi = Nothing
            }

        LaboratoryBloodGroupLabel ->
            { english = "Blood Group"
            , kinyarwanda = Just "Ubwoko bw'Amaraso"
            , kirundi = Nothing
            }

        LaboratoryBloodGroupTestResult ->
            { english = "Blood Group Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cy'ubwoko bw'amaraso"
            , kirundi = Nothing
            }

        LaboratoryBloodGroup value ->
            case value of
                BloodGroupA ->
                    { english = "A"
                    , kinyarwanda = Just "Ubwoko bwa A"
                    , kirundi = Nothing
                    }

                BloodGroupB ->
                    { english = "B"
                    , kinyarwanda = Just "Ubwoko bwa B"
                    , kirundi = Nothing
                    }

                BloodGroupAB ->
                    { english = "AB"
                    , kinyarwanda = Just "Ubwoko bwa AB"
                    , kirundi = Nothing
                    }

                BloodGroupO ->
                    { english = "O"
                    , kinyarwanda = Just "Ubwoko bwa O"
                    , kirundi = Nothing
                    }

        LaboratoryRhesusLabel ->
            { english = "Rhesus"
            , kinyarwanda = Just "Rezisi"
            , kirundi = Nothing
            }

        LaboratoryRhesusTestResult ->
            { english = "Rhesus Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cya Rezisi"
            , kirundi = Nothing
            }

        LaboratoryRhesus value ->
            case value of
                RhesusPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite Resisi pisitifu"
                    , kirundi = Nothing
                    }

                RhesusNegative ->
                    translationSet NegativeLabel

        LaboratoryProteinLabel ->
            { english = "Protein"
            , kinyarwanda = Just "Proteyine"
            , kirundi = Nothing
            }

        LaboratoryProteinTestResult ->
            { english = "Protein Test Result"
            , kinyarwanda = Just "Ibisubizo bya proteyine"
            , kirundi = Nothing
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
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LaboratoryGlucoseTestResult ->
            { english = "Glucose Test Result"
            , kinyarwanda = Just "Ibisubizo by'isukari mu nkari"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LaboratoryLeukocytesTestResult ->
            { english = "Leukocytes Test Result"
            , kinyarwanda = Just "Igisubizo k'uturemangingo twera"
            , kirundi = Nothing
            }

        LaboratoryLeukocytesValue value ->
            case value of
                LeukocytesNegative ->
                    translationSet NegativeLabel

                LeukocytesSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Insoro zera nke zigaragara mu nkari (+)"
                    , kirundi = Nothing
                    }

                LeukocytesMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Insoro zera ziringaniye zigaragara mu nkari (++)"
                    , kirundi = Nothing
                    }

                LeukocytesLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Insoro zera nyinshi zigaragara mu nkari (+++)"
                    , kirundi = Nothing
                    }

        LaboratoryNitriteLabel ->
            { english = "Nitrite"
            , kinyarwanda = Just "Umunyu wapimwe mu nkari"
            , kirundi = Nothing
            }

        LaboratoryNitriteTestResult ->
            { english = "Nitrite Test Result"
            , kinyarwanda = Just "Ibisubizo kumunyu wapimwe mu nkari"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LaboratoryUrobilinogenTestResult ->
            { english = "Urobilinogen Test Result"
            , kinyarwanda = Just "Igisubizo cya urobilinogene (mu nkari)"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LaboratoryHaemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo by'ikizamini gipima ingano y'amaraso"
            , kirundi = Nothing
            }

        LaboratoryHaemoglobinValue value ->
            case value of
                HaemoglobinNegative ->
                    translationSet NegativeLabel

                HaemoglobinNonHemolyzedTrace ->
                    { english = "Non-Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura nkeya zidashwanyutse"
                    , kirundi = Nothing
                    }

                HaemoglobinNonHemolyzedModerate ->
                    { english = "Non-Hemolyzed Moderate"
                    , kinyarwanda = Just "Insoro  zitukura  ziri mu rugero zidashwanyutse"
                    , kirundi = Nothing
                    }

                HaemoglobinHemolyzedTrace ->
                    { english = "Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura zashwanyutse"
                    , kirundi = Nothing
                    }

                HaemoglobinSmall ->
                    { english = "Small"
                    , kinyarwanda = Just "Ikigero gito cy'amaraso agaragara mu nkari"
                    , kirundi = Nothing
                    }

                HaemoglobinModerate ->
                    { english = "Moderate"
                    , kinyarwanda = Just "Ikigero kiringaniye cy'amaraso agaragara mu nkari"
                    , kirundi = Nothing
                    }

                HaemoglobinLarge ->
                    { english = "Large"
                    , kinyarwanda = Just "Ikigero kinini cy'amaraso (hemoglobini)  agaragara mu nkari"
                    , kirundi = Nothing
                    }

        LaboratoryKetoneLabel ->
            { english = "Ketone Test"
            , kinyarwanda = Just "Ikizamini cya Ketone mu nkari"
            , kirundi = Nothing
            }

        LaboratoryKetoneTestResult ->
            { english = "Ketone Test Result"
            , kinyarwanda = Just "Igisubizo cya Ketone (mu nkari)"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LaboratoryBilirubinTestResult ->
            { english = "Bilirubin Test Result"
            , kinyarwanda = Just "Igisubizo cya Bililibine (mu nkari)"
            , kirundi = Nothing
            }

        LaboratoryBilirubinValue value ->
            case value of
                BilirubinNegative ->
                    translationSet NegativeLabel

                BilirubinSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Byoroheje"
                    , kirundi = Nothing
                    }

                BilirubinMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Bikabije"
                    , kirundi = Nothing
                    }

                BilirubinLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Bikabije cyane"
                    , kirundi = Nothing
                    }

        LaboratoryHemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'amaraso"
            , kirundi = Nothing
            }

        LaboratoryRandomBloodSugarTestResult ->
            { english = "Random Blood Sugar Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
            , kirundi = Nothing
            }

        LaboratoryHIVPCRTestResult ->
            { english = "HIV PCR Test Result"
            , kinyarwanda = Just "Ibisubizo by'ikizamini cya PCR gipima Virusi itera SIDA"
            , kirundi = Nothing
            }

        LaboratoryHIVPCRViralLoadStatusQuestion ->
            { english = "Are there less than 20 copies/mm3"
            , kinyarwanda = Just "Haba hari kopi ziri munsi ya 20 kuri mirimrtro kibe"
            , kirundi = Nothing
            }

        LaboratoryCreatinineLabel ->
            { english = "Creatinine"
            , kinyarwanda = Just "Keleyatinine"
            , kirundi = Nothing
            }

        LaboratoryBUNLabel ->
            { english = "BUN"
            , kinyarwanda = Just "Ikizamini cy'Impyiko"
            , kirundi = Nothing
            }

        LaboratoryALTLabel ->
            { english = "ALT"
            , kinyarwanda = Just "Ikizamini cy'Impyiko"
            , kirundi = Nothing
            }

        LaboratoryASTLabel ->
            { english = "AST"
            , kinyarwanda = Just "Ikizamini cy'Umwijima"
            , kirundi = Nothing
            }

        LaboratoryPregnancyLabel ->
            { english = "Pregnancy"
            , kinyarwanda = Just "Ikizamini cyo Gutwita"
            , kirundi = Nothing
            }

        LaboratoryTest value ->
            case value of
                TestBloodGpRs ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    , kirundi = Nothing
                    }

                TestHemoglobin ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    , kirundi = Nothing
                    }

                TestHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                TestRandomBloodSugar ->
                    { english = "Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    , kirundi = Nothing
                    }

                TestSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                TestUrineDipstick ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    , kirundi = Nothing
                    }

                TestVitalsRecheck ->
                    { english = "Vitals Recheck"
                    , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
                    , kirundi = Nothing
                    }

                TestHIVPCR ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "PCR ipima Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TestCreatinine ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Nothing
                    }

                TestLiverFunction ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Nothing
                    }

                TestLipidPanel ->
                    translationSet LipidPanel

        PrenatalLabsCaseManagementEntryTypeResults ->
            { english = "ANC Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe ku mugore utwite"
            , kirundi = Nothing
            }

        PrenatalLabsCaseManagementEntryTypeVitals ->
            { english = "Vitals Recheck"
            , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
            , kirundi = Nothing
            }

        LabsEntryState state ->
            case state of
                LabsEntryPending ->
                    { english = "Pending"
                    , kinyarwanda = Just "Birategerejwe"
                    , kirundi = Nothing
                    }

                LabsEntryClosingSoon ->
                    { english = "Closing Soon"
                    , kinyarwanda = Just "Birafunga vuba"
                    , kirundi = Nothing
                    }

        LabsHistoryCompletedQuestion ->
            { english = "Have you updated all results that have been returned for this patient"
            , kinyarwanda = Just "Waba wujuje ibisubizo byose byaba byabonetse kuri uyu mubyeyi"
            , kirundi = Nothing
            }

        LabsHistoryCompletedInstructions ->
            { english = "Please update all outstanding lab results before proceeding"
            , kinyarwanda = Just "Gerageza gushyiramo ibisubizo byose mbere yo gukomeza"
            , kirundi = Nothing
            }

        LabsHistoryCompletedLabel ->
            { english = "This patient has pending lab results"
            , kinyarwanda = Just "Umubyeyi afite ibizamini bitarabonerwa ibisubizo"
            , kirundi = Nothing
            }

        LaboratoryCreatinineCreatinineResult ->
            { english = "Creatinine Result"
            , kinyarwanda = Just "Ibisubizo by'ikizamini cya Keleyatinine"
            , kirundi = Nothing
            }

        LaboratoryCreatinineBUNResult ->
            { english = "BUN Result"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'impyiko"
            , kirundi = Nothing
            }

        LaboratoryLipidPanelUnitOfMeasurementQuestion ->
            { english = "What units are the test results in"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LaboratoryLipidPanelTotalCholesterolLabel ->
            { english = "Total Cholesterol"
            , kinyarwanda = Just "Igipimo cy'ibinure byose mu maraso (Total cholesterol)"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu Bwoko bwa B"
                    , kirundi = Nothing
                    }

                TaskMalariaTest ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Nothing
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    , kirundi = Nothing
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini k'Inkari"
                    , kirundi = Nothing
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    , kirundi = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    , kirundi = Nothing
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "Ikizamini cya PCR gipima ubwandu bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Nothing
                    }

                TaskLipidPanelTest ->
                    translationSet LipidPanel

                TaskHbA1cTest ->
                    { english = "HBA1C"
                    , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Nothing
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV"
                    , kinyarwanda = Just "Ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Nothing
                    }

                TaskCompletePreviousTests ->
                    translationSet History

        LaboratoryTaskLabel task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse Gipima Virusi Itera SIDA"
                    , kirundi = Nothing
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Ikizamini cyihuse gipima Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Ikizamini gipima umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse cya Malariya"
                    , kirundi = Nothing
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus"
                    , kinyarwanda = Just "Ikizamini cyubwoko bw'amaraso na ReZisi"
                    , kirundi = Nothing
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    , kirundi = Nothing
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ikizamini gipima ingano y'amaraso"
                    , kirundi = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ikizamini gipima ingano y' isukari mu maraso"
                    , kirundi = Nothing
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "Ikizamini cya PCR gipima ubwandu bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Just "Keleyatinine"
                    , kirundi = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Just "Imikorere y'Umwijima"
                    , kirundi = Nothing
                    }

                TaskLipidPanelTest ->
                    translationSet LipidPanel

                TaskHbA1cTest ->
                    { english = "HBA1C"
                    , kinyarwanda = Just "Ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Nothing
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV"
                    , kinyarwanda = Just "Ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR Test Date"
                    , kinyarwanda = Just "Itariki yakorereweho ikizamini cya Mburugu"
                    , kirundi = Nothing
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cya Malariya"
                    , kirundi = Nothing
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'ubwoka bw'amaraso na Rezisi yayo"
                    , kirundi = Nothing
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'inkari"
                    , kirundi = Nothing
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'amaraso"
                    , kirundi = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'isukari mu maraso"
                    , kirundi = Nothing
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya PCR gipima virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cyo gutwita"
                    , kirundi = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya Keleyatinine"
                    , kirundi = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Date"
                    , kinyarwanda = Just "itariki y'ikizamini cy'Imikorere y'Umwijima"
                    , kirundi = Nothing
                    }

                TaskLipidPanelTest ->
                    { english = "Lipid Panel Test Date"
                    , kinyarwanda = Just "Itariki y'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Nothing
                    }

                TaskHbA1cTest ->
                    { english = "HBA1C Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Nothing
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Result"
                    , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
                    , kirundi = Nothing
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya PCR gipima Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cyo gutwita"
                    , kirundi = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Keleyatinine"
                    , kirundi = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'Imikorere y'Umwijima"
                    , kirundi = Nothing
                    }

                TaskLipidPanelTest ->
                    { english = "Lipid Panel Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Nothing
                    }

                TaskHbA1cTest ->
                    { english = "HBA1C Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'ikigereranyo cy'isukari mu maraso mu mezi atatu ashize"
                    , kirundi = Nothing
                    }

                TaskPartnerHIVTest ->
                    { english = "Partner HIV Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LabResults ->
            { english = "Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
            , kirundi = Nothing
            }

        LabResultsHistoryModeLabel mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    { english = "HIV Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Vursi itera SIDA"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHIVPCR _ ->
                    { english = "HIV PCR Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya PCR gipima Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                LabResultsHistoryPartnerHIV _ ->
                    { english = "Partner HIV Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Virusi itera SIDA k'umugabo we"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                LabResultsHistoryTotalCholesterol _ ->
                    { english = "Total Cholesterol"
                    , kinyarwanda = Just "Igipimo cy'ibinure byose mu maraso (Total cholesterol)"
                    , kirundi = Nothing
                    }

                LabResultsHistoryLDLCholesterol _ ->
                    { english = "LDL Cholesterol"
                    , kinyarwanda = Just "Ingano y'ibinure bibi mu maraso (LDL Cholesterol)"
                    , kirundi = Nothing
                    }

                LabResultsHistoryHDLCholesterol _ ->
                    { english = "HDL Cholesterol"
                    , kinyarwanda = Just "Ingano y'ibinure byiza mu maraso (HDL Cholesterol)"
                    , kirundi = Nothing
                    }

                LabResultsHistoryTriglycerides _ ->
                    { english = "Triglycerides"
                    , kinyarwanda = Just "Ingano y'ibinure bibitse mu mubiri (Triglycerides)"
                    , kirundi = Nothing
                    }

        LabResultsNormalRange mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    translationSet NegativeLabel

                LabResultsHistoryHIVPCR _ ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                LabResultsCurrentDipstickShort ->
                    { english = "Short Dipstick Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari gitanga ibisubizo bike"
                    , kirundi = Nothing
                    }

                LabResultsCurrentDipstickLong ->
                    { english = "Long Dipstick Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari gitanga ibisubizo byinshi"
                    , kirundi = Nothing
                    }

                LabResultsCurrentLipidPanel ->
                    { english = "Lipid Panel Results"
                    , kinyarwanda = Just "Ibisubizo by'ibizamini bipima ibinure (Lipid Panel)"
                    , kirundi = Nothing
                    }

        LastChecked ->
            { english = "Last checked"
            , kinyarwanda = Just "Isuzuma riheruka"
            , kirundi = Nothing
            }

        LastContacted ->
            { english = "Last Contacted"
            , kinyarwanda = Just "Igihe baheruka guhurira"
            , kirundi = Nothing
            }

        LastSuccesfulContactLabel ->
            { english = "Last Successful Contact"
            , kinyarwanda = Just "Itariki n'isaha yanyuma igikoresho giheruka gukoresherezaho interineti bikagenda neza"
            , kirundi = Nothing
            }

        LeaveEncounter ->
            { english = "Leave Encounter"
            , kinyarwanda = Just "Reka iki Gikorwa"
            , kirundi = Nothing
            }

        Left ->
            { english = "Left"
            , kinyarwanda = Just "Ibumoso"
            , kirundi = Nothing
            }

        LegCrampsReliefMethod method ->
            case method of
                ReliefMethodMuscleStretching ->
                    { english = "Muscle stretching"
                    , kinyarwanda = Just "Kurambura imitsi"
                    , kirundi = Nothing
                    }

                ReliefMethodDorsiflexion ->
                    { english = "Dorsiflexion"
                    , kinyarwanda = Just "Imyitozo ngororamubiri inanura amaguru & ibirenge"
                    , kirundi = Nothing
                    }

                ReliefMethodRelaxation ->
                    { english = "Relaxation"
                    , kinyarwanda = Just "Kuruhuka"
                    , kirundi = Nothing
                    }

                ReliefMethodSleepWithPillowBetweenLegs ->
                    { english = "Sleep with a pillow between the legs"
                    , kinyarwanda = Just "Ryama ushyize umusego hagati y'amaguru"
                    , kirundi = Nothing
                    }

                ReliefMethodHeatTherapy ->
                    { english = "Heat therapy"
                    , kinyarwanda = Just "Kuvura hakoreshejwe ubushyuhe"
                    , kirundi = Nothing
                    }

                ReliefMethodMassage ->
                    { english = "Massage"
                    , kinyarwanda = Just "Ubugororangingo"
                    , kirundi = Nothing
                    }

        LegLeft ->
            { english = "Left leg"
            , kinyarwanda = Just "Ukuguru kw'ibumoso"
            , kirundi = Nothing
            }

        LegRight ->
            { english = "Right leg"
            , kinyarwanda = Just "Ukuguru kw'iburyo"
            , kirundi = Nothing
            }

        Legs ->
            { english = "Legs"
            , kinyarwanda = Just "Amaguru"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        LevelOfEducation educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    , kirundi = Nothing
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    , kirundi = Nothing
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational Training School"
                    , kinyarwanda = Just "Imyuga"
                    , kirundi = Nothing
                    }

                SecondarySchool ->
                    { english = "Secondary School"
                    , kinyarwanda = Just "Ayisumbuye"
                    , kirundi = Nothing
                    }

                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    , kirundi = Nothing
                    }

                HigherEducation ->
                    { english = "Higher Education (University)"
                    , kinyarwanda = Just "(A0)"
                    , kirundi = Nothing
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma"
                    , kinyarwanda = Just "(A1)"
                    , kirundi = Nothing
                    }

                -- Not in use.
                MastersDegree ->
                    { english = "Masters Degree"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LevelOfEducationForResilience educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    , kirundi = Nothing
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    , kirundi = Nothing
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational School"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SecondarySchool ->
                    { english = "Finished Secondary School"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Not it use.
                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    , kirundi = Nothing
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma (A1)"
                    , kinyarwanda = Just "(A1)"
                    , kirundi = Nothing
                    }

                HigherEducation ->
                    { english = "Bachelors Degree (A0)"
                    , kinyarwanda = Just "(A0)"
                    , kirundi = Nothing
                    }

                MastersDegree ->
                    { english = "Masters Degree"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LinkToMother ->
            { english = "Link to mother"
            , kinyarwanda = Just "Guhuza n'amakuru y'umubyeyi"
            , kirundi = Nothing
            }

        LipidPanel ->
            { english = "Lipid Panel"
            , kinyarwanda = Just "Itsinda ry'ibizamini bipima ibinure (Lipid Panel)"
            , kirundi = Nothing
            }

        LiveChildren ->
            { english = "Live Children"
            , kinyarwanda = Just "Abana bariho"
            , kirundi = Nothing
            }

        LmpDateConfirmationLabel ->
            { english = "Please confirm the last menstrual period submitted by the CHW"
            , kinyarwanda = Just "Emeza itariki aherukira mu mihango yujujwe n' umujyanama w'ubuzima"
            , kirundi = Nothing
            }

        LmpDateConfirmationQuestion ->
            { english = "Do you want to confirm the above LMP"
            , kinyarwanda = Just "Urashaka kwemeza itariki uherukira mu mihango yavuzwe hejuru"
            , kirundi = Nothing
            }

        LmpDateConfidentHeader ->
            { english = "Is the Patient confident of LMP Date"
            , kinyarwanda = Just "Ese umubyeyi azi neza itariki aherukira mu mihango?"
            , kirundi = Nothing
            }

        LmpDateNotConfidentQuestion ->
            { english = "What is the reason the patient is unsure"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LmpDateNotConfidentReason value ->
            case value of
                ReasonIrregularMenses ->
                    { english = "Irregular Menses"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonOnFamilyPlanningMethod ->
                    { english = "On family planning method"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonCanNotRememberDates ->
                    { english = "Can't remember dates"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LmpDateNotConfidentReasonforReport value ->
            case value of
                ReasonIrregularMenses ->
                    { english = "Uncertain dating due to irregular menses"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonOnFamilyPlanningMethod ->
                    { english = "Uncertain dating since patient is on family planning method"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonCanNotRememberDates ->
                    { english = "Uncertain dating since patient can't remember dates"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        LmpDateHeader ->
            { english = "Last Menstrual Period Date"
            , kinyarwanda = Just "Itariki aherukira mu mihango"
            , kirundi = Nothing
            }

        LmpLabel ->
            { english = "Last Menstrual Period"
            , kinyarwanda = Just "Igihe aherukira mu mihango"
            , kirundi = Nothing
            }

        LmpRangeHeader ->
            { english = "When was the Patient's Last Menstrual Period"
            , kinyarwanda = Just "Ni ryari umubyeyi aherukira mu mihango?"
            , kirundi = Nothing
            }

        LmpRange range ->
            case range of
                Pages.Prenatal.Activity.Types.OneMonth ->
                    { english = "Within 1 month"
                    , kinyarwanda = Just "Mu kwezi kumwe"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.ThreeMonths ->
                    { english = "Within 3 months"
                    , kinyarwanda = Just "Mu mezi atatu"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.SixMonthsOrMore ->
                    { english = "Within 6 months, or more"
                    , kinyarwanda = Just "Mu mezi atandatu, no hejuru"
                    , kirundi = Nothing
                    }

        LoggedInAsPhrase ->
            { english = "You are logged in as"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        Lungs ->
            { english = "Lungs"
            , kinyarwanda = Just "Ibihaha"
            , kirundi = Nothing
            }

        LungsCPESign option ->
            case option of
                Wheezes ->
                    { english = "Wheezes"
                    , kinyarwanda = Just "Ijwi ryumvikana igihe umuntu ahumeka"
                    , kirundi = Nothing
                    }

                Crackles ->
                    { english = "Crackles"
                    , kinyarwanda = Just "Ijwi ryumvikana umuntu ahumeka ariko afite indwara z'ubuhumekero"
                    , kirundi = Nothing
                    }

                NormalLungs ->
                    translationSet Normal

        MainIncomeSource source ->
            case source of
                HomeBasedAgriculture ->
                    { english = "Homebased Agriculture / Livestock"
                    , kinyarwanda = Just "Ubuhinzi / Ubworozi"
                    , kirundi = Nothing
                    }

                CommercialAgriculture ->
                    { english = "Commercial Agriculture / Livestock"
                    , kinyarwanda = Just "Ubucuruzi bw'imyaka / Amatungo"
                    , kirundi = Nothing
                    }

                PublicEmployee ->
                    { english = "Public Employee"
                    , kinyarwanda = Just "Umukozi wa Leta"
                    , kirundi = Nothing
                    }

                PrivateBusinessEmpployee ->
                    { english = "Private Business Employee"
                    , kinyarwanda = Just "Umukozi w'igenga"
                    , kirundi = Nothing
                    }

        MainIncomeSourceQuestion ->
            { english = "What is the most important source of income for the household"
            , kinyarwanda = Just "Ese nihe urugo rukura ubushobozi bwo gutunga urugo"
            , kirundi = Nothing
            }

        MainMenuActivity activity ->
            case activity of
                MenuClinical ->
                    translationSet Clinical

                MenuParticipantDirectory ->
                    { english = "Participant Directory"
                    , kinyarwanda = Just "Ububiko bw'amakuru y'umurwayi"
                    , kirundi = Nothing
                    }

                MenuDashboards ->
                    { english = "Dashboards"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MenuCaseManagement ->
                    translationSet CaseManagement

                MenuDeviceStatus ->
                    translationSet DeviceStatus

                MenuWellbeing ->
                    { english = "Wellbeing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MenuStockManagement ->
                    { english = "Stock Management"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        MainWaterSource source ->
            case source of
                PipedWaterToHome ->
                    { english = "Piped Water to Home"
                    , kinyarwanda = Just "Amazi agera mu rugo"
                    , kirundi = Nothing
                    }

                PublicWaterTap ->
                    { english = "Public Water Tap"
                    , kinyarwanda = Just "Ivomo rusange"
                    , kirundi = Nothing
                    }

                RainWaterCollectionSystem ->
                    { english = "Rain Water Collection System"
                    , kinyarwanda = Just "Amazi y'imvura"
                    , kirundi = Nothing
                    }

                NaturalSourceFlowingWater ->
                    { english = "Natural Source - Flowing Water"
                    , kinyarwanda = Just "Umugezi utemba"
                    , kirundi = Nothing
                    }

                NaturalSourceStandingWater ->
                    { english = "Natural Source - Standing Water"
                    , kinyarwanda = Just "Amazi y'ibiyaga"
                    , kirundi = Nothing
                    }

                BottledWater ->
                    { english = "Bottled Water"
                    , kinyarwanda = Just "Amazi akorwa mu nganda (aza mu macupa)"
                    , kirundi = Nothing
                    }

        MainWaterPreparationOption option ->
            case option of
                Boiled ->
                    { english = "Boiled"
                    , kinyarwanda = Just "Barayateka"
                    , kirundi = Nothing
                    }

                PurificationSolution ->
                    { english = "Purification solution"
                    , kinyarwanda = Just "Bakoresha umuti usukura amazi"
                    , kirundi = Nothing
                    }

                Filtered ->
                    { english = "Filtered"
                    , kinyarwanda = Just "Barayayungurura"
                    , kirundi = Nothing
                    }

                Bottled ->
                    { english = "Bottled"
                    , kinyarwanda = Just "Amazi yo mu nganda (afunze mu macupa)"
                    , kirundi = Nothing
                    }

                NoWaterPreparationOption ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        MainWaterSourceQuestion ->
            { english = "What is the household's main source of water"
            , kinyarwanda = Just "Ni hehe h'ibanze urugo ruvana amazi"
            , kirundi = Nothing
            }

        MainWaterPreparationQuestion ->
            { english = "How is drinking water prepared"
            , kinyarwanda = Just "Ni gute amazi yo kunywa ategurwa"
            , kirundi = Nothing
            }

        MakeSureYouAreConnected ->
            { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542."
            , kinyarwanda = Just "Banza urebe ko ufite interineti. Ikibazo nigikomeza, hamagara The Ihangane Project kuri +250 788 817 542"
            , kirundi = Nothing
            }

        MalariaRapidDiagnosticTest ->
            { english = "Malaria Rapid Diagnostic Test"
            , kinyarwanda = Just "Igikoresho gipima Malariya ku buryo bwihuse"
            , kirundi = Nothing
            }

        MalariaRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Malaria"
            , kinyarwanda = Just "Uyu murwayi afite agakoko gateram Malariya"
            , kirundi = Nothing
            }

        MalariaRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye ku murwayi"
            , kirundi = Nothing
            }

        MalariaWithGIComplications ->
            { english = "Malaria with GI complications"
            , kinyarwanda = Just "Malariya iherekejwe no guhitwa cyangwa kuruka"
            , kirundi = Nothing
            }

        RapidTestResult result ->
            case result of
                RapidTestNegative ->
                    translationSet NegativeLabel

                RapidTestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Nothing
                    }

                RapidTestPositiveAndPregnant ->
                    { english = "Positive and Pregnant"
                    , kinyarwanda = Just "Afite ubwandu kandi aratwite"
                    , kirundi = Nothing
                    }

                RapidTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Nothing
                    }

                RapidTestUnableToRun ->
                    { english = "Unable to run"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    , kirundi = Nothing
                    }

                RapidTestUnableToRunAndPregnant ->
                    { english = "Unable to run and Pregnant"
                    , kinyarwanda = Just "Ntibishoboka gukorwa"
                    , kirundi = Nothing
                    }

        MalnutritionWithComplications ->
            { english = "Malnutrition with complications"
            , kinyarwanda = Just "Imirire mibi n'indwara ziyikomokaho"
            , kirundi = Nothing
            }

        MaritalStatusLabel ->
            { english = "Marital Status"
            , kinyarwanda = Just "Irangamimerere"
            , kirundi = Nothing
            }

        MaritalStatus status ->
            case status of
                Divorced ->
                    { english = "Divorced"
                    , kinyarwanda = Just "Yatandukanye n'uwo bashakanye"
                    , kirundi = Nothing
                    }

                Married ->
                    { english = "Married"
                    , kinyarwanda = Just "Arubatse"
                    , kirundi = Nothing
                    }

                Single ->
                    { english = "Single"
                    , kinyarwanda = Just "Ingaragu"
                    , kirundi = Nothing
                    }

                Widowed ->
                    { english = "Widowed"
                    , kinyarwanda = Just "Umupfakazi"
                    , kirundi = Nothing
                    }

                LivingWithPartner ->
                    { english = "Living with partner"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Religious ->
                    { english = "Religious"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        MastitisRecommendedTreatmentHeader forEarlyMastitisOrEngorgment ->
            if forEarlyMastitisOrEngorgment then
                { english = "This patient shows signs of Early Mastitis or Engorgement"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "This patient has Mastitis"
                , kinyarwanda = Just "Uyu mubyeyi afite uburwayi bw'amabere"
                , kirundi = Nothing
                }

        MastitisRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            , kirundi = Nothing
            }

        MeasurementNoChange ->
            { english = "No Change"
            , kinyarwanda = Just "nta cyahindutse"
            , kirundi = Nothing
            }

        MeasurementGained amount ->
            { english = "Gained " ++ String.fromFloat amount
            , kinyarwanda = Just <| "Kwiyongera " ++ String.fromFloat amount
            , kirundi = Nothing
            }

        MeasurementLost amount ->
            { english = "Lost " ++ String.fromFloat amount
            , kinyarwanda = Just <| "Kwiyongera " ++ String.fromFloat amount
            , kirundi = Nothing
            }

        MedicationCausingHypertension medication ->
            case medication of
                MedicationOestrogens ->
                    { english = "Oestrogens (Family Planning)"
                    , kinyarwanda = Just "Umusemburo wa Estrogene"
                    , kirundi = Nothing
                    }

                MedicationSteroids ->
                    { english = "Steroids (Prednisolone)"
                    , kinyarwanda = Just "Umusemburo wa iteroyide"
                    , kirundi = Nothing
                    }

                MedicationAmitriptyline ->
                    { english = "Amitriptyline"
                    , kinyarwanda = Just "Amitiributiline"
                    , kirundi = Nothing
                    }

                MedicationIbuprofen ->
                    { english = "Ibuprofen (Diclofenac)"
                    , kinyarwanda = Just "Ibiporofene cg Dikolofenake"
                    , kirundi = Nothing
                    }

                NoMedicationCausingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                MedicalConditionDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete(Indwara y'Igisukari)"
                    , kirundi = Nothing
                    }

                MedicalConditionKidneyDisease ->
                    { english = "Kidney Disease"
                    , kinyarwanda = Just "Indwara y'impyiko"
                    , kirundi = Nothing
                    }

                MedicalConditionPregnancy ->
                    { english = "Pregnancy"
                    , kinyarwanda = Just "Gutwita"
                    , kirundi = Nothing
                    }

                MedicalConditionHypertension ->
                    { english = "Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso"
                    , kirundi = Nothing
                    }

                MedicalConditionGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no utwite"
                    , kirundi = Nothing
                    }

                MedicalConditionPregnancyRelatedHypertension ->
                    { english = "Pregnancy Related Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso utewe no gutwita"
                    , kirundi = Nothing
                    }

                MedicalConditionNeuropathy ->
                    { english = "Neuropathy"
                    , kinyarwanda = Just "Indwara z'imyakura"
                    , kirundi = Nothing
                    }

                MedicalConditionRentalComplications ->
                    { english = "Rental Complications"
                    , kinyarwanda = Just "Ibibazo bitewe no kwangirika kw'impyiko"
                    , kirundi = Nothing
                    }

                MedicalConditionMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Nothing
                    }

                MedicalConditionTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Nothing
                    }

                MedicalConditionHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                MedicalConditionSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                MedicalConditionEyeComplications ->
                    { english = "Eye Complications"
                    , kinyarwanda = Just "Ibibazo by'amaso"
                    , kirundi = Nothing
                    }

                MedicalConditionAnemia ->
                    { english = "Anemia"
                    , kinyarwanda = Just "Indwara y'amaraso make"
                    , kirundi = Nothing
                    }

                MedicalConditionOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoMedicalConditions ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        MedicalConditionQuestion ->
            { english = "Have you ever been diagnosed with any of these conditions"
            , kinyarwanda = Just "Waba warigeze urwara imwe muri izi ndwara"
            , kirundi = Nothing
            }

        MedicationDistribution ->
            { english = "Medication Distribution"
            , kinyarwanda = Just "Gutanga Imiti"
            , kirundi = Nothing
            }

        MedicationTreatingDiabetes medication ->
            case medication of
                MedicationMetformin ->
                    { english = "Metformin"
                    , kinyarwanda = Just "Metiforumine"
                    , kirundi = Nothing
                    }

                MedicationGlibenclamide ->
                    { english = "Glibenclamide"
                    , kinyarwanda = Just "Girimbenkalamide"
                    , kirundi = Nothing
                    }

                MedicationInsulin ->
                    { english = "Insulin"
                    , kinyarwanda = Just "Insuline"
                    , kirundi = Nothing
                    }

                NoMedicationTreatingDiabetes ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        MedicationTreatingDiabetesQuestion ->
            { english = "Has the patient taken or currently take any of the following medications that treat diabetes"
            , kinyarwanda = Just "Umurwayi yaba yarafashe cg ari gufata imwe mu miti ikurikira ivura Diyabete"
            , kirundi = Nothing
            }

        MedicationTreatingHypertension medication ->
            case medication of
                MedicationAceInhibitors ->
                    { english = "Ace-Inhibitors (Example: Captopril)"
                    , kinyarwanda = Just "Caputopili"
                    , kirundi = Nothing
                    }

                MedicationARBs ->
                    { english = "Angiotensine Receptor Blockers (ARBs)"
                    , kinyarwanda = Just "Anjiyotensine"
                    , kirundi = Nothing
                    }

                MedicationHCTZ ->
                    { english = "HCTZ"
                    , kinyarwanda = Just "Idolokotiyazide"
                    , kirundi = Nothing
                    }

                MedicationCalciumChannelBlockers ->
                    { english = "Calcium Channel Blockers"
                    , kinyarwanda = Just "Kalisiyumu"
                    , kirundi = Nothing
                    }

                MedicationBetaBlockers ->
                    { english = "Beta-Blockers"
                    , kinyarwanda = Just "Beta boloka"
                    , kirundi = Nothing
                    }

                MedicationHydralazine ->
                    { english = "Hydralazine"
                    , kinyarwanda = Just "Idaralazine"
                    , kirundi = Nothing
                    }

                MedicationMethyldopa ->
                    { english = "Methyldopa"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoMedicationTreatingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        MedicationTreatingHypertensionQuestion ->
            { english = "Has the patient taken or currently take any of the following medications that treat hypertension"
            , kinyarwanda = Just "Umurwayi yaba yarafashe cg ari gufata imwe mu miti ikurikira ivura umuvuduko w'amaraso"
            , kirundi = Nothing
            }

        MedicalDiagnosis ->
            { english = "Medical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe na Muganga"
            , kirundi = Nothing
            }

        MedicalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisUterineMyoma ->
                    { english = "Uterine Myoma"
                    , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Nothing
                    }

                DiagnosisCardiacDisease ->
                    { english = "Cardiac Disease"
                    , kinyarwanda = Just "Indwara z'umutima"
                    , kirundi = Nothing
                    }

                DiagnosisRenalDisease ->
                    { english = "Renal Disease"
                    , kinyarwanda = Just "Indwara z'impyiko"
                    , kirundi = Nothing
                    }

                DiagnosisHypertensionBeforePregnancy ->
                    { english = "Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Nothing
                    }

                DiagnosisAsthma ->
                    { english = "Asthma"
                    , kinyarwanda = Just "Asthma (Agahema)"
                    , kirundi = Nothing
                    }

                DiagnosisBowedLegs ->
                    { english = "Bowed Legs"
                    , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
                    , kirundi = Nothing
                    }

                DiagnosisKnownHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virus itera SIDA"
                    , kirundi = Nothing
                    }

                DiagnosisMentalHealthHistory ->
                    { english = "History of Mental Health Problems"
                    , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
                    , kirundi = Nothing
                    }

        MedicationCausesSideEffectsQuestion ->
            { english = "Did you experience adverse events of the medication"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            , kirundi = Nothing
            }

        MedicationDistributionHelperAnemia ->
            { english = "Patient shows signs of Mild - Moderate Anemia"
            , kinyarwanda = Just "Umurwayi afite amaraso make byoroheje"
            , kirundi = Nothing
            }

        MedicationDistributionHelperDiscordantPartnership ->
            { english = "This patient is part of a discordant partnership"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite"
            , kirundi = Nothing
            }

        MedicationDistributionHelperDiscordantPartnershipNoARVs ->
            { english = "This patient is part of a discordant partnership in which the partner is not on ARVs"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite kandi ntago afata imiti igabanya ubukana"
            , kirundi = Nothing
            }

        MedicationDistributionHelperEarlyMastitisOrEngorgment ->
            { english = "This patient has signs of Early Mastitis or Engorgement"
            , kinyarwanda = Just "Uyu mubyeyi afite ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
            , kirundi = Nothing
            }

        MedicationDistributionHelperHIV ->
            { english = "This patient is HIV positive"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bute"
            , kirundi = Nothing
            }

        MedicationDistributionHelperMebendazole ->
            { english = "This patient is over 24 weeks EGA and has not had a dewormer in the last 6 months"
            , kinyarwanda = Just "Uyu mubyeyi atwite inda y'ibyumweru 24 kandi nta muti w'inzoka yafashe mu mezi 6 ashize"
            , kirundi = Nothing
            }

        MedicationDistributionHelperGonorrhea ->
            { english = "This patient has signs of possible Gonorrhea"
            , kinyarwanda = Just "Uyu mubyeyi agaragaza ibimenyetso by'umitezi"
            , kirundi = Nothing
            }

        MedicationDistributionHelperTrichomonasOrBacterialVaginosis ->
            { english = "This patient has signs of possible Trichomonas or Bacterial Vaginosis"
            , kinyarwanda = Just "Umubyeyii afite ibimenyetso bishobora kuba ari ibya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
            , kirundi = Nothing
            }

        MedicationDistributionHelperVitaminA ->
            { english = "This patient did not receive Vitamin A"
            , kinyarwanda = Just "Uyu mubyeyi ntiyahawe Vitamine A"
            , kirundi = Nothing
            }

        MedicationDistributionNoticeGonorrhea ->
            { english = "Note: It is also recommended to prescribe the partner"
            , kinyarwanda = Just "Icyitonderwa: Ni ngombwa kuvura uwo babana/bashakanye"
            , kirundi = Nothing
            }

        MedicationDistributionNoticeGonorrheaPartnerMed1 ->
            { english = "Ciprofloxacin (1000mg): by mouth as a single dose"
            , kinyarwanda = Just "Kunywa ikinini cya Ciplofoloxacine (1000mg) inshuro imwe"
            , kirundi = Nothing
            }

        MedicationDistributionNoticeGonorrheaPartnerMed2 ->
            { english = "Doxycycline (100mg): by mouth 2x a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini cya Doxycycline (100mg) inshuro ebyri ku munsi mu minsi irindwi"
            , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        MedicationForFeverPast6Hours ->
            { english = "Patient took medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Umurwayi yanyoye imiti y’umuriro mu masaha atandatu ashize"
            , kirundi = Nothing
            }

        MedicationHelpedEnding helped ->
            if helped then
                { english = "and improved"
                , kinyarwanda = Just "none yorohewe"
                , kirundi = Nothing
                }

            else
                { english = "but no improvement"
                , kinyarwanda = Just "ariko ntiyorohewe"
                , kirundi = Nothing
                }

        MedicationFeelBetterAfterTakingQuestion ->
            { english = "Do you feel better after taking medications"
            , kinyarwanda = Just "Wumva umeze neza nyuma yo gufata imiti"
            , kirundi = Nothing
            }

        MedicationForMalariaToday ->
            { english = "Patient received medication for malaria today before this visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya uyu munsi mbere yuko aza mu isuzuma"
            , kirundi = Nothing
            }

        MedicationForMalariaPastMonth ->
            { english = "Patient received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya mu kwezi gushize mbere yuko aza mu isuzuma uyu munsi "
            , kirundi = Nothing
            }

        MedicalFormHelper ->
            { english = "Please record if the mother was diagnosed with the following medical issues"
            , kinyarwanda = Just "Andika niba umubyeyi yaragaragaweho indwara zikurikira"
            , kirundi = Nothing
            }

        MedicationForFeverPast6HoursQuestion ->
            { english = "Have you taken any medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Hari imiti y'umuriro waba wafashe mu masaha atandatu ashize"
            , kirundi = Nothing
            }

        MedicationForMalariaTodayQuestion ->
            { english = "Did you receive medication for malaria today before this visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba wanyoye mbere y'uko uza kwivuza"
            , kirundi = Nothing
            }

        MedicationForMalariaWithinPastMonthQuestion ->
            { english = "Have you received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba waranyoye mukwezi gushize mbere yuko uza hano kwivuza"
            , kirundi = Nothing
            }

        MedicationHelpedQuestion ->
            { english = "Do you feel better after taking this"
            , kinyarwanda = Just "Urumva umeze neza nyuma yo kunywa iyi miti"
            , kirundi = Nothing
            }

        MedicationTaken ->
            { english = "Medication taken"
            , kinyarwanda = Just "Imiti yafashe"
            , kirundi = Nothing
            }

        MedicationTakenAsPrescribedQuestion ->
            { english = "Did you take the medication as prescribed"
            , kinyarwanda = Just "Wafashe imiti neza uko wayandikiwe na muganga"
            , kirundi = Nothing
            }

        MentalHealthHistory ->
            { english = "History of Mental Health Problems"
            , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
            , kirundi = Nothing
            }

        MessagingCenter ->
            { english = "Messaging Center"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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

        MiddleName ->
            { english = "Middle Name"
            , kinyarwanda = Just "Izina ryo hagati"
            , kirundi = Nothing
            }

        Minutes minutes ->
            { english =
                if minutes == 1 then
                    "1 Minute"

                else
                    String.fromInt minutes ++ " Minutes"
            , kinyarwanda = Just <| "Iminota " ++ String.fromInt minutes
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        MissedDosesOfMedicatgion val ->
            if val == 0 then
                { english = "No missed doses of medication"
                , kinyarwanda = Just "Yafashe kandi arangiza neza imiti uko yayandikiwe"
                , kirundi = Nothing
                }

            else
                { english = "Missed " ++ String.fromInt val ++ " doses of medication"
                , kinyarwanda = Just <| "Yasimbutse gufata imiti inshuro " ++ String.fromInt val
                , kirundi = Nothing
                }

        ModeOfDelivery mode ->
            case mode of
                VaginalDelivery (Spontaneous True) ->
                    { english = "Spontaneous vaginal delivery with episiotomy"
                    , kinyarwanda = Just "Yabyaye neza ariko bamwongereye"
                    , kirundi = Nothing
                    }

                VaginalDelivery (Spontaneous False) ->
                    { english = "Spontaneous vaginal delivery without episiotomy"
                    , kinyarwanda = Just "Yabyaye neza"
                    , kirundi = Nothing
                    }

                VaginalDelivery WithVacuumExtraction ->
                    { english = "Vaginal delivery with vacuum extraction"
                    , kinyarwanda = Just "Yabyaye neza ariko hanifashishijwe icyuma gikurura umwana"
                    , kirundi = Nothing
                    }

                CesareanDelivery ->
                    { english = "Cesarean delivery"
                    , kinyarwanda = Just "Yabyaye bamubaze"
                    , kirundi = Nothing
                    }

        ModeOfDeliveryLabel ->
            { english = "Mode of delivery"
            , kinyarwanda = Just "Uburyo yabyayemo"
            , kirundi = Nothing
            }

        ModeratelyUnderweight ->
            { english = "Moderately Underweight"
            , kinyarwanda = Just "Imirire mibi yoroheje ku biro"
            , kirundi = Nothing
            }

        ModeratePreeclampsia ->
            { english = "Moderate Preeclampsia"
            , kinyarwanda = Just "Preklampusi Yoroheje"
            , kirundi = Nothing
            }

        Month ->
            { english = "Month"
            , kinyarwanda = Just "Ukwezi"
            , kirundi = Nothing
            }

        MonthAbbrev ->
            { english = "mo"
            , kinyarwanda = Just "am"
            , kirundi = Nothing
            }

        MonthSinglePlural value ->
            if value == 1 then
                { english = "1 Month"
                , kinyarwanda = Just "Ukwezi 1"
                , kirundi = Nothing
                }

            else
                { english = String.fromInt value ++ " Months"
                , kinyarwanda = Just <| "Amezi " ++ String.fromInt value
                , kirundi = Nothing
                }

        MonthsOfStock ->
            { english = "Months of Stock"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MonthsOld ->
            { english = "months old"
            , kinyarwanda = Just "Amezi"
            , kirundi = Nothing
            }

        Mother ->
            { english = "Mother"
            , kinyarwanda = Just "Umubyeyi"
            , kirundi = Nothing
            }

        MotherDemographicInformation ->
            { english = "Mother Demographic Information"
            , kinyarwanda = Just "Umwirondoro w'umubyeyi"
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
            , kirundi = Nothing
            }

        MotherNameLabel ->
            { english = "Mother's Name"
            , kinyarwanda = Just "Izina ry'umubyeyi"
            , kirundi = Nothing
            }

        MotherNationalId ->
            { english = "Mother's National ID"
            , kinyarwanda = Just "Umubare w'indangamuntu y'umubyeyi"
            , kirundi = Nothing
            }

        Mothers ->
            { english = "Mothers"
            , kinyarwanda = Just "Ababyeyi"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        MuacHelper ->
            { english = "Make sure to measure at the center of the baby’s upper arm."
            , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
            , kirundi = Nothing
            }

        MyAccount ->
            { english = "My Account"
            , kinyarwanda = Just "Konti yanjye"
            , kirundi = Nothing
            }

        MyRelatedBy relationship ->
            translateMyRelatedBy relationship

        MyRelatedByQuestion relationship ->
            translateMyRelatedByQuestion relationship

        Name ->
            { english = "Name"
            , kinyarwanda = Just "Izina"
            , kirundi = Nothing
            }

        NationalIdNumber ->
            { english = "National ID Number"
            , kinyarwanda = Just "Numero y'irangamuntu"
            , kirundi = Nothing
            }

        NCDAANCVisitsCounseling ->
            { english = "Provide the counseling on the consequences that may occur to her and the baby if she doesn't attend ANC visit as per guidance"
            , kinyarwanda = Just "Gira umubyeyi inama umusobanurire ingaruka byagira ku mubyeyi no ku mwana kutitabira gahunda yo gupimisha inda inshuro zagenwe"
            , kirundi = Nothing
            }

        NCDABirthweightQuestion ->
            { english = "What was the child's birthweight"
            , kinyarwanda = Just "Umwana yavukanye ibiro bingahe"
            , kirundi = Nothing
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
            , kinyarwanda = Just "Umwana w'amezi 9 kugeza ku 12, mugaburire ifashabere nibura inshuro 3 kugera kuri 4 ku munsi"
            , kirundi = Nothing
            }

        NCDAMealFrequency12to24 ->
            { english = "A child between 12 to 24 months: Feed him/her complementary foods at least 5 times a day."
            , kinyarwanda = Just "Umwana w'amezi 12 kugeza ku 24, mugaburire ifashabere nibura inshuro 5 ku munsi"
            , kirundi = Nothing
            }

        NCDASignCounseling sign ->
            case sign of
                SupplementsDuringPregnancy ->
                    { english = "Provide the counseling to the mother on the consequences that may occur to the mother and the baby and refer the mother to the HC to receive the Iron/Folic Acid/MMS"
                    , kinyarwanda = Just "Gira umubyeyi inama ku ngaruka mbi zaba ku mwana cyangwa umubyeyi igihe atafashe neza ibinini by'ubutare niba kibifata umwohereze ku kigo nderabuzima gufata ibinini"
                    , kirundi = Nothing
                    }

                ChildBehindOnVaccination ->
                    { english = "Provide the counseling to the mother to update the child's vaccination record with a Nurse through a Standard Pediatric Visit"
                    , kinyarwanda = Just "Gira inama umubyeyi yo kuzuza inkingo zitanditse muri sisiteme ya E-heza abifashijwe n'umuforomo banyuze mu Kujyana Umwana mu Isuzumiro"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.OngeraMNP ->
                    { english = "Provides counseling on the importance of Ongera and advise them to go to the Health center to recieve them"
                    , kinyarwanda = Just "Gira inama umubyeyi cg undi umurera ibyiza byo gufata ongera unamugire inama yo kujya kuyifata ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                FiveFoodGroups ->
                    { english = "Provide counseling on how the mother can combine different food items based on the one they have in their area"
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
                    { english = "Provide the counseling about preventing acute malnutrition and send the child to the Health center"
                    , kinyarwanda = Just "Gira inama umubyeyi uburyo barinda imirire mibi unohereze umwana ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                ReceivingSupport ->
                    { english = "Provide counseling to the mother to take the child to the Health center so that they can get the support needed"
                    , kinyarwanda = Just "Gira inama umubyeyi yo kujyana umwana ku kigo nderabuzima bagaha umwana ubufasha bukwiriye"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasCleanWater ->
                    { english = "Provide counseling on how to prepare clean water like boiling it and using water purifier"
                    , kinyarwanda = Just "Bagire inama y'uburyo bwo basukura amazi nko kuyateka no gukoresha Sur’eau"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasHandwashingFacility ->
                    { english = "Provide counseling on the importance of handwashing facility, and tell them to buy it  and use it. If they don't have means advocate for them"
                    , kinyarwanda = Just "Bagire inama y'akamaro ka kandira ukarabe, abayifite bayikoreshe. Abadafite ubushobozi bakorerwe ubuvugizi"
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
                    { english = "Provides counseling on the importance of FBF and advise them to go to the Health center to recieve them"
                    , kinyarwanda = Nothing
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
                    , kirundi = Nothing
                    }

                SupplementsDuringPregnancy ->
                    { english = "Did the mother receive Iron, Folic Acid/MMS"
                    , kinyarwanda = Just "Umubyeyi yahawe ibinini bya Fer, Folic Acid cg MMS byongera amaraso"
                    , kirundi = Nothing
                    }

                TakenSupplementsPerGuidance ->
                    { english = "Has she taken it as per guidance (CHW observed)"
                    , kinyarwanda = Just "Yabifashe nkuko byagenwe (Umujyanamawubuzima abisuzume)"
                    , kirundi = Nothing
                    }

                ChildBehindOnVaccination ->
                    { english = "According to E-Heza the child is behind on vaccinations, is this correct"
                    , kinyarwanda = Just "Urebeye muri sisiteme ya E-heza, umwana ntago afite inkingo zose zagenwe, ese ni byo"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.OngeraMNP ->
                    { english = "Did the child receive Ongera-MNP"
                    , kinyarwanda = Just "Umwana yahawe Ongera intungamubiri"
                    , kirundi = Nothing
                    }

                TakingOngeraMNP ->
                    translationSet FoodSupplementationConsumedQuestion

                FiveFoodGroups ->
                    { english = "Does the child receive food items from the 5 food groups in the last 24 hours"
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
                    , kinyarwanda = Just "Umubyeyi cg umwana ni abagenerwa bikorwa b'amafaranga y’inkunga (e.g. VUP, NSDS"
                    , kirundi = Nothing
                    }

                ReceivingCashTransfer ->
                    { english = "Are they receiving it"
                    , kinyarwanda = Just "Bahabwa inkunga"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.ConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Just "Gufata inkunga z’ingoboka harimo ibiryo n'amatungo magufi"
                    , kirundi = Nothing
                    }

                ChildWithAcuteMalnutrition ->
                    { english = "Please check MUAC. Does the child have acute malnutrition"
                    , kinyarwanda = Just "Pima ikizigira. Umwana afite imirire mibi ihutiyeho"
                    , kirundi = Nothing
                    }

                TreatedForAcuteMalnutrition ->
                    { english = "Is the child being treated"
                    , kinyarwanda = Just "Umwana ari kuvurwa"
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
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasHandwashingFacility ->
                    { english = "Does the house have a handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe kandi irakoreshwa"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasToilets ->
                    { english = "Does the household have toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero bwujuje ibyangombwa"
                    , kirundi = Nothing
                    }

                Backend.Measurement.Model.HasKitchenGarden ->
                    { english = "Does the house have a kitchen garden"
                    , kinyarwanda = Just "Urugo rufite umurima w’igikoni"
                    , kirundi = Nothing
                    }

                InsecticideTreatedBednets ->
                    { english = "Is the mother using the insecticide-treated bednets"
                    , kinyarwanda = Just "Umubyei akoresha inzitiramubu iteye umuti"
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
                    translationSet FoodSupplementationConsumedQuestion

                ChildReceivesVitaminA ->
                    { english = "Did the child receive Vitamin A in the last six months"
                    , kinyarwanda = Just "Mu mezi atandatu ashize, umwana yahawe ikinini cya vitamini A"
                    , kirundi = Nothing
                    }

                ChildTakingVitaminA ->
                    { english = "Is the Vitamin A being consumed"
                    , kinyarwanda = Just "Vitamini yayifashe neza"
                    , kirundi = Nothing
                    }

                ChildReceivesDewormer ->
                    { english = "Did the child receive deworming medication in the last six months"
                    , kinyarwanda = Just "Mu mezi atandatu ashize, umwana yahawe ikinini cy’inzoka"
                    , kirundi = Nothing
                    }

                ChildTakingDewormer ->
                    { english = "Is the deworming medication being consumed"
                    , kinyarwanda = Just "Ikinini cy'inzoka yagifashe neza"
                    , kirundi = Nothing
                    }

                ChildReceivesECD ->
                    { english = "Do you sing lullabies, poems, and read books to your child, or play games with your child"
                    , kinyarwanda = Just "Uririmbira umwana ibihozo, n'imivugo, ukamusomera ibitabo mukanakina"
                    , kirundi = Nothing
                    }

                NoNCDASigns ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NCDAUpdateVaccineRecordMessage ->
            { english = "Please update the childs vaccine record with information from the vaccine card at the end of this scorecard visit"
            , kinyarwanda = Just "Uzuza inkingo zitanditse ukoresheje amakuru ari ku ifishi y'inkingo y'umwana nyuma yo kurangiza ibikorwa byo ku ifishi y'imikurire y'umwana"
            , kirundi = Nothing
            }

        NCDActivityTitle activity ->
            case activity of
                Backend.NCDActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.MedicalHistory ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.OutsideCare ->
                    translationSet OutsideCareLabel

        NCDANCServicesInstructions ->
            { english = "Refer patient to ANC services for further management of hypertension during pregnancy"
            , kinyarwanda = Just "Ohereza umubyeyi muri serivise yita ku babyeyi batwite bakurikrane byimbitse umuvuduko w'amaraso"
            , kirundi = Nothing
            }

        NCDAANCNewbornItemLabel item ->
            case item of
                RegularCheckups ->
                    { english = "Regular prenatal and postpartum checkups"
                    , kinyarwanda = Just "Yisuzumishije uko bikwiye atwite na nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                IronDuringPregnancy ->
                    { english = "Iron during pregnancy"
                    , kinyarwanda = Just "Yafashe umuti wongera amaraso atwite"
                    , kirundi = Nothing
                    }

        NCDAInfrastructureEnvironmentWashItemLabel item ->
            case item of
                Pages.WellChild.ProgressReport.Model.HasToilets ->
                    { english = "Household has toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.HasCleanWater ->
                    { english = "Household has clean water"
                    , kinyarwanda = Just "Urugo rufite amazi meza"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.HasHandwashingFacility ->
                    { english = "Household has handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.HasKitchenGarden ->
                    { english = "Household has kitchen garden"
                    , kinyarwanda = Just "Urugo rufite akarima k'igikoni"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.InsecticideTreatedBedNets ->
                    { english = "Insecticide treated bed nets"
                    , kinyarwanda = Just "Urugo rufite nzitiramibu ikoranye umuti"
                    , kirundi = Nothing
                    }

        NCDANutritionBehaviorItemLabel item ->
            case item of
                Pages.WellChild.ProgressReport.Model.BreastfedSixMonths ->
                    { english = "Breastfed baby for 6 mo without interruption"
                    , kinyarwanda = Just "Konsa umwana amezi 6 utamuvangiye"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.AppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 mo)"
                    , kinyarwanda = Just "Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)"
                    , kirundi = Nothing
                    }

                DiverseDiet ->
                    { english = "Does the child have a diverse diet?"
                    , kinyarwanda = Just "Umwana afata indyo yuzuye"
                    , kirundi = Nothing
                    }

                MealsADay ->
                    { english = "Number of times a child eats a day"
                    , kinyarwanda = Just "Inshuro umwana afata ifunguro ku munsi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentForDiarrhea ->
                    { english = "Treatment of diarrhea (ORS & Zinc)"
                    , kinyarwanda = Just "Kuvura impiswi(Ukoresheje Zinc cg ORS)"
                    , kirundi = Nothing
                    }

                SupportChildWithDisability ->
                    { english = "Provide support to a child with a disability "
                    , kinyarwanda = Just "Guha umwana ufite ubumuga ubufasha bwihariye"
                    , kirundi = Nothing
                    }

                ConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.ConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Just "Gufata inkunga z’ingoboka harimo ibiryo n'amatungo magufi"
                    , kirundi = Nothing
                    }

        NCDAUniversalInterventionsItemLabel item ->
            case item of
                Immunization ->
                    { english = "Immunization"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.VitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Deworming ->
                    { english = "Deworming"
                    , kinyarwanda = Just "Imiti y'inzoka"
                    , kirundi = Nothing
                    }

                Pages.WellChild.ProgressReport.Model.OngeraMNP ->
                    { english = "Use additional nutrients (Ongera)"
                    , kinyarwanda = Just "Koresha Ongera intungamubiri"
                    , kirundi = Nothing
                    }

                ECDServices ->
                    { english = "ECD services provided to child"
                    , kinyarwanda = Just "Umwana yahawe servise n'ikigo mboneza mikurire"
                    , kirundi = Nothing
                    }

        NCDAFillTheBlanksItemLabel item ->
            case item of
                HeightToAge ->
                    { english = "Level of stuning using child length mat"
                    , kinyarwanda = Just "Ikigero cyo kugwingira hakoreshejwe agasambi"
                    , kirundi = Nothing
                    }

                WeightToAge ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Nothing
                    }

                MuacValue ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira"
                    , kirundi = Nothing
                    }

                EdemaPresent ->
                    { english = "Edema"
                    , kinyarwanda = Just "Kubyimba"
                    , kirundi = Nothing
                    }

        NCDANoANVCVisitsOnRecord ->
            { english = "There are no recorded ANC visits for the mother of this child"
            , kinyarwanda = Just "Nta makuru agaragara yo gupimisha inda ku mubyeyi w'uyu mwana"
            , kirundi = Nothing
            }

        NCDANumberOfANCVisitsQuestion ->
            { english = "How many ANC standard visits did the mother receive"
            , kinyarwanda = Nothing
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
                    , kirundi = Nothing
                    }

                VisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Just "Uko areba byahindutse"
                    , kirundi = Nothing
                    }

                ChestPain ->
                    { english = "Chest Pain"
                    , kinyarwanda = Just "Kubabara mu gatuza"
                    , kirundi = Nothing
                    }

                FlankPain ->
                    { english = "Flank Pain"
                    , kinyarwanda = Just "Kubabara mu Ibondo"
                    , kirundi = Nothing
                    }

                Hematuria ->
                    { english = "Blood in Urine (Hematuria)"
                    , kinyarwanda = Just "Amaraso mu nkari"
                    , kirundi = Nothing
                    }

                SevereHeadaches ->
                    { english = "Severe Headaches"
                    , kinyarwanda = Just "Kuribwa umutww bikabije"
                    , kirundi = Nothing
                    }

                LossOfConciousness ->
                    { english = "Loss of Consciousness Since Last Visit"
                    , kinyarwanda = Just "Yataye ubwenge kandi ntiyumva kuva isura riheruka"
                    , kirundi = Nothing
                    }

                NoNCDDangerSigns ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NCDDiagnosisForProgressReport withRenalComplications isPregnant diagnosis ->
            let
                hypertensionInPregnancy =
                    { english = "Hypertension in Pregnancy"
                    , kinyarwanda = Just "Umuvuduko w'amaraso mu gihe utwite"
                    , kirundi = Nothing
                    }
            in
            case diagnosis of
                DiagnosisHypertensionStage1 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage One Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Stage One Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere"
                        , kirundi = Nothing
                        }

                DiagnosisHypertensionStage2 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage Two Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Stage Two Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Kabiri"
                        , kirundi = Nothing
                        }

                DiagnosisHypertensionStage3 ->
                    if isPregnant then
                        hypertensionInPregnancy

                    else if withRenalComplications then
                        { english = "Stage Three Hypertension with Renal Complications"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu n'ibibazo byo kudakora neza kwimpyiko"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Stage Three Hypertension"
                        , kinyarwanda = Just "Umuvuduko w'amaraso uri hejuru kurwego rwa Gatatu"
                        , kirundi = Nothing
                        }

                DiagnosisDiabetesInitial ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Nothing
                    }

                DiagnosisDiabetesRecurrent ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    , kirundi = Nothing
                    }

        NCDFamilyHistorySignQuestion sign ->
            case sign of
                SignHypertensionHistory ->
                    { english = "Has anyone in your family been told they have hypertension"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wabwiwe ko afite Umuvuduko w'amaraso"
                    , kirundi = Nothing
                    }

                SignHeartProblemHistory ->
                    { english = "Has anyone in your family been told they have a problem with their heart"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wagize ibibazo by'umutima"
                    , kirundi = Nothing
                    }

                SignDiabetesHistory ->
                    { english = "Has anyone in your family been told they have a problem with diabetes"
                    , kinyarwanda = Just "Haba hari umuntu wo mu muryango wagize ibibazo bya Diyabete"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                NoNCDGuidanceSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDHealthEducationHeader ->
            { english = "Stage One Hypertension"
            , kinyarwanda = Just "Umuvuduko w'amaraso uri ku rwego rwa mbere"
            , kirundi = Nothing
            }

        NCDHealthEducationInstructions ->
            { english = "Counsel patient on lifestyle changes and the root causes of hypertension"
            , kinyarwanda = Just "Igisha umurwayi ku bijyanye no guhindura imibereho n'iby'ibanze bishobora kuzamura umuvuduko"
            , kirundi = Nothing
            }

        NCDHealthEducationQuestion ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Just "Wahaye umubyeyi inyigisho zabugenewe ku buzima"
            , kirundi = Nothing
            }

        NCDLabsCaseManagementEntryTypeResults ->
            { english = "NCD Lab Results"
            , kinyarwanda = Just "Ibisubizo by'ibizamini by'indwara zitandura"
            , kirundi = Nothing
            }

        NCDMedicalHistoryTask task ->
            case task of
                TaskCoMorbidities ->
                    { english = "Co-Morbidities"
                    , kinyarwanda = Just "Ubundi burwayi asanganywe."
                    , kirundi = Nothing
                    }

                TaskMedicationHistory ->
                    { english = "Medication History"
                    , kinyarwanda = Just "Amakuru ku miti yafashe"
                    , kirundi = Nothing
                    }

                TaskSocialHistory ->
                    { english = "Social History"
                    , kinyarwanda = Just "Amakuru ku mibereho ye"
                    , kirundi = Nothing
                    }

                TaskFamilyHistory ->
                    { english = "Family History"
                    , kinyarwanda = Just "Amakuru ku muryango"
                    , kirundi = Nothing
                    }

                TaskOutsideCare ->
                    { english = "Outside Care"
                    , kinyarwanda = Just "Kuvurirwa ku rindi vuriro"
                    , kirundi = Nothing
                    }

        NCDNextStepsTask task ->
            case task of
                Pages.NCD.Activity.Types.TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                Pages.NCD.Activity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.Activity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Nothing
                    }

        NCDGroup1Symptom symptom ->
            case symptom of
                SwellingInLegs ->
                    { english = "Swelling in Legs"
                    , kinyarwanda = Just "Kubyimba amaguru"
                    , kirundi = Nothing
                    }

                UrinaryFrequency ->
                    { english = "Urinary Frequency"
                    , kinyarwanda = Just "Yihagarika inshuro nyinshi"
                    , kirundi = Nothing
                    }

                Anxiety ->
                    { english = "Anxiety"
                    , kinyarwanda = Just "Kubura amahoro"
                    , kirundi = Nothing
                    }

                WeightLoss ->
                    { english = "Weight Loss"
                    , kinyarwanda = Just "Gutakaza ibiro"
                    , kirundi = Nothing
                    }

                Palpitations ->
                    { english = "Palpitations"
                    , kinyarwanda = Just "Umutima urasimbagurika"
                    , kirundi = Nothing
                    }

                Tremor ->
                    { english = "Tremor"
                    , kinyarwanda = Just "Ibicuro"
                    , kirundi = Nothing
                    }

                SwellingInFace ->
                    { english = "Swelling in Face"
                    , kinyarwanda = Just "Kubyimba mu maso"
                    , kirundi = Nothing
                    }

                SwellingInAbdomen ->
                    { english = "Swelling in Abdomen"
                    , kinyarwanda = Just "Kubyimba Inda"
                    , kirundi = Nothing
                    }

                DizzinessWithChangingPosition ->
                    { english = "Dizziness with Changing Position"
                    , kinyarwanda = Just "Iyo ahinduye uko yari ameze ahita agira isereri"
                    , kirundi = Nothing
                    }

                MildHeadache ->
                    { english = "Mild Headache"
                    , kinyarwanda = Just "Kubabara umutwe byoroheje"
                    , kirundi = Nothing
                    }

                NoNCDGroup1Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NCDGroup2Symptom symptom ->
            case symptom of
                WeaknessOfOneSideOfTheBody ->
                    { english = "Weakness of One Side of the Body"
                    , kinyarwanda = Just "Kunanirwa igice kimwe cy'umubiri"
                    , kirundi = Nothing
                    }

                ProblemsWithWalking ->
                    { english = "Problems with Walking"
                    , kinyarwanda = Just "Kunanirwa kugenda"
                    , kirundi = Nothing
                    }

                ProblemsWithTalking ->
                    { english = "Problems with Talking"
                    , kinyarwanda = Just "Kunanirwa kuvuga"
                    , kirundi = Nothing
                    }

                DecreasedVision ->
                    { english = "Decreased Vision"
                    , kinyarwanda = Just "Kutareba neza"
                    , kirundi = Nothing
                    }

                BlurryVision ->
                    { english = "Blurry Vision"
                    , kinyarwanda = Just "Kureba ibikezikezi"
                    , kirundi = Nothing
                    }

                IncreasedFatigueWithDailyActivities ->
                    { english = "Increased Fatigue with Daily Activities"
                    , kinyarwanda = Just "Kwiyongera ku munaniro"
                    , kirundi = Nothing
                    }

                ShortOfBreathWhenLayingDown ->
                    { english = "Short of Breath When Laying Down"
                    , kinyarwanda = Just "Guhumeka nabi igihe aryamye"
                    , kirundi = Nothing
                    }

                ShortOfBreathAtNight ->
                    { english = "Short of Breath at Night"
                    , kinyarwanda = Just "Guhumeka nabi nijoro"
                    , kirundi = Nothing
                    }

                KidneyProblems ->
                    { english = "Kidney Problems"
                    , kinyarwanda = Just "Ibibazo by'impyiko"
                    , kirundi = Nothing
                    }

                NCDIncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Just "Kugira inyota cyane"
                    , kirundi = Nothing
                    }

                NoNCDGroup2Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NCDPainSymptom symptom ->
            case symptom of
                PainFlank ->
                    { english = "Flank"
                    , kinyarwanda = Just "Ibindo"
                    , kirundi = Nothing
                    }

                PainLowerBack ->
                    { english = "Lower Back"
                    , kinyarwanda = Just "Umugongo wo hasi"
                    , kirundi = Nothing
                    }

                PainFeet ->
                    { english = "Feet"
                    , kinyarwanda = Just "Ibirenge"
                    , kirundi = Nothing
                    }

                PainNeck ->
                    { english = "Neck"
                    , kinyarwanda = Just "Ijosi"
                    , kirundi = Nothing
                    }

                PainAbdomen ->
                    { english = "Abdomen"
                    , kinyarwanda = Just "Mu nda"
                    , kirundi = Nothing
                    }

                NoNCDPainSymptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NCDProgressReport ->
            { english = "NCD Progress Report"
            , kinyarwanda = Just "Raporo ku Burwayi Butandura"
            , kirundi = Nothing
            }

        NCDRecurrentActivitiesTitle activity ->
            case activity of
                Backend.NCDActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    , kirundi = Nothing
                    }

                Backend.NCDActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

        NCDRecurrentNextStepsTask task ->
            case task of
                Pages.NCD.RecurrentActivity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.RecurrentActivity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Nothing
                    }

        NCDRiskFactor value ->
            case value of
                RiskFactorSmokeCigarettes ->
                    { english = "Smokes Cigarettes"
                    , kinyarwanda = Just "Anywa Itabi"
                    , kirundi = Nothing
                    }

                RiskFactorConsumeSalt ->
                    { english = "Adds Salt to Diet"
                    , kinyarwanda = Just "Wongera Umunyu mu biryo"
                    , kirundi = Nothing
                    }

                RiskFactorHypertensionHistory ->
                    { english = "Family History of Hypertension"
                    , kinyarwanda = Just "Amakuru y'Uburwayi bw'Umuvuduko mu Muryango"
                    , kirundi = Nothing
                    }

                RiskFactorHearProblemHistory ->
                    { english = "Family History of Heart Problems"
                    , kinyarwanda = Just "Amakuru y'Indwara z'Umutima mu Muryango"
                    , kirundi = Nothing
                    }

                RiskFactorDiabetesHistory ->
                    { english = "Family History of Diabetes"
                    , kinyarwanda = Just "Amakuru y'Indwara ya Diyabete mu Muryango"
                    , kirundi = Nothing
                    }

        NCDSocialHistoryFoodQuestion ->
            { english = "What foods do you eat most"
            , kinyarwanda = Just "Ni ibihe biryo ukunda kurya cyane"
            , kirundi = Nothing
            }

        NCDSocialHistoryFoodQuestionInstructions ->
            { english = "Please check the most fitting group"
            , kinyarwanda = Just "Hitamo Itsinda rikwiriye"
            , kirundi = Nothing
            }

        NCDSocialHistorySignQuestion sign ->
            case sign of
                SignDrinkAlcohol ->
                    { english = "Do you drink any alcoholic beverages"
                    , kinyarwanda = Just "Ujya unywa ibikomoka kunzoga"
                    , kirundi = Nothing
                    }

                SignSmokeCigarettes ->
                    { english = "Do you smoke cigarettes"
                    , kinyarwanda = Just "Ujya unywa itabi"
                    , kirundi = Nothing
                    }

                SignConsumeSalt ->
                    { english = "Do you add salt to your food"
                    , kinyarwanda = Just "Ujya wongera umunyu mu biryo"
                    , kirundi = Nothing
                    }

                SignDifficult4TimesAYear ->
                    { english = "Would it be difficult for you to come to the health center 4 times a year"
                    , kinyarwanda = Just "Byakugora kuza ku kigo nderabuzima inshuro 4 mu mwaka"
                    , kirundi = Nothing
                    }

                SignHelpWithTreatmentAtHome ->
                    { english = "Are there people at home who can help you with treatment"
                    , kinyarwanda = Just "Hari umuntu mubana wagufasha gufata imiti"
                    , kirundi = Nothing
                    }

                NoNCDSocialHistorySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Neck ->
            { english = "Neck"
            , kinyarwanda = Just "Ijosi"
            , kirundi = Nothing
            }

        NeckCPESign option ->
            case option of
                EnlargedThyroid ->
                    { english = "Enlarged Thyroid"
                    , kinyarwanda = Just "Umwingo"
                    , kirundi = Nothing
                    }

                EnlargedLymphNodes ->
                    { english = "Enlarged Lymph Nodes"
                    , kinyarwanda = Just "Inturugunyu/Amatakara"
                    , kirundi = Nothing
                    }

                NormalNeck ->
                    translationSet Normal

        NegativeLabel ->
            { english = "Negative"
            , kinyarwanda = Just "Nta bwandu afite"
            , kirundi = Nothing
            }

        Never ->
            { english = "Never"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Next ->
            { english = "Next"
            , kinyarwanda = Just "Ibikurikiyeho"
            , kirundi = Nothing
            }

        NextAppointment ->
            { english = "Next Appointment"
            , kinyarwanda = Just "Itariki yo kugarukaho"
            , kirundi = Nothing
            }

        NextDue ->
            { english = "Next Due"
            , kinyarwanda = Just "itariki azahabwaho urukingo rukurikira"
            , kirundi = Nothing
            }

        NextDoseDue ->
            { english = "Next Dose Due"
            , kinyarwanda = Just "Itariki azafatiraho indi miti"
            , kirundi = Nothing
            }

        NextImmunisationVisit ->
            { english = "Next immunization visit"
            , kinyarwanda = Just "Ikingira rikurikira"
            , kirundi = Nothing
            }

        NextPediatricVisit ->
            { english = "Next pediatric visit"
            , kinyarwanda = Just "Isura ry'umwana rikurikira"
            , kirundi = Nothing
            }

        NextSteps ->
            { english = "Next Steps"
            , kinyarwanda = Just "Ibikurikiyeho"
            , kirundi = Nothing
            }

        NextStepsTask isChw task ->
            case task of
                NextStepsIsolation ->
                    if isChw then
                        { english = "Isolate Patient"
                        , kinyarwanda = Just "Shyira umurwayi mu kato"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Monitor at Home"
                        , kinyarwanda = Just "Gukurikiranira umurwayi mu rugo"
                        , kirundi = Nothing
                        }

                NextStepsContactHC ->
                    { english = "Contact Health Center"
                    , kinyarwanda = Just "Menyesha ikigo nderabuzima"
                    , kirundi = Nothing
                    }

                NextStepsCall114 ->
                    { english = "Call 114"
                    , kinyarwanda = Just "Hamagara 114"
                    , kirundi = Nothing
                    }

                NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.AcuteIllness.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Refer to Hospital"
                        , kinyarwanda = Just "Ohereza ku Bitaro"
                        , kirundi = Nothing
                        }

                Pages.AcuteIllness.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsContactTracing ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    , kirundi = Nothing
                    }

                NextStepsSymptomsReliefGuidance ->
                    -- We qualify it as Medication distribution, to keep
                    -- consistant with other types of Covid steps.
                    translationSet MedicationDistribution

        No ->
            { english = "No"
            , kinyarwanda = Just "Oya"
            , kirundi = Nothing
            }

        NoActivitiesCompleted ->
            { english = "No activities are entirely completed for the attending participants."
            , kinyarwanda = Just "Nta gikorwa cyarangiye cyose kubitabiriye."
            , kirundi = Nothing
            }

        NoActivitiesPending ->
            { english = "All activities are completed for the attending participants."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            , kirundi = Nothing
            }

        NoActivitiesCompletedForThisParticipant ->
            { english = "No activities are completed for this participant."
            , kinyarwanda = Just "Nta gikorwa cyarangiye kubitabiriye."
            , kirundi = Nothing
            }

        NoActivitiesPendingForThisParticipant ->
            { english = "All activities are completed for this participant."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            , kirundi = Nothing
            }

        NoContactReason reason ->
            case reason of
                ReasonNoAnswer ->
                    { english = "Did not answer"
                    , kinyarwanda = Just "Ntago yitabye"
                    , kirundi = Nothing
                    }

                ReasonWrongContactInfo ->
                    { english = "Wrong contact information"
                    , kinyarwanda = Just "Amakuru atariyo"
                    , kirundi = Nothing
                    }

                ReasonDeclinedFollowUp ->
                    { english = "Declined Follow Up"
                    , kinyarwanda = Just "Yanze gukurikiranwa"
                    , kirundi = Nothing
                    }

        NoGroupsFound ->
            { english = "No groups found."
            , kinyarwanda = Just "Nta matsinda yabonetse"
            , kirundi = Nothing
            }

        NoMatchesFound ->
            { english = "No matches found"
            , kinyarwanda = Just "Ibyo wifuza ntibiboneste"
            , kirundi = Nothing
            }

        NormalRange ->
            { english = "Normal Range"
            , kinyarwanda = Just "Ibimeze neza"
            , kirundi = Nothing
            }

        NoTreatmentAdministered ->
            { english = "No treatment administered"
            , kinyarwanda = Just "Nta muti watanzwe"
            , kirundi = Nothing
            }

        NoTreatmentRecorded ->
            { english = "No treatment recorded"
            , kinyarwanda = Just "Nta muti yanditswe"
            , kirundi = Nothing
            }

        NutritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            , kirundi = Nothing
            }

        ReasonForNonReferral reason ->
            case reason of
                ClientRefused ->
                    { english = "Client refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Nothing
                    }

                NoAmbulance ->
                    { english = "No ambulance available"
                    , kinyarwanda = Just "Nta mbangukiragutabara ihari"
                    , kirundi = Nothing
                    }

                ClientUnableToAffordFees ->
                    { english = "Client unable to afford fees"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Nothing
                    }

                ClientAlreadyInCare ->
                    { english = "Client already in care"
                    , kinyarwanda = Just "Umukiriya ari kwitabwaho"
                    , kirundi = Nothing
                    }

                ReasonForNonReferralNotIndicated ->
                    { english = "Not indicated"
                    , kinyarwanda = Just "Ntibyasabwe"
                    , kirundi = Nothing
                    }

                ReasonForNonReferralOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoReasonForNonReferral ->
                    { english = "No Reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    , kirundi = Nothing
                    }

        AdministrationNote note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    , kirundi = Nothing
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Nothing
                    }

                NonAdministrationHomeBirth ->
                    { english = "Home Birth"
                    , kinyarwanda = Just "Yabyariye mu rugo"
                    , kirundi = Nothing
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Nothing
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                AdministeredToday ->
                    { english = "Administered Today"
                    , kinyarwanda = Just "Yahawe umuti uyu munsi"
                    , kirundi = Nothing
                    }

                AdministeredPreviously ->
                    { english = "Already Received"
                    , kinyarwanda = Just "Byamaze kwakirwa"
                    , kirundi = Nothing
                    }

        AdministrationNoteForPrenatalImmunisation note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    , kirundi = Nothing
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Nothing
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Nothing
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Mother / Caregiver Declined"
                    , kinyarwanda = Just "Umubyeyi / Umurezi yanze"
                    , kirundi = Nothing
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    , kirundi = Nothing
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    , kirundi = Nothing
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        NoParticipantsPending ->
            { english = "All attending participants have completed their activities."
            , kinyarwanda = Just "Abaje bose barangirijwe"
            , kirundi = Nothing
            }

        NoParticipantsCompletedForThisActivity ->
            { english = "No participants have completed this activity yet."
            , kinyarwanda = Just "Ntawaje warangirijwe kukorerwa."
            , kirundi = Nothing
            }

        NoReferralRecorded ->
            { english = "No referral recorded"
            , kinyarwanda = Just "Nta koherezwa kwagaragaye"
            , kirundi = Nothing
            }

        NoParticipantsPendingForThisActivity ->
            { english = "All attending participants have completed this activitity."
            , kinyarwanda = Just "Ababje bose barangirijwe."
            , kirundi = Nothing
            }

        Normal ->
            { english = "Normal"
            , kinyarwanda = Just "Bimeze neza/Nta kibazo gihari"
            , kirundi = Nothing
            }

        NoChildrenRegisteredInTheSystem ->
            { english = "No children registered in the system"
            , kinyarwanda = Just "Ntamwana wanditswe muriyi sisiteme"
            , kirundi = Nothing
            }

        NoParticipantsFound ->
            { english = "No participants found"
            , kinyarwanda = Just "Ntamuntu ugaragaye"
            , kirundi = Nothing
            }

        NotAvailable ->
            { english = "not available"
            , kinyarwanda = Just "Ntibiboneste"
            , kirundi = Nothing
            }

        NotConnected ->
            { english = "Not Connected"
            , kinyarwanda = Just "Ntamurandasi"
            , kirundi = Nothing
            }

        NotFollowingRecommendationQuestion ->
            { english = "Why recommendations were not followed"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            , kirundi = Nothing
            }

        NotTaken ->
            { english = "Not taken"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            , kirundi = Nothing
            }

        NumberOfAbortions ->
            { english = "Number of Abortions"
            , kinyarwanda = Just "Umubare w'inda zavuyemo"
            , kirundi = Nothing
            }

        NumberOfChildrenUnder5 ->
            { english = "Number of Children under 5"
            , kinyarwanda = Just "Umubare w'abana bari munsi y'imyaka 5"
            , kirundi = Nothing
            }

        NumberOfCSections ->
            { english = "Number of C-Sections"
            , kinyarwanda = Just "Umubare w'inshuro yabazwe"
            , kirundi = Nothing
            }

        NumberOfLiveChildren ->
            { english = "Number of Live Children"
            , kinyarwanda = Just "Umubare w'abana bariho"
            , kirundi = Nothing
            }

        NumberOfStillbirthsAtTerm ->
            { english = "Number of Stillbirths at Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda bashyitse"
            , kirundi = Nothing
            }

        NumberOfStillbirthsPreTerm ->
            { english = "Number of Stillbirths pre Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda badashyitse"
            , kirundi = Nothing
            }

        NutritionActivityHelper activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.NCDA ->
                    translationSet ChildScorecard

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

        NutritionActivityTitle activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Nothing
                    }

                Backend.NutritionActivity.Model.NCDA ->
                    translationSet ChildScorecard

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

        NutritionAssessment assessment ->
            case assessment of
                AssesmentAcuteMalnutritionModerate ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi yoroheje ije vuba"
                    , kirundi = Nothing
                    }

                AssesmentAcuteMalnutritionSevere ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi ikabije ije vuba"
                    , kirundi = Nothing
                    }

                AssesmentUnderweightModerate ->
                    { english = "Moderately Underweight"
                    , kinyarwanda = Just "Imirire mibi yoroheje ku biro"
                    , kirundi = Nothing
                    }

                AssesmentUnderweightSevere ->
                    { english = "Severely Underweight"
                    , kinyarwanda = Just "Imirire mibi ikabije ku biro"
                    , kirundi = Nothing
                    }

                AssesmentDangerSignsNotPresent ->
                    { english = "Without Danger Signs"
                    , kinyarwanda = Just "Nta bimenyetso mpuruza"
                    , kirundi = Nothing
                    }

                AssesmentDangerSignsPresent ->
                    { english = "With Danger Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    , kirundi = Nothing
                    }

                AssesmentMalnutritionSigns _ ->
                    { english = "Malnutrition Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    , kirundi = Nothing
                    }

                AssesmentConsecutiveWeightLoss ->
                    { english = "Consecutive Weight Loss"
                    , kinyarwanda = Just "Gutakaza ibiro mu buryo bwikurikiranije"
                    , kirundi = Nothing
                    }

                NoNutritionAssessment ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        NutritionAssessmentTask task ->
            case task of
                TaskHeight ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    , kirundi = Nothing
                    }

                TaskHeadCircumference ->
                    { english = "Head Circumference"
                    , kinyarwanda = Just "Umuzenguruko w'umutwe"
                    , kirundi = Nothing
                    }

                TaskMuac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    , kirundi = Nothing
                    }

                TaskNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Nothing
                    }

                TaskWeight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    , kirundi = Nothing
                    }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Just "Imyumvire ku bijyanye n’imirire"
            , kirundi = Nothing
            }

        NutritionCaringOption option ->
            case option of
                CaredByParent ->
                    { english = "Parent"
                    , kinyarwanda = Just "Umubyeyi"
                    , kirundi = Nothing
                    }

                CaredByGrandparent ->
                    { english = "Grandparent"
                    , kinyarwanda = Just "Nyirakuru/Sekuru"
                    , kirundi = Nothing
                    }

                CaredBySibling ->
                    { english = "Sibling"
                    , kinyarwanda = Just "Umuvandimwe"
                    , kirundi = Nothing
                    }

                CaredByNeighbor ->
                    { english = "Neighbor"
                    , kinyarwanda = Just "Umuturanyi"
                    , kirundi = Nothing
                    }

                CaredByHouseHelper ->
                    { english = "House helper"
                    , kinyarwanda = Just "Umukozi wo mu rugo"
                    , kirundi = Nothing
                    }

                CaredByDaycare ->
                    { english = "Daycare"
                    , kinyarwanda = Just "Irerero"
                    , kirundi = Nothing
                    }

        NutritionFeedingSignQuestion sign ->
            case sign of
                ReceiveSupplement ->
                    { english = "Did you receive food supplement"
                    , kinyarwanda = Just "Waba warahawe inyongeramirire"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                EncouragedToEat ->
                    { english = "Does someone help / encourage the sick child to eat"
                    , kinyarwanda = Just "Hari umuntu waba afasha cyangwa ashishikariza umwana kurya"
                    , kirundi = Nothing
                    }

                RefusingToEat ->
                    { english = "Is the child refusing to eat"
                    , kinyarwanda = Just "Ese umwana yanga kurya"
                    , kirundi = Nothing
                    }

                FeedingSignBreastfeeding ->
                    { english = "Is the child currently breastfeeding (for children < 2)"
                    , kinyarwanda = Just "Umwana yaba yonka (ku bana bari munsi y'imyaka 2)"
                    , kirundi = Nothing
                    }

                CleanWaterAvailable ->
                    { english = "Is clean water available"
                    , kinyarwanda = Just "Ese mazi asukuye arahari"
                    , kirundi = Nothing
                    }

                EatenWithWater ->
                    { english = "Is water given to the child when eating the food supplement"
                    , kinyarwanda = Just "Ese umwana yaba ahabwa amazi yo kunwa igihe afata inyongeramirire"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        NutritionHygieneSignQuestion sign ->
            case sign of
                SoapInTheHouse ->
                    { english = "Is there soap for washing in the house"
                    , kinyarwanda = Just "Ese mu rugo haba hari isabune yo koga"
                    , kirundi = Nothing
                    }

                WashHandsBeforeFeeding ->
                    { english = "Do the caregiver and child wash hands before the child is fed"
                    , kinyarwanda = Just "Ese umurezi n'umwana bakaraba intoki mbere y'uko umwana agaburirwa"
                    , kirundi = Nothing
                    }

                FoodCovered ->
                    { english = "Is the food / RUTF covered and free from flies"
                    , kinyarwanda = Just "Ese ibiryo/RUTUFU birapfundikiye kandi nta sazi zibiriho"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                Measurement.Model.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                NextStepContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    , kirundi = Nothing
                    }

                NextStepFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
                    }

        NutritionSupplementType type_ ->
            case type_ of
                FortifiedPorridge ->
                    { english = "Fortified Porridge"
                    , kinyarwanda = Just "Igikoma kirimo Imyunyu ngugu na Vitamin"
                    , kirundi = Nothing
                    }

                Rutf ->
                    { english = "RUTF"
                    , kinyarwanda = Just "RUTUFU"
                    , kirundi = Nothing
                    }

                Ongera ->
                    { english = "Ongera intungamubiri at the village level / CHW"
                    , kinyarwanda = Just "Ongera Intungamubiri mu mudugudu/Ku mujyanama w'Ubuzima"
                    , kirundi = Nothing
                    }

                TherapeuticMilk ->
                    { english = "Therapeutic Milk"
                    , kinyarwanda = Just "Amata avura"
                    , kirundi = Nothing
                    }

                NoNutritionSupplementType ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        NitritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            , kirundi = Nothing
            }

        Observations ->
            { english = "Observations"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ObstetricalDiagnosis ->
            { english = "Obstetrical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe n'inzobere mu gusuzuma abagore batwite"
            , kirundi = Nothing
            }

        ObstetricalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisRhNegative ->
                    { english = "Patient is RH Negative"
                    , kinyarwanda = Just "Umurwayi afite Rezisi negatifu"
                    , kirundi = Nothing
                    }

                DiagnosisModerateUnderweight ->
                    { english = "Moderate underweight"
                    , kinyarwanda = Just "Ibiro bike bidakabije ugendeye ku myaka"
                    , kirundi = Nothing
                    }

                DiagnosisSevereUnderweight ->
                    { english = "Severe underweight"
                    , kinyarwanda = Just "Afite ibiro bikie bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisOverweight ->
                    { english = "Overweight"
                    , kinyarwanda = Just "Aftie ibiro byinshi"
                    , kirundi = Nothing
                    }

                DiagnosisObese ->
                    { english = "Obese"
                    , kinyarwanda = Just "Kubyibuha gukabije"
                    , kirundi = Nothing
                    }

                DisgnosisPeripheralEdema ->
                    { english = "Peripheral Edema"
                    , kinyarwanda = Just "Kubyimba amaguru n'amaboko"
                    , kirundi = Nothing
                    }

                DiagnosisFetusBreech ->
                    { english = "Fetus is in breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    , kirundi = Nothing
                    }

                DiagnosisFetusTransverse ->
                    { english = "Fetus is transverse"
                    , kinyarwanda = Just "Umwana aritambitse"
                    , kirundi = Nothing
                    }

                DiagnosisBreastExamination ->
                    { english = "Breast exam showed"
                    , kinyarwanda = Just "Gusuzuma amabere byagaragaje"
                    , kirundi = Nothing
                    }

                DiagnosisHypotension ->
                    { english = "Hypotension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso uri hasi"
                    , kirundi = Nothing
                    }

                DiagnosisPregnancyInducedHypertension ->
                    { english = "Pregnancy-induced hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisPreeclampsiaHighRisk ->
                    { english = "High Risk for Preeclampsia"
                    , kinyarwanda = Just "Afite ibyago byinshi byo kugira Preklampusi"
                    , kirundi = Nothing
                    }

        OK ->
            { english = "OK"
            , kinyarwanda = Just "Nibyo, yego"
            , kirundi = Nothing
            }

        Old ->
            { english = "old"
            , kinyarwanda = Just "imyaka"
            , kirundi = Nothing
            }

        On ->
            { english = "On"
            , kinyarwanda = Just "Ku itariki"
            , kirundi = Nothing
            }

        OneVisit ->
            { english = "One visit"
            , kinyarwanda = Just "Inshuro imwe"
            , kirundi = Nothing
            }

        OnceYouEndTheEncounter ->
            { english = "Once you end the Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe cyose urangije igikorwa ,nta bushobozi wongera kugira bwo guhindura ibyo winjije cyangwa amakuru."
            , kirundi = Nothing
            }

        OnceYouEndYourGroupEncounter ->
            { english = "Once you end your Group Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe ushoze igikorwa, ntabwo ushobora guhindura cg wongeremo andi makuru."
            , kirundi = Nothing
            }

        OngoingTreatmentTask task ->
            case task of
                OngoingTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Nothing
                    }

        OnlySickChild ->
            { english = "Only Sick Child"
            , kinyarwanda = Just "Umwana urwaye gusa"
            , kirundi = Nothing
            }

        Or ->
            { english = "or"
            , kinyarwanda = Just "cyangwa"
            , kirundi = Nothing
            }

        OutsideCareLabel ->
            { english = "Outside Care"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PackagesPerMonth ->
            { english = "packages / month"
            , kinyarwanda = Just "Amapaki ku kwezi"
            , kirundi = Nothing
            }

        Page ->
            { english = "Page"
            , kinyarwanda = Just "Paji"
            , kirundi = Nothing
            }

        Page404 ->
            { english = "404 page"
            , kinyarwanda = Just "404 paji"
            , kirundi = Nothing
            }

        PageNotFoundMsg ->
            { english = "Sorry, nothing found in this URL."
            , kinyarwanda = Just "Mutwihanganire ntabwo ubufasha mwasabye mubashije kuboneka."
            , kirundi = Nothing
            }

        Pallor ->
            { english = "Pallor"
            , kinyarwanda = Just "Kweruruka (k'urugingo rw'umubiri)"
            , kirundi = Nothing
            }

        Para ->
            { english = "Para"
            , kinyarwanda = Just "Imbyaro"
            , kirundi = Nothing
            }

        ParacetamolPrescriptionForAdult ->
            { english = "Every 4-6 hours as needed. Not to exceed 4g in 24h."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ParentsAliveAndHealthyQuestion ->
            { english = "Are both parents alive and healthy"
            , kinyarwanda = Just "Ese ababyeyi bombi bariho kandi bafite ubuzima bwiza"
            , kirundi = Nothing
            }

        PaleConjuctiva ->
            { english = "Pale Conjunctiva"
            , kinyarwanda = Just "Ibihenehene byeruruka"
            , kirundi = Nothing
            }

        PartialPlacentaPreviousDelivery ->
            { english = "Partial Placenta in previous delivery"
            , kinyarwanda = Just "Ubwo aheruka kubyara iya nyuma ntiyavuyeyo  yose (yaje igice)"
            , kirundi = Nothing
            }

        Participants ->
            { english = "Participants"
            , kinyarwanda = Just "Ubwitabire"
            , kirundi = Nothing
            }

        ParticipantReviewed ->
            { english = "I have reviewed and understand the above."
            , kinyarwanda = Just "Nasomye kandi numva ibyavzwe haruguru"
            , kirundi = Nothing
            }

        ParticipantSignature ->
            { english = "Participant Signature"
            , kinyarwanda = Just "Umukono w'umugenerwabikorwa"
            , kirundi = Nothing
            }

        ParticipantSummary ->
            { english = "Participant Summary"
            , kinyarwanda = Just "Umwirondoro w’Umwana"
            , kirundi = Nothing
            }

        ParticipantDemographicInformation ->
            { english = "Participant Demographic Information"
            , kinyarwanda = Just "Umwirondoro w'umugenerwabikorwa"
            , kirundi = Nothing
            }

        ParticipantInformation ->
            { english = "Participant Information"
            , kinyarwanda = Just "Amakauru ku mugenerwabikorwa"
            , kirundi = Nothing
            }

        PartnerHivTestResult ->
            { english = "What was the partners HIV Test result"
            , kinyarwanda = Just "Ni ikihe gisubizo cy'ubwandu bwa Virusi itera SIDA kuwo babana?"
            , kirundi = Nothing
            }

        PartnerReceivedHivCounseling ->
            { english = "Did partner receive HIV Counseling during this pregnancy"
            , kinyarwanda = Just "Umugabo yahawe ubujyanama kuri Virusi itera SIDA"
            , kirundi = Nothing
            }

        PartnerReceivedHivTesting ->
            { english = "Did partner receive HIV Testing during this pregnancy"
            , kinyarwanda = Just "Umugabo  yasuzumwe Virusi itera SIDA?"
            , kirundi = Nothing
            }

        PastDiagnosisReportReason ->
            { english = "As a result of entering lab results from past encounter"
            , kinyarwanda = Just "Nk'igisubizo cyo kwinjiza ibisubizo by'ibizamini by'ubushize"
            , kirundi = Nothing
            }

        PatientDiagnosedWithLabel ->
            { english = "The patient has been diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwo"
            , kirundi = Nothing
            }

        PatientExhibitAnyFindings ->
            { english = "Does the patient exhibit any of these findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bikurikira"
            , kirundi = Nothing
            }

        PatientExhibitAnyRespiratoryFindings ->
            { english = "Does the patient exhibit any of these Respiratory findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bimenyetso by'ubuhumekero"
            , kirundi = Nothing
            }

        PatientGotAnyDangerSigns ->
            { english = "Does the patient have any of these danger signs"
            , kinyarwanda = Just "Umurwayi afite kimwe muri ibi bimenyetso mpuruza"
            , kirundi = Nothing
            }

        PatientGotAnySymptoms ->
            { english = "Does the patient have any of these symptoms"
            , kinyarwanda = Just "Umurwayi yaba afite bimwe muri ibi bimenyetso"
            , kirundi = Nothing
            }

        PatientGotPainAnywhewre ->
            { english = "Does the patient have pain anywhere"
            , kinyarwanda = Just "Umurwayi hari aho yaba ababara"
            , kirundi = Nothing
            }

        PatientGotDiabetesHeader ->
            { english = "This patient has Diabetes"
            , kinyarwanda = Just "Uyu murwayi afite indwara ya Diyabete"
            , kirundi = Nothing
            }

        PatientGotDiabetesByGlucoseHeader fasting value ->
            if fasting then
                { english = "This patient has Diabetes with glucose levels before a meal (fasting) of " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu maraso mbere yo kurya binga na " ++ String.fromFloat value ++ " mg/dL"
                , kirundi = Nothing
                }

            else
                { english = "This patient has Diabetes with glucose levels after a meal (non-fasting) of " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu maraso nyuma yo kurya binga na " ++ String.fromFloat value ++ " mg/dL"
                , kirundi = Nothing
                }

        PatientGotDiabetesByUrineDip value ->
            { english = "This patient has Diabetes with Urine Dip glucose levels of " ++ value
            , kinyarwanda = Just <| "Afite Diyabete hamwe n'ibipimo by'isukari mu nkari bingana na " ++ value
            , kirundi = Nothing
            }

        PatientProgress ->
            { english = "Patient Progress"
            , kinyarwanda = Just "Uruhererekane rw'ibyakorewe umubyeyi"
            , kirundi = Nothing
            }

        PatientRecord ->
            { english = "Patient Record"
            , kinyarwanda = Just "Amakuru y'Umurwayi"
            , kirundi = Nothing
            }

        PatientInformation ->
            { english = "Patient Information"
            , kinyarwanda = Just "Amakuru k'umurwayi"
            , kirundi = Nothing
            }

        PatientIsolatedQuestion isChw ->
            if isChw then
                { english = "Have you isolated the patient"
                , kinyarwanda = Just "Washyize umurwayi mu kato"
                , kirundi = Nothing
                }

            else
                { english = "Is the patient able to self-isolate at home"
                , kinyarwanda = Just "Umurwayi ashobora kwishyira mu kato ka wenyine mu rugo"
                , kirundi = Nothing
                }

        PatientNotYetSeenAtHCLabel ->
            { english = " has not yet been seen at the health center for this pregnancy"
            , kinyarwanda = Just " ntiyigeze asuzumwa ku kigo nderabuzima kuri iyi nda atwite"
            , kirundi = Nothing
            }

        PatientRecordFilter filter ->
            case filter of
                Pages.PatientRecord.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    , kirundi = Nothing
                    }

                Pages.PatientRecord.Model.FilterAntenatal ->
                    translationSet AntenatalCare

                FilterDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Just "Umwirondoro"
                    , kirundi = Nothing
                    }

                FilterFamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Nothing
                    }

        PauseEncounter ->
            { english = "Pause Encounter"
            , kinyarwanda = Just "Igikorwa cyahagaritswe"
            , kirundi = Nothing
            }

        PatientShowsNoSignsOfCovid ->
            { english = "Patient shows no signs of Covid"
            , kinyarwanda = Just "Umurwayi nta bimenyetso bya Koronavirusi agaragaza"
            , kirundi = Nothing
            }

        Patients ->
            { english = "Patients"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PediatricVisit ->
            { english = "Pediatric Visit"
            , kinyarwanda = Just "Isura ry'umwana"
            , kirundi = Nothing
            }

        PediatricCareMilestone milestone ->
            case milestone of
                Milestone6Weeks ->
                    { english = "6 weeks"
                    , kinyarwanda = Just "Ibyumweru 6"
                    , kirundi = Nothing
                    }

                Milestone14Weeks ->
                    { english = "14 weeks"
                    , kinyarwanda = Just "Ibyumweru 14"
                    , kirundi = Nothing
                    }

                Milestone6Months ->
                    { english = "6 months"
                    , kinyarwanda = Just "Amezi 6"
                    , kirundi = Nothing
                    }

                Milestone9Months ->
                    { english = "9 months"
                    , kinyarwanda = Just "Amezi 9"
                    , kirundi = Nothing
                    }

                Milestone12Months ->
                    { english = "12 months"
                    , kinyarwanda = Just "Amezi 12"
                    , kirundi = Nothing
                    }

                Milestone15Months ->
                    { english = "15 months"
                    , kinyarwanda = Just "Amezi 15"
                    , kirundi = Nothing
                    }

                Milestone18Months ->
                    { english = "18 months"
                    , kinyarwanda = Just "Amezi 18"
                    , kirundi = Nothing
                    }

                Milestone2Years ->
                    { english = "2 years"
                    , kinyarwanda = Just "Imyaka 2"
                    , kirundi = Nothing
                    }

                Milestone3Years ->
                    { english = "3 years"
                    , kinyarwanda = Just "Imyaka 3"
                    , kirundi = Nothing
                    }

                Milestone4Years ->
                    { english = "4 years"
                    , kinyarwanda = Just "Imyaka 4"
                    , kirundi = Nothing
                    }

        People ->
            { english = "People"
            , kinyarwanda = Just "Abantu"
            , kirundi = Nothing
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
                , kirundi = Nothing
                }

            else
                { english = "Persistent storage has not been authorized. The browser may delete locally cached data if storage runs low."
                , kinyarwanda = Just "Ibikwa ry'amakuru ntabwo remejwe. Sisiteme mushakisha ukoreramo ishobora kubisiba umwanya ubaye muto."
                , kirundi = Nothing
                }

        Person ->
            { english = "Person"
            , kinyarwanda = Just "Umuntu"
            , kirundi = Nothing
            }

        PersonHasBeenSaved ->
            { english = "Person has been saved"
            , kinyarwanda = Just "Amakuru kuri uyu muntu yabitswe"
            , kirundi = Nothing
            }

        PertinentSymptoms ->
            { english = "Pertinent Symptoms"
            , kinyarwanda = Just " Ibimenyetso by'ingenzi"
            , kirundi = Nothing
            }

        PhotosTransferStatus ->
            { english = "Photos Transfer Status"
            , kinyarwanda = Just "Uko kohereza amafoto bihagaze"
            , kirundi = Nothing
            }

        PhysicalExam ->
            { english = "Physical Exam"
            , kinyarwanda = Just "Gusuzuma umurwayi"
            , kirundi = Nothing
            }

        PhysicalExamTask task ->
            case task of
                PhysicalExamVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    , kirundi = Nothing
                    }

                PhysicalExamCoreExam ->
                    { english = "Core Exam"
                    , kinyarwanda = Just "Ikizamini cy'ingenzi"
                    , kirundi = Nothing
                    }

                PhysicalExamMuac ->
                    { english = "Muac"
                    , kinyarwanda = Just "Ikizigira"
                    , kirundi = Nothing
                    }

                PhysicalExamAcuteFindings ->
                    { english = "Acute Findings"
                    , kinyarwanda = Just "Ibimenyetso biziyeho"
                    , kirundi = Nothing
                    }

                PhysicalExamNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    , kirundi = Nothing
                    }

        PlaceholderEnterHeight ->
            { english = "Enter height here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            , kirundi = Nothing
            }

        PlaceholderEnterMUAC ->
            { english = "Enter MUAC here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            , kirundi = Nothing
            }

        PlaceholderEnterParticipantName ->
            { english = "Enter participant name here"
            , kinyarwanda = Just "Andika izina ry'umurwayi hano"
            , kirundi = Nothing
            }

        PlaceholderEnterWeight ->
            { english = "Enter weight here…"
            , kinyarwanda = Just "Andika ibiro hano…"
            , kirundi = Nothing
            }

        PlaceholderSearchContactName ->
            { english = "Search contact name here"
            , kinyarwanda = Just "Shakisha izina ry'uwo bahuye"
            , kirundi = Nothing
            }

        PleaseCall ->
            { english = "Please call"
            , kinyarwanda = Just "Hamagara"
            , kirundi = Nothing
            }

        PleaseContact ->
            { english = "Please contact"
            , kinyarwanda = Just "Vugisha"
            , kirundi = Nothing
            }

        PleaseSelectGroup ->
            { english = "Please select the relevant Group for the new encounter"
            , kinyarwanda = Just "Hitamam itsinda rikwiriye kuri iri sura rishya"
            , kirundi = Nothing
            }

        PleaseSync ->
            { english = "Please sync data for selected Health Center."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PointOfCare ->
            { english = "Point of Care"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PositiveLabel ->
            { english = "Positive"
            , kinyarwanda = Just "Afite ubwandu"
            , kirundi = Nothing
            }

        PostpartumEncounter ->
            { english = "Postpartum Encounter"
            , kinyarwanda = Just "Igikorwa cya nyuma yo kubyara"
            , kirundi = Nothing
            }

        PostpartumHealingProblem problem ->
            case problem of
                NormalPostpartumHealing ->
                    { english = "Healing Normally"
                    , kinyarwanda = Just "Ari gukira neza"
                    , kirundi = Nothing
                    }

                HealingProblemSwelling ->
                    { english = "Swelling"
                    , kinyarwanda = Just "Harabyimbye"
                    , kirundi = Nothing
                    }

                HealingProblemDischarge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Harasohoka ibintu bidasanzwe"
                    , kirundi = Nothing
                    }

                HealingProblemReleaseOfSutures ->
                    { english = "Release (lâchage) of sutures"
                    , kinyarwanda = Just "Indodo zavuyemo"
                    , kirundi = Nothing
                    }

                HealingProblemHematoma ->
                    { english = "Hematoma"
                    , kinyarwanda = Just "Igisebe cyajemo amaraso"
                    , kirundi = Nothing
                    }

                HealingProblemBruising ->
                    { english = "Bruising"
                    , kinyarwanda = Just "imfunira/ahantu hasa n'umukara kubera amaraso atasohotse mu mubiri"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                PostpartumChildParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    , kirundi = Nothing
                    }

                PostpartumChildLabouredBreathing ->
                    { english = "Laboured or Rapid Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye cg guhumeka vuba vuba"
                    , kirundi = Nothing
                    }

                PostpartumChildAbnormalTemperature ->
                    { english = "High (Fever) or Low Temperature"
                    , kinyarwanda = Just "Igipimo cy'ubushyuhe kiri hejuru cg kiri hasi"
                    , kirundi = Nothing
                    }

                PostpartumChildInactiveNoMovement ->
                    { english = "Inactive or No Movement"
                    , kinyarwanda = Just "Uruhinja ntacyo rwumva cg ntirunyeganyega"
                    , kirundi = Nothing
                    }

                PostpartumChildBodyTurnedYellow ->
                    { english = "Whole Body Has Turned Yellow"
                    , kinyarwanda = Just "Umubiri wose wabaye umuhondo"
                    , kirundi = Nothing
                    }

                NoPostpartumChildDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Nothing
                    }

        PostpartumMotherDangerSign sign ->
            case sign of
                PostpartumMotheUterineBleeding ->
                    { english = "Excessive Uterinal Bleeding"
                    , kinyarwanda = Just "Umubyeyi ava bikabije cyane"
                    , kirundi = Nothing
                    }

                PostpartumMotherFever ->
                    { english = "High Temperature / Fever"
                    , kinyarwanda = Just "Guhinda umuriro mwinshi/Umuriro"
                    , kirundi = Nothing
                    }

                PostpartumMotherMigraine ->
                    { english = "Migraine"
                    , kinyarwanda = Just "Umutwe umurya cyane"
                    , kirundi = Nothing
                    }

                PostpartumMotherParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    , kirundi = Nothing
                    }

                PostpartumMotherAcuteAbdominalPain ->
                    { english = "Acute Abdominal Pain"
                    , kinyarwanda = Just "Kuribwa mu nda cyane"
                    , kirundi = Nothing
                    }

                PostpartumMotherLabouredBreathing ->
                    { english = "Laboured Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye"
                    , kirundi = Nothing
                    }

                NoPostpartumMotherDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    , kirundi = Nothing
                    }

        Predecessor predecessor ->
            case predecessor of
                PredecessorFather ->
                    { english = "Father"
                    , kinyarwanda = Just "Se"
                    , kirundi = Nothing
                    }

                PredecessorMother ->
                    { english = "Mother"
                    , kinyarwanda = Just "Nyina"
                    , kirundi = Nothing
                    }

                PredecessorGrandFather ->
                    { english = "Grand-Father"
                    , kinyarwanda = Just "Sekuru"
                    , kirundi = Nothing
                    }

                PredecessorGrandMother ->
                    { english = "Grand-Mother"
                    , kinyarwanda = Just "Nyirakuru"
                    , kirundi = Nothing
                    }

                NoPredecessors ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        PreeclampsiaPreviousPregnancy ->
            { english = "Preeclampsia in previous pregnancy "
            , kinyarwanda = Just "Ubushize yagize ibimenyetso bibanziriza guhinda umushyitsi"
            , kirundi = Nothing
            }

        PregnancyConclusion ->
            { english = "Pregnancy Conclusion"
            , kinyarwanda = Just "Iherezo ry'Inda"
            , kirundi = Nothing
            }

        PregnancyStart ->
            { english = "Pregnancy Start"
            , kinyarwanda = Just "Itangira ryo Gutwita"
            , kirundi = Nothing
            }

        PregnancySummarySignQuestion sign ->
            case sign of
                ApgarScores ->
                    { english = "Are APGAR scores available for this patient"
                    , kinyarwanda = Just "Ibipimo byubuzima ku ruhinja rukimara kuvuka birahari"
                    , kirundi = Nothing
                    }

                BirthLength ->
                    { english = "Is birth length available"
                    , kinyarwanda = Just "Uburebure umwana yavukanye burazwi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                PregnancyTestNegative ->
                    translationSet NegativeLabel

                PregnancyTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Nothing
                    }

                PregnancyTestUnableToConduct ->
                    { english = "Unable to conduct test"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    , kirundi = Nothing
                    }

        PregnancyTrimester trimester ->
            case trimester of
                FirstTrimester ->
                    { english = "First Trimester"
                    , kinyarwanda = Just "Igihembwe cya mbere"
                    , kirundi = Nothing
                    }

                SecondTrimester ->
                    { english = "Second Trimester"
                    , kinyarwanda = Just "Igihembwe cya kabiri"
                    , kirundi = Nothing
                    }

                ThirdTrimester ->
                    { english = "Third Trimester"
                    , kinyarwanda = Just "Igihembwe cya gatatu"
                    , kirundi = Nothing
                    }

        PregnancyUrineTest ->
            { english = "Urine Pregnancy Test"
            , kinyarwanda = Just "Ikizamini cy'inkari gisuzuma ko umugore atwite"
            , kirundi = Nothing
            }

        PrenatalActivityTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.History ->
                    { english = "History"
                    , kinyarwanda = Just "Amateka y'ibyamubayeho"
                    , kirundi = Nothing
                    }

                PregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    , kirundi = Nothing
                    }

                PrenatalPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                BirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.PregnancyOutcome ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Just "Iherezo ry'inda"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    { english = "Malaria Prevention"
                    , kinyarwanda = Just "Kwirinda Malariya"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.Medication ->
                    { english = "Medication"
                    , kinyarwanda = Just "Gufata Imiti"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Nothing
                    }

                PrenatalTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Nothing
                    }

                MaternalMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    , kirundi = Nothing
                    }

                PrenatalImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.Breastfeeding ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Just "Konsa"
                    , kirundi = Nothing
                    }

                SpecialityCare ->
                    { english = "Specialty Care"
                    , kinyarwanda = Just "Ubuvuzi bw'inzobere"
                    , kirundi = Nothing
                    }

                PostpartumTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Nothing
                    }

        PrenatalRecurrentActivitiesTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    , kirundi = Nothing
                    }

                Backend.PrenatalActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

                RecurrentExamination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    , kirundi = Nothing
                    }

                RecurrentMalariaPrevention ->
                    { english = "Malaria Prevention"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalAssesment assesment ->
            case assesment of
                AssesmentNormalPregnancy ->
                    { english = "Routine Pregnancy Follow Up"
                    , kinyarwanda = Just "Gukurikirana Umubyeyi Utwite Bisanzwe"
                    , kirundi = Nothing
                    }

                AssesmentHighRiskPregnancy ->
                    { english = "High Risk Pregnancy"
                    , kinyarwanda = Just "Inda Ibangamiwe n'ibibazo Bikomeye"
                    , kirundi = Nothing
                    }

        PrenatalDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    , kirundi = Nothing
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Nothing
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    , kirundi = Nothing
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Neurosyphilis"
                    , kinyarwanda = Just "Mburugu yageze mu bwonko"
                    , kirundi = Nothing
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    , kirundi = Nothing
                    }

                DiagnosisModerateAnemia ->
                    { english = "Mild to Moderate Anemia"
                    , kinyarwanda = Just "Amaraso Macye byoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemia ->
                    { english = "Severe Anemia"
                    , kinyarwanda = Just "Amaraso Macye Cyane"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Severe Anemia with Complications"
                    , kinyarwanda = Just "Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    , kirundi = Nothing
                    }

                DiagnosisMiscarriage ->
                    { english = "Miscarriage"
                    , kinyarwanda = Just "Inda yavuyemo"
                    , kirundi = Nothing
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Molar Pregnancy"
                    , kinyarwanda = Just "Atwite amahuri"
                    , kirundi = Nothing
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Placenta Previa"
                    , kinyarwanda = Just "Ingobyi iri hasi ku nkondo y'umura"
                    , kirundi = Nothing
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Placental Abruption"
                    , kinyarwanda = Just "Ingobyi yomotse hakiri kare"
                    , kirundi = Nothing
                    }

                DiagnosisUterineRupture ->
                    { english = "Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi yaturitse"
                    , kirundi = Nothing
                    }

                DiagnosisObstructedLabor ->
                    { english = "Obstructed Labor"
                    , kinyarwanda = Just "Inda yanze kuvuka "
                    , kirundi = Nothing
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Post Abortion Sepsis"
                    , kinyarwanda = Just "Afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    , kirundi = Nothing
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Ectopic Pregnancy"
                    , kinyarwanda = Just "Yasamiye hanze y'umura"
                    , kirundi = Nothing
                    }

                DiagnosisPROM ->
                    { english = "Premature Rupture of Membranes (PROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    , kirundi = Nothing
                    }

                DiagnosisPPROM ->
                    { english = "Preterm Premature Rupture of Membranes (PPROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    , kirundi = Nothing
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Nothing
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Nothing
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Persistent Heartburn"
                    , kinyarwanda = Just "Ikirungurira gihoraho"
                    , kirundi = Nothing
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Deep Vein Thrombosis"
                    , kinyarwanda = Just "Gufatana(Kuvura) gukabije kw'amaraso"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Intense Pelvic Pain"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent Pelvic Pain"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Urinary Tract Infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Urinary Tract Infection Continued"
                    , kinyarwanda = Just "Indwara y'ubwandu bukomeje bw'umuyoboro w'inkari"
                    , kirundi = Nothing
                    }

                DiagnosisPyelonephritis ->
                    { english = "Pyelonephritis"
                    , kinyarwanda = Just "Indwara yo kubyimba impyiko"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis Continued"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea Continued"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis Continued"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (indwara y'igisukari)"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no utwite"
                    , kirundi = Nothing
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    , kirundi = Nothing
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Nothing
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        PrenatalDiagnosisForProgressReport diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    , kirundi = Nothing
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi Itera SIDA"
                    , kirundi = Nothing
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Nothing
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    , kirundi = Nothing
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Suspected Neurosyphilis"
                    , kinyarwanda = Just "Arakekwaho Mburugu yageze mu bwonko"
                    , kirundi = Nothing
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    , kirundi = Nothing
                    }

                DiagnosisModerateAnemia ->
                    { english = "Anemia (Mild to Moderate)"
                    , kinyarwanda = Just "Amaraso Macye (byoroheje)"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemia ->
                    { english = "Anemia (Severe)"
                    , kinyarwanda = Just "Amaraso Macye (bikabije)"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Anemia (Severe with Complications)"
                    , kinyarwanda = Just "Amaraso Macye (Bikabije n'Ibibazo Bishamikiyeho)"
                    , kirundi = Nothing
                    }

                DiagnosisMiscarriage ->
                    { english = "Possible Miscarriage"
                    , kinyarwanda = Just "Ashobora kuba yavanyemo inda"
                    , kirundi = Nothing
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Possible Molar Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba atwite amahuri"
                    , kirundi = Nothing
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Possible Placenta Previa"
                    , kinyarwanda = Just "Ingobyi ishobora kuba iri hasi ku nkondo y'umura"
                    , kirundi = Nothing
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Possible Placental Abruption"
                    , kinyarwanda = Just "Ingobyi ishobora kuba yomotse hakiri kare"
                    , kirundi = Nothing
                    }

                DiagnosisUterineRupture ->
                    { english = "Possible Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi ishobora kuha yaturitse"
                    , kirundi = Nothing
                    }

                DiagnosisObstructedLabor ->
                    { english = "Possible Obstructed Labor"
                    , kinyarwanda = Just "Inda ishobora kuba yanze kuvuka "
                    , kirundi = Nothing
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Possible Post Abortion Sepsis"
                    , kinyarwanda = Just "Ashobora kuba afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    , kirundi = Nothing
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Possible Ectopic Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba yarasamiye hanze y'umura"
                    , kirundi = Nothing
                    }

                DiagnosisPROM ->
                    { english = "PROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    , kirundi = Nothing
                    }

                DiagnosisPPROM ->
                    { english = "PPROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    , kirundi = Nothing
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Nothing
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Nothing
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn in pregnancy"
                    , kinyarwanda = Just "Ikirungurira mu gihe umubyeyi atwite"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Heartburn in pregnancy (persistent)"
                    , kinyarwanda = Just "Ikirungurira gihoraho mu gihe umubyeyi atwite"
                    , kirundi = Nothing
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Possible DVT"
                    , kinyarwanda = Just "Ashobora kuba afite ibibazo by'imitsi, bituma amaraso adatembera neza mu mubiri"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Severe pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda igihe umubyeyi atwite"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Lower urinary tract infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Lower urinary tract infection (continued)"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi bukomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisPyelonephritis ->
                    { english = "Possible Pyelonephritis"
                    , kinyarwanda = Just "Ashobora kuba afite Indwara yo kubyimba impyiko"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis (continued)"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea (continued)"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis (continued)"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Possible Active Tuberculosis"
                    , kinyarwanda = Just "Ashobora kuba afite Igituntu"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yatewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    , kirundi = Nothing
                    }

                DiagnosisOther ->
                    { english = "Received a diagnosis from a different health care facility - please follow up with patient"
                    , kinyarwanda = Just "Yabwiwe uburwayi n'irindi vuriro - Gerageza ukurikirane umurwayi"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Nothing
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        PrenatalDiagnosisNonUrgentMessage diagnosis ->
            case diagnosis of
                DiagnosisHIV ->
                    { english = "Patient has tested positive for HIV"
                    , kinyarwanda = Just "Afite ubwandu bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Patient has a detectable HIV Viral Load"
                    , kinyarwanda = Just "Umurwayi agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Nothing
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Patient is HIV Negative with a discordant partner"
                    , kinyarwanda = Just "Umwe mubashakanye afite ubwandu bwa virusi itera SIDA"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilis ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    , kirundi = Nothing
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    , kirundi = Nothing
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Patient has tested positive for Syphilis and shows signs of Neurosyphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu kandi afite ibimenyetso bigaragaza ko yageze mu bwonko"
                    , kirundi = Nothing
                    }

                DiagnosisHepatitisB ->
                    { english = "Patient has tested positive for Hepatitis B"
                    , kinyarwanda = Just "Afite ubwandu bw'umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                DiagnosisMalaria ->
                    { english = "Patient has tested positive for Malaria"
                    , kinyarwanda = Just "Afite ubwandu bwa Malariya"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Patient has tested positive for Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite Malariya n'amaraso macye"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso make bikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Patient has tested positive for Malaria with Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso macye cyane"
                    , kirundi = Nothing
                    }

                DiagnosisModerateAnemia ->
                    { english = "Patient shows signs of Mild to Moderate Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'amaraso Macye byoroheje"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemia ->
                    { english = "Patient shows signs of Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Amaraso Macye Cyane"
                    , kirundi = Nothing
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Patient has tested positive for Severe Anemia with Complications"
                    , kinyarwanda = Just "Umubyeyi afite Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    , kirundi = Nothing
                    }

                DiagnosisChronicHypertensionImmediate ->
                    { english = "Patient shows signs of Chronic Hypertension"
                    , kinyarwanda = Just "Agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                    , kirundi = Nothing
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Patient shows signs of Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Aragaragaza ibimenyetso by'Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    , kirundi = Nothing
                    }

                DiagnosisModeratePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    , kirundi = Nothing
                    }

                DiagnosisSeverePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    , kirundi = Nothing
                    }

                DiagnosisSeverePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburn ->
                    { english = "Patient shows signs of Persistent Heartburn"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho"
                    , kirundi = Nothing
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Patient shows signs of Persistent Heartburn that is not responding to treatment"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho ariko imiti itari kuvura"
                    , kirundi = Nothing
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Patient shows signs of Deep Vein Thrombosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo Gufatana(Kuvura) gukabije kw'amaraso"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Patient shows signs of Intense Pelvic Pain"
                    , kinyarwanda = Just "mubyeyi agaragaza ibimenyetso by'ububabare bukabije mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Patient shows signs of Persistent Pelvic Pain"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ububabare buhoraho mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Patient shows signs of Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari"
                    , kirundi = Nothing
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Patient shows signs of Persistant Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
                    , kirundi = Nothing
                    }

                DiagnosisPyelonephritis ->
                    { english = "Patient shows signs of Pyelonephritis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Indwara yo kubyimba impyiko"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasis ->
                    { english = "Patient shows signs of a Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo"
                    , kirundi = Nothing
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Patient shows signs of a Persistant Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrhea ->
                    { english = "Patient shows signs of Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi"
                    , kirundi = Nothing
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Patient shows signs of Persistant Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Patient shows signs of Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Patient shows signs of Persistant Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Patient shows signs of Tuberculosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Igituntu"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Patient shows signs of Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete"
                    , kirundi = Nothing
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Patient shows signs of Gestational Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete yatewe no gutwita"
                    , kirundi = Nothing
                    }

                DiagnosisRhesusNegative ->
                    { english = "Patient has Rh-Negative status"
                    , kinyarwanda = Just "Umubyeyi afite Rezisi Negatifu"
                    , kirundi = Nothing
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Patient shows signs of Hyperemesis Gravidum"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije k'umugore utwite"
                    , kirundi = Nothing
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Patient shows signs of Severe Vomiting"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionPossible ->
                    { english = "Patient shows signs of possible depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Patient shows signs of fairly high possibility of depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisDepressionProbable ->
                    { english = "Patient shows signs of probable depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko ashobora kuba afite indwara y'agahinda gakabije"
                    , kirundi = Nothing
                    }

                DiagnosisSuicideRisk ->
                    { english = "Patient shows signs of being a suicide risk"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Patient shows signs of Urinary Incontinence"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kutabasha kunyara"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Patient shows signs of Infection"
                    , kinyarwanda = Just "Umubyei agaragaza ibimenyetso bya infegisiyo"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Patient shows signs of Excessive Bleeding"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuva cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Patient shows signs of Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Nothing
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Patient shows signs of Mastitis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bw'amabere"
                    , kirundi = Nothing
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                ChwFirstEncounter ->
                    { english = "First Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya mbere umugore utwite"
                    , kirundi = Nothing
                    }

                ChwSecondEncounter ->
                    { english = "Second Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya kabiri umugore utwite"
                    , kirundi = Nothing
                    }

                ChwThirdPlusEncounter ->
                    { english = "Third Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya gatatu umugore utwite"
                    , kirundi = Nothing
                    }

                ChwPostpartumEncounter ->
                    { english = "Postpartum"
                    , kinyarwanda = Just "Igihe cya nyuma cyo kubyara"
                    , kirundi = Nothing
                    }

        PrenatalFlankPainSign sign ->
            case sign of
                FlankPainLeftSide ->
                    { english = "Left side"
                    , kinyarwanda = Just "Uruhande rw'ibumoso"
                    , kirundi = Nothing
                    }

                FlankPainRightSide ->
                    { english = "Right side"
                    , kinyarwanda = Just "Uruhande rw'iburyo"
                    , kirundi = Nothing
                    }

                FlankPainBothSides ->
                    { english = "Both sides"
                    , kinyarwanda = Just "Impande zose"
                    , kirundi = Nothing
                    }

                NoFlankPain ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationSignsDiagnosis isInitial date sign ->
            case sign of
                EducationNauseaVomiting ->
                    if isInitial then
                        { english = "Nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka  bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationLegCramps ->
                    if isInitial then
                        { english = "Leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru bikomeza kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationLowBackPain ->
                    if isInitial then
                        { english = "Lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationConstipation ->
                    if isInitial then
                        { english = "Constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationVaricoseVeins ->
                    if isInitial then
                        { english = "Varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationLegPainRedness ->
                    if isInitial then
                        { english = "Leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                EducationPelvicPain ->
                    if isInitial then
                        { english = "Pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
                        }

                    else
                        { english = "Persistent pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                EducationLegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    , kirundi = Nothing
                    }

                EducationLowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    , kirundi = Nothing
                    }

                EducationConstipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    , kirundi = Nothing
                    }

                EducationHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Nothing
                    }

                EducationVaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    , kirundi = Nothing
                    }

                EducationLegPainRedness ->
                    { english = "Leg Pain or Redness"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    , kirundi = Nothing
                    }

                EducationPelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                EducationSaferSex ->
                    { english = "Safer Sex Practices"
                    , kinyarwanda = Just "Imibonano mpuzabitsina ikingiye"
                    , kirundi = Nothing
                    }

                EducationMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    , kirundi = Nothing
                    }

                EducationEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Nothing
                    }

                EducationMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationAppropriateProvided ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Just "Wahaye umubyeyi inyigisho zabugenewe ku buzima"
            , kirundi = Nothing
            }

        PrenatalHealthEducationQuestion isChw sign ->
            case sign of
                EducationExpectations ->
                    { english = "Have you provided health education and anticipatory guidance on what to expect during the pregnancy"
                    , kinyarwanda = Just "Watanze inyigisho z'ubuzima k'umugore utwite unamusobanurira ibishobora kumubaho"
                    , kirundi = Nothing
                    }

                EducationVisitsReview ->
                    { english = "Have you reviewed anticipated visits by the CHW and to the health center with the mother"
                    , kinyarwanda = Just "Waba waganiriye n'umubyeyi ibyerekeye gusurwa n'umujyanama w'ubuzima cyangwa kujya ku kigonderabuzima"
                    , kirundi = Nothing
                    }

                EducationWarningSigns ->
                    { english = "Have you provided health education and anticipatory guidance on pregnancy warning signs"
                    , kinyarwanda = Just "Watanze inyigisho ku bimenyetso mpuruza k'umugore utwite nuko yakwitwara aramuste agize kimwe muribyo"
                    , kirundi = Nothing
                    }

                EducationHemorrhaging ->
                    { english = "Have you provided education on post-partum hemorrhaging"
                    , kinyarwanda = Just "Watanze inyigisho ku kimenyesto cyo kuva cyane nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                EducationFamilyPlanning ->
                    if isChw then
                        { english = "Have you provided education on family planning"
                        , kinyarwanda = Just "Watanze inyigisho zijyanye no kuboneza urubyaro"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Have you counseled the patient on family planning options"
                        , kinyarwanda = Just "Waba wagiriye inama umurwayi (umubyeyi) uburyo bwo kuboneza urubyaro"
                        , kirundi = Nothing
                        }

                EducationBreastfeeding ->
                    { english = "Have you provided education on breastfeeding"
                    , kinyarwanda = Just "Watanze inyigisho ku birebana no konsa"
                    , kirundi = Nothing
                    }

                EducationImmunization ->
                    { english = "Have you provided education on immunizations"
                    , kinyarwanda = Just "Watanze inyigisho zijyanye na gahunda yo gukingiza"
                    , kirundi = Nothing
                    }

                EducationHygiene ->
                    { english = "Have you provided education on hygiene"
                    , kinyarwanda = Just "Watanze inyigisho ku bijyanye n'isuku"
                    , kirundi = Nothing
                    }

                EducationPositiveHIV ->
                    { english = "Have you counseled patient on positive HIV test meaning"
                    , kinyarwanda = Just "Waba wasobanuriye umurwayi (umubyeyi) icyo bisibanuye kugira ibisubizo biri positifu ku bwandu bw'agakoko gatera SIDA"
                    , kirundi = Nothing
                    }

                EducationSaferSexHIV ->
                    { english = "Have you counseled patient on safer sex practices"
                    , kinyarwanda = Just "Wagiriye inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
                    , kirundi = Nothing
                    }

                EducationPartnerTesting ->
                    { english = "Have you encouraged the patient’s partner to get tested"
                    , kinyarwanda = Just "Waba washishikarije umubyueyi kubwira uwo babana kwipimisha"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalHealthEducationDiabetesInform ->
            { english = "Counsel patient on healthy nutrition and exercise practices"
            , kinyarwanda = Just "Igisha umubyeyi ku mirire myiza no gukora imyitozo ngororamubiri"
            , kirundi = Nothing
            }

        PrenatalHealthEducationHivDetectableViralLoadInform ->
            { english = "Instruct the patient on the importance of strict adherence to their medication and the dangers of transmission to their child during labor and delivery"
            , kinyarwanda = Just "Igisha umubyeyi akamaro ku gufata imiti neza ndetse n'ingaruka zo kuba yakwanduza umwana mu gihe abyara"
            , kirundi = Nothing
            }

        PrenatalHealthEducationNauseaAndVomitingAdvise ->
            { english = "Advise the patient that small amounts of chamomile tea, ginger, and Vitamin B6 can help relieve these symptoms if these are available to the patient"
            , kinyarwanda = Just "Gira umubyeyi inama ko gufata icyayi cya Chamomile, tangawizi na vitamini B6 byagabanya ibimenyetso afite igihe byaba bihari"
            , kirundi = Nothing
            }

        PrenatalHealthEducationNauseaAndVomitingInform ->
            { english = "Inform the patient that the symptoms of nausea and vomiting usually resolve on their own in the second half of pregnancy"
            , kinyarwanda = Just "Menyesha umubyeyi ko ibimenyetso byo kugira iseseme no kuruka bigenda bigabanuka uko inda igenda ikura ( kumezi ane, atanu cyangwa atandatu)"
            , kirundi = Nothing
            }

        PrenatalHealthEducationLegCrampsInform ->
            { english = "Instruct the patient that the following may help relieve cramping in the legs"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira bishobora kugabanya ibinya mu maguru"
            , kirundi = Nothing
            }

        PrenatalHealthEducationLowBackPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent lower back pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho mu gihe atwite igabanya uburibwe bw'umugongo wo hasi"
            , kirundi = Nothing
            }

        PrenatalHealthEducationConstipationInform ->
            { english = "Instruct the patient that increasing the intake of fruits, vegetables, high fiber foods, and water can help relieve constipation symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko kurya imbuto, imboga, ibiryo bisukura umubiri (fibre) no kunywa amazi birinda kunanairwa kwituma"
            , kirundi = Nothing
            }

        PrenatalHealthEducationHeartburnInform ->
            { english = "Instruct the patient that the following may help relieve heartburn"
            , kinyarwanda = Just "Sobanurira umubyeyi ko ibi bikurikira bifasha mu kugabanya ikirungurira"
            , kirundi = Nothing
            }

        PrenatalHealthEducationVaricoseVeinsInform ->
            { english = "Instruct the patient that compression stockings (tight socks or leggings) and elevating their legs will help reduce varicose veins"
            , kinyarwanda = Just "Igisha umubyeyi ko kwambara ibintu bimufashe ku maguru (amasogisi,..) no gusegura amaguru igihe aryamye bizamurinda kubyimba kw'imitsi"
            , kirundi = Nothing
            }

        PrenatalHealthEducationLegPainRednessInform ->
            { english = "Instruct the patient that regular exercise and stretching can relieve leg pain or redness"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho izamurinda kuribwa amaguru ndetse no kuba yatukuara"
            , kirundi = Nothing
            }

        PrenatalHealthEducationPelvicPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent pelvic pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngoraramubiri ihoraho izamurinda kuribwa mu kiziba cy'inda"
            , kirundi = Nothing
            }

        PrenatalHealthEducationSaferSexInform ->
            { english = "Counsel patient on safer sex practices"
            , kinyarwanda = Just "Gira inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
            , kirundi = Nothing
            }

        PrenatalHealthEducationEarlyMastitisOrEngorgmentInform ->
            { english = "Instruct the patient that the following may help relieve symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira byamufasha kugabanya uburibwe"
            , kirundi = Nothing
            }

        PrenatalHealthEducationMentalHealthInform ->
            { english = "Provide information to support patients mental well being during pregnancy"
            , kinyarwanda = Just "Tanga inama zafasha umubyeyi utwite kubungabunga ubuzima bwo mu mutwe"
            , kirundi = Nothing
            }

        PrenatalNCDProgramHeaderPrefix ->
            { english = "This patient was diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwa"
            , kirundi = Nothing
            }

        PrenatalNCDProgramHeaderSuffix ->
            { english = "during her pregnancy"
            , kinyarwanda = Just "mu gihe yari atwite"
            , kirundi = Nothing
            }

        PrenatalNCDProgramInstructions ->
            { english = "Refer patient to NCD services for further management"
            , kinyarwanda = Just "Ohereza umurwayi muri serivisi y'indwara zitandura bamwiteho byimbitse"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.NextStepsFollowUp ->
                    { english = "CHW Follow Up"
                    , kinyarwanda = Just "Isura ry'umujyanama w'ubuzima"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Referral"
                        , kinyarwanda = Just "Kohereza"
                        , kirundi = Nothing
                        }

                Pages.Prenatal.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.NextStepsNewbornEnrolment ->
                    { english = "Newborn Enrollment"
                    , kinyarwanda = Just "Kwandika uruhinja"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.Activity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.Activity.Types.NextStepsWait ->
                    { english = "Wait"
                    , kinyarwanda = Just "Tegereza"
                    , kirundi = Nothing
                    }

        PrenatalRecurrentNextStepsTask task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.NextStepsSendToHC ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    , kirundi = Nothing
                    }

                Pages.Prenatal.RecurrentActivity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.RecurrentActivity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        PrenatalImmunisationDescription task ->
            case task of
                VaccineTetanus ->
                    { english = "The Tetanus vaccine prevents the patient from getting Tetanus which causes muscle spasms, fever, high blood pressure, and death."
                    , kinyarwanda = Just "Urukingo rw'agakwega rurinda umwana kwandura indwara y'agakwega (Tetanosi) itera kugagara kw'imitsi, umuriro, umuvuduko w'amaraso ndetse n'urupfu."
                    , kirundi = Nothing
                    }

        PrenatalImmunisationHeader task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    , kirundi = Nothing
                    }

        PrenatalImmunisationHistory task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus History"
                    , kinyarwanda = Just "Amakuru ku ndwara y'Agakwega"
                    , kirundi = Nothing
                    }

        PrenatalMentalHealthQuestion question ->
            case question of
                MentalHealthQuestion1 ->
                    { english = "I have been able to laugh and see the funny side of things"
                    , kinyarwanda = Just "Njya nshobora guseka kandi nkabona ibintu mu buryo bwiza"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion2 ->
                    { english = "I have looked forward with enjoyment to things"
                    , kinyarwanda = Just "Nategereje ko ibintu nezerewe"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion3 ->
                    { english = "I have blamed myself unnecessarily when things went wrong"
                    , kinyarwanda = Just "Njya niciraga urubanza iyo ibintu byabaga byagenze nabi"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion4 ->
                    { english = "I have been anxious or worried for no good reason"
                    , kinyarwanda = Just "Njya mpangayika nta mpamvu igaragara"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion5 ->
                    { english = "I have felt scared or panicky for no very good reason"
                    , kinyarwanda = Just "Njya ngira ubwoba cyangwa nkakuka umutima nta mpamvu ifatika"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion6 ->
                    { english = "Things have been getting on top of me"
                    , kinyarwanda = Just "Ibintu bijya bindenga "
                    , kirundi = Nothing
                    }

                MentalHealthQuestion7 ->
                    { english = "I have been so unhappy that I have had difficulty sleeping"
                    , kinyarwanda = Just "Njya numva mbabaye ku buryo ngira ikibazo cyo kudasinzira"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion8 ->
                    { english = "I have felt sad or miserable"
                    , kinyarwanda = Just "Njya numva mbabaye cyangwa mfite ishavu "
                    , kirundi = Nothing
                    }

                MentalHealthQuestion9 ->
                    { english = "I have been so unhappy that I have been crying"
                    , kinyarwanda = Just "Njya numva mbabaye cyane ku buryo ndira"
                    , kirundi = Nothing
                    }

                MentalHealthQuestion10 ->
                    { english = "The thought of harming myself has occurred to me"
                    , kinyarwanda = Just "Ibitekerezo byo kwigirira nabi bijya binzamo"
                    , kirundi = Nothing
                    }

        PrenatalMentalHealthOptionForQuestion question option ->
            case question of
                MentalHealthQuestion1 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I always could"
                            , kinyarwanda = Just "Buri gihe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not quite so much now"
                            , kinyarwanda = Just "Ubu ntago ari cyane"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely not so much now"
                            , kinyarwanda = Just "Ntago ari cyane na gato"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion2 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I ever did"
                            , kinyarwanda = Just "Nk'ibisanzwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Rather less than I used to"
                            , kinyarwanda = Just "Byaraganutse ugereranyije nuko byari bisanzwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely less than I used to"
                            , kinyarwanda = Just "Gake cyane ugereranyije n‘uko byari bisanzwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Hardly at all"
                            , kinyarwanda = Just "Habe na mba"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion3 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Si cyane"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, some of the time"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, akenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion4 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, not at all"
                            , kinyarwanda = Just "No, nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, very often"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion5 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not much"
                            , kinyarwanda = Just "Oya, ntago ari cyane"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite a lot"
                            , kinyarwanda = Just "Yego, akenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion6 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, I have been coping as well as ever"
                            , kinyarwanda = Just "Oya, ndabyakira nk'ibisanzwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, most of the time I have coped quite well"
                            , kinyarwanda = Just "Oya, akenshi njya nshoboraga kubyakira neza"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes I haven’t been coping as well as usual"
                            , kinyarwanda = Just "Yes, rimwe na rimwe ntago njya mbyakira neza"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time I haven’t been able to cope"
                            , kinyarwanda = Just "Yego, kenshi na kenshi ntago njya mbyakira neza"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion7 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not very often"
                            , kinyarwanda = Just "Oya, ntago ari kenshi"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion8 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Ntago ari kenshi"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion9 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, Nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Only occasionally"
                            , kinyarwanda = Just "Gisa rimwe na riwme"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "yego, kenshi"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, Kemshi na kenshi"
                            , kirundi = Nothing
                            }

                MentalHealthQuestion10 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Never"
                            , kinyarwanda = Just "Nta na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Sometimes"
                            , kinyarwanda = Just "rimwe na rimwe"
                            , kirundi = Nothing
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            , kirundi = Nothing
                            }

        PrenatalMentalHealthSpecialistHelper ->
            { english = "Refer patient to mental health specialist for further evaluation"
            , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
            , kirundi = Nothing
            }

        PrenatalMentalHealthSpecialistQuestion ->
            { english = "Does this health center have a mental health specialist available"
            , kinyarwanda = Just "Iki kigo nderabuzima gifite umuganga w'inzobere ku buzima bwo mu mutwe"
            , kirundi = Nothing
            }

        PrenatalMentalHealthWarningPopupMessage ->
            { english = "Patient shows signs of being a suicide risk"
            , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
            , kirundi = Nothing
            }

        PrenatalMentalHealthWarningPopupInstructions ->
            { english = "Contact mental health specialist immediately"
            , kinyarwanda = Just "Ihutire kureba umuganga w'inzobere mu buzima bwo mu mutwe"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                GivenMedicine ->
                    { english = "Were you given medicine"
                    , kinyarwanda = Just "Waba warahawe imiti"
                    , kirundi = Nothing
                    }

                PlannedFollowUpCareWithSpecialist ->
                    { english = "Do you have follow up care planned with a specialist"
                    , kinyarwanda = Just "Waba ufite gahunda yo gukurikiranwa n'umuganga w'inzobere"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 mu cyumweru mu byumweru 3"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "Kunywa inshuro 4 ku munsi mu minsi 14"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "Kunywa ibinini 4 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationMethyldopa2 ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationMethyldopa3 ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationMethyldopa4 ->
                    { english = "by mouth 4x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 4 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 1 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationTDF3TC ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationDolutegravir ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationIron1 ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 1 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationIron2 ->
                    { english = "one tab by mouth 2x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                OutsideCareMedicationPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "Ceftriaxone (1g)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForSyphilis ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                OutsideCareMedicationIron1 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationIron2 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    , kirundi = Nothing
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "Folic Acid (400IU)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoOutsideCareMedicationForAnemia ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrenatalPhotoHelper ->
            { english = "Take a picture of the mother's belly. Then you and the mother will see how the belly has grown!"
            , kinyarwanda = Just "Fata ifoto y'inda y'umubyeyi hanyuma uyimwereke arebe uko yakuze/yiyongereye."
            , kirundi = Nothing
            }

        PrenatalSymptom value ->
            case value of
                BurningWithUrination ->
                    { english = "Burning with Urination"
                    , kinyarwanda = Just "Kunyara Ukababara"
                    , kirundi = Nothing
                    }

                AbnormalVaginalDischarge ->
                    { english = "Abnormal Vaginal Discharge"
                    , kinyarwanda = Just "Gusohora ibintu bidasanzwe mu gitsina"
                    , kirundi = Nothing
                    }

                NauseaAndVomiting ->
                    { english = "Nausea and Vomiting"
                    , kinyarwanda = Just "Iseseme no kuruka"
                    , kirundi = Nothing
                    }

                Heartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Nothing
                    }

                LegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    , kirundi = Nothing
                    }

                LowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    , kirundi = Nothing
                    }

                CoughContinuous ->
                    { english = "Cough for >2 weeks"
                    , kinyarwanda = Just "Inkorora irengeje ibyumweru 2"
                    , kirundi = Nothing
                    }

                PelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    , kirundi = Nothing
                    }

                Constipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    , kirundi = Nothing
                    }

                VaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    , kirundi = Nothing
                    }

                LegPainRedness ->
                    { english = "Leg Pain or Redness (One Leg)"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    , kirundi = Nothing
                    }

                PostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Nothing
                    }

                PostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Nothing
                    }

                PostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    , kirundi = Nothing
                    }

                PostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    , kirundi = Nothing
                    }

                PostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    , kirundi = Nothing
                    }

                PostpartumPerinealPainOrDischarge ->
                    { english = "Perineal pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Nothing
                    }

                NoPrenatalSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        PrenatalSymptomQuestion value ->
            case value of
                SymptomQuestionDizziness ->
                    { english = "Are you experiencing dizziness"
                    , kinyarwanda = Just "Waba ujya ugira isereri"
                    , kirundi = Nothing
                    }

                SymptomQuestionLowUrineOutput ->
                    { english = "Are you experiencing low urine output"
                    , kinyarwanda = Just "Waba ujya unyara inkari nkeya"
                    , kirundi = Nothing
                    }

                SymptomQuestionDarkUrine ->
                    { english = "Are you experiencing dark urine"
                    , kinyarwanda = Just "Waba ujya unyara inkari zijimye"
                    , kirundi = Nothing
                    }

                SymptomQuestionPelvicPainHospitalization ->
                    { english = "Is there severe pain that requires referral to hospital"
                    , kinyarwanda = Just "Waba ufite ububabare bukabije busaba koherezwa ku bitaro"
                    , kirundi = Nothing
                    }

                SymptomQuestionLegPainRednessLeft ->
                    { english = "On which side are you experiencing leg pain or redness"
                    , kinyarwanda = Just "Ni uruhe ruhande rw'ukuguru ruribwa cyangwa rutukuye"
                    , kirundi = Nothing
                    }

                SymptomQuestionLegPainful ->
                    { english = "Is the leg painful"
                    , kinyarwanda = Just "Ubabara ukuguru"
                    , kirundi = Nothing
                    }

                SymptomQuestionLegSwollen ->
                    { english = "Is the leg swollen"
                    , kinyarwanda = Just "Ukuguru kurabyimbye"
                    , kirundi = Nothing
                    }

                SymptomQuestionLegWarm ->
                    { english = "Is the leg red or warm to the touch"
                    , kinyarwanda = Just "Ukuguru kuratukuye cyangwa kurashyushye iyo ukozeho"
                    , kirundi = Nothing
                    }

                SymptomQuestionNightSweats ->
                    { english = "Do you have night sweats"
                    , kinyarwanda = Just "Waba ubira ibyuya nijoro"
                    , kirundi = Nothing
                    }

                SymptomQuestionBloodInSputum ->
                    { english = "Do you have blood in sputum"
                    , kinyarwanda = Just "Waba ugira ikororwa kirimo amaraso"
                    , kirundi = Nothing
                    }

                SymptomQuestionWeightLoss ->
                    { english = "Do you have weight loss"
                    , kinyarwanda = Just "Waba waratakaje ibiro"
                    , kirundi = Nothing
                    }

                SymptomQuestionSevereFatigue ->
                    { english = "Do you have severe fatigue"
                    , kinyarwanda = Just "Waba ugira umunaniro ukabije"
                    , kirundi = Nothing
                    }

                SymptomQuestionVaginalItching ->
                    { english = "Do you experience vaginal itching"
                    , kinyarwanda = Just "Waba ufite uburyaryate mu gitsina"
                    , kirundi = Nothing
                    }

                SymptomQuestionVaginalDischarge ->
                    { english = "Do you experience vaginal discharge"
                    , kinyarwanda = Just "Ujya ubona ibintu bidasanzwe biva mu gitsina"
                    , kirundi = Nothing
                    }

                SymptomQuestionFrequentUrination ->
                    { english = "Do you experience urinating frequently"
                    , kinyarwanda = Just "Waba ujya kunyara buri kanya"
                    , kirundi = Nothing
                    }

                SymptomQuestionFlankPain ->
                    { english = "Do you experience flank pain"
                    , kinyarwanda = Just "Waba ujya uribwa mu ibondo"
                    , kirundi = Nothing
                    }

                SymptomQuestionPartnerUrethralDischarge ->
                    { english = "Does your partner have urethral discharge"
                    , kinyarwanda = Just "Umugabo wawe ajya agira ibintu bidasanzwe biva mu gitsina"
                    , kirundi = Nothing
                    }

                NoSymptomQuestions ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        PrenatalSymptomQuestionsHeader ->
            { english = "The patient has noted symptoms that require follow up questions"
            , kinyarwanda = Just "Umubyeyi yagaragaje ibimenyetso bisaba ibindi bibazo"
            , kirundi = Nothing
            }

        TestExecutionNote note ->
            case note of
                TestNoteRunToday ->
                    { english = "Run Today"
                    , kinyarwanda = Just "Ikizamini cyakozwe uyu munsi"
                    , kirundi = Nothing
                    }

                TestNoteRunPreviously ->
                    { english = "Run Previously"
                    , kinyarwanda = Just "Ikizamimi cyakozwe ubushize"
                    , kirundi = Nothing
                    }

                TestNoteLackOfReagents ->
                    { english = "Lack of Reagents"
                    , kinyarwanda = Just "Kubura kw'ibikoresho byo gupima"
                    , kirundi = Nothing
                    }

                TestNoteLackOfOtherSupplies ->
                    { english = "Lack of Other Supplies"
                    , kinyarwanda = Just "Kubura kw'ibindi bikoresho bicyenerwa mu gupima"
                    , kirundi = Nothing
                    }

                TestNoteNoEquipment ->
                    { english = "No Equipment"
                    , kinyarwanda = Just "Nta gikoresho gihari"
                    , kirundi = Nothing
                    }

                TestNoteBrokenEquipment ->
                    { english = "Broken Equipment"
                    , kinyarwanda = Just "Igikoresho gipima cyarangiritse"
                    , kirundi = Nothing
                    }

                TestNoteNotIndicated ->
                    { english = "Not Indicated"
                    , kinyarwanda = Just "Ikizamini nticyasabwe"
                    , kirundi = Nothing
                    }

                TestNoteKnownAsPositive ->
                    { english = "Known as Positive"
                    , kinyarwanda = Just "Asanzwe afite ubwandu"
                    , kirundi = Nothing
                    }

                TestNoteToBeDoneAtHospital ->
                    { english = "To be Done at Hospital"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TestResult result ->
            case result of
                TestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    , kirundi = Nothing
                    }

                TestNegative ->
                    translationSet NegativeLabel

                TestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    , kirundi = Nothing
                    }

        PrenatalVaccineLabel value ->
            case value of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    , kirundi = Nothing
                    }

        PreTerm ->
            { english = "Pre Term"
            , kinyarwanda = Just "Inda itaragera igihe"
            , kirundi = Nothing
            }

        PregnancyConcludedLabel ->
            { english = "or Pregnancy Concluded"
            , kinyarwanda = Just "Cyangwa Iherezo ry'inda"
            , kirundi = Nothing
            }

        PregnancyOutcomeLabel ->
            { english = "Pregnancy Outcome"
            , kinyarwanda = Just "Iherezo ry'inda"
            , kirundi = Nothing
            }

        PregnancyOutcome outcome ->
            case outcome of
                OutcomeLiveAtTerm ->
                    { english = "Live Birth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Kubyara umwana muzima/Ushyitse (ku byumweru 38 kuzamura)"
                    , kirundi = Nothing
                    }

                OutcomeLivePreTerm ->
                    { english = "Live Birth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Kubyara mwana udashyitse (munsi y'ibyumweru 38)"
                    , kirundi = Nothing
                    }

                OutcomeStillAtTerm ->
                    { english = "Stillbirth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda bageze igihe cyo kuvuka (ku byumweru 38 kuzamura)"
                    , kirundi = Nothing
                    }

                OutcomeStillPreTerm ->
                    { english = "Stillbirth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda batagejeje igihe cyo kuvuka (munsi y'ibyumweru 38)"
                    , kirundi = Nothing
                    }

                OutcomeAbortions ->
                    { english = "Abortions (before 24 weeks EGA)"
                    , kinyarwanda = Just "Kuvanamo inda (mbere y'ibyumweru 24)"
                    , kirundi = Nothing
                    }

        PreviousCSectionScar ->
            { english = "Previous C-section scar"
            , kinyarwanda = Just "Inkovu yaho babaze ubushize"
            , kirundi = Nothing
            }

        PreviousDelivery ->
            { english = "Previous Delivery"
            , kinyarwanda = Just "Kubyara guheruka"
            , kirundi = Nothing
            }

        PreviousDeliveryPeriods period ->
            case period of
                LessThan18Month ->
                    { english = "Less than 18 month ago"
                    , kinyarwanda = Just "Munsi y'amezi 18 ashize"
                    , kirundi = Nothing
                    }

                MoreThan5Years ->
                    { english = "More than 5 years ago"
                    , kinyarwanda = Just "Hejuru y'imyaka itanu ishize"
                    , kirundi = Nothing
                    }

                Neither ->
                    { english = "Neither"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        PreviousFloatMeasurement value ->
            { english = "Previous measurement: " ++ String.fromFloat value
            , kinyarwanda = Just <| "Ibipimo by'ubushize: " ++ String.fromFloat value
            , kirundi = Nothing
            }

        PreviousMeasurementNotFound ->
            { english = "No previous measurement on record"
            , kinyarwanda = Just "Nta gipimo cy'ubushize cyanditswe"
            , kirundi = Nothing
            }

        PriorTreatmentTask task ->
            case task of
                TreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    , kirundi = Nothing
                    }

        Profession ->
            { english = "Profession"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Programs ->
            { english = "Programs"
            , kinyarwanda = Just "Porogaramu"
            , kirundi = Nothing
            }

        ProgressPhotos ->
            { english = "Progress Photos"
            , kinyarwanda = Just "Uko amafoto agenda ahinduka"
            , kirundi = Nothing
            }

        ProgressReport ->
            { english = "Progress Report"
            , kinyarwanda = Just "Raporo y’ibyakozwe"
            , kirundi = Nothing
            }

        ProgressReports ->
            { english = "Progress Reports"
            , kinyarwanda = Just "Raporo z’ibyakozwe"
            , kirundi = Nothing
            }

        ProgressTimeline ->
            { english = "Progress Timeline"
            , kinyarwanda = Just "Uko inda igenda ikura"
            , kirundi = Nothing
            }

        ProgressTrends ->
            { english = "Progress Trends"
            , kinyarwanda = Just "Uko ibipimo bigenda bizamuka"
            , kirundi = Nothing
            }

        ProvideHealthEducationAndInstructToIsolate ->
            { english = "Provide health education and instruct them to self isolate at home"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrenatalParticipant ->
            { english = "Antenatal Participant"
            , kinyarwanda = Just "Umubyeyi witabiriye kwipimisha inda"
            , kirundi = Nothing
            }

        PrenatalParticipants ->
            { english = "Antenatal Participants"
            , kinyarwanda = Just "Ababyeyi bitabiriye kwipimisha inda"
            , kirundi = Nothing
            }

        PreTermPregnancy ->
            { english = "Number of Pre-term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima badashyitse"
            , kirundi = Nothing
            }

        TestDate ->
            { english = "Date of Test"
            , kinyarwanda = Just "Itariki y'Ikizamini"
            , kirundi = Nothing
            }

        TestName ->
            { english = "Test name"
            , kinyarwanda = Just "Izina ry'ikizamini"
            , kirundi = Nothing
            }

        TestPerformedQuestion ->
            { english = "Were you able to perform the test"
            , kinyarwanda = Just "Waba wakoze ikizamini"
            , kirundi = Nothing
            }

        TestPerformedTodayQuestion ->
            { english = "Did you perform this test today"
            , kinyarwanda = Just "Waba wakoze iki kizamini uyu munsi"
            , kirundi = Nothing
            }

        TestPrerequisiteQuestion value ->
            case value of
                PrerequisiteFastFor12h ->
                    { english = "Was this test performed before a meal"
                    , kinyarwanda = Just "Umurwayi yafatiwe iki kizamini mbere yo kurya"
                    , kirundi = Nothing
                    }

                PrerequisiteImmediateResult ->
                    { english = "Where was this test performed"
                    , kinyarwanda = Just "Iki Kizamini cyakozwe"
                    , kirundi = Nothing
                    }

                NoTestPrerequisites ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Nothing
                    }

        TestVariantUrineDipstickQuestion ->
            { english = "Which type of urine dipstick test was run"
            , kinyarwanda = Just "Ni ikihe kizamini cy'inkari cyakozwe"
            , kirundi = Nothing
            }

        TestResultQuestion ->
            { english = "What was the result of the test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TestResultsQuestion ->
            { english = "What were the results of the test"
            , kinyarwanda = Just "Ibisubizo by'ikizamini byabaye ibihe"
            , kirundi = Nothing
            }

        PriorDiagnosis ->
            { english = "Prior Diagnosis"
            , kinyarwanda = Just "Uburwayi yagize/yigeze kurwara"
            , kirundi = Nothing
            }

        ProvidedHealthEducationAction ->
            { english = "Provided health education and anticipatory guidance"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ProvideHealthEducation ->
            { english = "Provide health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Nothing
            }

        ProvideHealthEducationShort ->
            { english = "Provide health education and anticipatory guidance"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Nothing
            }

        ProvidedPreventionEducationQuestion ->
            { english = "Have you provided health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            , kirundi = Nothing
            }

        ProvidedPreventionEducationQuestionShort ->
            { english = "Have you provided health education and anticipatory guidance"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho"
            , kirundi = Nothing
            }

        ProvidedSymtomReliefGuidanceQuestion ->
            { english = "Have you provided the guidance for symptom relief"
            , kinyarwanda = Just "Wamusobanuriye ibijyanye n'imiti itangwa mukuvura ibimenyesto"
            , kirundi = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Just "Intara"
            , kirundi = Nothing
            }

        ReadToggle isRead ->
            if isRead then
                { english = "Unread"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = "Read"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        ReasonForCSection ->
            { english = "Reason for C-section"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        ReasonForNotBreastfeeding reason ->
            case reason of
                NotBreastfeedingBreastPain ->
                    { english = "Breast pain"
                    , kinyarwanda = Just "Ububabare bw'amabere"
                    , kirundi = Nothing
                    }

                NotBreastfeedingBreastRedness ->
                    { english = "Breast redness"
                    , kinyarwanda = Just "Amabere aratukuye"
                    , kirundi = Nothing
                    }

                NotBreastfeedingLowMilkProduction ->
                    { english = "Low milk production"
                    , kinyarwanda = Just "Amashereka adahagije"
                    , kirundi = Nothing
                    }

                NotBreastfeedingProblemsLatching ->
                    { english = "Problems latching"
                    , kinyarwanda = Just "Ibibazo byo konka"
                    , kirundi = Nothing
                    }

                NotBreastfeedingMedicalProblems ->
                    { english = "Medical Problems"
                    , kinyarwanda = Just "Ibibazo by'uburwayi"
                    , kirundi = Nothing
                    }

                NotBreastfeedingPersonalChoice ->
                    { english = "Personal Choice"
                    , kinyarwanda = Just "Amahitamo ye bwite"
                    , kirundi = Nothing
                    }

                NotBreastfeedingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TooIll ->
                    { english = "Too ill to leave alone"
                    , kinyarwanda = Just "Umurwayi ararembye ntagomba gusigara wenyine"
                    , kirundi = Nothing
                    }

                CanNotSeparateFromFamily ->
                    { english = "Unable to separate from family"
                    , kinyarwanda = Just "Ntibishoboka kumutandukanya n'umuryango"
                    , kirundi = Nothing
                    }

                OtherReason ->
                    { english = "Other"
                    , kinyarwanda = Just "Ikindi"
                    , kirundi = Nothing
                    }

                IsolationReasonNotApplicable ->
                    { english = "Not Applicable "
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    , kirundi = Nothing
                    }

        ReasonForNotProvidingHealthEducation reason ->
            case reason of
                PatientNeedsEmergencyReferral ->
                    { english = "Patient needs an emergency referral"
                    , kinyarwanda = Just "Umurwayi akeneye kwoherezwa ku ivuriro byihutirwa"
                    , kirundi = Nothing
                    }

                ReceivedEmergencyCase ->
                    { english = "Received an emergency case to treat"
                    , kinyarwanda = Just "Nakiriye undi murwayi ukeneye kuvurwa byihutirwa"
                    , kirundi = Nothing
                    }

                LackOfAppropriateEducationUserGuide ->
                    { english = "Lack of appropriate education user guide"
                    , kinyarwanda = Just "Nta mfashanyigisho yabugenewe ihari"
                    , kirundi = Nothing
                    }

                PatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Nothing
                    }

                PatientTooIll ->
                    { english = "Patient too ill"
                    , kinyarwanda = Just "Umurwayi ararembye"
                    , kirundi = Nothing
                    }

                NoReasonForNotProvidingHealthEducation ->
                    { english = "No reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    , kirundi = Nothing
                    }

        ReasonForNotTaking reason ->
            case reason of
                NotTakingAdverseEvent ->
                    { english = "Adverse event"
                    , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                    , kirundi = Nothing
                    }

                NotTakingNoMoney ->
                    { english = "No money for medication"
                    , kinyarwanda = Just "Nta mafaranga yo kwishyura imiti afite"
                    , kirundi = Nothing
                    }

                NotTakingMemoryProblems ->
                    { english = "Memory problems"
                    , kinyarwanda = Just "Ibibazo byo kwibagirwa"
                    , kirundi = Nothing
                    }

                NotTakingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoReasonForNotTakingSign ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        Received ->
            { english = "Received"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReceivedDewormingPill ->
            { english = "Has the mother received deworming pill"
            , kinyarwanda = Just "Umubyeyi yahawe ikinini cy'inzoka"
            , kirundi = Nothing
            }

        ReceivedFolicAcid ->
            { english = "Have you received Folic Acid"
            , kinyarwanda = Just "Wahawe ibinini bya Folic Acid"
            , kirundi = Nothing
            }

        ReceivedFrom ->
            { english = "Received From"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReceivedIronFolicAcid ->
            { english = "Has the mother received iron and folic acid supplement"
            , kinyarwanda = Just "Umubyeyi yahawe ibinini bya Fer cg Folic Acid byongera amaraso"
            , kirundi = Nothing
            }

        ReceivedMebendazole ->
            { english = "Has the mother received Mebendazole in the last 6 months"
            , kinyarwanda = Just "Ububyeyi yahawe umuti wa Mebendazole mu mezi 6 ashize"
            , kirundi = Nothing
            }

        ReceivedMosquitoNet ->
            { english = "Has the mother received a mosquito net"
            , kinyarwanda = Just "Umubyeyi yahawe inzitiramubu"
            , kirundi = Nothing
            }

        ReceivedVitaminA ->
            { english = "Have you received Vitamin A"
            , kinyarwanda = Just "Wahawe Vitamine A"
            , kirundi = Nothing
            }

        Recommendation114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "Send Patient to the nearest health center"
                    , kinyarwanda = Just "Ohereza umurwayi ku kigo nderabuzima kikwegereye"
                    , kirundi = Nothing
                    }

                SendToRRTCenter ->
                    { english = "Send patient to the Rapid Response Team center"
                    , kinyarwanda = Just "Ohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    , kirundi = Nothing
                    }

                SendToHospital ->
                    { english = "Send patient to the nearest hospital"
                    , kinyarwanda = Just "Ohereza umurwayi ku bitaro bikwegereye"
                    , kirundi = Nothing
                    }

                OtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoneNoAnswer ->
                    { english = "No answer"
                    , kinyarwanda = Just "Nta Gisubizo cyabonetse"
                    , kirundi = Nothing
                    }

                NoneBusySignal ->
                    { english = "Busy Signal"
                    , kinyarwanda = Just "Umurongo bawuvugiragaho"
                    , kirundi = Nothing
                    }

                NoneOtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

        RecommendationSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Team will come to village"
                    , kinyarwanda = Just "Itsinda rizaza mu mudugudu"
                    , kirundi = Nothing
                    }

                SendToSiteWithForm ->
                    { english = "Advised to send patient to site with referral form"
                    , kinyarwanda = Just "Nagiriwe inama yo kohereza umurwayi ku rwego rubishinzwe yitwaje impapuro zimwohereza"
                    , kirundi = Nothing
                    }

                OtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                NoneSentWithForm ->
                    { english = "No response. Sent patient with referral form."
                    , kinyarwanda = Just "Nta gisubizo. Nohereje umurwayi yitwaje impapuro zimwohereza."
                    , kirundi = Nothing
                    }

                NonePatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    , kirundi = Nothing
                    }

                NoneOtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Nothing
                    }

                RecommendationSiteNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    , kirundi = Nothing
                    }

        Recommended ->
            { english = "Recommended"
            , kinyarwanda = Just "Imiti yemewe"
            , kirundi = Nothing
            }

        RecommendedButNotGivenDueTo ->
            { english = "recommended but not given due to"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RecommendedSymptomRelief ->
            { english = "Recommended Symptom Relief"
            , kinyarwanda = Just "Imiti yemewe mukuvura ibimenyesto"
            , kirundi = Nothing
            }

        RecommendedTreatmentSignDosage sign ->
            case sign of
                TreatmentPenecilin1 ->
                    { english = "IM x 1"
                    , kinyarwanda = Just "IM inshuro 1"
                    , kirundi = Nothing
                    }

                TreatmentPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 buri cyumweru mu byumweru 3"
                    , kirundi = Nothing
                    }

                TreatmentErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "mu kanwa inshuro enye ku munsi mu minsi 14"
                    , kirundi = Nothing
                    }

                TreatmentAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "ibinini 4 abinywe mu kanwa umunsi umwe"
                    , kirundi = Nothing
                    }

                TreatmentCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    , kirundi = Nothing
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "1 tablet by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "kunywa ikinini 1 inshuro ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentNitrofurantoin ->
                    { english = "by mouth 2x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentAmoxicillin ->
                    { english = "by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 3 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentClotrimaxazole200 ->
                    { english = "vaginally every night x 3 night"
                    , kinyarwanda = Just "mu gitsina buri joro mu majoro 3"
                    , kirundi = Nothing
                    }

                TreatmentClotrimaxazole500 ->
                    { english = "vaginally one time"
                    , kinyarwanda = Just "inshuro imwe mu gitsina"
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa2 ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa3 ->
                    { english = "1 tablet by mouth three times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 3 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa4 ->
                    { english = "1 tablet by mouth four times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 4 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 1 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentHydrochlorothiazide ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Nothing
                    }

                TreatmentAmlodipine ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Nothing
                    }

                TreatmentNifedipine ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentCaptopril ->
                    { english = "1 tablet by mouth 3 times a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 3 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentLisinopril ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Nothing
                    }

                TreatmentAtenlol ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa buri munsi"
                    , kirundi = Nothing
                    }

                TreatmentCloxacillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentMastitisAmoxicillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentPenecilinV ->
                    { english = "2 tablets by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "ibinini 2 mu kanwa inshuri 3 ku munsi mu minsi 7"
                    , kirundi = Nothing
                    }

                TreatmentParacetamol ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    , kirundi = Nothing
                    }

                TreatmentIbuprofen ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    , kirundi = Nothing
                    }

                TreatmentMetformin1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Just "ikinini kimwe mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentMetformin2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Just "ibinini 2 mu kanwa mu gitondo n'ikinini kimwe mu kanwa nijoro"
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Just "ibinini 2 mu kanwa mu gitondo n'ikinini kimwe mu kanwa nijoro"
                    , kirundi = Nothing
                    }

                TreatmentMetformin2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Just "ibinini bibiri mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
                    }

                TreatmentGlipenclamide2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Just "ibinini bibiri mu kanwa inshuro 2 ku munsi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentCoartem ->
                    { english = "Coartem - 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                _ ->
                    translationSet (RecommendedTreatmentSignLabel sign)

        RecommendedTreatmentSignLabel sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "Give quinine sulphate per os 10 mg/kg/dose, 3 times a day for 7 days"
                    , kinyarwanda = Just "Tanga umuti wa Kinini mu kanwa: 10mg ku kilo, gatatu ku munsi, mu minsi irindwi"
                    , kirundi = Nothing
                    }

                TreatmentCoartem ->
                    { english = "Give Coartem 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Just "Tanga AL (Kowaritemu) ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    , kirundi = Nothing
                    }

                TreatmentWrittenProtocols ->
                    { english = "GI complications: followed Written Protocols"
                    , kinyarwanda = Just "Afite ibibazo by'urwungano ngogozi: Kurikiza amabwiriza"
                    , kirundi = Nothing
                    }

                TreatmentReferToHospital ->
                    { english = "Severe Malaria: Stabilize and Refer to Hospital"
                    , kinyarwanda = Just "Afite Malaria y'Igikatu: Tanga umuti w'ibanze uhite umwoherza ku bitaro"
                    , kirundi = Nothing
                    }

                NoTreatmentForMalaria ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    , kirundi = Nothing
                    }

                TreatmentPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Nothing
                    }

                TreatmentPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    , kirundi = Nothing
                    }

                TreatmentErythromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    , kirundi = Nothing
                    }

                TreatmentAzithromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "Aluminium Hydroxide (500mg)"
                    , kinyarwanda = Just "Idologiside d'Aluminiyumu miligarama 500"
                    , kirundi = Nothing
                    }

                TreatmentHealthEducationForHeartburn ->
                    { english = "Not dispensing medicine. Follow health education protocols."
                    , kinyarwanda = Just "Witanga umuti. Kurikiza amabwiriza ajyanye n'inyigisho z'buzima."
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        RecordAcuteIllnessOutcome ->
            { english = "Record Acute Illness Outcome"
            , kinyarwanda = Just "Andika iherezo ry'indwara ifatiyeho"
            , kirundi = Nothing
            }

        RecordPregnancyOutcome ->
            { english = "Record Pregnancy Outcome"
            , kinyarwanda = Just "Andika iherezo ry'inda"
            , kirundi = Nothing
            }

        RectalHemorrhoids ->
            { english = "Rectal Hemorrhoids"
            , kinyarwanda = Just "Kubyimba kw'imitsi y'ishyira(rectum)/Hemoroyide"
            , kirundi = Nothing
            }

        RecurringHighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BloodPressure ->
                    { english = "Blood Pressure"
                    , kinyarwanda = Just "Umuvuduko w'amaraso"
                    , kirundi = Nothing
                    }

        ReferredPatientToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Have you referred the patient to the health center"
                    , kinyarwanda = Just "Waba wohereje umurwayi ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Have you referred the patient to the hospital"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku bitaro"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Have you referred the patient to the specialist"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku muganaga w'inzobere"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Have you referred the patient to the ARV services"
                    , kinyarwanda = Just "Waba wohere umubyeyi muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "Have you referred the patient to NCD services"
                    , kinyarwanda = Just "Waba wohereje umubyeyi muri service y'indwara zitandura"
                    , kirundi = Nothing
                    }

                FacilityANCServices ->
                    { english = "Have you referred the patient to ANC services"
                    , kinyarwanda = Just "Wohereje umurwayi muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FacilityUltrasound ->
                    { english = "Have you referred the patient to ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReferredToFacility facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Referred to health center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Referred to hospital"
                    , kinyarwanda = Just "Yoherejwe ku bitaro"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Referred to mental health specialist"
                    , kinyarwanda = Just "Yoherejwe ku muganga w'inzobere w'ubuzima bwo mu mutwe"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Referred to ARV services"
                    , kinyarwanda = Just "Yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "Referred to NCD services"
                    , kinyarwanda = Just "Yoherejwe muri service y'indwara zitandura"
                    , kirundi = Nothing
                    }

                FacilityANCServices ->
                    { english = "Referred to ANC services"
                    , kinyarwanda = Just "Yoherejwe muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FacilityUltrasound ->
                    { english = "Referred to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReferredToFacilityNot facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Not referred to health center"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Not referred to hospital"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku bitaro"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Not referred to mental health specialist"
                    , kinyarwanda = Just "Ntabwo yoherejwe kwa muganga w'inzobere w'ubuzima bwo mu mutwe"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Not referred to ARV services"
                    , kinyarwanda = Just "Ntago yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "Not referred to NCD services"
                    , kinyarwanda = Just "Ntabwo yoherejwe muri service y'indwara zitandura"
                    , kirundi = Nothing
                    }

                FacilityANCServices ->
                    { english = "Not referred to ANC services"
                    , kinyarwanda = Just "Ntabwo yoherejwe muri serivise yita kubuzima bw'umubyeyi utwite"
                    , kirundi = Nothing
                    }

                FacilityUltrasound ->
                    { english = "Not referred to Ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReferredToFacilityPostpartum facility ->
            case facility of
                FacilityARVProgram ->
                    { english = "referred to ARV services for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise itanga imiti igabanya ubukana bwa Virusi itera SIDA kugirango akurikiranwe nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    { english = "referred to NCD program for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise y'indwara zitandura kugirango akurikiranwe nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReferToHospitalForFurtherEvaluation ->
            { english = "Refer patient to hospital for further evaluation"
            , kinyarwanda = Just "Ohereza umurwayi ku bitaro kugirango hakorwe isuzuma ryimbitse"
            , kirundi = Nothing
            }

        ReferToHospitalForTesting ->
            { english = "Refer patient to hospital for testing"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReferToProgramAction ->
            { english = "Refer patient to appropriate nutrition program"
            , kinyarwanda = Just "Ohereza umurwayi muri porogaramu y'imirire yabugenewe "
            , kirundi = Nothing
            }

        ReferToProgramQuestion ->
            { english = "Did you direct the patient to attend the next program session"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Register ->
            { english = "Register"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegisterContactHelper ->
            { english = "Not the contact you were looking for?"
            , kinyarwanda = Just "Ntabwo ari uwo washakishaga?"
            , kirundi = Nothing
            }

        RegisterParticipantHelper ->
            { english = "Not the participant you were looking for?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegisterNewContact ->
            { english = "Register a new contact"
            , kinyarwanda = Just "Andika umuntu mushya wahuye n'umurwayi"
            , kirundi = Nothing
            }

        RegisterNewParticipant ->
            { english = "Register a new participant"
            , kinyarwanda = Just "Andika umurwayi mushya"
            , kirundi = Nothing
            }

        RegistratingHealthCenter ->
            { english = "Registrating Health Center"
            , kinyarwanda = Just "Izina ry'ikigo nderabuzima umugenerwabikorwa abarizwamo"
            , kirundi = Nothing
            }

        RegistrationSuccessful ->
            { english = "Registration Successful"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegistrationSuccessfulParticipantAdded ->
            { english = "The participant has been added to E-Heza."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegistrationSuccessfulSuggestAddingChild ->
            { english = "The participant has been added to E-Heza. Would you like to add a child for this participant?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegistrationSuccessfulSuggestAddingMother ->
            { english = "The participant has been added to E-Heza. Would you like to add a mother for this participant?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RelationSuccessful ->
            { english = "Relation Successful"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RelationSuccessfulChildWithMother ->
            { english = "Child succesfully assocoated with mother."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RelationSuccessfulMotherWithChild ->
            { english = "Mother succesfully assocoated with child."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RenalDisease ->
            { english = "Renal Disease"
            , kinyarwanda = Just "Indwara z'impyiko"
            , kirundi = Nothing
            }

        RemainingForDownloadLabel ->
            { english = "Remaining for Download"
            , kinyarwanda = Just "Ibisigaye gukurwa kuri seriveri"
            , kirundi = Nothing
            }

        RemainingForUploadLabel ->
            { english = "Remaining for Upload"
            , kinyarwanda = Just "Ibisigaye koherezwa kuri seriveri"
            , kirundi = Nothing
            }

        RemainingTotalToUpload ->
            { english = "Remaining to upload, in total"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RemindMe ->
            { english = "Remind Me"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RemindMePhrase ->
            { english = "Remind me of this message in:"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReportAge age ->
            { english = "Age: " ++ age
            , kinyarwanda = Just <| "Imyaka: " ++ age
            , kirundi = Nothing
            }

        ReportComponentAntenatal component ->
            case component of
                ComponentAntenatalRiskFactors ->
                    { english = "Risk Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalMedicalDiagnoses ->
                    { english = "Medical Diagnoses"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalObstetricalDiagnoses ->
                    { english = "Obstetrical Diagnoses"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalCHWActivity ->
                    { english = "CHW Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalPatientProgress ->
                    { english = "Patient Progress"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalLabsResults ->
                    { english = "Labs Results"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentAntenatalProgressPhotos ->
                    { english = "Progress Photos"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReportComponentNCD component ->
            case component of
                ComponentNCDRiskFactors ->
                    { english = "Risk Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentNCDActiveDiagnosis ->
                    { english = "Active Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentNCDMedicalDiagnosis ->
                    { english = "Medical Diagnosis"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentNCDPatientProgress ->
                    { english = "Patient Progress"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentNCDLabsResults ->
                    { english = "Labs Results"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReportComponentWellChild component ->
            case component of
                ComponentWellChildActiveDiagnoses ->
                    { english = "Acute Illness History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentWellChildImmunizationHistory ->
                    { english = "Immunization Histor"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentWellChildECD ->
                    { english = "Early Childhood Development"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentWellChildGrowth ->
                    { english = "Growth"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ComponentWellChildNextAppointment ->
                    { english = "Next Appointment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReportDOB dob ->
            { english = "DOB: " ++ dob
            , kinyarwanda = Just <| "Itariki y'amavuko: " ++ dob
            , kirundi = Nothing
            }

        ReportRemaining remaining ->
            { english = String.fromInt remaining ++ " remaning"
            , kinyarwanda = Just <| String.fromInt remaining ++ " iyibutswa rya raporo"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        ReportResultsOfParticipantsSearch total ->
            case total of
                1 ->
                    { english = "There is 1 participant that matches your search."
                    , kinyarwanda = Just "Hari umujyenerwabikorwa 1 uhuye nuwo washatse"
                    , kirundi = Nothing
                    }

                _ ->
                    { english = "There are " ++ String.fromInt total ++ " participants that match your search."
                    , kinyarwanda = Just <| "Hari abagenerwabikorwa " ++ String.fromInt total ++ " bahuye nuwo ushaka mu ishakiro"
                    , kirundi = Nothing
                    }

        ReportTab tab ->
            case tab of
                TabSPVReport ->
                    { english = "Standard Pediatric Report"
                    , kinyarwanda = Just "Raporo ku Isuzuma ry'Umwana"
                    , kirundi = Nothing
                    }

                TabNCDAScoreboard ->
                    translationSet ChildScorecard

        Reports ->
            { english = "Reports"
            , kinyarwanda = Just "Raporo"
            , kirundi = Nothing
            }

        RecentAndUpcomingGroupEncounters ->
            { english = "Recent and upcoming Group Encounters"
            , kinyarwanda = Just "Ahabarizwa amatsinda aheruka gukorerwa n'agiye gukorerwa"
            , kirundi = Nothing
            }

        ReportCompleted { pending, completed } ->
            { english = String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Raporo irarangiye"
            , kirundi = Nothing
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
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        RespiratoryRate ->
            { english = "Respiratory Rate"
            , kinyarwanda = Just "Inshuro ahumeka"
            , kirundi = Nothing
            }

        ResponsePeriod period ->
            case period of
                LessThan30Min ->
                    { english = "Less than 30 min"
                    , kinyarwanda = Just "Munsi y'iminota mirongo itatu"
                    , kirundi = Nothing
                    }

                Between30min1Hour ->
                    { english = "30 min - 1 hour"
                    , kinyarwanda = Just "Hagati y’iminota mirongo itatu n’isaha"
                    , kirundi = Nothing
                    }

                Between1Hour2Hour ->
                    { english = "1 hour - 2 hours"
                    , kinyarwanda = Just "Hagati y'isaha n'amasaha abiri"
                    , kirundi = Nothing
                    }

                Between2Hour1Day ->
                    { english = "2 hours - 1 day"
                    , kinyarwanda = Just "Hagati y'amasaha abiri n'umunsi"
                    , kirundi = Nothing
                    }

                ResponsePeriodNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    , kirundi = Nothing
                    }

        Result ->
            { english = "Result"
            , kinyarwanda = Just "Igisubizo"
            , kirundi = Nothing
            }

        ResultOfContacting114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "114 recommended to send patient to the nearest health center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku kigo nderabuzima kinyegereye"
                    , kirundi = Nothing
                    }

                SendToRRTCenter ->
                    { english = "114 recommended to send patient to Rapid Response Team center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    , kirundi = Nothing
                    }

                SendToHospital ->
                    { english = "114 recommended to send patient to the nearest hospital"
                    , kinyarwanda = Just "Ku 114 bangiriye inama yo kohereza umurwayi ku bitaro binyegereye"
                    , kirundi = Nothing
                    }

                OtherRecommendation114 ->
                    { english = "114 did not recommended to send patient to site"
                    , kinyarwanda = Just "Ku 114 bansabye kutohereza umurwayi"
                    , kirundi = Nothing
                    }

                NoneNoAnswer ->
                    { english = "Not able to talk to 114 - no answer"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- nta gisubizo"
                    , kirundi = Nothing
                    }

                NoneBusySignal ->
                    { english = "Not able to talk to 114 - busy signal"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- umurongo bawuvugiragaho"
                    , kirundi = Nothing
                    }

                NoneOtherRecommendation114 ->
                    { english = "Not able to talk to 114 - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- Izindi mpamvu"
                    , kirundi = Nothing
                    }

        ResultOfContactingRecommendedSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Site recommendation: Team will come to village"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Itsinda rizaza mu mudugudu"
                    , kirundi = Nothing
                    }

                SendToSiteWithForm ->
                    { english = "Site recommendation: Send patient to site with referral form"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Twohereze umurwayi yitwaje impapuro zimwohereza"
                    , kirundi = Nothing
                    }

                OtherRecommendationSite ->
                    { english = "Site recommendation: Other"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Ibindi"
                    , kirundi = Nothing
                    }

                NoneSentWithForm ->
                    { english = "Not able to talk to site due - no response. Sent patient with referral form"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera- nta gisubizo cyabonetse. Nohereje umurwayi yitwaje impapuro zimwohereza"
                    , kirundi = Nothing
                    }

                NonePatientRefused ->
                    { english = "Did not talk to site as patient has refused"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera umurwayi yanze"
                    , kirundi = Nothing
                    }

                NoneOtherRecommendationSite ->
                    { english = "Not able to talk to site - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe- Izindi mpamvu"
                    , kirundi = Nothing
                    }

                RecommendationSiteNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    , kirundi = Nothing
                    }

        ResultsMissing ->
            { english = "Results Missing"
            , kinyarwanda = Just "Ibisubizo Ntibihari"
            , kirundi = Nothing
            }

        ResultsPending ->
            { english = "Results Pending"
            , kinyarwanda = Just "Ibisubizo birategerejwe"
            , kirundi = Nothing
            }

        Retry ->
            { english = "Retry"
            , kinyarwanda = Just "Kongera kugerageza"
            , kirundi = Nothing
            }

        ReviewCaseWith144Respondent ->
            { english = "Review case with 114 Respondent"
            , kinyarwanda = Just "Ongera ukore isuzuma ufatanije n’ukwitabye kuri 114"
            , kirundi = Nothing
            }

        Reviewed ->
            { english = "Reviewed"
            , kinyarwanda = Just "Byarebwe"
            , kirundi = Nothing
            }

        ReviewPriorDiagnosis ->
            { english = "Review Prior Diagnosis"
            , kinyarwanda = Just "Kureba uburwayi yagize/yigeze kurwara"
            , kirundi = Nothing
            }

        RhNegative ->
            { english = "RH Negative"
            , kinyarwanda = Just "Ubwoko bw'amaraso ni Negatifu"
            , kirundi = Nothing
            }

        Right ->
            { english = "Right"
            , kinyarwanda = Just "Iburyo"
            , kirundi = Nothing
            }

        RiskFactorAlert factor ->
            case factor of
                FactorNumberOfCSections number ->
                    if number == 1 then
                        { english = "1 previous C-section"
                        , kinyarwanda = Just "Yabazwe inshuro imwe ubushize"
                        , kirundi = Nothing
                        }

                    else
                        { english = String.fromInt number ++ " previous C-sections"
                        , kinyarwanda = Just <| String.fromInt number ++ " ubushize yarabazwe"
                        , kirundi = Nothing
                        }

                FactorCSectionInPreviousDelivery ->
                    { english = "C-section in previous delivery"
                    , kinyarwanda = Just "Yarabazwe ku nda ishize"
                    , kirundi = Nothing
                    }

                FactorCSectionReason ->
                    { english = "C-section in previous delivery due to"
                    , kinyarwanda = Just "Ubushize yabazwe abyara kubera"
                    , kirundi = Nothing
                    }

                FactorPreviousDeliveryPeriod ->
                    { english = "Previous delivery"
                    , kinyarwanda = Just "kubyara guheruka"
                    , kirundi = Nothing
                    }

                FactorSuccessiveAbortions ->
                    { english = "Patient experienced successive abortions"
                    , kinyarwanda = Just "Umubyeyi yavanyemo inda zikurikiranye"
                    , kirundi = Nothing
                    }

                FactorSuccessivePrematureDeliveries ->
                    { english = "Patient experienced successive preterm deliveries"
                    , kinyarwanda = Just "Umubyeyi yabyaye inda zidashyitse zikurikiranye"
                    , kirundi = Nothing
                    }

                FactorStillbornPreviousDelivery ->
                    { english = "Stillbirth in previous delivery"
                    , kinyarwanda = Just "Ubushize yabyaye umwana upfuye(wapfiriye mu nda)"
                    , kirundi = Nothing
                    }

                FactorBabyDiedOnDayOfBirthPreviousDelivery ->
                    { english = "Live Birth but the baby died the same day in previous delivery"
                    , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
                    , kirundi = Nothing
                    }

                FactorPartialPlacentaPreviousDelivery ->
                    { english = "Patient had partial placenta in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize iya nyuma ntiyavutse yose/yaje igice"
                    , kirundi = Nothing
                    }

                FactorSevereHemorrhagingPreviousDelivery ->
                    { english = "Patient experienced severe hemorrhage in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yaravuye cyane/bikabije ku nda y'ubushize"
                    , kirundi = Nothing
                    }

                FactorPreeclampsiaPreviousPregnancy ->
                    { english = "Patient had preeclampsia in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso bibanziriza kugagara ku nda y'ubushize"
                    , kirundi = Nothing
                    }

                FactorConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Just "Ubushize mubyeyi yagize ibimenyetso byo kugagara/Guhinda umushyitsi abyara"
                    , kirundi = Nothing
                    }

                FactorConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions and resulted in becoming unconscious after delivery"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso byo kugagara nyuma yo kubyara bimuviramo kutumva/guta ubwenge"
                    , kirundi = Nothing
                    }

                FactorIncompleteCervixPreviousPregnancy ->
                    { english = "Patient had an Incomplete Cervix in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize inkondo y'umura ntiyashoboye kwifunga neza"
                    , kirundi = Nothing
                    }

                FactorVerticalCSectionScar ->
                    { english = "Vertical C-Section Scar"
                    , kinyarwanda = Just "Inkovu yo kubagwa irahagaze"
                    , kirundi = Nothing
                    }

                FactorGestationalDiabetesPreviousPregnancy ->
                    { english = "Patient had Gestational Diabetes in previous pregnancy"
                    , kinyarwanda = Just "Ubushize umubyeyi yagize indwara ya Diyabete itewe no gutwita"
                    , kirundi = Nothing
                    }

        RiskFactors ->
            { english = "Risk Factors"
            , kinyarwanda = Just "Abashobora kwibasirwa n'indwara runaka (kubera impamvu zitandukanye:kuba atwite..)"
            , kirundi = Nothing
            }

        SachetsPerDayHelper weight recommendation ->
            { english = "The recommended amount for a " ++ String.fromFloat weight ++ " kg child is " ++ String.fromFloat recommendation ++ " sachets a day"
            , kinyarwanda = Just <| "Amasashe yemewe ku mwana w'ibiro " ++ String.fromFloat weight ++ " ni " ++ String.fromFloat recommendation ++ " ku munsi"
            , kirundi = Nothing
            }

        SachetsPerDayQuestion ->
            { english = "How many sachets of supplement is given to the child per day"
            , kinyarwanda = Just "Ni amasashe angahe ahabwa umwana ku munsi"
            , kirundi = Nothing
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            , kirundi = Nothing
            }

        SaveAndNext ->
            { english = "Save & Next"
            , kinyarwanda = Just "Bika & ukomeze"
            , kirundi = Nothing
            }

        SaveAndRecordOutcome ->
            { english = "Save & Record Outcome"
            , kinyarwanda = Just "Bika & Andika iherezo ry'uburwayi"
            , kirundi = Nothing
            }

        SavedMoneyQuestion ->
            { english = "Have you saved money for use at the health center while you give birth"
            , kinyarwanda = Just "Wazigamye amafaranga yo gukoresha ku kigo nderabuzima igihe cyo kubyara"
            , kirundi = Nothing
            }

        SaveError ->
            { english = "Save Error"
            , kinyarwanda = Just "Kubika error (ikosa mu kubika)"
            , kirundi = Nothing
            }

        ScheduleFollowUp ->
            { english = "Schedule Follow Up"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Search ->
            { english = "Search"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SearchByName ->
            { english = "Search by Name"
            , kinyarwanda = Just "Gushakisha izina"
            , kirundi = Nothing
            }

        SearchEhezaForExistingParticipants ->
            { english = "Search E-Heza to see if the contact already exists"
            , kinyarwanda = Just "Reba muri E-heza niba abo bahuye basanzwe barimo"
            , kirundi = Nothing
            }

        SearchExistingParticipants ->
            { english = "Search Existing Participants"
            , kinyarwanda = Just "Gushaka abagenerwabikorwa basanzwe muri sisiteme"
            , kirundi = Nothing
            }

        SearchHelper ->
            { english = "Search to see if the participant already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Shakisha kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba atagaragara, mwandike nku mushya."
            , kirundi = Nothing
            }

        SearchHelperFamilyMember ->
            { english = "Search to see if the additional family member already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Kanda ku Ishakiro kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba uwo muntu atagaragara mu ishakiro, mwandike nk'umugenerwabikorwa mushya."
            , kirundi = Nothing
            }

        SecondName ->
            { english = "Second Name"
            , kinyarwanda = Just "Izina ry'umuryango"
            , kirundi = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Just "Umurenge"
            , kirundi = Nothing
            }

        SeeDosageScheduleByWeight ->
            { english = "See dosage schedule by Weight"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SeeLabResults ->
            { english = "See Lab Results"
            , kinyarwanda = Just "Reba Ibisubizo by'Ibizamini Byafashwe"
            , kirundi = Nothing
            }

        SeeMore ->
            { english = "See More"
            , kinyarwanda = Just "Reba Ibindi"
            , kirundi = Nothing
            }

        SelectAntenatalVisit ->
            { english = "Select an Antenatal Visit"
            , kinyarwanda = Just "Hitamo inshuro aje kwipimishaho inda"
            , kirundi = Nothing
            }

        SelectAllDiagnoses ->
            { english = "Select all diagnoses"
            , kinyarwanda = Just "Hitamo uburwayi bwose bwagaragaye"
            , kirundi = Nothing
            }

        SelectAllSigns ->
            { english = "Select all signs that are present"
            , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite"
            , kirundi = Nothing
            }

        SelectDangerSigns ->
            { english = "Please select one or more of the danger signs the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            , kirundi = Nothing
            }

        SelectDate ->
            { english = "Select Date"
            , kinyarwanda = Just "Hitamo Itariki"
            , kirundi = Nothing
            }

        SelectedFamilyPlanningMethod ->
            { english = "Selected Family Planning Method"
            , kinyarwanda = Just "Uburyo bwo kuboneza urubyaro bwatoranijwe"
            , kirundi = Nothing
            }

        SelectIllnessSymptoms ->
            { english = "Please select one or more symptoms the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cyangwa byinshi mu bimenyetso umurwayi afite"
            , kirundi = Nothing
            }

        SelectPostpartumChildDangerSigns ->
            { english = "Please select one or more of the danger signs the child is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umwana  yaba afite?"
            , kirundi = Nothing
            }

        SelectPostpartumMotherDangerSigns ->
            { english = "Please select one or more of the danger signs the mother is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            , kirundi = Nothing
            }

        SelectedProgram ->
            { english = "Selected Program"
            , kinyarwanda = Just "Porogaramu Yatoranyijwe"
            , kirundi = Nothing
            }

        SelectedVillage ->
            { english = "Selected Village"
            , kinyarwanda = Just "Umudugudu Watoranyijwe"
            , kirundi = Nothing
            }

        SelectEncounterType ->
            { english = "Select an encounter type"
            , kinyarwanda = Just "Hitamo ubwoko bw'icyiciro cyo gukorera"
            , kirundi = Nothing
            }

        SelectLanguage ->
            { english = "Select language"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectExistingAcuteIllness ->
            { english = "Select Existing Acute Illness"
            , kinyarwanda = Just "Hitamo Indwara ifatiyeho iheruka kuvurwa"
            , kirundi = Nothing
            }

        SelectExistingAcuteIllnessToRecordOutcome ->
            { english = "Select Existing Acute Illness to Record Outcome"
            , kinyarwanda = Just "Hitamo indwara ifatiyeho iheruka kuvurwa kugira ngo wandike iherezo ryayo"
            , kirundi = Nothing
            }

        SelectGroup ->
            { english = "Select Group..."
            , kinyarwanda = Just "Hitamo itsinda ryawe..."
            , kirundi = Nothing
            }

        SelectProgram ->
            { english = "Select Program"
            , kinyarwanda = Just "Hitamo porogaramu"
            , kirundi = Nothing
            }

        SelectYourGroup ->
            { english = "Select your Group"
            , kinyarwanda = Just "Hitamo itsinda ryawe"
            , kirundi = Nothing
            }

        SelectYourHealthCenter ->
            { english = "Select your Health Center"
            , kinyarwanda = Just "Hitamo ikigo nderabuzima"
            , kirundi = Nothing
            }

        SelectYourVillage ->
            { english = "Select your village"
            , kinyarwanda = Just "Hitamo umudugudu wawe"
            , kirundi = Nothing
            }

        SelectedHCDownloading ->
            { english = "Downloading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectedHCNotSynced ->
            { english = "Data is not synced"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectedHCSyncing ->
            { english = "Data is syncing"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Send ->
            { english = "Send"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsApp ->
            { english = "Send via WhatsApp"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppComponentsSelectionHeader reportType ->
            case reportType of
                Components.SendViaWhatsAppDialog.Model.ReportWellChild ->
                    { english = "Please select which sections of the Standard Pediatric Visit Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Components.SendViaWhatsAppDialog.Model.ReportAntenatal ->
                    { english = "Please select which sections of the Antenatal Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                -- Not in use, because AcuteIllness does not allow
                -- components selection.
                Components.SendViaWhatsAppDialog.Model.ReportAcuteIllness ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Components.SendViaWhatsAppDialog.Model.ReportNCD ->
                    { english = "Please select which sections of the NCD Report you would like to send:"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        SendViaWhatsAppConfirmationBeforeExecutingHeader ->
            { english = "By pressing send you are releasing the selected documents to:"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppConfirmationBeforeExecutingInstructions ->
            { english = "This action will take up to one minute to complete."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppConfirmationBeforeExecutingQuestion ->
            { english = "Would you like to send?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppConsentQuestion ->
            { english = "Does the patient consent to having their medical records sent via WhatsApp?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppExecutionResultFailure ->
            { english = "Action Failed. Please try again. If problem persists, please contact system administrator."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppExecutionResultSomethingWentWrong ->
            { english = "Something went wrong. Please contact system administrator."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppExecutionResultSuccess ->
            { english = "Success. Report will be sent when device has internet conneciton."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppNoticeOfNonRespobsibility ->
            { english = "Please note that the medical professional and E-Heza will not be liable for what happens to these medical reports once released."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneInputHeader ->
            { english = "Enter the correct phone number for the patient:"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneVerificationHeader ->
            { english = "The phone number we have on file for this patient is:"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneVerificationQuestion ->
            { english = "Is this the correct number for the patient's WhatsApp?"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneUpdateAtProfileQuestionPrefix ->
            { english = "Would you like to update the patient profile for"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneUpdateAtProfileQuestionSuffix ->
            { english = "with the number"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SendViaWhatsAppPhoneUpdateConfirmationMessasge ->
            { english = "The patient record has been updated."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerActive ->
            { english = "The app is installed on this device."
            , kinyarwanda = Just "Apulikasiyo muri icyi cyuma cy'inkoranabuhanga yinjijwe."
            , kirundi = Nothing
            }

        ServiceWorkerCurrent ->
            { english = "You have the current version of the app."
            , kinyarwanda = Just "Ufite apulikasiyo nshya igezweho uyu munsi"
            , kirundi = Nothing
            }

        ServiceWorkerCheckForUpdates ->
            { english = "Check for updates"
            , kinyarwanda = Just "Kugenzura ibyavuguruwe"
            , kirundi = Nothing
            }

        ServiceWorkerInstalling ->
            { english = "A new version of the app has been detected and is being downloaded. You can continue to work while this is in progress."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerInstalled ->
            { english = "A new version of the app has been downloaded."
            , kinyarwanda = Just "Gufungura verisio nshyashya byarangiye."
            , kirundi = Nothing
            }

        ServiceWorkerSkipWaiting ->
            { english = "Activate new version of the app"
            , kinyarwanda = Just "Gufungura verisio nshyashya"
            , kirundi = Nothing
            }

        ServiceWorkerRestarting ->
            { english = "The app should reload momentarily with the new version."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerActivating ->
            { english = "A new version of the app is preparing itself for use."
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        ServiceWorkerRegNotAsked ->
            { english = "We have not yet attempted to install the app on this device."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerRegLoading ->
            { english = "Installation of the app on this device is progressing."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerRegErr ->
            { english = "There was an error installing the app on this device. To try again, reload this page."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ServiceWorkerRegSuccess ->
            { english = "The app was successfully registered with this device."
            , kinyarwanda = Just "Igikorwa cyo gushyira apulikasiyo kuri iki gikoresho cy'ikoranabuhanga cyagenze neza."
            , kirundi = Nothing
            }

        ServiceWorkerStatus ->
            { english = "Deployment Status"
            , kinyarwanda = Just "Ibijyanye no kuvugurura no kongerera ubushobozi sisiteme"
            , kirundi = Nothing
            }

        SevereAcuteMalnutrition ->
            { english = "Severe acute malnutrition"
            , kinyarwanda = Just "Imirire mibi ikabije imaze igihe gito"
            , kirundi = Nothing
            }

        SevereHemorrhagingPreviousDelivery ->
            { english = "Severe Hemorrhaging in previous delivery (>500 ml)"
            , kinyarwanda = Just "Ubushize yavuye cyane akimara kubyara hejuru ya Ml 500"
            , kirundi = Nothing
            }

        Shared ->
            { english = "Shared"
            , kinyarwanda = Just "Ayisangira n'abandi"
            , kirundi = Nothing
            }

        Signature ->
            { english = "Signature"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SignOnDoorPostedQuestion ->
            { english = "Have you posted signs on the door indicating that the space is an isolation area"
            , kinyarwanda = Just "Waba washyize ibimenyetso ku rugi byerekana ko iki cyumba ari ikijyamo abantu bari mu kato"
            , kirundi = Nothing
            }

        SpecialityCareHeaderPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe uburwayi bwa"
            , kirundi = Nothing
            }

        SpecialityCareHeaderSuffix ->
            { english = "during your pregnancy"
            , kinyarwanda = Just "Mu gihe wari utwite"
            , kirundi = Nothing
            }

        SpecialityCareSignQuestion sign ->
            case sign of
                EnrolledToARVProgram ->
                    { english = "Are you currently enrolled in ARV services at the health center"
                    , kinyarwanda = Just "Waba wanditswe muri serivise itanaga imiti igabanya ubukana bwa Vurusi itera SIDA ku kigo nderabuzima"
                    , kirundi = Nothing
                    }

                EnrolledToNCDProgram ->
                    { english = "Are you currently enrolled in NCD services at the health center"
                    , kinyarwanda = Just "Waba usanzwe wanditse muri serivisi y'indwara zitandura ku kigo nderabusima"
                    , kirundi = Nothing
                    }

                NoSpecialityCareSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StillbornPreviousDelivery ->
            { english = "Stillborn in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana upfuye"
            , kirundi = Nothing
            }

        StockCorrectionReason value ->
            case value of
                ReasonInputError ->
                    { english = "Error in input"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonExpiration ->
                    { english = "Expired stock"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonMissing ->
                    { english = "Missing stock / unaccounted for"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReasonOther ->
                    { english = "Other"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StockManagement ->
            { english = "Stock Management"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementMenu value ->
            case value of
                MenuReceiveStock ->
                    { english = "Receive Stock"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MenuViewMonthDetails ->
                    { english = "View current month details"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MenuCorrectEntry ->
                    { english = "Correct entry"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StockManagementBatchNumberQuestion ->
            { english = "What is the batch number"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementCorrectionTypeLabel ->
            { english = "Please select the type of the correct"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementCorrectionEntryType value ->
            case value of
                EntryAddition ->
                    { english = "Addition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                EntrySubstraction ->
                    { english = "Substraction"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        StockManagementCorrectionReasonLabel ->
            { english = "Please select the reason for the correct"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementDateExpiresQuestion ->
            { english = "What is the expiration date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementEnterSignatureLabel ->
            { english = "Please enter your signature"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementQuantityAddedQuestion ->
            { english = "How much stock is being received"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementQuantityCorrectionLabel ->
            { english = "Please enter the quantity"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementSelectDateLabel ->
            { english = "Select a date for this entry"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StockManagementSupplierQuestion ->
            { english = "Where was this received from"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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

        SubsequentEncounter ->
            { english = "Subsequent Encounter"
            , kinyarwanda = Just "Igikorwa gikurikiyeho"
            , kirundi = Nothing
            }

        SubsequentAntenatalVisit ->
            { english = "Subsequent Antenatal Visit"
            , kinyarwanda = Just "Igihe cyo kongera kwipimisha inda"
            , kirundi = Nothing
            }

        SubsequentEncounterReferral encounterType ->
            if encounterType == AcuteIllnessEncounterCHW then
                { english = "CHW Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku mujyanama w'ubuzima"
                , kirundi = Nothing
                }

            else
                { english = "Health Center Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku kigo nderabuzima"
                , kirundi = Nothing
                }

        SuccessiveAbortions ->
            { english = "Successive Abortions"
            , kinyarwanda = Just "Inda zavuyemo zikurikiranye"
            , kirundi = Nothing
            }

        SuccessivePrematureDeliveries ->
            { english = "Successive Premature Deliveries"
            , kinyarwanda = Just "Inda zavutse zidashyitse zikurikiranye"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseAlert ->
            { english = "Suspected COVID-19 case"
            , kinyarwanda = Just "Acyekwaho kwandura COVID-19"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseAlertHelper ->
            { english = "Please isolate immediately from family and contact health center"
            , kinyarwanda = Just "Mutandukanye n'umuryango we byihuse uhite umenyesha Ikigo nderabuzima"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseIsolate ->
            { english = "Isolate immediately from family"
            , kinyarwanda = Just "Mutandukanye ako kanya n'umuryango we umushyire mu kato"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseContactHC ->
            { english = "Contact health center immediately"
            , kinyarwanda = Just "Menyesha ikigo nderabuzima ako kanya"
            , kirundi = Nothing
            }

        SuspectedCovid19CasePerformRapidTest ->
            { english = "Perform a rapid test immediately"
            , kinyarwanda = Just "Kora ikizamini nonaha"
            , kirundi = Nothing
            }

        SuspectedCovid19CaseReferToHCForTesting ->
            { english = "Refer to Health Center for testing"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SymptomRelief type_ ->
            case type_ of
                SymptomReliefParacetamol ->
                    { english = "Paracetamol for Fever"
                    , kinyarwanda = Just "Umuti wa Paracetamoro ugabanya umuriro"
                    , kirundi = Nothing
                    }

                SymptomReliefVitaminC ->
                    { english = "Effervescent Vitamin C tablets"
                    , kinyarwanda = Just "Ibinini bya Vitamin C"
                    , kirundi = Nothing
                    }

                SymptomReliefPaidoterineSyrup ->
                    { english = "Paidoterin syrup as a decongestant"
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero"
                    , kirundi = Nothing
                    }

                SymptomReliefCoughMixture ->
                    { english = "Cough mixtures such as Ascoril, Bronchalene, etc."
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero, n'indi"
                    , kirundi = Nothing
                    }

        Symptoms ->
            { english = "Symptoms"
            , kinyarwanda = Just "Ibimenyetso"
            , kirundi = Nothing
            }

        SymptomsAtFirstEncounter ->
            { english = "Symptoms at first encounter"
            , kinyarwanda = Just "Ibimenyetso ku isuzuma rya mbere"
            , kirundi = Nothing
            }

        SymptomsGeneralSign sign ->
            case sign of
                BodyAches ->
                    { english = "Body Aches"
                    , kinyarwanda = Just "Ububabare bw'umubiri wose"
                    , kirundi = Nothing
                    }

                Chills ->
                    { english = "Chills"
                    , kinyarwanda = Just "Gutengurwa"
                    , kirundi = Nothing
                    }

                SymptomGeneralFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Nothing
                    }

                Headache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kubabara umutwe"
                    , kirundi = Nothing
                    }

                NightSweats ->
                    { english = "Night Sweats"
                    , kinyarwanda = Just "Kubira ibyuya nijoro"
                    , kirundi = Nothing
                    }

                Lethargy ->
                    { english = "Lethargy"
                    , kinyarwanda = Just "Guhwera"
                    , kirundi = Nothing
                    }

                PoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    , kirundi = Nothing
                    }

                UnableToDrink ->
                    { english = "Unable to Drink"
                    , kinyarwanda = Just "Ntashobora kunywa"
                    , kirundi = Nothing
                    }

                UnableToEat ->
                    { english = "Unable to Eat"
                    , kinyarwanda = Just "Ntashobora kurya"
                    , kirundi = Nothing
                    }

                IncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Just "Afite inyota cyane"
                    , kirundi = Nothing
                    }

                DryMouth ->
                    { english = "Dry/Sticky Mouth"
                    , kinyarwanda = Just "Iminwa yumye"
                    , kirundi = Nothing
                    }

                SevereWeakness ->
                    { english = "Severe Weakness"
                    , kinyarwanda = Just "Yacitse intege cyane"
                    , kirundi = Nothing
                    }

                YellowEyes ->
                    { english = "Yellow Eyes"
                    , kinyarwanda = Just "Amaso y'umuhondo"
                    , kirundi = Nothing
                    }

                CokeColoredUrine ->
                    { english = "Coca-Cola Colored Urine"
                    , kinyarwanda = Just "Inkari zisa na kokakola"
                    , kirundi = Nothing
                    }

                SymptomsGeneralConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Nothing
                    }

                SpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    , kirundi = Nothing
                    }

                NoSymptomsGeneral ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        SymptomsGISign sign ->
            case sign of
                SymptomGIAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Nothing
                    }

                BloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    , kirundi = Nothing
                    }

                Nausea ->
                    { english = "Nausea"
                    , kinyarwanda = Just "Afite iseseme"
                    , kirundi = Nothing
                    }

                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea - >3 liquid stools in the last 24 hours"
                    , kinyarwanda = Just "Nta maraso yituma- yituma ibyoroshye inshuro zirenze 3 mu masaha 24"
                    , kirundi = Nothing
                    }

                Vomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    , kirundi = Nothing
                    }

                NoSymptomsGI ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        SymptomsGISignAbbrev sign ->
            case sign of
                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea"
                    , kinyarwanda = Just "Nta maraso yituma"
                    , kirundi = Nothing
                    }

                _ ->
                    translationSet (SymptomsGISign sign)

        SymptomsRespiratorySign sign ->
            case sign of
                BloodInSputum ->
                    { english = "Blood in Sputum"
                    , kinyarwanda = Just "Amaraso mu gikororwa"
                    , kirundi = Nothing
                    }

                Cough ->
                    { english = "Cough"
                    , kinyarwanda = Just "Inkorora"
                    , kirundi = Nothing
                    }

                NasalCongestion ->
                    { english = "Nasal Congestion"
                    , kinyarwanda = Just "Gufungana mu mazuru"
                    , kirundi = Nothing
                    }

                ShortnessOfBreath ->
                    { english = "Shortness of Breath"
                    , kinyarwanda = Just "Guhumeka nabi"
                    , kirundi = Nothing
                    }

                SoreThroat ->
                    { english = "Sore Throat"
                    , kinyarwanda = Just "Kubabara mu muhogo"
                    , kirundi = Nothing
                    }

                LossOfSmell ->
                    { english = "Loss of Smell"
                    , kinyarwanda = Just "Kudahumurirwa"
                    , kirundi = Nothing
                    }

                StabbingChestPain ->
                    { english = "Stabbing Chest Pain"
                    , kinyarwanda = Just "Kubabara mu gatuza"
                    , kirundi = Nothing
                    }

                NoSymptomsRespiratory ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    , kirundi = Nothing
                    }

        SymptomsTask task ->
            case task of
                SymptomsGeneral ->
                    { english = "General"
                    , kinyarwanda = Just "Ibimenyesto rusange"
                    , kirundi = Nothing
                    }

                SymptomsRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Just "Ubuhumekero"
                    , kirundi = Nothing
                    }

                SymptomsGI ->
                    { english = "GI"
                    , kinyarwanda = Just "Urwungano ngogozi"
                    , kirundi = Nothing
                    }

        SyphilisRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Syphilis"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bwa Mburugu"
            , kirundi = Nothing
            }

        SyphilisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Nothing
            }

        SyphilisRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            , kirundi = Nothing
            }

        SyphilisRecommendedTreatmentWarning ->
            { english = "If Erythromycin or Azithromycin used, must treat newborn immediately after delivery (does not cross into placenta)"
            , kinyarwanda = Just "Niba ari Erythromicine cg Azithromycine wakoresheje, ugomba kuvura uruhinja rukivuka (Uyu muti ntiwinjira mu ngobyi y'umwana)"
            , kirundi = Nothing
            }

        GroupEncounterClosed ->
            { english = "Group Encounter closed"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                FacilityHospital ->
                    { english = "Send patient to the hospital"
                    , kinyarwanda = Just "Ohereza umurwayi kwa muganga"
                    , kirundi = Nothing
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Refer patient to mental health specialist for further evaluation"
                    , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
                    , kirundi = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Direct patient to the appropriate location"
                    , kinyarwanda = Just "Yobora umurwayi ahantu habugenewe"
                    , kirundi = Nothing
                    }

                FacilityNCDProgram ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

                FacilityANCServices ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

                FacilityUltrasound ->
                    { english = "Send patient to ultrasound"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ShowAll ->
            { english = "Show All"
            , kinyarwanda = Just "Erekana amazina yose"
            , kirundi = Nothing
            }

        StartEncounter ->
            { english = "Start an encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            , kirundi = Nothing
            }

        StartEndDate ->
            { english = "Start - End"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StrartNewAcuteIllnessHelper ->
            { english = "If existing Acute Illness is not part of the list above, start a new encounter"
            , kinyarwanda = Just "Niba Indwara ifatiyeho iheruka kuvurwa itagaragara ku rutonde rwavuzwe haruguru , tangira isuzuma rishya"
            , kirundi = Nothing
            }

        StartDate ->
            { english = "Start Date"
            , kinyarwanda = Just "Itariki utangireyeho"
            , kirundi = Nothing
            }

        EndDate ->
            { english = "End Date"
            , kinyarwanda = Just "Itariki urangirijeho"
            , kirundi = Nothing
            }

        StartingStock ->
            { english = "Starting Stock"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StartSyncing ->
            { english = "Start Syncing"
            , kinyarwanda = Just "Tangira uhuze amakuru kuri seriveri"
            , kirundi = Nothing
            }

        StatusLabel ->
            { english = "Status"
            , kinyarwanda = Just "Uko bihagaze kugeza ubu"
            , kirundi = Nothing
            }

        StopSyncing ->
            { english = "Stop Syncing"
            , kinyarwanda = Just "Tangira gukura amakuru kuri seriveri"
            , kirundi = Nothing
            }

        Submit ->
            { english = "Submit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Success ->
            { english = "Success"
            , kinyarwanda = Just "Byagezweho"
            , kirundi = Nothing
            }

        SyncGeneral ->
            { english = "Sync Status (General)"
            , kinyarwanda = Just "Ibijyanye no guhuza amakuru yafashwe n'igikoresho cy'ikoranabuhanga n'abitse kuri seriveri"
            , kirundi = Nothing
            }

        TabletSinglePlural value ->
            if value == "1" then
                { english = "1 tablet"
                , kinyarwanda = Just "Ikinini cyimwe"
                , kirundi = Nothing
                }

            else
                { english = value ++ " tablets"
                , kinyarwanda = Just <| "ibinini " ++ value
                , kirundi = Nothing
                }

        TakenCareOfBy ->
            { english = "Taken care of by"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TakingMedicationAsPrescribed taking ->
            if taking then
                { english = "Taking medication as prescribed"
                , kinyarwanda = Just "Yafashe imiti uko yayandikiwe"
                , kirundi = Nothing
                }

            else
                { english = "Not taking medication as prescribed because of"
                , kinyarwanda = Just "Ntabwo yafashe imiti uko yayandikiwe kubera ko"
                , kirundi = Nothing
                }

        TasksCompleted completed total ->
            { english = String.fromInt completed ++ "/" ++ String.fromInt total ++ " Tasks Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ "/" ++ String.fromInt total ++ " Ibikorwa byarangiye"
            , kirundi = Nothing
            }

        TargetedInterventions ->
            { english = "Targeted Interventions"
            , kinyarwanda = Just "Ibikorwa bifasha umwana mu buryo bwihariye"
            , kirundi = Nothing
            }

        TelephoneNumber ->
            { english = "Telephone Number"
            , kinyarwanda = Just "Numero ya telefoni"
            , kirundi = Nothing
            }

        Term ->
            { english = "Term"
            , kinyarwanda = Just "Inda igeze igihe"
            , kirundi = Nothing
            }

        TermPregnancy ->
            { english = "Number of Term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima bashyitse"
            , kirundi = Nothing
            }

        ThisActionCannotBeUndone ->
            { english = "This action cannot be undone."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ThisGroupHasNoMothers ->
            { english = "This Group has no mothers assigned to it."
            , kinyarwanda = Just "Iki cyiciro nta mubyeyi cyagenewe."
            , kirundi = Nothing
            }

        Time ->
            { english = "Time"
            , kinyarwanda = Just "igihe"
            , kirundi = Nothing
            }

        To ->
            { english = "to"
            , kinyarwanda = Just "kuri"
            , kirundi = Nothing
            }

        ToThePatient ->
            { english = "to the patient"
            , kinyarwanda = Just "ku murwayi"
            , kirundi = Nothing
            }

        Training ->
            { english = "Training"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TrainingGroupEncounterCreateSuccessMessage ->
            { english = "Training encounters were created."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TrainingGroupEncounterDeleteSuccessMessage ->
            { english = "Training encounters were deleted."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TransportationPlanQuestion ->
            { english = "Have you planned for transportation to and from the health center to give birth"
            , kinyarwanda = Just "Waba warateganije uburyo uzagera ku kigo nderabuzima ugiye kubyara ndetse n'uburyo uzavayo nyuma yo kubyara"
            , kirundi = Nothing
            }

        TraveledToCOVID19CountryQuestion ->
            { english = "Have you traveled to any country or district in Rwanda known to have COVID-19 in the past 14 days"
            , kinyarwanda = Just "Waba waragiye mu gihugu cyangwa mu karere mu Rwanda bizwi ko hagaragayemo ubwandu bwa Covid 19 mu minsi 14 ishize"
            , kirundi = Nothing
            }

        TravelHistory ->
            { english = "Travel History"
            , kinyarwanda = Just "Amukuru ku ngendo"
            , kirundi = Nothing
            }

        TreatedWith ->
            { english = "Treated with"
            , kinyarwanda = Just "Bivurwa na"
            , kirundi = Nothing
            }

        TreatedWithNot ->
            { english = "Not treated with"
            , kinyarwanda = Just "Ntibivurwa na"
            , kirundi = Nothing
            }

        Treatment ->
            { english = "Treatment"
            , kinyarwanda = Just "Ubuvuzi"
            , kirundi = Nothing
            }

        TreatmentDetailsAnemia ->
            { english = "At the previous visit you were given Iron (120mg), one 60mg tablet to be taken 2x a day for 3 months and Folic Acid (400 IU) to be taken daily for 3 months."
            , kinyarwanda = Just "Mu isura riheruka wahawe umuti (Ubutare or Feri) wongera amaraso(120mg), miligarama 60 inshuro ebyiri ku munsi mu mezi atatu na Acide folike(400 UI)inshuro imwe ku munsi mu miezi atatu."
            , kirundi = Nothing
            }

        TreatmentDetailsHIV dolutegravir arvs ->
            if dolutegravir && arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day and Doltegravir (50mg) to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine ikinini kimwe ku munsi na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                , kirundi = Nothing
                }

            else if dolutegravir then
                { english = "At the previous visit you were given Doltegravir (50mg), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura rishize wahawe ikinini cya Dulutogaraviri(50mg), ikinini kimwe ku munsi."
                , kirundi = Nothing
                }

            else if arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa3 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 3x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro eshatu ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Nothing
                    }

                TreatmentMethyldopa4 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Nothing
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day and Amlodipine (5mg), by mouth 1x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi na Amlodipine (5mg), mu kanwa inshuro imwe ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentCoartem ->
                    { english = "At the previous visit you were given Coartem, 4 tablets to be taken by mouth twice per day x 3 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe AL (Kowaritemu), ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TreatmentPenecilin3 ->
                    { english = "At the previous visit you were given Penicillin (2.4 million units), IM 1x a week for 3 weeks."
                    , kinyarwanda = Just "Mu isura rishize wahawe Penisilini (inite Miliyoni 2.4 ), IM inshuro 1, IM inshuro 1 buri cyumweru mu byumweru 3."
                    , kirundi = Nothing
                    }

                TreatmentErythromycin ->
                    { english = "At the previous visit you were given Erythromycin (500mg), by mouth 4x a day for 14 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Erythromicine (500mg), mu kanwa inshuro enye ku munsi mu minsi 14."
                    , kirundi = Nothing
                    }

                TreatmentAzithromycin ->
                    { english = "At the previous visit you were given Azithromycin (2g), 4 tabs by mouth x one day."
                    , kinyarwanda = Just "Mu isura rishize wahawe Azithromycine (2g), Ibinini 4 abinywe mu kanwa umunsi umwe."
                    , kirundi = Nothing
                    }

                TreatmentCeftriaxon ->
                    { english = "At the previous visit you were given Ceftriaxone (1g), IM daily x 10 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Ceftriaxone (1g), IM buri munsi mu minsi 10."
                    , kirundi = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TreatmentReviewQuestionAdverseEvents ->
            { english = "Have you experienced any adverse events"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            , kirundi = Nothing
            }

        TreatmentReviewQuestionAdverseEventsHospitalization ->
            { english = "The patient had an adverse reaction to the medication. Would you like to refer them to the hospital as next step"
            , kinyarwanda = Just "Umurwayi yabonye ibintu bidasanzwe byatewe n'imiti yahawe. Waba ushaka kumwhoreza ku bitaro nk'igikorwa gikurikiyeho"
            , kirundi = Nothing
            }

        TreatmentReviewQuestionMedicationByPMTCT ->
            { english = "Did you receive medicine from PMTCT"
            , kinyarwanda = Just "Wahawe imiti muri PMTCT"
            , kirundi = Nothing
            }

        TreatmentReviewQuestionMissedDoses ->
            { english = "Have you missed any doses"
            , kinyarwanda = Just "Haba hari imiti wasimbutse gufata"
            , kirundi = Nothing
            }

        TreatmentReviewQuestionStillTaking ->
            { english = "Are you still taking this medication"
            , kinyarwanda = Just "Uracyari gufata imiti"
            , kirundi = Nothing
            }

        TreatmentReviewQuestionStillTakingForHIV ->
            { english = "Are you still taking ARVs"
            , kinyarwanda = Just "Uracyari gufata imiti igabanya ubukana bwa virusi itera SIDA"
            , kirundi = Nothing
            }

        TreatmentReviewTask forModeratePreeclamsia task ->
            case task of
                TreatmentReviewPrenatalMedication ->
                    { english = "Prenatal Medication"
                    , kinyarwanda = Just "Imiti yo gufata mu gihe utwite"
                    , kirundi = Nothing
                    }

                TreatmentReviewHIV ->
                    { english = "HIV Medication"
                    , kinyarwanda = Just "Imiti ya Virusi Itera SIDA"
                    , kirundi = Nothing
                    }

                TreatmentReviewHypertension ->
                    if forModeratePreeclamsia then
                        { english = "Moderate Preeclamsia Medication"
                        , kinyarwanda = Just "Imiti Preklampusi Yoroheje"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Hypertension Medication"
                        , kinyarwanda = Just "Imiti y'Umuvuduko w'Amaraso"
                        , kirundi = Nothing
                        }

                TreatmentReviewMalaria ->
                    { english = "Malaria Medication"
                    , kinyarwanda = Just "Imiti ya Malariya"
                    , kirundi = Nothing
                    }

                TreatmentReviewAnemia ->
                    { english = "Anemia Medication"
                    , kinyarwanda = Just "Imiti ivura indwara y'Amaraso make"
                    , kirundi = Nothing
                    }

                TreatmentReviewSyphilis ->
                    { english = "Syphilis Medication"
                    , kinyarwanda = Just "Imiti ya Mburugu"
                    , kirundi = Nothing
                    }

        TreatmentReviewWarningPopupMessage ->
            { english = "Patient non-adherent"
            , kinyarwanda = Just "Uyu murwayi ntabwo yubahiriza gahunda yo kunywa imiti uko bisabwa"
            , kirundi = Nothing
            }

        TreatmentReviewWarningPopupInstructions ->
            { english = "Further evaluation necessary"
            , kinyarwanda = Just "Gusuzuma byimbitse"
            , kirundi = Nothing
            }

        TrySyncing ->
            { english = "Try syncing with backend"
            , kinyarwanda = Just "Gerageza guhuza amakuru y'iki gikoresho cy'ikoranabuhanga n'abakoze E-Heza"
            , kirundi = Nothing
            }

        TuberculosisPast ->
            { english = "Tuberculosis in the past"
            , kinyarwanda = Just "Yigeze kurwara igituntu"
            , kirundi = Nothing
            }

        TuberculosisPresent ->
            { english = "Tuberculosis in the present"
            , kinyarwanda = Just "Arwaye igituntu"
            , kirundi = Nothing
            }

        TuberculosisInstructions ->
            { english = "Follow TB protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kuvura igitintu"
            , kirundi = Nothing
            }

        TuberculosisInstructionsFollowed ->
            { english = "followed TB protocols"
            , kinyarwanda = Just "Hakurikijwe amabwiriza yo kuvura igitintu"
            , kirundi = Nothing
            }

        TuberculosisWarning ->
            { english = "Patient is high risk for active Tuberculosis"
            , kinyarwanda = Just "Umubyeyi afite ibyago byinshi byo kuba afite igituntu"
            , kirundi = Nothing
            }

        TwoVisits ->
            { english = "Two visits"
            , kinyarwanda = Just "Inshuro ebyiri"
            , kirundi = Nothing
            }

        Type ->
            { english = "Type"
            , kinyarwanda = Just "Ubwoko bw'Urukingo"
            , kirundi = Nothing
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

        UndeterminedDiagnoses ->
            { english = "Undetermined Diagnoses"
            , kinyarwanda = Just "Uburwayi ntibusobanutse"
            , kirundi = Nothing
            }

        UndeterminedDiagnosisMessage ->
            { english = "undetermined diagnosis - followed Post-Partum Protocols"
            , kinyarwanda = Just "Uburwayi ntibusobanutse - hakurikijwe mabwiriza yo kwita ku mubyeyi wabyaye"
            , kirundi = Nothing
            }

        UnitCopiesPerMM3 ->
            { english = "copies/mm3"
            , kinyarwanda = Just "Kopi/mm3"
            , kirundi = Nothing
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

        UnitMilliGramsPerDeciliter ->
            { english = "mg/dL"
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
            , kirundi = Nothing
            }

        Unknown ->
            { english = "Unknown"
            , kinyarwanda = Just "Ntabizi"
            , kirundi = Nothing
            }

        Update ->
            { english = "Update"
            , kinyarwanda = Just "Kuvugurura"
            , kirundi = Nothing
            }

        UpdateError ->
            { english = "Update Error"
            , kinyarwanda = Just "ikosa mwivugurura"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                VariantLongTest ->
                    { english = "Urine Dipstick Long"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    , kirundi = Nothing
                    }

        UrineDipstickTestVariant variant ->
            case variant of
                VariantShortTest ->
                    { english = "Short Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo bike"
                    , kirundi = Nothing
                    }

                VariantLongTest ->
                    { english = "Long Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    , kirundi = Nothing
                    }

        UrinaryTractInfectionRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Urinary Tract Infection"
            , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
            , kirundi = Nothing
            }

        UrinaryTractInfectionRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            , kirundi = Nothing
            }

        UrinaryTractInfectionRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            , kirundi = Nothing
            }

        UterineMyoma ->
            { english = "Uterine Myoma"
            , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
            , kirundi = Nothing
            }

        VaccinationCatchUpRequiredQuestion ->
            { english = "Are there previous immunizations that are not in E-Heza that need to be recorded"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        VaccinationStatus status ->
            case status of
                StatusBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Ntibyakozwe"
                    , kirundi = Nothing
                    }

                StatusCompleted ->
                    { english = "Completed"
                    , kinyarwanda = Just "Byarakozwe"
                    , kirundi = Nothing
                    }

                StatusUpToDate ->
                    { english = "Up To Date"
                    , kinyarwanda = Just "Biri ku gihe"
                    , kirundi = Nothing
                    }

        VaccinationNoDosesAdministered ->
            { english = "There are no recorded immunizations for this patient"
            , kinyarwanda = Just "Nta makuru ku nkigo agaragara"
            , kirundi = Nothing
            }

        VaccineDoseAdministeredPreviouslyPrenatalQuestion vaccineType ->
            { english = "Did the patient receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Uyu mubyeyi yaba yarabonye urukingo rw'" ++ vaccineType ++ "bakaba batarabyanditse"
            , kirundi = Nothing
            }

        VaccineDoseAdministeredPreviouslyWellChildQuestion vaccineType ->
            { english = "Did the child receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Umwana yaba yarabonye " ++ vaccineType ++ " bakaba batarabyanditse"
            , kirundi = Nothing
            }

        VaccineDoseAdministeredTodayPrenatalQuestion vaccineType ->
            { english = "Will the patient receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umubyeyi arahabwa urukingo rw'" ++ vaccineType ++ " uyu munsi"
            , kirundi = Nothing
            }

        VaccineDoseAdministeredTodayWellChildQuestion vaccineType ->
            { english = "Will the child receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umwana arahabwa " ++ vaccineType ++ " uyu munsi"
            , kirundi = Nothing
            }

        VaccineType vaccineType ->
            case vaccineType of
                WellChildVaccine wellChildVaccineType ->
                    case wellChildVaccineType of
                        VaccineBCG ->
                            { english = "BCG Bacilius Calmette - Guérin Vaccine (BCG)"
                            , kinyarwanda = Just "Urukingo rw'igituntu"
                            , kirundi = Nothing
                            }

                        VaccineOPV ->
                            { english = "Oral Polio Vaccine (OPV)"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                            , kirundi = Nothing
                            }

                        VaccineDTP ->
                            { english = "DTP - HepB - Hib Vaccine"
                            , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                            , kirundi = Nothing
                            }

                        VaccinePCV13 ->
                            { english = "Pneumoccocal Vaccine (PCV 13)"
                            , kinyarwanda = Just "Urukingo rw'umusonga"
                            , kirundi = Nothing
                            }

                        VaccineRotarix ->
                            { english = "Rotavirus (Rotarix) Vaccine"
                            , kinyarwanda = Just "Urukingo rw'impiswi"
                            , kirundi = Nothing
                            }

                        VaccineIPV ->
                            { english = "Inactivated Polio Vaccine"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                            , kirundi = Nothing
                            }

                        VaccineMR ->
                            { english = "Measles-Rubella Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                            , kirundi = Nothing
                            }

                        VaccineHPV ->
                            { english = "HPV Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Inkondo y'umura"
                            , kirundi = Nothing
                            }

                PrenatalVaccine prenatalVaccineType ->
                    case prenatalVaccineType of
                        VaccineTetanus ->
                            { english = "Tetanus"
                            , kinyarwanda = Just "Agakwega"
                            , kirundi = Nothing
                            }

        VaginalExamination ->
            { english = "Vaginal Examination"
            , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina"
            , kirundi = Nothing
            }

        VaginalExamSign sign ->
            case sign of
                FoulSmellingLochia ->
                    { english = "Foul Smelling Lochia"
                    , kinyarwanda = Just "Ibisanza binuka"
                    , kirundi = Nothing
                    }

                ExcessiveVaginalBleeding ->
                    { english = "Bleeding"
                    , kinyarwanda = Just "Kuva"
                    , kirundi = Nothing
                    }

                NormalVaginalExam ->
                    { english = "Normal"
                    , kinyarwanda = Just "Bisanzwe"
                    , kirundi = Nothing
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
            , kirundi = Nothing
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Just "Umudugudu"
            , kirundi = Nothing
            }

        Visits ->
            { english = "visits"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        VitaminAWarningPopupMessage ->
            { english = "Patient did not recieve Vitamin A"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WaitForVitalsRecheckHelper ->
            { english = "Patient needs to return in 2 hours to confirm blood pressure. Instruct the patient to wait until called for further testing."
            , kinyarwanda = Just "Umurwayi agomba kugaruka mu masaha 2 kugira ngo twemeze neza umuvuduko w'amaraso. Saba umurwayi kwihangana kugeza umuhamagaye kugira ngo yongere asuzumwe."
            , kirundi = Nothing
            }

        WaitForLabsResultsHelper ->
            { english = "Patient has labs pending. Instruct the patient to wait until called for lab results and further diagnoses."
            , kinyarwanda = Just "Umurwayi afite ibisubizo ategereje bya Laboratwari. Musabe gutegereza kugeza umuhamagaye ngo afate ibisubizo anamenye indwara afite."
            , kirundi = Nothing
            }

        WaitInstructions ->
            { english = "To proceed with more encounters while you wait for test results or a vitals recheck, touch \"Pause Encounter\" below to leave this encounter. You can return to it from the case management screen."
            , kinyarwanda = Just "Kugira ngo ukomeze ufate andi masuzuma menshi mu gihe utegereje ibisubizo cyangwa se gusubiramo ibipimo by'ubuzima, kanda mu nsi hano ahanditse ngo \" Ba uhagaritse igikorwa\" kugira ngo usohoke kuri iri suzuma. Ushobora kurigarukaho unyuze ku kibaho cyo Kuvura uburwayi."
            , kirundi = Nothing
            }

        Warning ->
            { english = "Warning"
            , kinyarwanda = Just "Impuruza"
            , kirundi = Nothing
            }

        WasFbfDistirbuted activity ->
            case activity of
                ChildActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    , kirundi = Nothing
                    }

                MotherActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    , kirundi = Nothing
                    }

        WeekSinglePlural value ->
            if value == 1 then
                { english = "1 Week"
                , kinyarwanda = Just "1 Icyumweru"
                , kirundi = Nothing
                }

            else
                { english = String.fromInt value ++ " Weeks"
                , kinyarwanda = Just <| String.fromInt value ++ " Ibyumweru"
                , kirundi = Nothing
                }

        Weight ->
            { english = "Weight"
            , kinyarwanda = Just "Ibiro"
            , kirundi = Nothing
            }

        WelcomeUser name ->
            { english = "Welcome " ++ name
            , kinyarwanda = Just <| "Murakaza neza " ++ name
            , kirundi = Nothing
            }

        Wellbeing ->
            { english = "Wellbeing"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WellChildActivityTitle activity ->
            case activity of
                WellChildDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    , kirundi = Nothing
                    }

                WellChildNutritionAssessment ->
                    { english = "Nutrition Assessment"
                    , kinyarwanda = Just "Gusuzuma imirire"
                    , kirundi = Nothing
                    }

                WellChildECD ->
                    { english = "ECD"
                    , kinyarwanda = Just "Kwita ku mikurire y'abana bato"
                    , kirundi = Nothing
                    }

                WellChildMedication ->
                    { english = "Medication"
                    , kinyarwanda = Just "Gufata imiti"
                    , kirundi = Nothing
                    }

                WellChildPregnancySummary ->
                    { english = "Birth History"
                    , kinyarwanda = Just "Amakuru y'uko yavutse"
                    , kirundi = Nothing
                    }

                WellChildImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Nothing
                    }

                WellChildNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    , kirundi = Nothing
                    }

                WellChildPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    , kirundi = Nothing
                    }

                WellChildNCDA ->
                    translationSet ChildScorecard

        WellChildDangerSignsTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskSymptomsReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    , kirundi = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    , kirundi = Nothing
                    }

        WellChildEncounterPopup popupType ->
            case popupType of
                PopupDangerSigns ->
                    { english = "Child shows signs of acute illness. Please close this encounter and continue in an “Acute Illness” encounter immediately."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PopupECD ecdPopupType ->
                    case ecdPopupType of
                        ChildBehind ->
                            { english = "Child is behind on ECD milestones. Continue to monitor the child and provide anticipatory guidance to the caregiver."
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
                            }

                        ReferToSpecialist ->
                            { english = "Child is behind on ECD milestones. Refer the child to a specialist."
                            , kinyarwanda = Nothing
                            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        WellChildMicrocephalyWarning ->
            { english = "Child shows signs of microcephaly. Monitor for developmental, nutritional, and genetic problems.  Please refer to a specialist if concerned for genetic syndrome or other problems."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WellChildImmunisationDescription task ->
            case task of
                VaccineBCG ->
                    { english = "BCG protects your child from getting the worst complications of tuberculosis, which can affect the lungs and could be deadly for young children."
                    , kinyarwanda = Just "Urukingo rw'igituntu rurinda umwana ibyago byo kuba yakwandura igituntu, ndeste nibyago byashamikiraho bishobora kwibasira ibihaha, ibi bikaba byanahitana umwana akiri muto."
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    { english = "Prevents the child from getting lockjaw (Tetanus), whooping cough (Pertussis), liver failure (Hepatitis B), breathing problems and fever (Diptheria)."
                    , kinyarwanda = Just "Rurinda umwana indwara ya agakwega, kokolishe, umwijima wo mubwoko bwa B, n'ibibazo, ibibazo byo guhumeka n'umuriro (Akaniga)."
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV prevents certain types of cancer from developing in your child."
                    , kinyarwanda = Just "Rurinda umwana kurwara zimwe muri kanseri"
                    , kirundi = Nothing
                    }

                VaccineIPV ->
                    { english = "Is the final vaccine to prevent Polio in children. IPV boosts the effects of the previous polio vaccines your child received."
                    , kinyarwanda = Just "Ni urukingo rwa nyuma rw'imbasa ku bana, rwongerera imbaraga / rushimangira inkingo z'imbasa yabonye mbere."
                    , kirundi = Nothing
                    }

                VaccineMR ->
                    { english = "Prevents the child from contracting a highly contagious viral infection that causes a fever, lesions, and diarrhea. MR is very dangerous for pregnant women, causing miscarriage or birth defects. Vaccinating your child prevents the spread of the disease in the community."
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineOPV ->
                    { english = "OPV prevents the child from contracting the Polio Virus, which affects the spinal cord and can cause paralysis."
                    , kinyarwanda = Just "Uru rukingo rurinda umwana kwandura Virusi itera indwara y'imbasa, iyo virusi ifata ururenda ruba mu ruti rw'umugongo bigatera umwana ubumuga bw'ingingo (Amaguru cg amaboko)."
                    , kirundi = Nothing
                    }

                VaccinePCV13 ->
                    { english = "Protects against any disease caused by a specific bacteria that can lead to lung infections."
                    , kinyarwanda = Just "Rurinda umwana indwara ziterwa n'udukoko twangiza ibihaha."
                    , kirundi = Nothing
                    }

                VaccineRotarix ->
                    { english = "Protects against diarrhea caused by the Rotavirus. Diarrhea is the 3rd leading cause of death of children in Rwanda."
                    , kinyarwanda = Just "Rurinda umwana impiswi ziterwa n'udukoko twa rotavirusi. Impiswi ni impamvu ya gatatu itera imfu z'abana mu Rwanda."
                    , kirundi = Nothing
                    }

        WellChildImmunisationDosage task ->
            case task of
                VaccineBCG ->
                    { english = "There is one dose of BCG and it is given at birth."
                    , kinyarwanda = Just "Urukingo rw'igituntu rutangwa inshuro imwe umwana akimara kuvuka."
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    { english = "There are 3 doses of DTP-HepB-Hib - 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa inshuro eshatu inkingo zikurikira:(urukingo rw'agakwega, Hepatite yo mubwoko bwa B, nigihuka) yujuje ibyumweru 6, ibyumweru 10, no ku byumweru 14."
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "There are 2 doses of HPV - at 12 years and 12.5 years."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'inkondo y'umura inshuro 2 - ku myaka 12 n'imyaka 12.5."
                    , kirundi = Nothing
                    }

                VaccineIPV ->
                    { english = "There is only one dose of the inactivated vaccine at 14 weeks."
                    , kinyarwanda = Just "Uru rukingo aruhabwa inshuro imwe gusa ku byumweru 14."
                    , kirundi = Nothing
                    }

                VaccineMR ->
                    { english = "There are 2 doses of Measles-Rubella - at 9 months and 15 months."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'Iseru na Rubeyole inshuro 2: Afite Amezi 9, n'amezi 15."
                    , kirundi = Nothing
                    }

                VaccineOPV ->
                    { english = "There are 4 doses of OPV - at birth, 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'imbasa inshuro 4:Akivuka, ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    , kirundi = Nothing
                    }

                VaccinePCV13 ->
                    { english = "There are 3 doses of PCV 13 - 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'umusonga inshuro 3:Ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    , kirundi = Nothing
                    }

                VaccineRotarix ->
                    { english = "There are 2 doses of Rotarix - 6 weeks and 10 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'impiswi inshuro 2:Ku byumweru 6, no ku byumweru 10."
                    , kirundi = Nothing
                    }

        WellChildImmunisationHeader task ->
            case task of
                VaccineBCG ->
                    { english = "Bacillus Calmette - Guérin (BCG)"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    { english = "Diptheria, Hepatitis B, Tetanus, and Pertussis"
                    , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "Human Papillomavirus (HPV)"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    , kirundi = Nothing
                    }

                VaccineIPV ->
                    { english = "Inactivated Polio Vaccine (IPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Nothing
                    }

                VaccineMR ->
                    { english = "Measles-Rubella (MR)"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Nothing
                    }

                VaccineOPV ->
                    { english = "Oral Polio Vaccine (OPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                    , kirundi = Nothing
                    }

                VaccinePCV13 ->
                    { english = "Pneumococcal Vaccine (PCV 13)"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Nothing
                    }

                VaccineRotarix ->
                    { english = "Rotavirus Vaccine (Rotarix)"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

        WellChildImmunisationHistory task ->
            case task of
                VaccineBCG ->
                    { english = "BCG History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    { english = "DTP - HepB - Hib History"
                    , kinyarwanda = Just "Amakuru kuri DTP - HepB - Hib"
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV History"
                    , kinyarwanda = Just "Amakuru ku rukingo rw'inkondo y'umura"
                    , kirundi = Nothing
                    }

                VaccineIPV ->
                    { english = "IPV History"
                    , kinyarwanda = Just "Amakuru k' Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Nothing
                    }

                VaccineMR ->
                    { english = "Measles-Rubella History"
                    , kinyarwanda = Just "amakuru k'Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Nothing
                    }

                VaccineOPV ->
                    { english = "OPV History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'imbasa rutangwa mu kanwa"
                    , kirundi = Nothing
                    }

                VaccinePCV13 ->
                    { english = "PCV 13 History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'umusonga"
                    , kirundi = Nothing
                    }

                VaccineRotarix ->
                    { english = "Rotarix History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

        WellChildImmunisationTask task ->
            case task of
                Measurement.Model.TaskBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskDTP ->
                    { english = "DTP - HepB - Hib"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskPCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

                Measurement.Model.TaskOverview ->
                    { english = "Overview"
                    , kinyarwanda = Just "Ishusho Rusange"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    , kirundi = Nothing
                    }

                TaskSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        , kirundi = Nothing
                        }

                    else
                        { english = "Refer to Program"
                        , kinyarwanda = Nothing
                        , kirundi = Nothing
                        }

                TaskFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                SymptomConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    , kirundi = Nothing
                    }

                SymptomLethargyOrUnresponsiveness ->
                    { english = "Lethargy or unresponsiveness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    , kirundi = Nothing
                    }

                SymptomDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    , kirundi = Nothing
                    }

                SymptomVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                SymptomPalmoplantarPallor ->
                    { english = "Palmoplantar pallor"
                    , kinyarwanda = Just "Kweruruka mu biganza no mu bworo bw'ibirenge"
                    , kirundi = Nothing
                    }

                SymptomHistoryOfFever ->
                    { english = "History of fever"
                    , kinyarwanda = Just "Amakuru yerekeye umuriro yagize mu bihe byashize"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                NoWellChildSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    , kirundi = Nothing
                    }

        WellChildVaccineLabel vaccineType ->
            case vaccineType of
                VaccineBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    , kirundi = Nothing
                    }

                VaccineDTP ->
                    { english = "DTP - HepB - Hib"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Unkondo y'Umura"
                    , kirundi = Nothing
                    }

                VaccineIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    , kirundi = Nothing
                    }

                VaccineMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    , kirundi = Nothing
                    }

                VaccineOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    , kirundi = Nothing
                    }

                VaccinePCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    , kirundi = Nothing
                    }

                VaccineRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    , kirundi = Nothing
                    }

        WhatDoYouWantToDo ->
            { english = "What do you want to do?"
            , kinyarwanda = Just "Urashaka gukora iki?"
            , kirundi = Nothing
            }

        WhatType ->
            { english = "What type"
            , kinyarwanda = Just "Ubuhe bwoko"
            , kirundi = Nothing
            }

        WhatWasTheirResponse ->
            { english = "What was their response"
            , kinyarwanda = Just "Ni iki bagusubije"
            , kirundi = Nothing
            }

        WhoCaresForTheChildDuringTheDay ->
            { english = "Who cares for the child during the day"
            , kinyarwanda = Just "Ni inde wita ku mwana ku manywa"
            , kirundi = Nothing
            }

        WhoInFamilyHasCondition ->
            { english = "Who in the family has this condition"
            , kinyarwanda = Just "Ni inde mu muryango ufite iki kibazo"
            , kirundi = Nothing
            }

        WhyNot ->
            { english = "Why not"
            , kinyarwanda = Just "Kubera iki"
            , kirundi = Nothing
            }

        WhyDifferentFbfAmount activity ->
            case activity of
                ChildActivity _ ->
                    { english = "Select why child received a different amount of FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MotherActivity _ ->
                    { english = "Select why mother received a different amount of FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        WrittenProtocolsFollowed ->
            { english = "Written protocols followed"
            , kinyarwanda = Just "Amabwiriza yanditse yakurikijwe"
            , kirundi = Nothing
            }

        Year ->
            { english = "Year"
            , kinyarwanda = Just "Umwaka"
            , kirundi = Nothing
            }

        YearsOld int ->
            { english = String.fromInt int ++ " years old"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt int
            , kirundi = Nothing
            }

        Yes ->
            { english = "Yes"
            , kinyarwanda = Just "Yego"
            , kirundi = Nothing
            }

        YouAreNotAnAdmin ->
            { english = "You are not logged in as an Administrator."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        YourGroupEncounterHasBeenSaved ->
            { english = "Your Group Encounter has been saved."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ZScoreHeadCircumferenceForAge ->
            { english = "Z-Score Head Circumference for Age: "
            , kinyarwanda = Just "Z-score ku muzenguruko w'umutwe ugereranije n'imyaka afite: "
            , kirundi = Nothing
            }

        ZScoreHeightForAge ->
            { english = "Z-Score Height for Age: "
            , kinyarwanda = Just "Z-score Uburebure ku myaka: "
            , kirundi = Nothing
            }

        ZScoreMuacForAge ->
            { english = "MUAC for Age: "
            , kinyarwanda = Just "MUAC ku myaka: "
            , kirundi = Nothing
            }

        ZScoreWeightForAge ->
            { english = "Z-Score Weight for Age: "
            , kinyarwanda = Just "Z-score Ibiro ku myaka: "
            , kirundi = Nothing
            }

        ZScoreWeightForHeight ->
            { english = "Z-Score Weight for Height: "
            , kinyarwanda = Just "Z-score Ibiro ku uburebure: "
            , kirundi = Nothing
            }


translateMyRelatedBy : MyRelatedBy -> TranslationSet String
translateMyRelatedBy relationship =
    case relationship of
        MyChild ->
            { english = "Child"
            , kinyarwanda = Just "Umwana"
            , kirundi = Nothing
            }

        MyParent ->
            { english = "Parent"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MyCaregiven ->
            { english = "Care given"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MyCaregiver ->
            { english = "Caregiver"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        MyParent ->
            { english = "is the child of"
            , kinyarwanda = Just "ni umwana wa"
            , kirundi = Nothing
            }

        MyCaregiven ->
            { english = "is the caregiver for"
            , kinyarwanda = Just "ni umurezi wa"
            , kirundi = Nothing
            }

        MyCaregiver ->
            { english = "is given care by"
            , kinyarwanda = Just "arerwa na"
            , kirundi = Nothing
            }


translateActivePage : Page -> TranslationSet String
translateActivePage page =
    case page of
        DevicePage ->
            { english = "Device Status"
            , kinyarwanda = Just "Uko igikoresho cy'ikoranabuhanga gihagaze"
            , kirundi = Nothing
            }

        PinCodePage ->
            { english = "PIN Code"
            , kinyarwanda = Just "Umubare w'ibanga"
            , kirundi = Nothing
            }

        PageNotFound url ->
            { english = "Missing"
            , kinyarwanda = Just "Ibibura"
            , kirundi = Nothing
            }

        ServiceWorkerPage ->
            { english = "Deployment"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    { english = "Clinical"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ClinicsPage ->
                    { english = "Groups"
                    , kinyarwanda = Just "Itsinda"
                    , kirundi = Nothing
                    }

                ClinicalProgressReportPage _ _ ->
                    { english = "Clinical Progress Report"
                    , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
                    , kirundi = Nothing
                    }

                CreatePersonPage _ _ ->
                    { english = "Create Person"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                DashboardPage dashboardPage ->
                    { english = "Dashboards"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GlobalCaseManagementPage ->
                    { english = "Case Management"
                    , kinyarwanda = Just "Gukurikirana Umurwayi"
                    , kirundi = Nothing
                    }

                DemographicsReportPage _ _ ->
                    { english = "Demographics Report"
                    , kinyarwanda = Just "Raporo y'umwirondoro"
                    , kirundi = Nothing
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

                SessionPage sessionId sessionPage ->
                    case sessionPage of
                        ActivitiesPage ->
                            { english = "Activities"
                            , kinyarwanda = Just "Ibikorwa"
                            , kirundi = Nothing
                            }

                        ActivityPage activityType ->
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

                        ChildPage childId ->
                            { english = "Child"
                            , kinyarwanda = Just "Umwana"
                            , kirundi = Nothing
                            }

                        MotherPage motherId ->
                            { english = "Mother"
                            , kinyarwanda = Just "Umubyeyi"
                            , kirundi = Nothing
                            }

                        NextStepsPage childId _ ->
                            { english = "Next Steps"
                            , kinyarwanda = Just "Ibikurikiyeho"
                            , kirundi = Nothing
                            }

                        ProgressReportPage childId ->
                            { english = "Progress Report"
                            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
                            , kirundi = Nothing
                            }

                PrenatalEncounterPage _ ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                AcuteIllnessEncounterPage _ ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Isuzuma  ry'uburwayi butunguranye"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                HomeVisitActivityPage _ _ ->
                    { english = "Home Visit Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildParticipantPage _ _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    , kirundi = Nothing
                    }

                WellChildEncounterPage _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                NCDEncounterPage _ ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Just "Isuzuma ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NCDActivityPage _ _ ->
                    { english = "NCD Activity"
                    , kinyarwanda = Just "Igikorwa ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NCDRecurrentEncounterPage _ ->
                    { english = "NCD Recurrent Encounter"
                    , kinyarwanda = Just "Isuzuma Rigaruka ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NCDRecurrentActivityPage _ _ ->
                    { english = "NCD Recurrent Activity"
                    , kinyarwanda = Just "Igikorwa Kigaruka ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                NCDProgressReportPage _ ->
                    { english = "NCD Progress Report"
                    , kinyarwanda = Just "Raporo ku Burwayi Butandura"
                    , kirundi = Nothing
                    }

                TraceContactPage _ ->
                    { english = "Trace Contact"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PatientRecordPage _ _ ->
                    { english = "Patient Record"
                    , kinyarwanda = Just "Amakuru y'Umurwayi"
                    , kirundi = Nothing
                    }

                PrenatalLabsHistoryPage _ _ _ ->
                    { english = "Labs History"
                    , kinyarwanda = Just "Amakuru ku Bizamini byafashwe"
                    , kirundi = Nothing
                    }

                MessagingCenterPage ->
                    { english = "Messaging Center"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellbeingPage ->
                    { english = "Wellbeing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                StockManagementPage ->
                    { english = "Stock Management"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildScoreboardParticipantPage _ ->
                    { english = "Child Scoreboard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifishi y'Imikurire y'Umwana"
                    , kirundi = Nothing
                    }

                ChildScoreboardEncounterPage _ ->
                    { english = "Child Scorecard Encounter"
                    , kinyarwanda = Just "Isuzuma ku Ifishi y'Imikurire y'Umwana"
                    , kirundi = Nothing
                    }

                ChildScoreboardActivityPage _ _ ->
                    { english = "Child Scorecard Activity"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ChildScoreboardReportPage _ ->
                    translationSet ChildScorecard


translateAdherence : Adherence -> TranslationSet String
translateAdherence adherence =
    case adherence of
        PrescribedAVRs ->
            { english = "Ask the mother to name or describe her prescribed AVRs. Can she correctly describe her medication?"
            , kinyarwanda = Just "Saba umubyeyi kuvuga izina ry’imiti igabanya ubukana bamuhaye. Ese abashije kuyivuga neza?"
            , kirundi = Nothing
            }

        CorrectDosage ->
            { english = "Can she tell you the correct dosage?"
            , kinyarwanda = Just "Yaba abasha kukubwira neza uburyo ayifata?"
            , kirundi = Nothing
            }

        TimeOfDay ->
            { english = "Can she tell you the correct time of day to make her ARVs?"
            , kinyarwanda = Just "Yaba abasha kukubwira amasaha ayifatiraho buri munsi?"
            , kirundi = Nothing
            }

        Adhering ->
            { english = "Based on your conversations with her, do you think she is adhering to her ARV regimen?"
            , kinyarwanda = Just "Ugendeye ku kiganiro mwagiranye, utekereza ko ari gufata imiti ye neza?"
            , kirundi = Nothing
            }


translateCounselingTimingHeading : CounselingTiming -> TranslationSet String
translateCounselingTimingHeading timing =
    case timing of
        Entry ->
            { english = "Entry Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama ku ntangiriro:"
            , kirundi = Nothing
            }

        MidPoint ->
            { english = "Mid Program Review Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama hagati mu gusubiramo gahunda:"
            , kirundi = Nothing
            }

        Exit ->
            { english = "Exit Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama kumuntu usohotse muri gahunda:"
            , kirundi = Nothing
            }

        BeforeMidpoint ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            , kirundi = Nothing
            }

        BeforeExit ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            , kirundi = Nothing
            }


translateChartPhrase : ChartPhrase -> TranslationSet String
translateChartPhrase phrase =
    case phrase of
        AgeCompletedMonthsYears ->
            { english = "Age (completed months and years)"
            , kinyarwanda = Just "Imyaka uzuza amezi n'imyaka"
            , kirundi = Nothing
            }

        AgeWeeks ->
            { english = "Age (weeks)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Birth ->
            { english = "Birth"
            , kinyarwanda = Just "kuvuka"
            , kirundi = Nothing
            }

        ChartAgeRange range ->
            case range of
                RangeBirthToThirteenWeeks ->
                    { english = "Birth to 13-weeks (z-scores)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                RangeBirthToTwoYears ->
                    { english = "Birth to 2-years (z-scores)"
                    , kinyarwanda = Just "kuvuka (Kuva avutse)  kugeza ku myaka 2 Z-score"
                    , kirundi = Nothing
                    }

                RangeBirthToFiveYears ->
                    { english = "Birth to 5-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 0-5"
                    , kirundi = Nothing
                    }

                RangeFiveToTenYears ->
                    { english = "5 to 10-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-10"
                    , kirundi = Nothing
                    }

                RangeFiveToNineteenYears ->
                    { english = "5 to 19-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-19"
                    , kirundi = Nothing
                    }

        HeadCircumferenceCm ->
            { english = "Head Circumference (cm)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HeadCircumferenceForAge gender ->
            case gender of
                Male ->
                    { english = "Head Circumference Boys"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Head Circumference Girls"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        HeightCm ->
            { english = "Height (cm)"
            , kinyarwanda = Just "Uburebure cm"
            , kirundi = Nothing
            }

        HeightForAge gender ->
            case gender of
                Male ->
                    { english = "Height-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Height-For-Age Girls"
                    , kinyarwanda = Just "Uburebure ku myaka/ umukobwa"
                    , kirundi = Nothing
                    }

        LengthCm ->
            { english = "Length (cm)"
            , kinyarwanda = Just "Uburebure cm"
            , kirundi = Nothing
            }

        LengthForAge gender ->
            case gender of
                Male ->
                    { english = "Length-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Length-For-Age Girls"
                    , kinyarwanda = Just "uburebure ku myaka UMUKOBWA"
                    , kirundi = Nothing
                    }

        Months ->
            { english = "Months"
            , kinyarwanda = Just "Amezi"
            , kirundi = Nothing
            }

        OneYear ->
            { english = "1 year"
            , kinyarwanda = Just "Umwaka umwe"
            , kirundi = Nothing
            }

        WeightForAge gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Age Boys"
                    , kinyarwanda = Just "Ibiro ku myaka umuhungu"
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Weight-For-Age Girls"
                    , kinyarwanda = Just "ibiro ku myaka umukobwa"
                    , kirundi = Nothing
                    }

        WeightForLength gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Height Boys"
                    , kinyarwanda = Just "Ibiro ku Uburebure umuhungu"
                    , kirundi = Nothing
                    }

                Female ->
                    { english = "Weight-For-Height Girls"
                    , kinyarwanda = Just "ibiro ku uburebure umukobwa"
                    , kirundi = Nothing
                    }

        WeightKg ->
            { english = "Weight (kg)"
            , kinyarwanda = Just "Ibiro kg"
            , kirundi = Nothing
            }

        YearsPlural value ->
            { english = String.fromInt value ++ " years"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt value
            , kirundi = Nothing
            }

        ZScoreChartsAvailableAt ->
            { english = "Z-score charts available at"
            , kinyarwanda = Just "Raporo ku mikurire y'umwana"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        CallsTo114 ->
            { english = "Calls to 114"
            , kinyarwanda = Just "Inshuro bahamagaye 114"
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        ChildrenWhoDied ->
            { english = "Children Who Died"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CompletedProgramLabel ->
            { english = "Completed Program"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CurrentPregnancies ->
            { english = "Currently Pregnant Women"
            , kinyarwanda = Just "Abagore basanzwe batwite"
            , kirundi = Nothing
            }

        CommunityLevelCases ->
            { english = "Community Level Cases"
            , kinyarwanda = Just "Umubare w'ababonetse ku rwego rw'umudugudu"
            , kirundi = Nothing
            }

        ComplicatedMalariaReferredToHC ->
            { english = "Complicated Malaria Referred to HC"
            , kinyarwanda = Just "Abarwaye Malariya y'ikigatu boherejwe ku Kigo Nderabuzima"
            , kirundi = Nothing
            }

        ComplicatedGIInfectionsReferredToHc ->
            { english = "Complicated GI Infections Referred to Health Center"
            , kinyarwanda = Just "Uburwayi bwo munda bukomeye bwoherejwe ku kigo nderabuzima"
            , kirundi = Nothing
            }

        DiagnosisUndetermined ->
            { english = "Diagnosis Undetermined"
            , kinyarwanda = Just "Uburwayi budasobanutse"
            , kirundi = Nothing
            }

        DiagnosedCases ->
            { english = "Diagnosed Cases"
            , kinyarwanda = Just "Umubare w'indwara zavuwe"
            , kirundi = Nothing
            }

        FamilyPlanningLabel ->
            { english = "Family Planning"
            , kinyarwanda = Just "Kuboneza Urubyaro"
            , kirundi = Nothing
            }

        FamilyPlanningOutOfWomen { total, useFamilyPlanning } ->
            { english = String.fromInt useFamilyPlanning ++ " out of " ++ String.fromInt total ++ " women"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FamilyThatMoved ->
            { english = "Families Who Moved"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FeversByCause ->
            { english = "Fevers by Cause"
            , kinyarwanda = Just "Impamvu zateye umuriro"
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

                FeverCauseRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                FeverCauseGI ->
                    { english = "Gastrointeritis"
                    , kinyarwanda = Just "Indwara yo mu nda"
                    , kirundi = Nothing
                    }

                FeverCauseUnknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    , kirundi = Nothing
                    }

        FeverOfUnknownOrigin ->
            { english = " Fever of Unknown Origin"
            , kinyarwanda = Just "Umuriro utazwi icyawuteye"
            , kirundi = Nothing
            }

        Filter filter ->
            case filter of
                Stunting ->
                    { english = "Stunting"
                    , kinyarwanda = Just "Igwingira"
                    , kirundi = Nothing
                    }

                Underweight ->
                    { english = "Underweight"
                    , kinyarwanda = Just "Ibiro bidahagije"
                    , kirundi = Nothing
                    }

                Wasting ->
                    { english = "Wasting"
                    , kinyarwanda = Just "Kunanuka Bikabije"
                    , kirundi = Nothing
                    }

                Dashboard.MUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MissedSession ->
                    { english = "Missed Sessions"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        FilterProgramType filterProgramType ->
            case filterProgramType of
                FilterAllPrograms ->
                    { english = "All"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        Filters ->
            { english = "Filters"
            , kinyarwanda = Just "Guhitamo"
            , kirundi = Nothing
            }

        GirlsFilterLabel ->
            { english = "Girls"
            , kinyarwanda = Just "Umukobwa"
            , kirundi = Nothing
            }

        GoodNutritionLabel ->
            { english = "% Good nutrition"
            , kinyarwanda = Just "% Abafite imirire myiza"
            , kirundi = Nothing
            }

        HomeDeliveries ->
            { english = "Home Deliveries"
            , kinyarwanda = Just "Ababyariye mu Rugo"
            , kirundi = Nothing
            }

        HealthFacilityDeliveries ->
            { english = "Health Facility Deliveries"
            , kinyarwanda = Just "Ababyariye ku Ivuriro"
            , kirundi = Nothing
            }

        HealthCenterReferrals ->
            { english = "Health Center Referrals"
            , kinyarwanda = Just "Aboherejwe ku kigo nderabuzima"
            , kirundi = Nothing
            }

        IncidenceOf ->
            { english = "Incidence of"
            , kinyarwanda = Just "Umubare w'abana bashya bafite"
            , kirundi = Nothing
            }

        LastUpdated ->
            { english = "Last updated"
            , kinyarwanda = Just "Ivugurura riheruka"
            , kirundi = Nothing
            }

        LoadingDataGeneral ->
            { english = "Loading dashboard stats..."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Moderate ->
            { english = "Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MissedSessionsLabel ->
            { english = "Missed Session"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ModeratelyMalnourished ->
            { english = "Moderately Malnourished"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MothersInANC ->
            { english = "Mothers in ANC"
            , kinyarwanda = Just "Ababyeyi bari muri serivisi ikurikirana abagore batwite"
            , kirundi = Nothing
            }

        NewCasesLabel ->
            { english = "New Cases"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewCasesPerMonth ->
            { english = "New cases per month"
            , kinyarwanda = Just "Abashya bagaragaye mu kwezi"
            , kirundi = Nothing
            }

        NewPregnancy ->
            { english = "New Identified Pregnancies"
            , kinyarwanda = Just "Abagore bashya batwite"
            , kirundi = Nothing
            }

        NewBeneficiaries ->
            { english = "New Beneficiaries"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewbornsInCare ->
            { english = "Newborns in Care"
            , kinyarwanda = Just "Impinja zikurikiranwa"
            , kirundi = Nothing
            }

        NoDataGeneral ->
            { english = "No data for this health center."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NoDataForPeriod ->
            { english = "No data for the selected period."
            , kinyarwanda = Just "Nta bipimo bigaragara muri iki gihe wahisemo"
            , kirundi = Nothing
            }

        PatientsManagedAtHome ->
            { english = "Managed at Home"
            , kinyarwanda = Just "Abavuriwe mu Rugo"
            , kirundi = Nothing
            }

        PatientCurrentlyUnderCare ->
            { english = "Currently Under Care"
            , kinyarwanda = Just "Abacyitabwaho"
            , kirundi = Nothing
            }

        PercentageLabel period ->
            case period of
                Dashboard.OneYear ->
                    { english = "from last year"
                    , kinyarwanda = Just "Guhera umwaka ushize"
                    , kirundi = Nothing
                    }

                Dashboard.ThisMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dashboard.LastMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PeriodFilter period ->
            case period of
                Dashboard.OneYear ->
                    { english = "1 year"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dashboard.ThisMonth ->
                    { english = "This month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dashboard.LastMonth ->
                    { english = "Last month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "Three months"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ProgramType ->
            { english = "Program Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResolvedCases ->
            { english = " Resolved Cases: Currently in Care"
            , kinyarwanda = Just "Abavuwe: Bacyitabwaho"
            , kirundi = Nothing
            }

        Severe ->
            { english = "Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
                    , kirundi = Nothing
                    }

        SyncNotice ->
            { english = "If the dashboard statistics doesn't load shortly, please sync data from the backend."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TotalBeneficiaries ->
            { english = "Total Beneficiaries"
            , kinyarwanda = Just "Umubare w'abana bose bafite"
            , kirundi = Nothing
            }

        TotalMalnourished ->
            { english = "Total Malnourished"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TotalEncountersLabel ->
            { english = "Total encounters completed"
            , kinyarwanda = Just "Ibikorwa byose byarangiye"
            , kirundi = Nothing
            }

        TotalAssessment ->
            { english = "Total # of Assessments"
            , kinyarwanda = Just "Umubare wose w'Amasuzuma Yakozwe"
            , kirundi = Nothing
            }

        UncomplicatedMalariaByChws ->
            { english = "Uncomplicated Malaria Managed by CHWs"
            , kinyarwanda = Just "Abarwaye Malariya yorohejwe yavuwe n'abajyanama b'ubuzima"
            , kirundi = Nothing
            }

        UncomplicatedMalariaInPregnancyReferredToHc ->
            { english = "Uncomplicated Malaria in Pregnancy Referred to HC"
            , kinyarwanda = Just "Ababyeyi batwite bafite Malariya yoroheje boherejwe ku kigo nderabuzima"
            , kirundi = Nothing
            }

        UncomplicatedGIInfectionByCHWS ->
            { english = "Uncomplicated GI Infections Managed by CHWs"
            , kinyarwanda = Just "Uburwayi bwo mu nda bworoheje bwavuwe n'abajyanama w'ubuzima"
            , kirundi = Nothing
            }

        UseFamilyPlanning ->
            { english = "use family planning"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Within4MonthsOfDueDate ->
            { english = "Within 4 Months of Due Date"
            , kinyarwanda = Just "Inda ibura amezi 4 ngo ivuke"
            , kirundi = Nothing
            }

        WithDangerSigns ->
            { english = "With Danger Signs"
            , kinyarwanda = Just "Abafite Ibimenyetso Mpuruza"
            , kirundi = Nothing
            }


translateLoginPhrase : LoginPhrase -> TranslationSet String
translateLoginPhrase phrase =
    case phrase of
        CheckingCachedCredentials ->
            { english = "Checking cached credentials"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ForgotPassword1 ->
            { english = "Forgot your password?"
            , kinyarwanda = Just "Wibagiwe ijambo ry'ibanga?"
            , kirundi = Nothing
            }

        ForgotPassword2 ->
            { english = "Call The Ihangane Project at +250 788 817 542"
            , kinyarwanda = Just "Hamagara The Ihangane Project kuri +250 788 817 542(Hamagara kumushinga wa ihangane"
            , kirundi = Nothing
            }

        LoggedInAs ->
            { english = "Logged in as"
            , kinyarwanda = Just "Kwinjira nka"
            , kirundi = Nothing
            }

        LoginRejected method ->
            case method of
                ByAccessToken ->
                    { english = "Your access token has expired. You will need to sign in again."
                    , kinyarwanda = Just "Igihe cyo gukoresha sisitemu cyarangiye . Ongera winjore muri sisitemu"
                    , kirundi = Nothing
                    }

                ByPassword ->
                    { english = "The server rejected your username or password."
                    , kinyarwanda = Just "Seriveri yanze ijambo ryo kwinjira cg ijambo ry'ibanga"
                    , kirundi = Nothing
                    }

        LoginError error ->
            translateHttpError error

        LoginToSyncHealthCenters ->
            { english = "Please log in before syncing health centers"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Logout ->
            { english = "Logout"
            , kinyarwanda = Just "Gufunga"
            , kirundi = Nothing
            }

        LogoutInProgress ->
            { english = "Logout in progress ..."
            , kinyarwanda = Just "sisitemi irikwifunga"
            , kirundi = Nothing
            }

        LogoutFailed ->
            { english = "Logout Failed"
            , kinyarwanda = Just "Gufunga byanze"
            , kirundi = Nothing
            }

        Password ->
            { english = "Password"
            , kinyarwanda = Just "Ijambo ry'ibanga"
            , kirundi = Nothing
            }

        PinCode ->
            { english = "PIN code"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PinCodeRejected ->
            { english = "Your PIN code was not recognized."
            , kinyarwanda = Just "Umubare wawe w'ibanga ntabwo uzwi."
            , kirundi = Nothing
            }

        SignIn ->
            { english = "Sign In"
            , kinyarwanda = Just "Kwinjira"
            , kirundi = Nothing
            }

        SignOut ->
            { english = "Sign Out"
            , kinyarwanda = Just "Gusohoka muri sisiteme"
            , kirundi = Nothing
            }

        Username ->
            { english = "Username"
            , kinyarwanda = Just "Izina ryo kwinjira"
            , kirundi = Nothing
            }

        WorkOffline ->
            { english = "Work Offline"
            , kinyarwanda = Just "Gukora nta internet"
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
                , kirundi = Nothing
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                , kirundi = Nothing
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                , kirundi = Nothing
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                , kirundi = Nothing
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                , kirundi = Nothing
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                , kirundi = Nothing
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                , kirundi = Nothing
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                , kirundi = Nothing
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                , kirundi = Nothing
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                , kirundi = Nothing
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                , kirundi = Nothing
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                , kirundi = Nothing
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                , kirundi = Nothing
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                , kirundi = Nothing
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                , kirundi = Nothing
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                , kirundi = Nothing
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                , kirundi = Nothing
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                , kirundi = Nothing
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                , kirundi = Nothing
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                , kirundi = Nothing
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ugu"
                , kirundi = Nothing
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                , kirundi = Nothing
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                , kirundi = Nothing
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                , kirundi = Nothing
                }


translateMonthYY : Month -> Int -> Bool -> TranslationSet String
translateMonthYY month year short =
    translateMonth month short
        |> (\set ->
                { english = set.english ++ " " ++ String.fromInt year
                , kinyarwanda = Maybe.map (\kinyarwanda -> kinyarwanda ++ " " ++ String.fromInt year) set.kinyarwanda
                , kirundi = Nothing
                }
           )


translateHttpError : Http.Error -> TranslationSet String
translateHttpError error =
    case error of
        Http.NetworkError ->
            { english = "Something went wrong. Please refresh the page and try again. If problem persisits, please contact system administrator."
            , kinyarwanda = Just "Hari ikitagenze neza. Ongera ugerageze ukoraho, niba ikibazo gikomeje hamagara umuyobozi wa sisiteme."
            , kirundi = Nothing
            }

        Http.Timeout ->
            { english = "The request to the server timed out."
            , kinyarwanda = Just "Ibyo wasabye kuri seriveri byarengeje igihe."
            , kirundi = Nothing
            }

        Http.BadUrl url ->
            { english = "URL is not valid: " ++ url
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Http.BadStatus response ->
            { english = "The server indicated the following error:"
            , kinyarwanda = Just "Aya makosa yagaragaye hamagara kuri seriveri:"
            , kirundi = Nothing
            }

        Http.BadPayload message response ->
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
            , kirundi = Nothing
            }

        InvalidBirthDate ->
            { english = "is invalid"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidBirthDateForAdult ->
            { english = "is invalid - adult should at least 13 years old"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidBirthDateForChild ->
            { english = "is invalid - child should be below the age of 13"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InvalidHmisNumber ->
            { english = "is invalid - child should be between 1 and 15"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LengthError correctLength ->
            { english = "should contain " ++ String.fromInt correctLength ++ " characters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        LettersOnly ->
            { english = "should contain only letter characters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RequiredField ->
            { english = "is a required field"
            , kinyarwanda = Just "ni ngombwa kuhuzuza"
            , kirundi = Nothing
            }

        UnknownGroup ->
            { english = "is not a known Group"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnknownProvince ->
            { english = "is not a known province"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnknownDistrict ->
            { english = "is not a known district"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnknownSector ->
            { english = "is not a known sector"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnknownCell ->
            { english = "is not a known cell"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnknownVillage ->
            { english = "is not a known village"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        InvalidString ->
            { english = "is not a valid string"
            , kinyarwanda = Just "Ntibyemewe kwandikama inyuguti"
            , kirundi = Nothing
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
