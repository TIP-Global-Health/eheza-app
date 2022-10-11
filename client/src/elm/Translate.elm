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
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTiming(..), CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..), IndividualEncounterType(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (..)
import Backend.NCDActivity.Model exposing (NCDActivity(..), NCDRecurrentActivity(..))
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
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
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), PediatricCareMilestone(..))
import Date exposing (Month)
import Form.Error exposing (ErrorValue(..))
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html, text)
import Http
import Measurement.Model
    exposing
        ( FloatInputConstraints
        , LaboratoryTask(..)
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
import Pages.NCD.Activity.Types exposing (ExaminationTask(..), MedicalHistoryTask(..))
import Pages.NCD.ProgressReport.Model exposing (NCDRiskFactor(..))
import Pages.NCD.RecurrentActivity.Types
import Pages.Nutrition.Activity.Model
import Pages.Page exposing (..)
import Pages.PatientRecord.Model exposing (PatientRecordFilter(..))
import Pages.Prenatal.Activity.Types
    exposing
        ( EarlyMastitisOrEngorgmentReliefMethod(..)
        , ExaminationTask(..)
        , HeartburnReliefMethod(..)
        , HistoryTask(..)
        , LegCrampsReliefMethod(..)
        , LmpRange(..)
        , TreatmentReviewTask(..)
        )
import Pages.Prenatal.Model exposing (HypertensionTreatementUpdateOption(..))
import Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..))
import Pages.Prenatal.RecurrentActivity.Types
import Pages.Report.Model exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), PaneEntryStatus(..))
import Pages.TraceContact.Model exposing (NoContactReason(..))
import Pages.WellChild.Activity.Types exposing (NextStepsTask(..), NutritionAssessmentTask(..), VaccinationStatus(..))
import Pages.WellChild.Encounter.Model exposing (ECDPopupType(..), WarningPopupType(..))
import Pages.WellChild.ProgressReport.Model exposing (ECDStatus(..))
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
    | And
    | AndSentence
    | AntenatalProgressReport
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
    | BreastfeedingSignQuestion BreastfeedingSign
    | BeatsPerMinuteUnitLabel
    | BeginNewEncounter
    | BloodGlucose
    | BloodPressure
    | BloodPressureElevatedOcassions
    | BloodPressureDiaLabel
    | BloodPressureSysLabel
    | BMI
    | BMIHelper
    | BodyTemperature
    | Born
    | BoughtClothesQuestion
    | BowedLegs
    | BpmUnit Int
    | BreathsPerMinuteUnitLabel
    | BreastExam
    | BreastExamSign BreastExamSign
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
    | ChildNutritionSignLabel ChildNutritionSign
    | ChildNutritionSignReport ChildNutritionSign
    | ChildOf
    | Children
    | ChildrenNames
    | ChildrenNationalId
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
    | FatherName
    | FatherNationalId
    | FbfDistribution ClinicType
    | FbfToReceive Activity Float
    | FetalHeartRate
    | FetalMovement
    | FetalPresentationLabel
    | FetalPresentation FetalPresentation
    | Fetch
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
    | ForIllustrativePurposesOnly
    | FormError (ErrorValue ValidationError)
    | FormField String
    | FundalHeight
    | Gender Gender
    | GenderLabel
    | GestationalDiabetesPreviousPregnancy
    | Glass String
    | GoHome
    | GroupAssessment
    | Gravida
    | GroupEncounter
    | Growth
    | HalfOfDosage String
    | HandedReferralFormQuestion
    | HandPallor
    | Hands
    | HandsCPESign HandsCPESign
    | HCRecommendation HCRecommendation
    | HCResponseQuestion
    | HCResponsePeriodQuestion
    | HeadCircumferenceHelper
    | HeadCircumferenceNotTakenLabel
    | HeadHair
    | HealthCenter
    | HealthCenterDetermined
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
    | IdleWaitingForSync
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
    | InitialResultsDisplay InitialResultsDisplay
    | IntractableVomiting Bool
    | IntractableVomitingQuestion
    | InstructionsChooseOneMedication
    | InstructionsChooseTwoMedications
    | IsCurrentlyBreastfeeding
    | IsolatedAtHome
    | KilogramShorthand
    | KilogramsPerMonth
    | KnownAsPositiveQuestion LaboratoryTask
    | KnownPositive
    | KnownPositiveHepatitisB
    | KnownPositiveHIV
    | LabelOnePregnancyEpisodeOpen
    | LabelSeenHealthcareProviderForPregnancy
    | LabelDocumentPregnancyOutcome
    | LabHistory
    | LaboratoryCreatinineCreatinineResult
    | LaboratoryCreatinineBUNResult
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
    | LinkToMother
    | LiveChildren
    | LmpDateConfirmationLabel
    | LmpDateConfirmationQuestion
    | LmpDateConfidentHeader
    | LmpDateHeader
    | LmpLabel
    | LmpRangeHeader
    | LmpRange LmpRange
    | Location
    | LoginPhrase LoginPhrase
    | Low
    | LowRiskCase
    | Lungs
    | LungsCPESign LungsCPESign
    | MainIncomeSource MainIncomeSource
    | MainIncomeSourceQuestion
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
    | MastitisRecommendedTreatmentHeader
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
    | MMHGUnit
    | MiddleName
    | MinutesAgo Int
    | MissedDosesOfMedicatgion Int
    | ModeOfDelivery ModeOfDelivery
    | ModeOfDeliveryLabel
    | ModeratelyUnderweight
    | ModeratePreeclampsia
    | Month
    | MonthAbbrev
    | MonthSinglePlural Int
    | MonthsOld
    | Mother
    | MotherDemographicInformation
    | MotherName String
    | MotherNameLabel
    | MotherNationalId
    | Mothers
    | MUAC
    | MuacHelper
    | MyAccount
    | MyRelatedBy MyRelatedBy
    | MyRelatedByQuestion MyRelatedBy
    | Name
    | NationalIdNumber
    | NCDASignQyestion NCDASign
    | NCDActivityTitle NCDActivity
    | NCDANCServicesInstructions
    | NCDDangerSign NCDDangerSign
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
    | NutritionCaringOption CaringOption
    | NutritionFeedingSignQuestion NutritionFeedingSign
    | NutritionFoodSecuritySignQuestion NutritionFoodSecuritySign
    | NutritionHelper
    | NutritionHygieneSignQuestion NutritionHygieneSign
    | NutritionNextStepsTask Measurement.Model.NextStepsTask
    | NitritionSigns
    | NutritionSupplementType NutritionSupplementType
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
    | ParticipantDirectory
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
    | PediatricCareMilestone PediatricCareMilestone
    | PediatricVisit
    | People
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
    | PrenatalLaboratoryBloodGroupLabel
    | PrenatalLaboratoryBloodGroupTestResult
    | PrenatalLaboratoryBloodGroup BloodGroup
    | PrenatalLaboratoryRhesusLabel
    | PrenatalLaboratoryRhesusTestResult
    | PrenatalLaboratoryRhesus Rhesus
    | PrenatalLaboratoryProteinLabel
    | PrenatalLaboratoryProteinTestResult
    | PrenatalLaboratoryProteinValue ProteinValue
    | PrenatalLaboratoryPHLabel
    | PrenatalLaboratoryPHTestResult
    | PrenatalLaboratoryPHValue PHValue
    | PrenatalLaboratoryGlucoseLabel
    | PrenatalLaboratoryGlucoseTestResult
    | PrenatalLaboratoryGlucoseValue GlucoseValue
    | PrenatalLaboratoryLeukocytesLabel
    | PrenatalLaboratoryLeukocytesTestResult
    | PrenatalLaboratoryLeukocytesValue LeukocytesValue
    | PrenatalLaboratoryNitriteLabel
    | PrenatalLaboratoryNitriteTestResult
    | PrenatalLaboratoryNitriteValue NitriteValue
    | PrenatalLaboratoryUrobilinogenLabel
    | PrenatalLaboratoryUrobilinogenTestResult
    | PrenatalLaboratoryUrobilinogenValue UrobilinogenValue
    | PrenatalLaboratoryHaemoglobinLabel
    | PrenatalLaboratoryHaemoglobinTestResult
    | PrenatalLaboratoryHaemoglobinValue HaemoglobinValue
    | PrenatalLaboratoryKetoneLabel
    | PrenatalLaboratoryKetoneTestResult
    | PrenatalLaboratoryKetoneValue KetoneValue
    | PrenatalLaboratoryBilirubinLabel
    | PrenatalLaboratoryBilirubinTestResult
    | PrenatalLaboratoryBilirubinValue BilirubinValue
    | PrenatalLaboratoryHemoglobinTestResult
    | PrenatalLaboratoryRandomBloodSugarTestResult
    | PrenatalLaboratoryHIVPCRTestResult
    | PrenatalLaboratoryHIVPCRViralLoadStatusQuestion
    | PrenatalLaboratoryCreatinineLabel
    | PrenatalLaboratoryBUNLabel
    | PrenatalLaboratoryALTLabel
    | PrenatalLaboratoryALSLabel
    | PrenatalLaboratoryPregnancyLabel
    | LaboratoryTest LaboratoryTest
    | PrenatalLabsCaseManagementEntryTypeResults
    | PrenatalLabsCaseManagementEntryTypeVitals
    | LabsEntryState LabsEntryState
    | PrenatalLabsHistoryCompletedQuestion
    | PrenatalLabsHistoryInstructions
    | PrenatalLabsHistoryLabel
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
    | PrenatalUrineDipstickTestLabel TestVariant
    | PrenatalUrineDipstickTestVariant TestVariant
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
    | ReasonForCSection
    | ReasonForNotBreastfeeding BreastfeedingSign
    | ReasonForNotIsolating ReasonForNotIsolating
    | ReasonForNotTaking ReasonForNotTaking
    | ReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | ReceivedDewormingPill
    | ReceivedFolicAcid
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
    | RenalDisease
    | ReportAge String
    | ReportDOB String
    | ReportRemaining Int
    | ReportResultsOfContactsSearch Int
    | ReportResultsOfParticipantsSearch Int
    | Reports
    | RecentAndUpcomingGroupEncounters
    | ReportCompleted { pending : Int, completed : Int }
    | ResolveMonth Bool Month
    | ResolveMonthYY Int Bool Month
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
    | SelectedHCUploading
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
    | SignOnDoorPostedQuestion
    | SocialHistoryHivTestingResult SocialHistoryHivTestingResult
    | SpecialityCareHeaderPrefix
    | SpecialityCareHeaderSuffix
    | SpecialityCareSignQuestion SpecialityCareSign
    | StillbornPreviousDelivery
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
    | TelephoneNumber
    | Term
    | TermPregnancy
    | TestDate
    | TestName
    | TestPerformedQuestion
    | TestPerformedTodayQuestion
    | TestPrerequisiteQuestion TestPrerequisite
    | TestResultQuestion
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
    | Unknown
    | Update
    | UpdateError
    | Uploading
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
    | VitaminAWarningPopupMessage
    | WaitForVitalsRecheckHelper
    | WaitForLabsResultsHelper
    | WaitInstructions
    | Warning
    | WasFbfDistirbuted Activity
    | WeekSinglePlural Int
    | Weight
    | WelcomeUser String
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
    | WellChildImmunisationTask Pages.WellChild.Activity.Types.ImmunisationTask
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
            }

        AbdomenCPESign option ->
            case option of
                Hepatomegaly ->
                    { english = "Hepatomegaly"
                    , kinyarwanda = Just "Kubyimba umwijima"
                    }

                Splenomegaly ->
                    { english = "Splenomegaly"
                    , kinyarwanda = Just "Kubyimba urwangashya"
                    }

                TPRightUpper ->
                    { english = "Tender to Palpation right upper"
                    , kinyarwanda = Just "Igice cyo hejuru iburyo kirababara  iyo ugikanze"
                    }

                TPRightLower ->
                    { english = "Tender to Palpation right lower"
                    , kinyarwanda = Just "Igice cyo hasi iburyo kirababara  iyo ugikanze"
                    }

                TPLeftUpper ->
                    { english = "Tender to Palpation left upper"
                    , kinyarwanda = Just "Igice cyo hejuru ibumoso kirababara  iyo ugikanze"
                    }

                TPLeftLower ->
                    { english = "Tender to Palpation left lower"
                    , kinyarwanda = Just "Igice cyo hasi ibumoso kirababara  iyo ugikanze"
                    }

                Hernia ->
                    { english = "Hernia"
                    , kinyarwanda = Just "Urugingo ruyobera cg rwinjira mu rundi"
                    }

                NormalAbdomen ->
                    translationSet Normal

        Abnormal ->
            { english = "Abnormal"
            , kinyarwanda = Nothing
            }

        Abortions ->
            { english = "Abortions"
            , kinyarwanda = Just "Inda yavuyemo"
            }

        AccompaniedByPartner ->
            { english = "Was the patient accompanied by partner during the assessment"
            , kinyarwanda = Just "Umubyeyi yaherekejwe n'umugabo we mu gihe yaje kwipimisha"
            }

        AccompanyToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Will you accompany the patient to the health center"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku kigonderabuzima"
                    }

                FacilityHospital ->
                    { english = "Will you accompany the patient to the hospital"
                    , kinyarwanda = Just "Uraherekeza umubyeyi ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Will you accompany the patient to mental health specialist"
                    , kinyarwanda = Just "Uzaherekeza umurwayi ku muganaga winzobere k'ubuzima bwo mu mutwe"
                    }

                FacilityARVProgram ->
                    { english = "Will you accompany the patient to ARV services"
                    , kinyarwanda = Just "Uraherekeza umubyei muri erivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Will you accompany the patient to NCD services"
                    , kinyarwanda = Just "Uzaherekeza umurwayi muri serivisi y'indwara zitandura"
                    }

                FacilityANCServices ->
                    { english = "Will you accompany the patient to ANC services"
                    , kinyarwanda = Nothing
                    }

        AccessDenied ->
            { english = "Access denied"
            , kinyarwanda = Just "Kwinjira ntibyemera"
            }

        Actions ->
            { english = "Actions"
            , kinyarwanda = Just "Ibikorwa"
            }

        ActionsTaken ->
            { english = "Actions Taken"
            , kinyarwanda = Just "Ibyakozwe"
            }

        ActionsToTake ->
            { english = "Actions To Take"
            , kinyarwanda = Just "Ibigomba gukorwa"
            }

        AcuteFindingsGeneralSign sign ->
            case sign of
                LethargicOrUnconscious ->
                    { english = "Lethargic Or Unconscious"
                    , kinyarwanda = Just "Yahwereye cyangwa yataye ubwenge"
                    }

                AcuteFindingsPoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    }

                SunkenEyes ->
                    { english = "Sunken Eyes"
                    , kinyarwanda = Just "Amaso yahenengeye"
                    }

                PoorSkinTurgor ->
                    { english = "Poor Skin Turgor"
                    , kinyarwanda = Just "Uruhu rwumye"
                    }

                Jaundice ->
                    { english = "Jaundice"
                    , kinyarwanda = Just "Umuhondo/umubiri wahindutse umuhondo"
                    }

                NoAcuteFindingsGeneralSigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        AcuteFindingsRespiratorySign sign ->
            case sign of
                Stridor ->
                    { english = "Stridor"
                    , kinyarwanda = Just "Guhumeka ajwigira"
                    }

                NasalFlaring ->
                    { english = "Nasal Flaring"
                    , kinyarwanda = Just "Amazuru abyina igihe ahumeka"
                    }

                SevereWheezing ->
                    { english = "Severe Wheezing"
                    , kinyarwanda = Just "Guhumeka nabi cyane ajwigira"
                    }

                SubCostalRetractions ->
                    { english = "Sub-Costal Retractions"
                    , kinyarwanda = Just "Icyena mu mbavu"
                    }

                NoAcuteFindingsRespiratorySigns ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        AcuteIllnessAdverseEvent event ->
            case event of
                AdverseEventRashOrItching ->
                    { english = "Rash or Itching"
                    , kinyarwanda = Just "Kwishima cyangwa gusesa uduheri (turyaryata)"
                    }

                AdverseEventFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    }

                AdverseEventDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    }

                AdverseEventVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    }

                AdverseEventFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "umunaniro"
                    }

                AdverseEventOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoAdverseEvent ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        AcuteIllnessAdverseEventKindsQuestion ->
            { english = "What kind of adverse events"
            , kinyarwanda = Just "Ni ibihe bintu wabonye bidasanzwe (bitewe n'imiti wafashe)"
            }

        AcuteIllnessDangerSign sign ->
            case sign of
                DangerSignConditionNotImproving ->
                    { english = "Condition not improving"
                    , kinyarwanda = Just "Yanyoye imiti ariko ntiyoroherwa"
                    }

                DangerSignUnableDrinkSuck ->
                    { english = "Unable to Drink/Suck"
                    , kinyarwanda = Just "Ntashoboye kunywa/konka"
                    }

                DangerSignVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    }

                DangerSignConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    }

                DangerSignLethargyUnconsciousness ->
                    { english = "Lethargy or Unconsciousness"
                    , kinyarwanda = Just "Yahwereye cyangwa ntiyumva"
                    }

                DangerSignRespiratoryDistress ->
                    { english = "Respiratory Distress"
                    , kinyarwanda = Just "Ahumeka bimugoye"
                    }

                DangerSignSpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    }

                DangerSignBloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    }

                DangerSignNewSkinRash ->
                    { english = "New Skin Rash"
                    , kinyarwanda = Just "Yasheshe uduheri dushya"
                    }

                NoAcuteIllnessDangerSign ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        AcuteIllnessDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Complicated Malaria"
                    , kinyarwanda = Just "Malariya y'igikatu"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Gastrointestinal Infection with Complications"
                    , kinyarwanda = Just "Indwara yo mu nda ikabije"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Gastrointestinal Infection without Complications"
                    , kinyarwanda = Just "Indwara yo mu nda yoroheje"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Ibicurane n'inkorora byoroheje"
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Acute Respiratory Infection with Complications"
                    , kinyarwanda = Just "Indwara y'ubuhumekero ikabije"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Umusonga woroheje"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    }

        AcuteIllnessDiagnosisWarning diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19 case"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Malaria with Complications"
                    , kinyarwanda = Just "Afite Malariya y'igikatu"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Malaria Without Complications"
                    , kinyarwanda = Just "Afite Malariya yoroheje"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Suspected Gastrointestinal Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda ikabije"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Suspected Gastrointestinal Infection (without Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara yo mu nda yoroheje"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Inkorora n'ibicurane byoroheje "
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Suspected Acute Respiratory Infection (with Complications)"
                    , kinyarwanda = Just "Aracyekwaho indwara y'ubuhumekero ikabije"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Suspected Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Aracyekwaho umusonga woroheje"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    }

                NoAcuteIllnessDiagnosis ->
                    { english = "No Diagnosis"
                    , kinyarwanda = Nothing
                    }

        AcuteIllnessExisting ->
            { english = "Existing Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho iheruka kuvurwa"
            }

        AcuteIllnessHistory ->
            { english = "Acute Illness History"
            , kinyarwanda = Just "Amakuru ku ndwara ifatiyeho"
            }

        AcuteIllnessLowRiskCaseHelper ->
            { english = "This patient is a low risk case and should be sent home to be monitored by a CHW"
            , kinyarwanda = Just "Uyu murwayi nta byago afite byo kuba yaranduye Covid-19, agomba koherezwa mu rugo agakurikiranwa n'umujyanama w'ubuzima"
            }

        AcuteIllnessNew ->
            { english = "New Acute Illness"
            , kinyarwanda = Just "Indwara ifatiyeho nshyashya"
            }

        AcuteIllnessOutcomeLabel ->
            { english = "Acute Illness Outcome"
            , kinyarwanda = Just "Iherezo ry'indwara ifatiyeho\n"
            }

        AcuteIllnessStatus status ->
            case status of
                AcuteIllnessBegan ->
                    { english = "Began"
                    , kinyarwanda = Nothing
                    }

                AcuteIllnessUpdated ->
                    { english = "Updated"
                    , kinyarwanda = Nothing
                    }

                AcuteIllnessResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    }

        ActiveDiagnosis ->
            { english = "Active Diagnosis"
            , kinyarwanda = Just "Uburwayi Bwasuzumwe"
            }

        AcuteIllnessOutcome outcome ->
            case outcome of
                OutcomeIllnessResolved ->
                    { english = "Illness Resolved"
                    , kinyarwanda = Just "Indwara Yarakize"
                    }

                OutcomeLostToFollowUp ->
                    { english = "Lost to Follow Up"
                    , kinyarwanda = Just "Umurwayi yaburiwe irengero"
                    }

                OutcomeMovedOutsideCA ->
                    { english = "Moved outside the catchment area"
                    , kinyarwanda = Just "Umurwayi yimukiye ahandi"
                    }

                OutcomePatientDied ->
                    { english = "Patient Died"
                    , kinyarwanda = Just "Umurwayi yarapfuye"
                    }

                Backend.IndividualEncounterParticipant.Model.OutcomeReferredToHC ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    }

                OutcomeOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

        AddChild ->
            { english = "Add Child"
            , kinyarwanda = Just "Ongeraho umwana"
            }

        AddContact ->
            { english = "Add Contact"
            , kinyarwanda = Just "Ongeraho uwo bahuye"
            }

        AddedToPatientRecordOn ->
            { english = "Added to patient record on"
            , kinyarwanda = Just "Yongewe ku makuru y'umurwayi kuwa"
            }

        AddFamilyMember ->
            { english = "Add Family Member"
            , kinyarwanda = Nothing
            }

        AddFamilyMemberFor name ->
            { english = "Add Family Member for " ++ name
            , kinyarwanda = Nothing
            }

        AddNewParticipant ->
            { english = "Add new participant"
            , kinyarwanda = Just "Ongeramo Umugenerwabikorwa musha"
            }

        AddParentOrCaregiver ->
            { english = "Add Parent or Caregiver"
            , kinyarwanda = Just "Ongeraho umubyeyi cyangwa umurezi"
            }

        AddToGroup ->
            { english = "Add to Group..."
            , kinyarwanda = Just "Ongeraho itsinda..."
            }

        Admin ->
            { english = "Administration"
            , kinyarwanda = Just "Abakuriye"
            }

        Administer ->
            { english = "Administer"
            , kinyarwanda = Just "Tanga umuti"
            }

        AdministerAzithromycinHelper ->
            { english = "By mouth 1x"
            , kinyarwanda = Just "Inshuro imwe mu kanwa"
            }

        AdministerCeftriaxoneHelper ->
            { english = "IM once"
            , kinyarwanda = Just "Urushinge mu mikaya inshuro imwe"
            }

        AdministerMebendezoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            }

        AdministerMetronidazoleHelper ->
            { english = "By mouth twice a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini inshuro ebyiri ku munsi mu minsi irindwi"
            }

        AdministerAlbendazoleHelper ->
            { english = "Give the child one tablet by mouth"
            , kinyarwanda = Just "Ha umwana ikinini kimwe akinywe"
            }

        AdministerPrenatalMebendezoleHelper ->
            { english = "1 dose once a day for one day"
            , kinyarwanda = Just "Ikinini kimwe inshuro imwe ku munsi mu munsi umwe"
            }

        AdministerFolicAcidHelper ->
            { english = "Take daily for 3 months"
            , kinyarwanda = Just "Fata imiti buri munsi mu gihe cy'amexi 3"
            }

        AdministerHIVARVHelper ->
            { english = "Take 1x a day by mouth"
            , kinyarwanda = Just "Fata ikinini 1 ku munsi mu kanwa"
            }

        AdministerIronHelper ->
            { english = "Take 1 60 mg tabs 2x a day x 3 months"
            , kinyarwanda = Just "Fata mg 1 60 inshuro 2 ku munsi mu mezi atatu"
            }

        AdministerParacetamolHelper ->
            { english = "Take 1 tablet by mouth 3 times a day for 5 days"
            , kinyarwanda = Just "Fata ikinini 1 mu kanwa inshuro 3 ku munsi mu minsi 5"
            }

        AdministerVitaminAHelperPrenatal ->
            { english = "Vitamin A is given once"
            , kinyarwanda = Just "Vitamine A itangwa inshuro 1"
            }

        AdministerVitaminAHelperWellChild ->
            { english = "Put the correct number of drops directly into the mouth of the child"
            , kinyarwanda = Just "Shyira mu kanwa k'umwana ibitonyanga bigenwe"
            }

        Administered ->
            { english = "Administered"
            , kinyarwanda = Just "Umuti watanzwe"
            }

        AdministeredMedicationQuestion ->
            { english = "Have you administered"
            , kinyarwanda = Just "Watanze umuti"
            }

        AdministeredOneOfAboveMedicinesQuestion ->
            { english = "Have you administered one of the above medicines to the patient"
            , kinyarwanda = Just "Waba wahaye umurwyayi umwe mu miti yavuzwe haruguru"
            }

        AddressInformation ->
            { english = "Address Information"
            , kinyarwanda = Just "Aho atuye/Aho abarizwa"
            }

        AfterEachLiquidStool ->
            { english = "after each liquid stool"
            , kinyarwanda = Just "buri uko amaze kwituma ibyoroshye"
            }

        AgeWord ->
            { english = "Age"
            , kinyarwanda = Just "Imyaka"
            }

        Activities ->
            { english = "Activities"
            , kinyarwanda = Just "Ibikorwa"
            }

        ActivitiesCompleted count ->
            { english = "Completed (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibyarangiye (" ++ String.fromInt count ++ ")"
            }

        ActivitiesHelp activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                    , kinyarwanda = Just "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                    }

                MotherActivity Lactation ->
                    { english = "Ideally a mother exclusively breastfeeds her infant for at least 6 months. Every mother should be asked about how she is feeding her infant each month."
                    , kinyarwanda = Just "Ni byiza ko umubyeyi yonsa umwana we byibuze amezi 6 nta kindi amuvangiye. Buri mubyeyi agomba kubazwa uko agaburira umwana we buri kwezi."
                    }

                MotherActivity MotherFbf ->
                    { english = "If a mother is breastfeeding, she should receive FBF every month. If she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Please review the following forms with the participant."
                    , kinyarwanda = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   }
                -}
                ChildActivity ChildFbf ->
                    { english = "Every child should receive FBF every month. If he/she did not receive the specified amount, please record the amount distributed and select the reason why."
                    , kinyarwanda = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    }

        ActivitiesLabel activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Which, if any, of the following methods do you use?"
                    , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                    }

                MotherActivity Lactation ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms:"
                    , kinyarwanda = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                   , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                   }
                -}
                ChildActivity ChildFbf ->
                    { english = "Enter the amount of CSB++ (FBF) distributed below."
                    , kinyarwanda = Just "Andika ingano ya  CSB++ (FBF) yahawe hano."
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height:"
                    , kinyarwanda = Just "Uburebure:"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC:"
                    , kinyarwanda = Just "Ikizigira cy'akaboko:"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Select all signs that are present:"
                    , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo:"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight:"
                    , kinyarwanda = Just "Ibiro:"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors:"
                    , kinyarwanda = Just "Impamvu zateye uburwayi:"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up:"
                    , kinyarwanda = Just "Gukurikirana umurwayi:"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education:"
                    , kinyarwanda = Just "Inyigisho ku buzima:"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center:"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima:"
                    }

        ActivitiesTitle activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro?"
                    }

                MotherActivity Lactation ->
                    { english = "Lactation"
                    , kinyarwanda = Just "Konsa"
                    }

                MotherActivity MotherFbf ->
                    { english = "FBF Mother"
                    , kinyarwanda = Just "FBF y'umubyeyi"
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms"
                    , kinyarwanda = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Counseling"
                   , kinyarwanda = Just "Ubujyanama"
                   }
                -}
                ChildActivity ChildFbf ->
                    { english = "FBF Child"
                    , kinyarwanda = Just "FBF y'umwana"
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    }

        ActivitityTitleAchi ->
            { english = "Aheza Child"
            , kinyarwanda = Just "Aheza igenewe umwana"
            }

        ActivityProgressReport activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    }

                MotherActivity Lactation ->
                    { english = "Lactation"
                    , kinyarwanda = Nothing
                    }

                MotherActivity MotherFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    }

                MotherActivity ParticipantConsent ->
                    { english = "Forms"
                    , kinyarwanda = Nothing
                    }

                {- ChildActivity Counseling ->
                   { english = "Counseling"
                   , kinyarwanda = Nothing
                   }
                -}
                ChildActivity ChildFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    }

                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition Signs"
                    , kinyarwanda = Just "Ibimenyetso by'imirire"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

                ChildActivity Activity.Model.ContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    }

                ChildActivity Activity.Model.FollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

                ChildActivity Activity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                ChildActivity Activity.Model.SendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    }

        ActivitiesToComplete count ->
            { english = "To Do (" ++ String.fromInt count ++ ")"
            , kinyarwanda = Just <| "Ibisabwa gukora (" ++ String.fromInt count ++ ")"
            }

        ActivitityLabelAchi ->
            { english = "Enter the amount of Aheza distributed below."
            , kinyarwanda = Just "Uzuza hano ingano ya Aheza utanze"
            }

        ActivePage page ->
            translateActivePage page

        AcuteIllnessActivityTitle activity ->
            case activity of
                AcuteIllnessSymptoms ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kongera kureba ibimenyetso"
                    }

                AcuteIllnessPhysicalExam ->
                    { english = "Physical Exam"
                    , kinyarwanda = Just "Gusuzuma"
                    }

                AcuteIllnessPriorTreatment ->
                    { english = "Prior Treatment History"
                    , kinyarwanda = Just "Amakuru ku miti yafashe"
                    }

                AcuteIllnessLaboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    }

                AcuteIllnessExposure ->
                    { english = "Exposure / Travel History"
                    , kinyarwanda = Just "Afite ibyago byo kwandura/amakuru ku ngendo yakoze"
                    }

                AcuteIllnessNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

                AcuteIllnessOngoingTreatment ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    }

                AcuteIllnessDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    }

        Adherence adherence ->
            translateAdherence adherence

        AdverseEventSinglePlural val ->
            if val == 1 then
                { english = "Adverse event"
                , kinyarwanda = Just "Ikintu kidasanzwe (bitewe n'imiti wafashe)"
                }

            else
                { english = "Adverse events"
                , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                }

        Age months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " iminsi"
            }

        AgeDays days ->
            { english = String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt days ++ " Iminsi"
            }

        AgeMonthsWithoutDay months ->
            { english = String.fromInt months ++ " months"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi"
            }

        AgeSingleBoth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Umunsi"
            }

        AgeSingleMonth months days ->
            { english = String.fromInt months ++ " month " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| String.fromInt months ++ " Ukwezi " ++ String.fromInt days ++ " Iminsi"
            }

        AgeSingleDayWithMonth months days ->
            { english = String.fromInt months ++ " months " ++ String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt months ++ " Amezi " ++ String.fromInt days ++ " Umunsi"
            }

        AgeSingleDayWithoutMonth months days ->
            { english = String.fromInt days ++ " day"
            , kinyarwanda = Just <| String.fromInt days ++ " Umunsi"
            }

        AlertChwToFollowUp ->
            { english = "Alert CHW to follow up with patient"
            , kinyarwanda = Just "Menyesha umujyanama w'ubuzima gukurikirana umurwayi"
            }

        AgeOneYearOld ->
            { english = "One year old"
            , kinyarwanda = Just "Umwaka umwe"
            }

        AgeOneYearAndOneMonth ->
            { english = "One year and one month"
            , kinyarwanda = Just "Umwaka n'ukwezi kumwe"
            }

        AgeOneYearWithMonths months ->
            { english = "One year and " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| "Umwaka n'amezi " ++ String.fromInt months
            }

        AgeYearsWithSingleMonth years month ->
            { english = String.fromInt years ++ " years " ++ String.fromInt month ++ " month"
            , kinyarwanda = Just <| " Imyaka " ++ String.fromInt years ++ " Ukwezi " ++ String.fromInt month
            }

        AgeYearsAndMonths years months ->
            { english = String.fromInt years ++ " years " ++ String.fromInt months ++ " months"
            , kinyarwanda = Just <| " Imyaka " ++ String.fromInt years ++ " Amezi " ++ String.fromInt months
            }

        AILaboratoryTask task ->
            case task of
                LaboratoryMalariaTesting ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    }

                LaboratoryCovidTesting ->
                    { english = "Covid Rapid Test"
                    , kinyarwanda = Just "Ikizamini cya Covid-19 cyihuse"
                    }

        And ->
            { english = "and"
            , kinyarwanda = Just "na"
            }

        AndSentence ->
            { english = "and"
            , kinyarwanda = Just "maze"
            }

        AntenatalProgressReport ->
            { english = "Antenatal Progress Report"
            , kinyarwanda = Nothing
            }

        AmbulancArrivalPeriodQuestion ->
            { english = "How long did it take the ambulance to arrive"
            , kinyarwanda = Just "Bitwara igihe kingana gute ngo imbangukiragutabara ihagere"
            }

        AgeSingleMonthWithoutDay month ->
            { english = String.fromInt month ++ " month"
            , kinyarwanda = Just <| String.fromInt month ++ " Ukwezi"
            }

        AppName ->
            { english = "E-Heza System"
            , kinyarwanda = Just "E-heza sisiteme"
            }

        AppointmentConfirmation ->
            { english = "Appointment Confirmation"
            , kinyarwanda = Just "Kwemeza itariki yo kugaruka"
            }

        AppointmentConfirmationInstrunction ->
            { english = "The patient should visit the health center on the following date"
            , kinyarwanda = Just "Umubyeyi agomba kujya ku kigo nderabuzima ku itariki ikurikira"
            }

        All ->
            { english = "All"
            , kinyarwanda = Just "Uburwayi bwose"
            }

        AllowedValuesRangeHelper constraints ->
            { english = "Allowed values are between " ++ String.fromFloat constraints.minVal ++ " and " ++ String.fromFloat constraints.maxVal ++ "."
            , kinyarwanda = Just <| "Imibare yemewe iri hagati ya " ++ String.fromFloat constraints.minVal ++ " na " ++ String.fromFloat constraints.maxVal ++ "."
            }

        AreYouSure ->
            { english = "Are you sure?"
            , kinyarwanda = Just "Urabyizeye?"
            }

        Assessment ->
            { english = "Assessment"
            , kinyarwanda = Just "Ipimwa"
            }

        Asthma ->
            { english = "Asthma"
            , kinyarwanda = Just "Asthma (Agahema)"
            }

        At ->
            { english = "at"
            , kinyarwanda = Nothing
            }

        Attendance ->
            { english = "Attendance"
            , kinyarwanda = Just "Ubwitabire"
            }

        AvoidingGuidanceReason value ->
            case value of
                AvoidingGuidanceHypertensionLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    }

                AvoidingGuidanceHypertensionKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    }

                AvoidingGuidanceHypertensionPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    }

                AvoidingGuidanceHypertensionPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    }

                AvoidingGuidanceHypertensionReinforceAdherence ->
                    { english = "Reinforce adherence of existing dosage"
                    , kinyarwanda = Just "Shimangira umwigisha akamaro ko kubahiriza gufata imiti asanganwe"
                    }

                AvoidingGuidanceHypertensionOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

        Baby ->
            { english = "Baby"
            , kinyarwanda = Just "Umwana"
            }

        BabyDiedOnDayOfBirthPreviousDelivery ->
            { english = "Live Birth but the baby died the same day in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
            }

        BabyName name ->
            { english = "Baby: " ++ name
            , kinyarwanda = Just <| "Umwana: " ++ name
            }

        Back ->
            { english = "Back"
            , kinyarwanda = Nothing
            }

        BackendError ->
            { english = "Error contacting backend"
            , kinyarwanda = Just "Seriveri yerekanye amakosa akurikira"
            }

        BreastfeedingSignQuestion sign ->
            case sign of
                IsBreastfeeding ->
                    { english = "Are you breastfeeding"
                    , kinyarwanda = Just "Waba wonsa"
                    }

                BreastPain ->
                    { english = "Are you experiencing breast pain"
                    , kinyarwanda = Just "Waba ubabara amabere"
                    }

                BreastRedness ->
                    { english = "Are you experiencing breast redness"
                    , kinyarwanda = Just "Amabere yawe yaba atukuye"
                    }

                EnoughMilk ->
                    { english = "Do you have enough milk for your baby to breastfeed at least 8 times per day"
                    , kinyarwanda = Just "Waba ufite amashereka ahagije yo konsa umwana wawe nibura inshuro 8 kumunsi"
                    }

                LatchingWell ->
                    { english = "Is the baby latching well"
                    , kinyarwanda = Just "Umwana aronka neza"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        BeatsPerMinuteUnitLabel ->
            { english = "bpm"
            , kinyarwanda = Just "Inshuro umutima utera ku munota"
            }

        BeginNewEncounter ->
            { english = "Begin a New Encounter"
            , kinyarwanda = Just "Tangira igikorwa gishya"
            }

        BloodGlucose ->
            { english = "Blood Glucose"
            , kinyarwanda = Nothing
            }

        BloodPressure ->
            { english = "Blood Pressure"
            , kinyarwanda = Just "Umuvuduko w'amaraso"
            }

        BloodPressureElevatedOcassions ->
            { english = "Blood Pressure Elevated occasions"
            , kinyarwanda = Nothing
            }

        BloodPressureDiaLabel ->
            { english = "Diastolic"
            , kinyarwanda = Just "Umuvuduko w'amaraso wo hasi"
            }

        BloodPressureSysLabel ->
            { english = "Systolic"
            , kinyarwanda = Just "Umubare w'umuvuduko w'amaraso wo hejuru"
            }

        BMI ->
            { english = "BMI"
            , kinyarwanda = Nothing
            }

        BMIHelper ->
            { english = "Calculated based on Height and Weight"
            , kinyarwanda = Just "Byabazwe hashingiwe ku burebure n'ibiro"
            }

        BodyTemperature ->
            { english = "Body Temperature"
            , kinyarwanda = Just "Ubushyuhe bw'umubiri"
            }

        Born ->
            { english = "Born"
            , kinyarwanda = Just "Kuvuka/ itariki y'amavuko"
            }

        BoughtClothesQuestion ->
            { english = "Have you bought clothes and other essential items for the child"
            , kinyarwanda = Just "Waba waraguze imyenda n'ibindi bikoresho by'ibanze bikenewe ku mwana"
            }

        BowedLegs ->
            { english = "Bowed Legs"
            , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
            }

        BpmUnit respiratoryRate ->
            { english = String.fromInt respiratoryRate ++ " bpm"
            , kinyarwanda = Just <| "Inshuro ahumeka ku munota " ++ String.fromInt respiratoryRate
            }

        BreathsPerMinuteUnitLabel ->
            { english = "bpm"
            , kinyarwanda = Just "Inshuro ahumeka ku munota"
            }

        BreastExam ->
            { english = "Breast Exam"
            , kinyarwanda = Just "Gusuzuma amabere"
            }

        BreastExamQuestion ->
            { english = "Did you show the patient how to perform a self breast exam"
            , kinyarwanda = Just "Weretse umubyeyi uko yakwisuzuma amabere?"
            }

        BreastExamSign option ->
            case option of
                Mass ->
                    { english = "Mass"
                    , kinyarwanda = Just "Utubyimba mu Ibere"
                    }

                Discharge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Gusohoka kw'ibintu bidasanzwe"
                    }

                Infection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    }

                NormalBreast ->
                    translationSet Normal

                Warmth ->
                    { english = "Warmth"
                    , kinyarwanda = Just "Ubushyuhe"
                    }

        BrittleHair ->
            { english = "Brittle Hair"
            , kinyarwanda = Just "Gucurama no guhindura ibara ku misatsi"
            }

        ByMouthDaylyForXDays days ->
            { english = "by mouth daily x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "ku munsi / mu  minsi " ++ String.fromInt days
            }

        ByMouthTwiceADayForXDays days ->
            { english = "by mouth twice per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            }

        ByMouthThreeTimesADayForXDays days ->
            { english = "by mouth three times per day x " ++ String.fromInt days ++ " days"
            , kinyarwanda = Just <| "inshuro ebyiri ku munsi/ mu minsi " ++ String.fromInt days
            }

        Call114 ->
            { english = "Call 114"
            , kinyarwanda = Just "Hamagara 114"
            }

        Called114Question ->
            { english = "Were you able to talk with 114"
            , kinyarwanda = Just "Wabashije kuvugana n’abantu bo kuri 114"
            }

        Cancel ->
            { english = "Cancel"
            , kinyarwanda = Just "Guhagarika"
            }

        CandidiasisRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Candidiasis"
            , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso bya Kandidoze"
            }

        CandidiasisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            }

        CandidiasisRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            }

        CannotStartEncounterLabel ->
            { english = "You cannot open a new encounter, as there's already a completed encounter today for"
            , kinyarwanda = Just "Ntago bishoboka gutangira isuzuma rishya, kuko hari isuzuma ryarangiye uyu munsi rya"
            }

        CardiacDisease ->
            { english = "Cardiac Disease"
            , kinyarwanda = Just "Indwara z'umutima"
            }

        CaregiverAccompanyQuestion ->
            { english = "Do you have a caregiver to accompany you to the health center when you give birth"
            , kinyarwanda = Just "Ufite umuntu wo kuguherekeza ku kigo nderabuzima igihe ugiye kubyara"
            }

        CaregiverName ->
            { english = "Caregiver's Name"
            , kinyarwanda = Nothing
            }

        CaregiverNationalId ->
            { english = "Caregiver's National ID"
            , kinyarwanda = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Just "Akagali"
            }

        CaseManagement ->
            { english = "Case Management"
            , kinyarwanda = Just "Gukurikirana Umurwayi"
            }

        CaseManagementFilterLabel filter ->
            case filter of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    { english = "Antenatal Care"
                    , kinyarwanda = Just "Isuzuma ku mugore utwite"
                    }

                FilterNutrition ->
                    { english = "Home Visit"
                    , kinyarwanda = Just "Gusura Umurwayi"
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Nothing
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Nothing
                    }

        CaseManagementPaneHeader encounterType ->
            case encounterType of
                Pages.GlobalCaseManagement.Model.FilterAcuteIllness ->
                    { english = "Acute Illness Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi wavuwe indwara zifatiyeho"
                    }

                Pages.GlobalCaseManagement.Model.FilterAntenatal ->
                    { english = "Antenatal Care Follow Up"
                    , kinyarwanda = Just "Gukurikirana umugore utwite"
                    }

                FilterNutrition ->
                    { english = "Child Nutrition Follow Up"
                    , kinyarwanda = Just "Gukurikirana imirire y'umwana"
                    }

                FilterContactsTrace ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    }

                FilterPrenatalLabs ->
                    { english = "ANC Labs"
                    , kinyarwanda = Nothing
                    }

                FilterNCDLabs ->
                    { english = "NCD Labs"
                    , kinyarwanda = Nothing
                    }

        CentimeterShorthand ->
            { english = "cm"
            , kinyarwanda = Just "cm"
            }

        Celsius ->
            { english = "Celsius"
            , kinyarwanda = Just "Serisiyusi"
            }

        CelsiusAbbrev ->
            { english = "°C"
            , kinyarwanda = Nothing
            }

        ChartPhrase phrase ->
            translateChartPhrase phrase

        CheckAllThatApply ->
            { english = "Please check all that apply"
            , kinyarwanda = Just "Emeza ibiribyo/ibishoboka byose"
            }

        CheckIn ->
            { english = "Check in:"
            , kinyarwanda = Just "Kureba abaje"
            }

        ChildCleanQuestion ->
            { english = "Is the sick child clean"
            , kinyarwanda = Just "Ese umwana urwaye afite isuku"
            }

        ChildHmisNumber ->
            { english = "Child HMIS Number"
            , kinyarwanda = Just "Numero y'umwana muri HMIS"
            }

        ChildDemographicInformation ->
            { english = "Child Demographic Information"
            , kinyarwanda = Nothing
            }

        ChildNutritionSignLabel sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso"
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "Kubura apeti /kunanirwa kurya"
                    }

        ChildNutritionSignReport sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None"
                    , kinyarwanda = Just "Nta bimenyetso"
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "kubura apeti (kunanirwa kurya)"
                    }

        Children ->
            { english = "Children"
            , kinyarwanda = Just "Abana"
            }

        ChildrenNames ->
            { english = "Children's names"
            , kinyarwanda = Just "Amazina y'umwana"
            }

        ChildrenNationalId ->
            { english = "Children's National ID"
            , kinyarwanda = Just "Indangamuntu y'umwana"
            }

        CHWAction value ->
            case value of
                ActionPregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    }

                ActionLabs ->
                    { english = "Labs"
                    , kinyarwanda = Just "Ibizamini byafashwe"
                    }

                ActionDangerSignsPresent ->
                    { english = "Danger Signs Present"
                    , kinyarwanda = Just "Hagaragaye Ibimenyetso Mpuruza"
                    }

                ActionReferredToHealthCenter ->
                    { english = "Referred to Health Center"
                    , kinyarwanda = Just "Yoherejwe Ku Kigonderbuzima"
                    }

                ActionAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Just "Kwemeza Itariki yo Kugarukaho"
                    }

                ActionHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku Buzima"
                    }

                ActionBirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    }

        ChwActivity ->
            { english = "Chw Activity"
            , kinyarwanda = Just "Igikorwa cy'Umujyana w'Ubuzima"
            }

        ChildOf ->
            { english = "Child of"
            , kinyarwanda = Just "Umwana wa"
            }

        Clear ->
            { english = "Clear"
            , kinyarwanda = Just "Gukuraho"
            }

        ClickTheCheckMark ->
            { english = "Click the check mark if the mother / caregiver is in attendance. The check mark will appear green when a mother / caregiver has been signed in."
            , kinyarwanda = Just "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
            }

        ClinicType clinicType ->
            case clinicType of
                Achi ->
                    { english = "Achi"
                    , kinyarwanda = Nothing
                    }

                Chw ->
                    { english = "CHW"
                    , kinyarwanda = Nothing
                    }

                Fbf ->
                    { english = "Fbf"
                    , kinyarwanda = Nothing
                    }

                Pmtct ->
                    { english = "Pmtct"
                    , kinyarwanda = Nothing
                    }

                Sorwathe ->
                    { english = "Sorwathe"
                    , kinyarwanda = Nothing
                    }

        Clinical ->
            { english = "Clinical"
            , kinyarwanda = Just "Amakuru y’ubuvuzi"
            }

        Dashboard dashboard ->
            translateDashboard dashboard

        ClinicalProgressReport ->
            { english = "Clinical Progress Report"
            , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
            }

        CloseAcuteIllnessLabel ->
            { english = "or Close an Acute Illness"
            , kinyarwanda = Just "Cyangwa Ufunge Indwara ifatiyeho iheruka kuvurwa"
            }

        CloseAndContinue ->
            { english = "Close & Continue"
            , kinyarwanda = Nothing
            }

        ColorAlertIndication indication ->
            case indication of
                ColorAlertRed ->
                    { english = "Red"
                    , kinyarwanda = Just "Umutuku"
                    }

                ColorAlertYellow ->
                    { english = "Yellow"
                    , kinyarwanda = Just "Umuhondo"
                    }

                ColorAlertGreen ->
                    { english = "Green"
                    , kinyarwanda = Just "Icyatsi"
                    }

        Completed ->
            { english = "Completed"
            , kinyarwanda = Nothing
            }

        CompleteFacilityReferralForm facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Complete a health center referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi ku kigo Nderabuzima"
                    }

                FacilityHospital ->
                    { english = "Complete a hospital referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Complete a referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo koherza umurwayi"
                    }

                FacilityARVProgram ->
                    { english = "Complete an ARV services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rumwohereza muri serivice itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Complete a NCD services referral form"
                    , kinyarwanda = Just "Uzuza urupapuro rwo kohereza umurwayi muri service y'indwara zitandura"
                    }

                FacilityANCServices ->
                    { english = "Complete a ANC services referral form"
                    , kinyarwanda = Nothing
                    }

        Contacted114 ->
            { english = "Contacted 114"
            , kinyarwanda = Just "Namenyesheje 114"
            }

        ContactedHC ->
            { english = "Contacted health center"
            , kinyarwanda = Just "Yamenyesheje ikigo nderabuzima"
            }

        ContactedHCQuestion ->
            { english = "Have you contacted the health center"
            , kinyarwanda = Just "Wamenyesheje ikigo nderabuzima"
            }

        ContactedRecommendedSiteQuestion ->
            { english = "Did you contact the recommended site"
            , kinyarwanda = Just "Wamenyesheje urwego rushinzwe gukurikirana umurwayi"
            }

        ContactInitiatedQuestion ->
            { english = "Where you able to speak with the contact"
            , kinyarwanda = Nothing
            }

        ContactName ->
            { english = "Contact Name"
            , kinyarwanda = Nothing
            }

        ContactsTracingCompleteDetails ->
            { english = "Please fill in contact details"
            , kinyarwanda = Just "Uzuza amakuru arambuye y'umuntu wahuye n'umurwayi"
            }

        ContactsTracingHelper ->
            { english = "Please record everyone that the patient has come into contact within 2 days of their symptoms beginning"
            , kinyarwanda = Just "Andika umuntu wese wahuye n'umurwayi mu minshi 2 ishize ibimenyetso bigaragaye"
            }

        ContactWithCOVID19SymptomsHelper ->
            { english = "Symptoms include: fever, dry cough, and shortness of breath"
            , kinyarwanda = Just "Ibimenyetso birimo: umuriro, inkorora y'akayi no guhumeka nabi"
            }

        ContactWithCOVID19SymptomsQuestion ->
            { english = "Have you had contacts with others who exhibit symptoms or have been exposed to COVID-19"
            , kinyarwanda = Just "Waba warigeze uhura n'abantu bagaragaje ibimenyetso bya covid-19 cyangwa n'abari bafite ibyago byo kuyandura"
            }

        Continued ->
            { english = "Continued"
            , kinyarwanda = Just "Yakomeje"
            }

        ContributingFactor factor ->
            case factor of
                FactorLackOfBreastMilk ->
                    { english = "Lack of breast milk (for children < 6 months)"
                    , kinyarwanda = Just "Kubura kw'amashereka (ku mwana uri munsi y'amezi atandatu)"
                    }

                FactorMaternalMastitis ->
                    { english = "Maternal mastitis (for children < 6 months)"
                    , kinyarwanda = Just "Umubyeyi yabyimbye amabere (ku mwana uri munsi y'amezi atandatu)"
                    }

                FactorPoorSuck ->
                    { english = "Poor suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    }

                FactorDiarrheaOrVomiting ->
                    { english = "Diarrhea or vomiting"
                    , kinyarwanda = Just "Impiswi cyangwa kuruka"
                    }

                NoContributingFactorsSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    }

        ContributingFactorsQuestion ->
            { english = "Has patient or patient’s mother experienced any of the following"
            , kinyarwanda = Just "Umurwayi cyangwa umubyeyi we bagaragaje ibimenyetso bikurikira"
            }

        ConvulsionsAndUnconsciousPreviousDelivery ->
            { english = "Experienced convulsions and resulted in becoming unconscious after delivery"
            , kinyarwanda = Just "Ubushize yahinze umushyitsi bimuviramo kutumva akimara kubyara"
            }

        ConvulsionsPreviousDelivery ->
            { english = "Experienced convulsions in previous delivery"
            , kinyarwanda = Just "Ubushize yahinze umushyitsi abyara"
            }

        CurrentIllnessBegan ->
            { english = "Current illness began"
            , kinyarwanda = Just "Igihe ubu burwayi afite bwatangiriye"
            }

        CSectionScar scar ->
            case scar of
                Vertical ->
                    { english = "Vertical"
                    , kinyarwanda = Just "Irahagaze"
                    }

                Horizontal ->
                    { english = "Horizontal"
                    , kinyarwanda = Just "Iratambitse"
                    }

                NoScar ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        GroupNotFound ->
            { english = "Group not found"
            , kinyarwanda = Nothing
            }

        Group ->
            { english = "Group"
            , kinyarwanda = Just "Itsinda"
            }

        Groups ->
            { english = "Groups"
            , kinyarwanda = Just "Itsinda"
            }

        Close ->
            { english = "Close"
            , kinyarwanda = Just "Gufunga"
            }

        Closed ->
            { english = "Closed"
            , kinyarwanda = Just "Gufunga"
            }

        GroupUnauthorized ->
            { english = "You are not authorized to work with this Group."
            , kinyarwanda = Nothing
            }

        ConfirmDeleteTrainingGroupEncounters ->
            { english = "Are you sure you want to delete all training Group Encounters?"
            , kinyarwanda = Nothing
            }

        DeliveryComplication complication ->
            case complication of
                ComplicationGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yaje umugore utwite"
                    }

                ComplicationEmergencyCSection ->
                    { english = "Emergency C-Section"
                    , kinyarwanda = Just "Kubagwa bitewe n'impamvu zihutirwa"
                    }

                ComplicationPreclampsia ->
                    { english = "Preeclampsia"
                    , kinyarwanda = Just "Umuvuduko w'amaraso uza uje k'umugore twite (Preclampsia)"
                    }

                ComplicationMaternalHemmorhage ->
                    { english = "Maternal Hemorrhage"
                    , kinyarwanda = Just "Kuva amaraso ku mubyeyi utwite cyangwa nyuma yo kubyara"
                    }

                ComplicationHiv ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    }

                ComplicationMaternalDeath ->
                    { english = "Maternal Death"
                    , kinyarwanda = Just "Urupfu rw'umubyeyi"
                    }

                ComplicationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoDeliveryComplications ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        DeliveryComplicationsPresentQuestion ->
            { english = "Were there any complications with the delivery"
            , kinyarwanda = Just "Haba hari ibibazo umubyeyi yagize abyara"
            }

        DeliveryComplicationsSelectionLabel ->
            { english = "Which of the following were present"
            , kinyarwanda = Just "Ni ibiki byagaragaye muri ibi bikurikira"
            }

        ConditionImproving isImproving ->
            if isImproving then
                { english = "Improving"
                , kinyarwanda = Just "Ari koroherwa"
                }

            else
                { english = "Not improving"
                , kinyarwanda = Just "Ntabwo ari koroherwa"
                }

        ConditionImprovingQuestion ->
            { english = "Is your condition improving"
            , kinyarwanda = Just "Urumva uri koroherwa"
            }

        ConfirmationRequired ->
            { english = "Please confirm:"
            , kinyarwanda = Nothing
            }

        Connected ->
            { english = "Connected"
            , kinyarwanda = Just "Ufite interineti (murandasi)"
            }

        ContactExposure ->
            { english = "Contact Exposure"
            , kinyarwanda = Nothing
            }

        ContactInformation ->
            { english = "Contact Information"
            , kinyarwanda = Just "Uburyo bwakwifashishwa mu kugera ku mugenerwabikorwa"
            }

        Continue ->
            { english = "Continue"
            , kinyarwanda = Just "Gukomeza"
            }

        CounselingTimingHeading timing ->
            translateCounselingTimingHeading timing

        CounselingTopic topic ->
            { english = topic.english
            , kinyarwanda = topic.kinyarwanda
            }

        CounselorReviewed ->
            { english = "I have reviewed the above with the participant."
            , kinyarwanda = Nothing
            }

        CovidContactTracing ->
            { english = "Covid Contact Tracing"
            , kinyarwanda = Just "Gushakisha abahuye n'uwanduye Covid-19"
            }

        CovidTestingInstructions ->
            { english = "Perform a COVID-19 Rapid Test to confirm patient’s diagnosis"
            , kinyarwanda = Just "Kora ikizamini cyihuse cya Covid-19 kugira ngo hemezwe icyo umurwayi arwaye"
            }

        CounselorSignature ->
            { english = "Entry Counselor Signature"
            , kinyarwanda = Nothing
            }

        CSectionInPreviousDelivery ->
            { english = "C-section in previous delivery"
            , kinyarwanda = Just "Yarabazwe ku nda ishize"
            }

        CSectionReason ->
            { english = "Reason for C-section"
            , kinyarwanda = Just "Impamvu yo kubagwa"
            }

        CSectionReasons reason ->
            case reason of
                Breech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    }

                Emergency ->
                    { english = "Emergency"
                    , kinyarwanda = Just "Ibyihutirwa"
                    }

                FailureToProgress ->
                    { english = "Failure to Progress"
                    , kinyarwanda = Just "Ntibyiyongera"
                    }

                Backend.Measurement.Model.None ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

                Other ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

        CreateGroupEncounter ->
            { english = "Create Group Encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            }

        CreateRelationship ->
            { english = "Create Relationship"
            , kinyarwanda = Just "Ibijyanye no guhuza amasano"
            }

        CreateTrainingGroupEncounters ->
            { english = "Create All Training Group Encounters"
            , kinyarwanda = Nothing
            }

        CurrentlyPregnant ->
            { english = "Currently Pregnant"
            , kinyarwanda = Just "Aratwite"
            }

        CurrentlyPregnantQuestion ->
            { english = "Is the patient currently pregnant"
            , kinyarwanda = Just "Umurwayi aratwite"
            }

        ChwDashboardLabel ->
            { english = "CHW Snapshot"
            , kinyarwanda = Just "Ishusho y'ibyagezweho"
            }

        DeleteTrainingGroupEncounters ->
            { english = "Delete All Training Group Encounters"
            , kinyarwanda = Nothing
            }

        DashboardLabel ->
            { english = "Dashboard"
            , kinyarwanda = Just "Ikibaho cy’amakuru y’ingenzi"
            }

        DateReceived ->
            { english = "Date Received"
            , kinyarwanda = Just "Italiki yakiriweho"
            }

        DeliveryLocation ->
            { english = "Delivery Location"
            , kinyarwanda = Just "Aho yabyariye"
            }

        DeliveryOutcome ->
            { english = "Delivery Outcome"
            , kinyarwanda = Nothing
            }

        DangerSign sign ->
            case sign of
                VaginalBleeding ->
                    { english = "Vaginal bleeding"
                    , kinyarwanda = Just "Kuva"
                    }

                HeadacheBlurredVision ->
                    { english = "Severe headaches with blurred vision"
                    , kinyarwanda = Just "Kuribwa umutwe bidasanzwe ukareba ibikezikezi"
                    }

                Convulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    }

                AbdominalPain ->
                    { english = "Severe Abdominal pain"
                    , kinyarwanda = Just "Kuribwa mu nda bikabije"
                    }

                DifficultyBreathing ->
                    { english = "Difficulty breathing"
                    , kinyarwanda = Just "Guhumeka nabi"
                    }

                Backend.Measurement.Model.Fever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    }

                ExtremeWeakness ->
                    { english = "Extreme weakness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    }

                ImminentDelivery ->
                    { english = "Imminent delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    }

                Labor ->
                    { english = "Labor"
                    , kinyarwanda = Just "Kujya ku nda"
                    }

                LooksVeryIll ->
                    { english = "Looks very ill"
                    , kinyarwanda = Just "Ararembye cyane"
                    }

                SevereVomiting ->
                    { english = "Severe vomiting"
                    , kinyarwanda = Just "Araruka bikabije"
                    }

                Unconscious ->
                    { english = "Unconscious"
                    , kinyarwanda = Just "Yataye ubwenge"
                    }

                GushLeakingVaginalFluid ->
                    { english = "Gush or leaking of vaginal fluid"
                    , kinyarwanda = Just "Ibintu biva mu gitsina by'uruzi"
                    }

                NoDangerSign ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso/nta na kimwe"
                    }

        DangerSignsLabelForChw ->
            { english = "Danger Signs"
            , kinyarwanda = Just "Ibimenyetso Mpuruza"
            }

        DangerSignsLabelForNurse ->
            { english = "Patient shows signs of"
            , kinyarwanda = Just "Umurwayi aragaragaza ibimenyetso bya"
            }

        DangerSignsTask task ->
            case task of
                ReviewDangerSigns ->
                    { english = "Review Danger Signs"
                    , kinyarwanda = Just "Kureba ibimenyetso mpuruza"
                    }

        Date ->
            { english = "Date"
            , kinyarwanda = Just "Itariki"
            }

        DateConcludedEstimatedQuestion ->
            { english = "What was the estimated due date for the child"
            , kinyarwanda = Just "Ni iyihe taliki yari iteganyijwe ko umubyeyi azabyariraho"
            }

        DateOfContact ->
            { english = "Date of Contact"
            , kinyarwanda = Just "Itariki bahuriyeho"
            }

        DateOfLastAssessment ->
            { english = "Date of last Assessment"
            , kinyarwanda = Just "Amakuru y'ipimwa rirangiye"
            }

        DatePregnancyConcluded ->
            { english = "Date Pregnancy Concluded"
            , kinyarwanda = Just "Itariki y'iherezo ryo gutwita"
            }

        Day ->
            { english = "Day"
            , kinyarwanda = Just "Umunsi"
            }

        DayAbbrev ->
            { english = "Day"
            , kinyarwanda = Just "Umu"
            }

        DaySinglePlural value ->
            if value == 1 then
                { english = "1 Day"
                , kinyarwanda = Just "1 Umunsi"
                }

            else
                { english = String.fromInt value ++ " Days"
                , kinyarwanda = Just <| String.fromInt value ++ " Iminsi"
                }

        DateOfBirth ->
            { english = "Date of Birth"
            , kinyarwanda = Just "Itariki y'amavuko"
            }

        Days ->
            { english = "days"
            , kinyarwanda = Just "Iminsi"
            }

        DaysAbbrev ->
            { english = "days"
            , kinyarwanda = Just "Imi"
            }

        DaysPresent ->
            { english = "Days present"
            , kinyarwanda = Just "Igihe gishize"
            }

        DaysSinglePlural value ->
            if value == 1 then
                { english = "1 day"
                , kinyarwanda = Just "Umunsi 1"
                }

            else
                { english = String.fromInt value ++ " days"
                , kinyarwanda = Just <| "Iminsi " ++ String.fromInt value
                }

        Delete ->
            { english = "Delete"
            , kinyarwanda = Just "Gusiba"
            }

        DemographicInformation ->
            { english = "Demographic Information"
            , kinyarwanda = Just "Umwirondoro"
            }

        DemographicsReport ->
            { english = "Demographics Report"
            , kinyarwanda = Just "Raporo y'umwirondoro"
            }

        DetectableViralLoad ->
            { english = "Detectable Viral Load"
            , kinyarwanda = Just "Ingano ya virusi itera SIDA iracyagaragara mu maraso"
            }

        Device ->
            { english = "Device"
            , kinyarwanda = Just "Igikoresho"
            }

        DeviceNotAuthorized ->
            { english =
                """This device has not yet been authorized to sync data with the backend, or the
                authorization has expired or been revoked. To authorize or re-authorize this
                device, enter a pairing code below. This will permit sensitive data to be stored
                on this device and updated to the backend. You should only authorize devices that
                are under your control and which are secure."""
            , kinyarwanda = Nothing
            }

        DeviceStatus ->
            { english = "Device Status"
            , kinyarwanda = Just "Uko igikoresho cy'ikoranabuhanga gihagaze"
            }

        Diabetes ->
            { english = "Diabetes"
            , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
            }

        DiagnosedAtAnotherFacilityPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe"
            }

        DiagnosedAtAnotherFacilitySuffix ->
            { english = "at another facility and were given medication. Which medication was given?"
            , kinyarwanda = Just "Ku rindi vuriro wagiyeho ugahabwa imiti. Ni iyihe miti wahawe?"
            }

        DiagnosedByOutsideCare ->
            { english = "Diagnosed by outside care"
            , kinyarwanda = Just "Yasuzumiwe ku rindi vuriro"
            }

        DiagnosedOn ->
            { english = "Diagnosed on"
            , kinyarwanda = Nothing
            }

        Diagnosis ->
            { english = "Diagnosis"
            , kinyarwanda = Just "Uburwayi bwabonetse"
            }

        DiagnosisDate ->
            { english = "Diagnosis Date"
            , kinyarwanda = Just "Itariki y'Isuzuma"
            }

        DifferenceBetweenDueAndDeliveryDates ->
            { english = "Difference between due date and delivery date"
            , kinyarwanda = Just "Ikinyuranyo kiri hagati y'amatariki"
            }

        Disabled ->
            { english = "Disabled"
            , kinyarwanda = Nothing
            }

        DistributionNotice notice ->
            case notice of
                DistributedFully ->
                    { english = "Complete"
                    , kinyarwanda = Nothing
                    }

                DistributedPartiallyLackOfStock ->
                    { english = "Lack of stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    }

                DistributedPartiallyOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Izindi mpamvu"
                    }

        District ->
            { english = "District"
            , kinyarwanda = Just "Akarere"
            }

        DOB ->
            { english = "DOB"
            , kinyarwanda = Nothing
            }

        Done ->
            { english = "Done"
            , kinyarwanda = Nothing
            }

        Downloading ->
            { english = "Downloading"
            , kinyarwanda = Nothing
            }

        DropzoneDefaultMessage ->
            { english = "Touch here to take a photo, or drop a photo file here."
            , kinyarwanda = Just "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
            }

        DueDate ->
            { english = "Due Date"
            , kinyarwanda = Just "Itariki azabyariraho"
            }

        DueTo ->
            { english = "Due to"
            , kinyarwanda = Just "Kubera"
            }

        EarlyChildhoodDevelopment ->
            { english = "Early Childhood Development"
            , kinyarwanda = Just "Gahunda ikomatanije y'imikurire"
            }

        EarlyMastitisOrEngorgmentReliefMethod method ->
            case method of
                ReliefMethodBreastMassage ->
                    { english = "Massage"
                    , kinyarwanda = Just "Korera amabere masage"
                    }

                ReliefMethodIncreaseFluid ->
                    { english = "Increase fluid"
                    , kinyarwanda = Just "Ongera ibyo kunywa"
                    }

                ReliefMethodBreastfeedingOrHandExpression ->
                    { english = "continue breastfeeding or use hand expression"
                    , kinyarwanda = Just "komeza konsa cyangwa ukoreshe ikiganza wikame"
                    }

        ECDSignQuestion sign ->
            case sign of
                FollowMothersEyes ->
                    { english = "Does the child follow their mothers eyes"
                    , kinyarwanda = Just "Umwana akurikiza amaso nyina"
                    }

                MoveArmsAndLegs ->
                    { english = "Does the child move their arms and legs"
                    , kinyarwanda = Just "Umwana anyeganyeza amaboko n'amaguru"
                    }

                RaiseHandsUp ->
                    { english = "Does the child raise their hands up"
                    , kinyarwanda = Just "Umwana azamura ibiganza"
                    }

                Smile ->
                    { english = "Does the child smile"
                    , kinyarwanda = Just "Umwana araseka"
                    }

                RollSideways ->
                    { english = "Does the child roll from left to right and right to left"
                    , kinyarwanda = Just "Umwana yihindukiza ku mpande, iburyo ni’ibumoso ikindi gihe akagana ibumoso n'iburyo"
                    }

                BringHandsToMouth ->
                    { english = "Does the child bring their hands to their mouth"
                    , kinyarwanda = Just "Umwana akoza ibiganza bye ku murwa"
                    }

                HoldHeadWithoutSupport ->
                    { english = "Does the child hold their head steady without support"
                    , kinyarwanda = Just "Umwana abasha kwemesha umutwe we ubwe ntawumufashe"
                    }

                HoldAndShakeToys ->
                    { english = "Does the child hold and shake toys and swing at dangling toys"
                    , kinyarwanda = Just "Umwana ashobora gufata akanazunguza ibikinisho ndetse akabinyeganyeza iyo afite ibikinisho bivuga"
                    }

                ReactToSuddenSounds ->
                    { english = "Does the child react to sudden noises or sounds"
                    , kinyarwanda = Just "Umwana agaragaza ko yumvise amajwi cg urusaku bitunguranye"
                    }

                UseConsonantSounds ->
                    { english = "Is the child using consonant sounds in babbling, for example “da, da, da”"
                    , kinyarwanda = Just "Umwana akoresha amajwi yumvikanamo inyuguti igihe yivugisha, urugero:da,da,da,.."
                    }

                RespondToSoundWithSound ->
                    { english = "Does the child respond to sound by making sound"
                    , kinyarwanda = Just "Umwana asubirisha ijwi igihe yumvise irindi ijwi"
                    }

                TurnHeadWhenCalled ->
                    { english = "Does the child turn their head when their name is called"
                    , kinyarwanda = Just "Umwana ahindukiza umutwe iyo hari uhamagaye izina rye"
                    }

                SitWithoutSupport ->
                    { english = "Can the child sit without support for a short while, for example sit on the floor on their own"
                    , kinyarwanda = Just "Umwana ashobora kwicara akanya gato nta kintu cyangwa umuntu umufashe"
                    }

                SmileBack ->
                    { english = "Does the child smile back at you"
                    , kinyarwanda = Just "Umwana yaba agusekera iyo umusekeye"
                    }

                RollTummyToBack ->
                    { english = "Can the child roll from their tummy to their back on their own"
                    , kinyarwanda = Just "Umwana ashobora kubura inda akagarama nta muntu umufashije"
                    }

                ReachForToys ->
                    { english = "Does the child reach for nearby toys on their own"
                    , kinyarwanda = Just "Umwana ashobora gufata ibikinisho bimwegereye"
                    }

                UseSimpleGestures ->
                    { english = "Does the child use simple gestures such as waving “bye-bye”"
                    , kinyarwanda = Just "Umwana akoresha ibimenyetso byoroheje nko gupepera iyo musezeranaho"
                    }

                StandOnTheirOwn ->
                    { english = "Can the child stand on their own"
                    , kinyarwanda = Just "Umwana ashobora guhagarara nta muntu umufashe"
                    }

                CopyDuringPlay ->
                    { english = "Does the child copy you during play"
                    , kinyarwanda = Just "Umwana yigana ibyo urimo gukora igihe mukina"
                    }

                SayMamaDada ->
                    { english = "Does the child say “mama” and “dada”"
                    , kinyarwanda = Just "Umwana ashobora kuvuga “mama” cyangwa “dada”"
                    }

                CanHoldSmallObjects ->
                    { english = "Can the child hold small objects that fit inside their hands"
                    , kinyarwanda = Just "Umwana ashobora gufata ibintu bito bikwiye mu kiganza cye"
                    }

                LooksWhenPointedAt ->
                    { english = "Does the child look at something when you point to it and say “look”"
                    , kinyarwanda = Just "Umwana ashobora kwerekeza amaso ku kintu cyose umweretse"
                    }

                UseSingleWords ->
                    { english = "Does the child use several single words to get what they want"
                    , kinyarwanda = Just "Umwana akoresha amagambo mato mato kandi yungikanye ashaka kugira icyo akubwira /agusaba"
                    }

                WalkWithoutHelp ->
                    { english = "Does the child walk without help"
                    , kinyarwanda = Just "Umwana ashobora kugenda nta muntu umufashije"
                    }

                PlayPretend ->
                    { english = "Does the child play pretend - like talking on a toy phone"
                    , kinyarwanda = Just "Umwana ajya akina asa nk'uvugira kuri telefoni"
                    }

                PointToThingsOfInterest ->
                    { english = "Does the child point to interesting things"
                    , kinyarwanda = Just "Umwana atunga intoki ibintu bimunejeje"
                    }

                UseShortPhrases ->
                    { english = "Does the child use 2-4 word phrases"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro igizwe n'amagambo 2 kugera kuri 4"
                    }

                InterestedInOtherChildren ->
                    { english = "Does the child show interest in other children"
                    , kinyarwanda = Just "Umwana agaragaza ko yishimiye abandi bana"
                    }

                FollowSimpleInstructions ->
                    { english = "Does the child follow simple instructions"
                    , kinyarwanda = Just "Umwana akurikiza amabwiriza yoroheje ahawe"
                    }

                KickBall ->
                    { english = "Can the child kick a ball"
                    , kinyarwanda = Just "Umwana ashobora gutera umupira"
                    }

                PointAtNamedObjects ->
                    { english = "Does the child point to something - like a toy or a picture - when you name it"
                    , kinyarwanda = Just "Umwana ashobora kukwereka ikintu agitunga urutoki iyo uvuze izina ryacyo, Urugero:Igikinisho cg ifoto"
                    }

                DressThemselves ->
                    { english = "Can the child dress themselves"
                    , kinyarwanda = Just "Umwana ashobora kwiyambika"
                    }

                WashHandsGoToToiled ->
                    { english = "Can the child wash their hands on their own and go to the toilet in the designated area on their own"
                    , kinyarwanda = Just "Umwana ashobora kwikarabya intoki, akanijyana mu bwiherero ahateganijwe wenyine"
                    }

                KnowsColorsAndNumbers ->
                    { english = "Does the child know basic colors and numbers"
                    , kinyarwanda = Just "Umwana azi amabara n'imibare by'ibanze"
                    }

                UseMediumPhrases ->
                    { english = "Does the child use 4-5 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo 2 kugera kuri 4"
                    }

                PlayMakeBelieve ->
                    { english = "Does the child play make-believe"
                    , kinyarwanda = Just "Umwana akunda gukina yigana"
                    }

                FollowThreeStepInstructions ->
                    { english = "Does the child follow 3-step commands - like “get dressed, comb your hair, and wash your face“"
                    , kinyarwanda = Just "Umwana ashobora gukurikiza amabwiriza nka 3 aherewe icyarimwe, Urugero:Ambara, Sokoza umusatsi unakarabe mu maso"
                    }

                StandOnOneFootFiveSeconds ->
                    { english = "Can the child hop and stand on one foot for up to 5 seconds"
                    , kinyarwanda = Just "Umwana ashobora guhagarara ku kaguru kamwe akandi kanenetse mu gihe cy'amasegonda 5"
                    }

                UseLongPhrases ->
                    { english = "Does the child use 5-6 word sentences"
                    , kinyarwanda = Just "Umwana ashobora gukora interuro zigizwe n'amagambo atanu cyangwa atandatu"
                    }

                ShareWithOtherChildren ->
                    { english = "Does the child share and take turns with other children"
                    , kinyarwanda = Just "Umwana ashobora gusangira no kujya ahererekanya ibintu n'abandi bana"
                    }

                CountToTen ->
                    { english = "Can the child count to 10"
                    , kinyarwanda = Just "Umwana ashobora kubara kugeza ku 10"
                    }

                NoECDSigns ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        ECDStatus status ->
            case status of
                StatusOnTrack ->
                    { english = "On Track"
                    , kinyarwanda = Just "Biri ku gihe"
                    }

                StatusECDBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Biri inyuma"
                    }

                StatusOffTrack ->
                    { english = "Off Track"
                    , kinyarwanda = Just "Ntibyakozwe"
                    }

                NoECDStatus ->
                    { english = "No Status"
                    , kinyarwanda = Nothing
                    }

        Edd ->
            { english = "EDD"
            , kinyarwanda = Just "Itariki y'agateganyo yo kubyara"
            }

        EddHeader ->
            { english = "Estimated Date of Delivery"
            , kinyarwanda = Just "Itariki y'agateganyo azabyariraho"
            }

        Edema ->
            { english = "Edema"
            , kinyarwanda = Just "Kubyimba"
            }

        EditRelationship ->
            { english = "Edit Relationship"
            , kinyarwanda = Nothing
            }

        Ega ->
            { english = "EGA"
            , kinyarwanda = Just "Ibyumweru inda imaze"
            }

        EgaHeader ->
            { english = "Estimated Gestational Age"
            , kinyarwanda = Just "Amezi y'agateganyo y'inda"
            }

        EgaWeeks ->
            { english = "EGA (Weeks)"
            , kinyarwanda = Just "EGA (Ibyumweru)"
            }

        ElevatedRespiratoryRate ->
            { english = "Elevated respiratory rate"
            , kinyarwanda = Just "Inshuro ahumeka zazamutse"
            }

        EmergencyReferralHelperReferToHC ->
            { english = "Refer patient to health center immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku kigonderabuzima byihuse"
            }

        EmergencyReferralHelperReferToHospitalForEvaluation ->
            translationSet ReferToHospitalForFurtherEvaluation

        EmergencyReferralHelperReferToHospitalForImmediateDelivery ->
            { english = "Refer patient to hospital for immediate delivery"
            , kinyarwanda = Just "Ohereza umubyeyi aka kanya ku bitaro abyarireyo"
            }

        EmergencyReferralHelperReferToHospitalImmediately ->
            { english = "Refer patient to hospital immediately"
            , kinyarwanda = Just "Ohereza umurwayi ku bitaro byihuse"
            }

        EmergencyReferralHelperReferToMaternityWard ->
            { english = "Refer to Maternity Ward Immediately"
            , kinyarwanda = Just "Ihutire kohereza umubyeyi aho babyarira"
            }

        EmergencyReferralHelperReferToEmergencyObstetricCareServices ->
            { english = "Stabilize and Refer to Emergency Obstetric Care Services"
            , kinyarwanda = Just "Tanga umuti w'ibanze uhite wohereza umubyeyi muri serivice zita ku babyeyi"
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Just ""
            }

        EncounterDate ->
            { english = "Encounter Date"
            , kinyarwanda = Just "Itariki igikorwa cyakoreweho"
            }

        EncounterTypeFollowUpQuestion encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Do you want to start a subsequent Acute Illness encounter for"
                    , kinyarwanda = Just "Urashaka Gutangira Ibikorwa bikurikiyeho ku burwayi bwa"
                    }

                AntenatalEncounter ->
                    { english = "What type of Antenatal encounter would you like to start for"
                    , kinyarwanda = Just "Ni irihe suzuma ku mugore utwite ushaka gutangira kuri"
                    }

                HomeVisitEncounter ->
                    { english = "Do you want to start a Home Visit assessment for"
                    , kinyarwanda = Just "Urashaka gutangira igikorwa cyo gusura mu rugo"
                    }

                InmmunizationEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                WellChildEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                NCDEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        EncounterTypePageLabel page ->
            case page of
                AcuteIllnessPage subPage ->
                    case subPage of
                        OverviewPage ->
                            { english = "Overview"
                            , kinyarwanda = Just "Ishusho Rusange"
                            }

                        Covid19Page ->
                            { english = "COVID-19"
                            , kinyarwanda = Nothing
                            }

                        MalariaPage ->
                            { english = "Malaria"
                            , kinyarwanda = Just "Malariya"
                            }

                        GastroPage ->
                            { english = "Gastro"
                            , kinyarwanda = Just "Indwara yo mu nda"
                            }

                NutritionPage ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'Umwana"
                    }

                AntenatalPage ->
                    { english = "Antenatal Care"
                    , kinyarwanda = Just "Isuzuma ku Mugore Utwite"
                    }

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
                    }

                WarningECDMilestoneReferToSpecialist ->
                    { english = "Missed ECD Milestone" ++ suffix_
                    , kinyarwanda = Nothing
                    }

                WarningHeadCircumferenceMicrocephaly ->
                    { english = "Microcephaly"
                    , kinyarwanda = Nothing
                    }

                WarningHeadCircumferenceMacrocephaly ->
                    { english = "Macrocephaly"
                    , kinyarwanda = Nothing
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        EndEncounter ->
            { english = "End Encounter"
            , kinyarwanda = Just "Rangiza ibyo wakoraga"
            }

        EndEncounterQuestion ->
            { english = "End Encounter?"
            , kinyarwanda = Just "Gusoza igikorwa?"
            }

        EndGroupEncounter ->
            { english = "End Group Encounter"
            , kinyarwanda = Just "Gusoza igikorwa"
            }

        EnrolNewborn ->
            { english = "Enroll Newborn"
            , kinyarwanda = Just "Andika Uruhinja"
            }

        EnrolNewbornHelper enrolled ->
            if enrolled then
                { english = "Newborn is already enrolled"
                , kinyarwanda = Just "Uruhinja rusanzwe rwanditse"
                }

            else
                { english = "Click on 'Enroll Newborn' button to perform enrollment"
                , kinyarwanda = Just "Kanda kuri 'Andika Uruhinja' kugira ngo urwandike"
                }

        EnrollToProgramAction ->
            { english = "Enroll patient in program and direct them to the next program session"
            , kinyarwanda = Nothing
            }

        EnrollToProgramQuestion ->
            { english = "Have you enrolled the patient in the appropriate nutrition program"
            , kinyarwanda = Nothing
            }

        EnterAmountDistributed ->
            { english = "Enter amount distributed"
            , kinyarwanda = Nothing
            }

        EnterPairingCode ->
            { english = "Enter pairing code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            }

        EntryStatusAntenatal status ->
            case status of
                StatusOngoing ->
                    { english = "Open"
                    , kinyarwanda = Nothing
                    }

                StatusResolved ->
                    { english = "Concluded"
                    , kinyarwanda = Just "Byasojwe"
                    }

        EntryStatusDiagnosis status ->
            case status of
                StatusOngoing ->
                    { english = "Ongoing"
                    , kinyarwanda = Nothing
                    }

                StatusResolved ->
                    { english = "Resolved"
                    , kinyarwanda = Nothing
                    }

        MemoryQuota quota ->
            { english = "Memory used " ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya wa memori (ushobora kubika amakuru igihe gito) ungana na MB" ++ String.fromInt (quota.usedJSHeapSize // (1024 * 1024)) ++ " kuri MB" ++ String.fromInt (quota.jsHeapSizeLimit // (1024 * 1024))
            }

        StorageQuota quota ->
            { english = "Storage used " ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " MB of available " ++ String.fromInt (quota.quota // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Just <| "Hamaze gukoreshwa umwanya ungana na MB" ++ String.fromInt (quota.usage // (1024 * 1024)) ++ " umwanya wose ungana na MB" ++ String.fromInt (quota.quota // (1024 * 1024))
            }

        SubmitPairingCode ->
            { english = "Submit Pairing Code"
            , kinyarwanda = Just "Umubare uhuza igikoresho cy'ikoranabuhanga na apulikasiyo"
            }

        EPDSPreformedOn ->
            { english = "EPDS performed on"
            , kinyarwanda = Just "Igipimo cy'ukuntu yiyumva nyuma yo kubyara cyakozwe kuwa"
            }

        EpisiotomyOrPerinealTearQuestion ->
            { english = "Did the patient have an episiotomy or a perineal tear"
            , kinyarwanda = Just "Umubyeyi baramwongereye cg yaracitse abyara"
            }

        EpisiotomyOrPerinealTearHealingQuestion ->
            { english = "Is it healing normally"
            , kinyarwanda = Just "Igisebe kiri gukira neza"
            }

        ErrorCheckLocalConfig ->
            { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly"
            , kinyarwanda = Nothing
            }

        ErrorConfigurationError ->
            { english = "Configuration error"
            , kinyarwanda = Just "Ikosa mu igena miterere"
            }

        Estimated ->
            { english = "Estimated"
            , kinyarwanda = Just "Itariki y'amavuko igenekerejwe"
            }

        ExaminationTask task ->
            case task of
                Vitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    }

                Pages.Prenatal.Activity.Types.NutritionAssessment ->
                    { english = "Nutrition Assessment"
                    , kinyarwanda = Just "Gusuzuma imirire"
                    }

                CorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Just "Isuzuma ryimbitse"
                    }

                ObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Just "Ibipimo by'inda"
                    }

                Pages.Prenatal.Activity.Types.BreastExam ->
                    translationSet BreastExam

                GUExam ->
                    { english = "GU Exam"
                    , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina n'inzira z'inkari"
                    }

        ExaminationTaskRecurrent task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.ExaminationVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    }

        ExposureTask task ->
            case task of
                ExposureTravel ->
                    { english = "Travel History"
                    , kinyarwanda = Just "Amakuru y'ingendo wakoze"
                    }

                ExposureExposure ->
                    { english = "Contact Exposure"
                    , kinyarwanda = Just "Abantu mwahuye"
                    }

        Failure ->
            { english = "Failure"
            , kinyarwanda = Nothing
            }

        Extremities ->
            { english = "Extremities"
            , kinyarwanda = Just "Ku mpera z'ibice by'umubiri (ibiganza,ibirenge)"
            }

        Eyes ->
            { english = "Eyes"
            , kinyarwanda = Just "Amaso"
            }

        Facility ->
            { english = "Facility"
            , kinyarwanda = Just "Ivuriro"
            }

        FamilyInformation ->
            { english = "Family Information"
            , kinyarwanda = Just "Amakuru ku muryango"
            }

        FamilyMembers ->
            { english = "Family Members"
            , kinyarwanda = Just "Abagize umuryango"
            }

        FamilyPlanningCurentlyQuestion ->
            { english = "Which, if any, of the following methods do you use"
            , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha"
            }

        FamilyPlanningInFutureQuestion ->
            { english = "Which, if any, of these methods will you use after your pregnancy"
            , kinyarwanda = Just "Niba buhari, ni ubuhe buryo uzakoresha nyuma yo kubyara?"
            }

        FamilyPlanningSignLabel sign ->
            case sign of
                AutoObservation ->
                    { english = "Auto-observation"
                    , kinyarwanda = Just "Kwigenzura ururenda"
                    }

                Condoms ->
                    { english = "Condoms"
                    , kinyarwanda = Just "Udukingirizo"
                    }

                CycleBeads ->
                    { english = "Cycle beads"
                    , kinyarwanda = Just "Urunigi"
                    }

                CycleCounting ->
                    { english = "Cycle counting"
                    , kinyarwanda = Just "Kubara "
                    }

                Hysterectomy ->
                    { english = "Hysterectomy"
                    , kinyarwanda = Just "Bakuyemo nyababyeyi"
                    }

                Implants ->
                    { english = "Implants"
                    , kinyarwanda = Just "Akapira ko mu kaboko"
                    }

                Injectables ->
                    { english = "Injectables"
                    , kinyarwanda = Just "Urushinge"
                    }

                IUD ->
                    { english = "IUD"
                    , kinyarwanda = Just "Akapira ko mu mura (agapira ko munda ibyara)"
                    }

                LactationAmenorrhea ->
                    { english = "Lactation amenorrhea"
                    , kinyarwanda = Just "Uburyo bwo konsa"
                    }

                NoFamilyPlanning ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta buryo bwo kuboneza urubyaro yahisemo"
                    }

                OralContraceptives ->
                    { english = "Oral contraceptives"
                    , kinyarwanda = Just "Ibinini"
                    }

                Spermicide ->
                    { english = "Spermicide"
                    , kinyarwanda = Just "Ibinini byica intangangabo bicishwa mu gitsina"
                    }

                TubalLigatures ->
                    { english = "Tubal ligatures"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagore"
                    }

                Vasectomy ->
                    { english = "Vasectomy"
                    , kinyarwanda = Just "Gufunga umuyoborantanga ku bagabo"
                    }

        FamilyUbudehe ->
            { english = "Family Ubudehe"
            , kinyarwanda = Just "Icyiciro cy'ubudehe umuryango uherereyemo"
            }

        FbfDistribution clinicType ->
            case clinicType of
                Achi ->
                    { english = "Aheza Distribution"
                    , kinyarwanda = Just "Gutanga Aheza"
                    }

                _ ->
                    { english = "FBF Distribution"
                    , kinyarwanda = Just "Gutanga FBF (Shishakibondo)"
                    }

        FbfToReceive activity amount ->
            case activity of
                MotherActivity _ ->
                    { english = "Mother should receive: " ++ String.fromFloat amount ++ " kgs of CSB++ (FBF)"
                    , kinyarwanda = Nothing
                    }

                ChildActivity _ ->
                    { english = "Child should receive: " ++ String.fromFloat amount ++ " kgs of CSB++ (FBF)"
                    , kinyarwanda = Nothing
                    }

        FatherName ->
            { english = "Father's Name"
            , kinyarwanda = Nothing
            }

        FatherNationalId ->
            { english = "Father's National ID"
            , kinyarwanda = Nothing
            }

        FetalHeartRate ->
            { english = "Fetal Heart Rate"
            , kinyarwanda = Just "Uko umutima w'umwana utera"
            }

        FetalMovement ->
            { english = "Fetal Movement"
            , kinyarwanda = Just "Uko umwana akina mu nda"
            }

        FetalPresentationLabel ->
            { english = "Fetal Presentation"
            , kinyarwanda = Just "Uko umwana ameze mu nda"
            }

        FetalPresentation option ->
            case option of
                FetalBreech ->
                    { english = "Breech"
                    , kinyarwanda = Just "Abanje ikibuno(umwana yaje yicaye)"
                    }

                Cephalic ->
                    { english = "Cephalic"
                    , kinyarwanda = Just "Umwana abanje umutwe"
                    }

                Transverse ->
                    { english = "Transverse"
                    , kinyarwanda = Just "Gitambitse (Umwana aritambitse)"
                    }

                Twins ->
                    { english = "Twins"
                    , kinyarwanda = Just "Impanga"
                    }

                Backend.Measurement.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    }

        Fetch ->
            { english = "Fetch"
            , kinyarwanda = Just "Gushakisha"
            }

        FilterByName ->
            { english = "Filter by name"
            , kinyarwanda = Just "Hitamo izina ryuwo ushaka"
            }

        Finish ->
            { english = "Finish"
            , kinyarwanda = Just "Soza igikorwa"
            }

        FirstAntenatalVisit ->
            { english = "First Antenatal Visit"
            , kinyarwanda = Just "Kwipimisha inda bwa mbere"
            }

        FirstName ->
            { english = "First Name"
            , kinyarwanda = Just "Izina ry'idini"
            }

        FiveVisits ->
            { english = "Five visits"
            , kinyarwanda = Just "Inshuro eshanu"
            }

        FoodGroup group ->
            case group of
                FoodGroupVegetables ->
                    { english = "Vegetables"
                    , kinyarwanda = Nothing
                    }

                FoodGroupCarbohydrates ->
                    { english = "Carbohydrates"
                    , kinyarwanda = Nothing
                    }

                FoodGroupProtein ->
                    { english = "Protein"
                    , kinyarwanda = Nothing
                    }

        FollowPostpartumProtocols ->
            { english = "Follow Postpartum Protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kwita ku mubyeyi wabyaye"
            }

        FollowUpWithPatientIn ->
            { english = "Follow up with patient in"
            , kinyarwanda = Just "Kurikirana umurwayi uri mu bitaro"
            }

        FollowUpWithPatientOn ->
            { english = "Follow up with patient on"
            , kinyarwanda = Just "Gukurikirana Umurwayi Ku itariki"
            }

        FollowUpByChwLabel ->
            { english = "CHW should follow up with patient in"
            , kinyarwanda = Just "Umujyanama w'ubuzima agomba gukurikirana umurwayi mu"
            }

        FollowUpLabel ->
            { english = "Follow up with the patient in"
            , kinyarwanda = Just "Gukurikirana umurwayi mu"
            }

        FollowUpWithMotherLabel ->
            { english = "Follow up with the mother in"
            , kinyarwanda = Just "Gukurikirana umubyeyi mu"
            }

        FollowUpOption option ->
            case option of
                OneDay ->
                    { english = "1 Day"
                    , kinyarwanda = Just "Umunsi 1"
                    }

                ThreeDays ->
                    { english = "3 Days"
                    , kinyarwanda = Just "Iminsi 3"
                    }

                OneWeek ->
                    { english = "1 Week"
                    , kinyarwanda = Just "Icyumweru 1"
                    }

                TwoWeeks ->
                    { english = "2 Weeks"
                    , kinyarwanda = Just "Ibyumweru 2"
                    }

                OneMonths ->
                    { english = "1 Month"
                    , kinyarwanda = Just "Ukwezi 1"
                    }

                TwoMonths ->
                    { english = "2 Months"
                    , kinyarwanda = Just "Amezi 2"
                    }

                ThreeMonths ->
                    { english = "3 Months"
                    , kinyarwanda = Just "Amezi 3"
                    }

        FollowUpDueOption option ->
            case option of
                OverDue ->
                    { english = "Past Due"
                    , kinyarwanda = Just "Itariki yarenze"
                    }

                DueToday ->
                    { english = "Due Today"
                    , kinyarwanda = Just "Itariki yageze uyu munsi"
                    }

                DueThisWeek ->
                    { english = "This week"
                    , kinyarwanda = Just "Iki cyumweru"
                    }

                DueThisMonth ->
                    { english = "This Month"
                    , kinyarwanda = Just "Uku kwezi"
                    }

                DueNextMonth ->
                    { english = "Next Month"
                    , kinyarwanda = Just "Ukwezi gutaha"
                    }

        ForIllustrativePurposesOnly ->
            { english = "For illustrative purposes only"
            , kinyarwanda = Just "Ku mpamvu zumvikana gusa"
            }

        FormError errorValue ->
            translateFormError errorValue

        FormField field ->
            translateFormField field

        FundalHeight ->
            { english = "Fundal Height"
            , kinyarwanda = Just "Uburebure bwa Nyababyeyi"
            }

        Gender gender ->
            case gender of
                Male ->
                    { english = "Male"
                    , kinyarwanda = Just "Gabo"
                    }

                Female ->
                    { english = "Female"
                    , kinyarwanda = Just "Gore"
                    }

        GenderLabel ->
            { english = "Gender"
            , kinyarwanda = Just "Igitsina"
            }

        GestationalDiabetesPreviousPregnancy ->
            { english = "Gestational Diabetes in previous pregnancy"
            , kinyarwanda = Just "Ubushize yarwaye Diyabete itewe no gutwita"
            }

        Glass value ->
            { english = value ++ " Glass"
            , kinyarwanda = Just <| "Ikirahuri " ++ value
            }

        GoHome ->
            { english = "Go to main page"
            , kinyarwanda = Just "Kujya ahabanza"
            }

        GroupAssessment ->
            { english = "Group Encounter"
            , kinyarwanda = Just "Gukorera itsinda"
            }

        GroupEncounter ->
            { english = "Group Encounter"
            , kinyarwanda = Nothing
            }

        Growth ->
            { english = "Growth"
            , kinyarwanda = Just "Imikurire"
            }

        Gravida ->
            { english = "Gravida"
            , kinyarwanda = Just "Inda zose watwise"
            }

        HalfOfDosage dosage ->
            { english = "half of " ++ dosage ++ " dosage"
            , kinyarwanda = Nothing
            }

        HandedReferralFormQuestion ->
            { english = "Did you hand the referral form to the patient"
            , kinyarwanda = Just "Wahaye umurwayi urupapuro rumwohereza"
            }

        HandPallor ->
            { english = "Hand Pallor"
            , kinyarwanda = Just "Ikiganza cyerurutse"
            }

        Hands ->
            { english = "Hands"
            , kinyarwanda = Just "Ibiganza"
            }

        HandsCPESign option ->
            case option of
                PallorHands ->
                    translationSet Pallor

                EdemaHands ->
                    translationSet Edema

                NormalHands ->
                    translationSet Normal

        HCRecommendation recommendation ->
            case recommendation of
                SendAmbulance ->
                    { english = "agreed to call the District Hospital to send an ambulance"
                    , kinyarwanda = Just "bemeranya guhamagara ibitaro ngo byohereze imbangukiragutabara"
                    }

                HomeIsolation ->
                    { english = "advised patient to stay home in isolation"
                    , kinyarwanda = Just "bagira inama umurwayi yo kuguma mu rugo mu kato"
                    }

                ComeToHealthCenter ->
                    { english = "advised patient to go to the health center for further evaluation"
                    , kinyarwanda = Just "kimugira inama yo kujya ku kigo nderabuzima gukoresha isuzuma ryimbitse"
                    }

                ChwMonitoring ->
                    { english = "CHW should continue to monitor"
                    , kinyarwanda = Just "cyemeza ko umujyanama w’ubuzima agomba gukomeza gukurikirana umurwayi"
                    }

                HCRecommendationNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    }

        HCResponseQuestion ->
            { english = "What was the Health Center's response"
            , kinyarwanda = Just "Ni ikihe gisubizo cyavuye ku kigo nderabuzima"
            }

        HCResponsePeriodQuestion ->
            { english = "How long did it take the Health Center to respond"
            , kinyarwanda = Just "Byatwaye igihe kingana gute ngo ikigo nderabuzima gisubize"
            }

        HeadCircumferenceHelper ->
            { english = "Using a tape measure, wrap the tape around the widest possible circumference; above the ears and midway between the eyebrows and the hairline to the occipital prominence on the back of the head."
            , kinyarwanda = Just "Wifashishije metero bushumi kandi umwana aryamye agaramye, zengurutsa iyo metero ku mutwe w'umwana hejuru y'amatwi uhereye inyuma, izenguruke ku gahanga  kugeza ugeze aho watangiriye."
            }

        HeadCircumferenceNotTakenLabel ->
            { english = "Please check if the head circumference was not taken today"
            , kinyarwanda = Just "Reba niba ibipimo by'umuzenguruko w'umutwe bitafashwe uyu munsi"
            }

        HeadHair ->
            { english = "Head/Hair"
            , kinyarwanda = Just "Umutwe/Umusatsi"
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Just "Ikigo Nderabuzima"
            }

        HealthCenterDetermined ->
            { english = "Health center determined this is a"
            , kinyarwanda = Just "Ikigo nderabuzima cyagaragaje ko"
            }

        HealthEducationProvidedQuestion ->
            { english = "Have you provided health education (or anticipatory guidance)"
            , kinyarwanda = Just "Watanze ikiganiro ku buzima (Cyangwa ubujyanama bw'ibanze)"
            }

        HealthInsuranceQuestion ->
            { english = "Do you have health insurance"
            , kinyarwanda = Just "Ufite ubwishingizi bwo kwivuza"
            }

        Heart ->
            { english = "Heart"
            , kinyarwanda = Just "Umutima"
            }

        HeartburnReliefMethod method ->
            case method of
                ReliefMethodAvoidLargeMeals ->
                    { english = "Avoid large, fatty meals"
                    , kinyarwanda = Just "Irinde ibiribwa byinshi, byongera ibinure"
                    }

                ReliefMethodCeaseSmoking ->
                    { english = "Cease smoking "
                    , kinyarwanda = Just "Hagarika kunywa itabi"
                    }

                ReliefMethodAvoidAlcohom ->
                    { english = "Avoid alcohol consumption "
                    , kinyarwanda = Just "Irinde kunywa ibisindisha"
                    }

                ReliefMethodSleepWithHeadRaised ->
                    { english = "Sleep with their head raised in the bed"
                    , kinyarwanda = Just "Gerageza kuryama umutwe wegutse/useguye"
                    }

        HeartburnRecommendedTreatmentHeader ->
            { english = "This patient has signs of persistent heartburn"
            , kinyarwanda = Just "Umubyeyi afite ikirungurira gihoraho"
            }

        HeartburnRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye wo guha uyu murwayi"
            }

        HeartMurmur ->
            { english = "Heart Murmur"
            , kinyarwanda = Just "Ijwi ry'umutima igihe utera"
            }

        HeartCPESign sign ->
            case sign of
                IrregularRhythm ->
                    { english = "Irregular Rhythm"
                    , kinyarwanda = Just "Injyana ihindagurika"
                    }

                NormalRateAndRhythm ->
                    { english = "Normal Rate And Rhythm"
                    , kinyarwanda = Just "Bimeze neza/Injyana imeze neza"
                    }

                SinusTachycardia ->
                    { english = "Sinus Tachycardia"
                    , kinyarwanda = Just "Gutera k'umutima birenze cyane igipimo gisanzwe"
                    }

        HeartRate ->
            { english = "Heart Rate"
            , kinyarwanda = Just "Gutera k'umutima (inshuro umutima utera)"
            }

        Height ->
            { english = "Height"
            , kinyarwanda = Just "Uburebure"
            }

        High ->
            { english = "High"
            , kinyarwanda = Nothing
            }

        HighRiskCase ->
            { english = "high-risk case"
            , kinyarwanda = Just "afite ibyago byinshi byo kuba yaranduye"
            }

        HighRiskCaseHelper ->
            { english = "This patient is a high risk case and should be sent to a hospital for further treatment"
            , kinyarwanda = Just "Uyu murwayi afite ibyago byinshi, agomba koherezwa ku bitaro bakamuha ubuvuzi bwimbitse"
            }

        HighRiskFactor factor ->
            case factor of
                HighRiskConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery and became unconscious after delivery"
                    , kinyarwanda = Nothing
                    }

                HighRiskConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Nothing
                    }

        HighRiskFactors ->
            { english = "High Risk Factors"
            , kinyarwanda = Just "Abafite ibyago byinshi byo"
            }

        HighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BodyTemperature ->
                    { english = "Body Temperature"
                    , kinyarwanda = Just "Ubushyuhe bw'umubiri"
                    }

                Backend.PrenatalActivity.Model.FetalHeartRate ->
                    { english = "No fetal heart rate noted"
                    , kinyarwanda = Just "Umutima w'umwana ntutera"
                    }

                Backend.PrenatalActivity.Model.FetalMovement ->
                    { english = "No fetal movement noted"
                    , kinyarwanda = Just "Umwana ntakina mu nda"
                    }

                Backend.PrenatalActivity.Model.HeartRate ->
                    { english = "Heart Rate"
                    , kinyarwanda = Nothing
                    }

                Backend.PrenatalActivity.Model.RespiratoryRate ->
                    { english = "Respiratory Rate"
                    , kinyarwanda = Just "Inshuro ahumeka"
                    }

        HighSeverityAlerts ->
            { english = "High Severity Alerts"
            , kinyarwanda = Just "Bimenyetso mpuruza bikabije"
            }

        HistoryTask task ->
            case task of
                Obstetric ->
                    { english = "Obstetric History"
                    , kinyarwanda = Just "Amateka y'inda zibanza (ku nda yatwise)"
                    }

                Medical ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    }

                Social ->
                    { english = "Partner Information"
                    , kinyarwanda = Just "Amakuru y'uwo bashakanye (umugabo)"
                    }

                OutsideCare ->
                    { english = "Outside Care"
                    , kinyarwanda = Just "Kuvurirwa ku rindi vuriro"
                    }

        HIV ->
            { english = "HIV"
            , kinyarwanda = Just "Amaguru atameze neza(yagize imitego)"
            }

        HIVPCRResult result ->
            case result of
                ResultSuppressedViralLoad ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    }

                ResultDetectibleViralLoad value ->
                    { english = String.fromFloat value
                    , kinyarwanda = Nothing
                    }

        HIVStatus status ->
            case status of
                HIVExposedInfant ->
                    { english = "HIV-exposed Infant"
                    , kinyarwanda = Just "Umwana uvuka ku mubyeyi ubana n'ubwandu bwa virusi ya SIDA"
                    }

                Negative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                NegativeDiscordantCouple ->
                    { english = "Negative - discordant couple"
                    , kinyarwanda = Just "Nta bwandu afite ariko abana n'ubufite"
                    }

                Positive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    }

                Backend.Person.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntabizi"
                    }

        HIVStatusLabel ->
            { english = "HIV Status"
            , kinyarwanda = Just "Uko ahagaze ku bijyanye n'ubwandu bwa virusi ya SIDA"
            }

        HIVTreatmentSign sign ->
            case sign of
                HIVTreatmentNoMedicineNotSeenAtPMTCT ->
                    { english = "Never seen at PMTCT"
                    , kinyarwanda = Just "Ntiyigeze agera muri PMTCT"
                    }

                HIVTreatmentNoMedicineOutOfStock ->
                    { english = "Stock Out"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    }

                HIVTreatmentNoMedicinePatientRefused ->
                    { english = "Patient Refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    }

                HIVTreatmentNoMedicineOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                -- We do not require translation for other signs.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        Home ->
            { english = "Home"
            , kinyarwanda = Just "Mu rugo"
            }

        HomeVisitActivityTitle activity ->
            case activity of
                Feeding ->
                    { english = "Feeding"
                    , kinyarwanda = Just "Kugaburira umwana"
                    }

                Caring ->
                    { english = "Caring"
                    , kinyarwanda = Just "Kwita ku mwana"
                    }

                Hygiene ->
                    { english = "Hygiene"
                    , kinyarwanda = Just "Isuku"
                    }

                FoodSecurity ->
                    { english = "Food Security"
                    , kinyarwanda = Just "Kwihaza ku biribwa"
                    }

        HouseholdSize ->
            { english = "Household Size"
            , kinyarwanda = Nothing
            }

        HowManyDoses ->
            { english = "How many doses"
            , kinyarwanda = Just "Ingahe"
            }

        HaveAnyOfTheFollowingQuestion ->
            { english = "Do you have any of the following"
            , kinyarwanda = Just "Waba wagize ibi bikurikira?"
            }

        HttpError error ->
            translateHttpError error

        HowManyPerWeek ->
            { english = "How many per week"
            , kinyarwanda = Nothing
            }

        Hypertension ->
            { english = "Hypertension"
            , kinyarwanda = Just "Indwara y'umuvuduko w'amaraso"
            }

        HypertensionAndPregnantHeader ->
            { english = "This patient has Hypertension and is pregnant"
            , kinyarwanda = Nothing
            }

        HypertensionBeforePregnancy ->
            { english = "Hypertension before pregnancy"
            , kinyarwanda = Just "Umuvuduko w'amaraso mbere yo gutwita"
            }

        HypertensionRecommendedTreatmentHeader isChronic ->
            if isChronic then
                { english = "This patient shows signs of Chronic hypertension"
                , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                }

            else
                { english = "This patient shows signs of Pregnancy-Induced hypertension"
                , kinyarwanda = Nothing
                }

        HypertensionRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            }

        HypertensionRecommendedTreatmentUpdateHeader forModeratePreeclamsia ->
            if forModeratePreeclamsia then
                { english = "This patient was previously diagnosed with Moderate Preeclamsia"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize preekalampusi"
                }

            else
                { english = "This patient was previously diagnosed with Hypertension"
                , kinyarwanda = Just "Mu isuzuma rishize uyu mubyeyi yagize umuvuduko w'amaraso"
                }

        HypertensionRecommendedTreatmentUpdateBPLabel ->
            { english = "The patients current BP is"
            , kinyarwanda = Just "Ubu umubyeyi afite umuvuduko w'amaraso ungana na"
            }

        HypertensionRecommendedTreatmentUpdateCurrentTreatment ->
            { english = "The patient is currently prescribed"
            , kinyarwanda = Just "Ubu umubyeyi afata imiti ikurikira"
            }

        HypertensionRecommendedTreatmentUpdateNewTreatment value ->
            case value of
                TreatementUpdateMaintainCurrentDoasage ->
                    { english = "It is recommended that the medication remain unchanged -"
                    , kinyarwanda = Just "Birasabwa ko imiti idahinduka -"
                    }

                TreatementUpdateIncreaseOneDose ->
                    { english = "It is recommended that the medication is increased one dosage level to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze imwe kuri"
                    }

                TreatementUpdateIncreaseTwoDoses ->
                    { english = "It is recommended that the medication is increased two dosage levels to"
                    , kinyarwanda = Just "Birasabwa ko agomba kongererwa doze ebyiri kuri"
                    }

                -- We're not required to view this option.
                TreatementUpdateHospitalize ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        HypertensionRecommendedTreatmentUpdateNoCurrentTreatment ->
            { english = "The patient is currently receiving no treatment"
            , kinyarwanda = Just "Ubu umubyeyi nta muti ari gufata"
            }

        HypertensionRecommendedTreatmentUpdateStartTreatment ->
            { english = "It is recommended to start treatment with"
            , kinyarwanda = Just "Arasabwa gutangirira kuri iyi miti"
            }

        HypertensionStageAndRenalComplicationsHeader renalComplications diagnosis ->
            case diagnosis of
                DiagnosisHypertensionStage1 ->
                    if renalComplications then
                        { english = "This patient has Stage One Hypertension with Renal Complications"
                        , kinyarwanda = Nothing
                        }

                    else
                        { english = "This patient has Stage One Hypertension"
                        , kinyarwanda = Nothing
                        }

                DiagnosisHypertensionStage2 ->
                    if renalComplications then
                        { english = "This patient has Stage Two Hypertension with Renal Complications"
                        , kinyarwanda = Nothing
                        }

                    else
                        { english = "This patient has Stage Two Hypertension"
                        , kinyarwanda = Nothing
                        }

                DiagnosisHypertensionStage3 ->
                    { english = "This patient has Stage Three Hypertension"
                    , kinyarwanda = Nothing
                    }

                -- We should never get here.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        IdleWaitingForSync ->
            { english = "Idle, waiting for next Sync cycle"
            , kinyarwanda = Nothing
            }

        IllnessSymptom symptom ->
            case symptom of
                IllnessSymptomHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    }

                IllnessSymptomVisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Just "Uko areba byahindutse"
                    }

                IllnessSymptomRash ->
                    { english = "Rash on body, feet or hands"
                    , kinyarwanda = Just "Ari kwishimagura ku mubiri: ku birenge cg ibiganza"
                    }

                IllnessSymptomPainlessUlcerMouth ->
                    { english = "Painless ulcer in mouth"
                    , kinyarwanda = Just "Agasebe kataryana mu kanwa"
                    }

                IllnessSymptomPainlessUlcerGenitals ->
                    { english = "Painless ulcer in genital area"
                    , kinyarwanda = Just "Agasebe kataryana mu myanya ndangagitsina"
                    }

                NoIllnessSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        Immunisation ->
            { english = "Immunization"
            , kinyarwanda = Just "Inkingo"
            }

        ImmunisationHistory ->
            { english = "Immunization History"
            , kinyarwanda = Just "Amakuru ku nkingo yafashe"
            }

        IncompleteCervixPreviousPregnancy ->
            { english = "Incomplete Cervix in previous pregnancy"
            , kinyarwanda = Just "Ubushize inkondo y'umura ntiyashoboye kwifunga neza"
            }

        IndexPatient ->
            { english = "Index Patient"
            , kinyarwanda = Just "Umubare w'umurwayi"
            }

        IndividualEncounter ->
            { english = "Individual Encounter"
            , kinyarwanda = Just "Gukorera umuntu umwe"
            }

        IndividualEncounterFirstVisit encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "First Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa cya mbere ku burwayi"
                    }

                AntenatalEncounter ->
                    { english = "First Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mugore utwite"
                    }

                InmmunizationEncounter ->
                    { english = "First Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "First Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma rya mbere ku mirire"
                    }

                HomeVisitEncounter ->
                    { english = "First Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo bwambere"
                    }

                WellChildEncounter ->
                    { english = "First Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura rya mbere ku mwana"
                    }

                NCDEncounter ->
                    { english = "First NCD Encounter"
                    , kinyarwanda = Nothing
                    }

        IndividualEncounterLabel encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Igikorwa ku burwayi butunguranye"
                    }

                AntenatalEncounter ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    }

                HomeVisitEncounter ->
                    { english = "Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo"
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Newborn Exam"
                        , kinyarwanda = Just "Isuzuma ry'uruhinja"
                        }

                    else
                        { english = "Standard Pediatric Visit Encounter"
                        , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                        }

                NCDEncounter ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Nothing
                    }

        IndividualEncounterSelectVisit encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Select Acute Illness Visit"
                    , kinyarwanda = Just "Hitamo inshuro aje kuri ubwo burwayi butunguranye"
                    }

                AntenatalEncounter ->
                    { english = "Select Antenatal Visit"
                    , kinyarwanda = Just "Hitamo isuzuma k’umugore utwite"
                    }

                InmmunizationEncounter ->
                    { english = "Select Inmmunization Visit"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "Select Nutrition Visit"
                    , kinyarwanda = Just "Hitamo isuzuma ry’imirire"
                    }

                HomeVisitEncounter ->
                    { english = "Select Home Visit"
                    , kinyarwanda = Just "Hitamo Gusura Umurwayi"
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Select Newborn Exam Visit"
                        , kinyarwanda = Just "Hitamo isuzuma ry'Uruhinja"
                        }

                    else
                        { english = "Select Standard Pediatric Visit"
                        , kinyarwanda = Just "Hitamo isura ry'umwana"
                        }

                NCDEncounter ->
                    { english = "Select NCD Visit"
                    , kinyarwanda = Nothing
                    }

        IndividualEncounterSubsequentVisit encounterType ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Subsequent Acute Illness Encounter"
                    , kinyarwanda = Just "Ibikorwa bikurikiyeho kuri ubwo burwayi butunguraye"
                    }

                AntenatalEncounter ->
                    { english = "Subsequent Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma rikurikiyeho ku mugore utwite"
                    }

                InmmunizationEncounter ->
                    { english = "Subsequent Inmmunization Encounter"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "Subsequent Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma rikurikiyeho ku mugore utwite"
                    }

                HomeVisitEncounter ->
                    { english = "Subsequent Home Visit"
                    , kinyarwanda = Nothing
                    }

                WellChildEncounter ->
                    { english = "Subsequent Standard Pediatric Visit"
                    , kinyarwanda = Nothing
                    }

                NCDEncounter ->
                    { english = "Subsequent NCD Visit"
                    , kinyarwanda = Nothing
                    }

        IndividualEncounterType encounterType isChw ->
            case encounterType of
                AcuteIllnessEncounter ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    }

                AntenatalEncounter ->
                    { english = "Antenatal Care"
                    , kinyarwanda = Just "Isuzuma ku mugore utwite"
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "Child Nutrition"
                    , kinyarwanda = Just "Imirire y'umwana"
                    }

                HomeVisitEncounter ->
                    { english = "Home Visit"
                    , kinyarwanda = Just "Gusura Umurwayi"
                    }

                WellChildEncounter ->
                    if isChw then
                        { english = "Newborn Exam"
                        , kinyarwanda = Just "Isuzuma ry'uruhinja"
                        }

                    else
                        { english = "Standard Pediatric Visit"
                        , kinyarwanda = Just "Kujyana Umwana mu Isuzumiro"
                        }

                NCDEncounter ->
                    { english = "Noncommunicable Diseases"
                    , kinyarwanda = Nothing
                    }

        IndividualEncounterTypes ->
            { english = "Individual Encounter Types"
            , kinyarwanda = Nothing
            }

        InitialResultsDisplay display ->
            case display of
                InitialResultsHidden ->
                    { english = "Display all mothers / caregivers"
                    , kinyarwanda = Just "Kugaragaza ababyeyi bose / abarezi"
                    }

                InitialResultsShown ->
                    { english = "Hide all mothers / caregivers"
                    , kinyarwanda = Just "Hisha ababyeyi bose / abarezi"
                    }

        IntractableVomiting isIntractable ->
            if isIntractable then
                { english = "Intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bikabije"
                }

            else
                { english = "Non-intractable Vomiting"
                , kinyarwanda = Just "Kuruka Bidakabije"
                }

        IntractableVomitingQuestion ->
            { english = "Is Vomiting Intractable"
            , kinyarwanda = Just "Kuruka bikabije"
            }

        InstructionsChooseOneMedication ->
            { english = "Choose one of the medications from the list to prescribe to the patient"
            , kinyarwanda = Nothing
            }

        InstructionsChooseTwoMedications ->
            { english = "Choose two of the medications from the list to prescribe to the patient"
            , kinyarwanda = Nothing
            }

        IsCurrentlyBreastfeeding ->
            { english = "Is the mother currently breastfeeding her infant"
            , kinyarwanda = Just "Muri iki gihe, umubyeyi yonsa umwana we?"
            }

        IsolatedAtHome ->
            { english = "Isolated at home"
            , kinyarwanda = Just "Yashyizwe mu kato mu rugo"
            }

        KilogramShorthand ->
            { english = "kg"
            , kinyarwanda = Just "kg"
            }

        KilogramsPerMonth ->
            { english = "kgs / month"
            , kinyarwanda = Nothing
            }

        KnownAsPositiveQuestion task ->
            case task of
                TaskHIVTest ->
                    { english = "Is this patient known to be HIV positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite ubwandu bwa virusi itera SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Is this patient known to be Syphilis - RPR positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite uburwayi bwa Mburugu"
                    }

                TaskHepatitisBTest ->
                    { english = "Is this patient known to be Hepatitis B positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara y'umwijima yo mu bwoko bwa B"
                    }

                TaskMalariaTest ->
                    { english = "Is this patient known to be Malaria positive"
                    , kinyarwanda = Just "Uyu murwayi yaba asanzwe azwi ko afite indwara ya Malariya"
                    }

                TaskPregnancyTest ->
                    { english = "Is this patient known to be pregnant"
                    , kinyarwanda = Nothing
                    }

                TaskBloodGpRsTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                TaskUrineDipstickTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                TaskHemoglobinTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                TaskRandomBloodSugarTest ->
                    -- Known as positive is not applicable for this test, therefore,
                    -- no translation is needed.
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskHIVPCRTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskCreatinineTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskLiverFunctionTest ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                -- Known as positive is not applicable for this test, therefore,
                -- no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        KnownPositive ->
            { english = "Known Positive"
            , kinyarwanda = Just "Asanzwe arwaye"
            }

        KnownPositiveHepatitisB ->
            { english = "Known Hepatitis B positive"
            , kinyarwanda = Just "Asanzwe afite indwara y'Umwijima wo mu bwoko bwa B"
            }

        KnownPositiveHIV ->
            { english = "Known HIV positive"
            , kinyarwanda = Just "Asanzwe afite Ubwandu bw'agakoko gatera SIDA"
            }

        LabelOnePregnancyEpisodeOpen ->
            { english = "There is one pregnancy episode that is open"
            , kinyarwanda = Just "Hari isuzuma rigifunguye ku mugore utwite"
            }

        LabelSeenHealthcareProviderForPregnancy ->
            { english = "Have you seen a healthcare provider for current pregnancy"
            , kinyarwanda = Just "Waba warigeze usuzumwa n'umuganga kuri iyinda utwite"
            }

        LabelDocumentPregnancyOutcome ->
            { english = "No - document pregnancy outcome"
            , kinyarwanda = Just "Ntabwo iherezo ry'inda ryanditswe"
            }

        LabHistory ->
            { english = "Lab History"
            , kinyarwanda = Just "Amakuru ku bizamini byakozwe"
            }

        LaboratoryCreatinineCreatinineResult ->
            { english = "Creatinine Result"
            , kinyarwanda = Nothing
            }

        LaboratoryCreatinineBUNResult ->
            { english = "BUN Result"
            , kinyarwanda = Nothing
            }

        LaboratoryLiverFunctionAltResult ->
            { english = "ALT Result"
            , kinyarwanda = Nothing
            }

        LaboratoryLiverFunctionAstResult ->
            { english = "AST Result"
            , kinyarwanda = Nothing
            }

        LaboratoryTask task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Mburugu"
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu Bwoko bwa B"
                    }

                TaskMalariaTest ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini k'Inkari"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Nothing
                    }

                TaskCompletePreviousTests ->
                    { english = "History"
                    , kinyarwanda = Nothing
                    }

        LaboratoryTaskLabel task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse Gipima Virusi Itera SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR"
                    , kinyarwanda = Just "Ikizamini cyihuse gipima Mburugu"
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Ikizamini gipima umwijima wo mu bwoko bwa B"
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT"
                    , kinyarwanda = Just "Ikizamini cyihuse cya Malariya"
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus"
                    , kinyarwanda = Just "Ikizamini cyubwoko bw'amaraso na ReZisi"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ikizamini gipima ingano y'amaraso"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar"
                    , kinyarwanda = Just "Ikizamini gipima ingano y' isukari mu maraso"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR"
                    , kinyarwanda = Nothing
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy"
                    , kinyarwanda = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine"
                    , kinyarwanda = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function"
                    , kinyarwanda = Nothing
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        LaboratoryTaskDate task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV Antibody Test Date"
                    , kinyarwanda = Just "Itariki yakorereweho ikizamini cya Virus itera SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR Test Date"
                    , kinyarwanda = Just "Itariki yakorereweho ikizamini cya Mburugu"
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'umwijima wo mu bwoko bwa B"
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cya Malariya"
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'ubwoka bw'amaraso na Rezisi yayo"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini cy'inkari"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'amaraso"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Date"
                    , kinyarwanda = Just "Itariki yakoreweho ikizamini gipima ingano y'isukari mu maraso"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Date"
                    , kinyarwanda = Just "Itariki y'ikizamini cya PCR gipima virusi itera SIDA"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Date"
                    , kinyarwanda = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Date"
                    , kinyarwanda = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Date"
                    , kinyarwanda = Nothing
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        LaboratoryTaskResult task ->
            case task of
                TaskHIVTest ->
                    { english = "HIV Antibody Test Result"
                    , kinyarwanda = Just "Igisubizo cy'abasirikari barwanya Virusi itera SIDA"
                    }

                TaskSyphilisTest ->
                    { english = "Syphilis - RPR Test Result"
                    , kinyarwanda = Just "Igisubizo cy'ikizamini cya Mburugu"
                    }

                TaskHepatitisBTest ->
                    { english = "Hepatitis B Test Result"
                    , kinyarwanda = Just "Ibisubizo bya hepatite B"
                    }

                TaskMalariaTest ->
                    { english = "Malaria RDT Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya Malariya"
                    }

                TaskBloodGpRsTest ->
                    { english = "Blood Group + Rhesus Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'ubwoka bw'amaraso na Rezisi yayo"
                    }

                TaskUrineDipstickTest ->
                    { english = "Urine Dipstick Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cy'inkari"
                    }

                TaskHemoglobinTest ->
                    { english = "Hemoglobin Test Result"
                    , kinyarwanda = Just "Igisubizo by'ikizamini gipima ingano y'amaraso"
                    }

                TaskRandomBloodSugarTest ->
                    { english = "Random Blood Sugar Test Result"
                    , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
                    }

                TaskHIVPCRTest ->
                    { english = "HIV PCR Test Result"
                    , kinyarwanda = Just "Ibisubizo by'ikizamini cya PCR gipima Virusi itera SIDA"
                    }

                TaskPregnancyTest ->
                    { english = "Pregnancy Test Result"
                    , kinyarwanda = Nothing
                    }

                TaskCreatinineTest ->
                    { english = "Creatinine Test Result"
                    , kinyarwanda = Nothing
                    }

                TaskLiverFunctionTest ->
                    { english = "Liver Function Test Result"
                    , kinyarwanda = Nothing
                    }

                -- Not in use, so no translation is needed.
                TaskCompletePreviousTests ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        LaboratoryTaskResultsHelper ->
            { english = "When ready, update test results via case management"
            , kinyarwanda = Just "Ibisubizo nibiboneka, uhite ubyandika unyuze ku Gukurikirana umurwayi"
            }

        LabResults ->
            { english = "Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
            }

        LabResultsHistoryModeLabel mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    { english = "HIV Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Vursi itera SIDA"
                    }

                LabResultsHistoryHIVPCR _ ->
                    { english = "HIV PCR Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya PCR gipima Virusi itera SIDA"
                    }

                LabResultsHistorySyphilis _ ->
                    { english = "Syphilis Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Mburugu"
                    }

                LabResultsHistoryHepatitisB _ ->
                    { english = "Hepatitis B Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Umwijima wo mu bwoko bwa B"
                    }

                LabResultsHistoryMalaria _ ->
                    { english = "Malaria Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Malariya"
                    }

                LabResultsHistoryProtein _ ->
                    { english = "Protein Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Proteyine"
                    }

                LabResultsHistoryPH _ ->
                    { english = "pH Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya pH"
                    }

                LabResultsHistoryGlucose _ ->
                    { english = "Glucose Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Isukari mu nkari"
                    }

                LabResultsHistoryLeukocytes _ ->
                    { english = "Leukocytes Test History"
                    , kinyarwanda = Just "Amakuru ku kizami cy' Uturemangingo twera"
                    }

                LabResultsHistoryNitrite _ ->
                    { english = "Nitrite Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'Umunyu wapimwe mu nkari"
                    }

                LabResultsHistoryUrobilinogen _ ->
                    { english = "Urobilinogen Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya urobilinogene (mu nkari)"
                    }

                LabResultsHistoryHaemoglobin _ ->
                    { english = "Hemoglobin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'amaraso"
                    }

                LabResultsHistoryKetone _ ->
                    { english = "Ketone Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Ketone mu nkari"
                    }

                LabResultsHistoryBilirubin _ ->
                    { english = "Bilirubin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cya Bililibine (mu nkari)"
                    }

                LabResultsHistoryRandomBloodSugar _ ->
                    { english = "Random Blood Sugar Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'isukari mu maraso"
                    }

                LabResultsHistoryHemoglobin _ ->
                    { english = "Hemoglobin Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ingano y'amaraso"
                    }

                LabResultsHistoryBloodGroup _ ->
                    { english = "Blood Group Test History"
                    , kinyarwanda = Just "Amakuru ku kizamini cy'ubwoko bw'Amaraso"
                    }

                LabResultsHistoryRhesus _ ->
                    { english = "Rhesus Test History"
                    , kinyarwanda = Just "Amakuru kuri kizamini cya Rezisi"
                    }

                LabResultsHistoryCreatinine _ ->
                    { english = "Creatinine Test History"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryBUN _ ->
                    { english = "BUN Test History"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryALT _ ->
                    { english = "ALT Test History"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryAST _ ->
                    { english = "AST Test History"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryPregnancy _ ->
                    { english = "Pregnancy Test History"
                    , kinyarwanda = Nothing
                    }

        LabResultsNormalRange mode ->
            case mode of
                LabResultsHistoryHIV _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryHIVPCR _ ->
                    { english = "<20 copies"
                    , kinyarwanda = Just "Munsi ya kopi 20"
                    }

                LabResultsHistorySyphilis _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryHepatitisB _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryMalaria _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryProtein _ ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryPH _ ->
                    { english = "4.5-8"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryGlucose _ ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryLeukocytes _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryNitrite _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryUrobilinogen _ ->
                    { english = "1 mg/dL or less"
                    , kinyarwanda = Just "1 mg/dl cyangwa munsi"
                    }

                LabResultsHistoryHaemoglobin _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryKetone _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryBilirubin _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                LabResultsHistoryRandomBloodSugar _ ->
                    { english = "74-110 mg/dL"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryHemoglobin _ ->
                    { english = "11-16.5 g/dL"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryBloodGroup _ ->
                    { english = "NA"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryRhesus _ ->
                    { english = "Positive"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryCreatinine _ ->
                    { english = "0.5-1.3 mg/dL"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryBUN _ ->
                    { english = "6-24 mg/dL"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryALT _ ->
                    { english = "7-56 IU/L"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryAST _ ->
                    { english = "8-33 IU/L"
                    , kinyarwanda = Nothing
                    }

                LabResultsHistoryPregnancy _ ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

        LabResultsPaneHeader mode ->
            case mode of
                LabResultsCurrentMain ->
                    { english = "Lab Results"
                    , kinyarwanda = Nothing
                    }

                LabResultsCurrentDipstickShort ->
                    { english = "Short Dipstick Lab Results"
                    , kinyarwanda = Nothing
                    }

                LabResultsCurrentDipstickLong ->
                    { english = "Long Dipstick Lab Results"
                    , kinyarwanda = Nothing
                    }

        LastChecked ->
            { english = "Last checked"
            , kinyarwanda = Just "Isuzuma riheruka"
            }

        LastContacted ->
            { english = "Last Contacted"
            , kinyarwanda = Just "Igihe baheruka guhurira"
            }

        LastSuccesfulContactLabel ->
            { english = "Last Successful Contact"
            , kinyarwanda = Just "Itariki n'isaha yanyuma igikoresho giheruka gukoresherezaho interineti bikagenda neza"
            }

        LeaveEncounter ->
            { english = "Leave Encounter"
            , kinyarwanda = Nothing
            }

        Left ->
            { english = "Left"
            , kinyarwanda = Nothing
            }

        LegCrampsReliefMethod method ->
            case method of
                ReliefMethodMuscleStretching ->
                    { english = "Muscle stretching"
                    , kinyarwanda = Nothing
                    }

                ReliefMethodDorsiflexion ->
                    { english = "Dorsiflexion"
                    , kinyarwanda = Nothing
                    }

                ReliefMethodRelaxation ->
                    { english = "Relaxation"
                    , kinyarwanda = Nothing
                    }

                ReliefMethodSleepWithPillowBetweenLegs ->
                    { english = "Sleep with a pillow between the legs"
                    , kinyarwanda = Nothing
                    }

                ReliefMethodHeatTherapy ->
                    { english = "Heat therapy"
                    , kinyarwanda = Nothing
                    }

                ReliefMethodMassage ->
                    { english = "Massage"
                    , kinyarwanda = Nothing
                    }

        LegLeft ->
            { english = "Left leg"
            , kinyarwanda = Nothing
            }

        LegRight ->
            { english = "Right leg"
            , kinyarwanda = Nothing
            }

        Legs ->
            { english = "Legs"
            , kinyarwanda = Just "Amaguru"
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
            }

        LevelOfEducation educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational Training School"
                    , kinyarwanda = Just "Imyuga"
                    }

                SecondarySchool ->
                    { english = "Secondary School"
                    , kinyarwanda = Just "Ayisumbuye"
                    }

                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    }

                HigherEducation ->
                    { english = "Higher Education (University)"
                    , kinyarwanda = Just "(A0)"
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma"
                    , kinyarwanda = Just "(A1)"
                    }

        LinkToMother ->
            { english = "Link to mother"
            , kinyarwanda = Just "Guhuza n'amakuru y'umubyeyi"
            }

        LiveChildren ->
            { english = "Live Children"
            , kinyarwanda = Just "Abana bariho"
            }

        LmpDateConfirmationLabel ->
            { english = "Please confirm the last menstrual period submitted by the CHW"
            , kinyarwanda = Nothing
            }

        LmpDateConfirmationQuestion ->
            { english = "Do you want to confirm the above LMP"
            , kinyarwanda = Nothing
            }

        LmpDateConfidentHeader ->
            { english = "Is the Patient confident of LMP Date"
            , kinyarwanda = Just "Ese umubyeyi azi neza itariki aherukira mu mihango?"
            }

        LmpDateHeader ->
            { english = "Last Menstrual Period Date"
            , kinyarwanda = Just "Itariki aherukira mu mihango"
            }

        LmpLabel ->
            { english = "Last Menstrual Period"
            , kinyarwanda = Nothing
            }

        LmpRangeHeader ->
            { english = "When was the Patient's Last Menstrual Period"
            , kinyarwanda = Just "Ni ryari umubyeyi aherukira mu mihango?"
            }

        LmpRange range ->
            case range of
                OneMonth ->
                    { english = "Within 1 month"
                    , kinyarwanda = Just "Mu kwezi kumwe"
                    }

                ThreeMonth ->
                    { english = "Within 3 months"
                    , kinyarwanda = Just "Mu mezi atatu"
                    }

                SixMonth ->
                    { english = "Within 6 months"
                    , kinyarwanda = Just "Mu mezi atandatu"
                    }

        Location ->
            { english = "Location"
            , kinyarwanda = Just "Aho Ruzatangirwa"
            }

        LoginPhrase phrase ->
            translateLoginPhrase phrase

        Low ->
            { english = "Low"
            , kinyarwanda = Just "Kwemeza amakosa"
            }

        LowRiskCase ->
            { english = "low-risk case"
            , kinyarwanda = Just "afite ibyago bike byo kuba yaranduye"
            }

        Lungs ->
            { english = "Lungs"
            , kinyarwanda = Just "Ibihaha"
            }

        LungsCPESign option ->
            case option of
                Wheezes ->
                    { english = "Wheezes"
                    , kinyarwanda = Just "Ijwi ryumvikana igihe umuntu ahumeka"
                    }

                Crackles ->
                    { english = "Crackles"
                    , kinyarwanda = Just "Ijwi ryumvikana umuntu ahumeka ariko afite indwara z'ubuhumekero"
                    }

                NormalLungs ->
                    translationSet Normal

        MainIncomeSource source ->
            case source of
                HomeBasedAgriculture ->
                    { english = "Homebased Agriculture / Livestock"
                    , kinyarwanda = Just "Ubuhinzi / Ubworozi"
                    }

                CommercialAgriculture ->
                    { english = "Commercial Agriculture / Livestock"
                    , kinyarwanda = Just "Ubucuruzi bw'imyaka / Amatungo"
                    }

                PublicEmployee ->
                    { english = "Public Employee"
                    , kinyarwanda = Just "Umukozi wa Leta"
                    }

                PrivateBusinessEmpployee ->
                    { english = "Private Business Employee"
                    , kinyarwanda = Just "Umukozi w'igenga"
                    }

        MainIncomeSourceQuestion ->
            { english = "What is the most important source of income for the household"
            , kinyarwanda = Just "Ese nihe urugo rukura ubushobozi bwo gutunga urugo"
            }

        MainWaterSource source ->
            case source of
                PipedWaterToHome ->
                    { english = "Piped Water to Home"
                    , kinyarwanda = Just "Amazi agera mu rugo"
                    }

                PublicWaterTap ->
                    { english = "Public Water Tap"
                    , kinyarwanda = Just "Ivomo rusange"
                    }

                RainWaterCollectionSystem ->
                    { english = "Rain Water Collection System"
                    , kinyarwanda = Just "Amazi y'imvura"
                    }

                NaturalSourceFlowingWater ->
                    { english = "Natural Source - Flowing Water"
                    , kinyarwanda = Just "Umugezi utemba"
                    }

                NaturalSourceStandingWater ->
                    { english = "Natural Source - Standing Water"
                    , kinyarwanda = Just "Amazi y'ibiyaga"
                    }

                BottledWater ->
                    { english = "Bottled Water"
                    , kinyarwanda = Just "Amazi akorwa mu nganda (aza mu macupa)"
                    }

        MainWaterPreparationOption option ->
            case option of
                Boiled ->
                    { english = "Boiled"
                    , kinyarwanda = Just "Barayateka"
                    }

                PurificationSolution ->
                    { english = "Purification solution"
                    , kinyarwanda = Just "Bakoresha umuti usukura amazi"
                    }

                Filtered ->
                    { english = "Filtered"
                    , kinyarwanda = Just "Barayayungurura"
                    }

                Bottled ->
                    { english = "Bottled"
                    , kinyarwanda = Just "Amazi yo mu nganda (afunze mu macupa)"
                    }

                NoWaterPreparationOption ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        MainWaterSourceQuestion ->
            { english = "What is the household's main source of water"
            , kinyarwanda = Just "Ni hehe h'ibanze urugo ruvana amazi"
            }

        MainWaterPreparationQuestion ->
            { english = "How is drinking water prepared"
            , kinyarwanda = Just "Ni gute amazi yo kunywa ategurwa"
            }

        MakeSureYouAreConnected ->
            { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542."
            , kinyarwanda = Just "Banza urebe ko ufite interineti. Ikibazo nigikomeza, hamagara The Ihangane Project kuri +250 788 817 542"
            }

        MalariaRapidDiagnosticTest ->
            { english = "Malaria Rapid Diagnostic Test"
            , kinyarwanda = Just "Igikoresho gipima Malariya ku buryo bwihuse"
            }

        MalariaRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Malaria"
            , kinyarwanda = Just "Uyu murwayi afite agakoko gateram Malariya"
            }

        MalariaRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukwiye ku murwayi"
            }

        MalariaWithGIComplications ->
            { english = "Malaria with GI complications"
            , kinyarwanda = Just "Malariya iherekejwe no guhitwa cyangwa kuruka"
            }

        RapidTestResult result ->
            case result of
                RapidTestNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                RapidTestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    }

                RapidTestPositiveAndPregnant ->
                    { english = "Positive and Pregnant"
                    , kinyarwanda = Just "Afite ubwandu kandi aratwite"
                    }

                RapidTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    }

                RapidTestUnableToRun ->
                    { english = "Unable to run"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    }

                RapidTestUnableToRunAndPregnant ->
                    { english = "Unable to run and Pregnant"
                    , kinyarwanda = Nothing
                    }

        MalnutritionWithComplications ->
            { english = "Malnutrition with complications"
            , kinyarwanda = Just "Imirire mibi n'indwara ziyikomokaho"
            }

        MaritalStatusLabel ->
            { english = "Marital Status"
            , kinyarwanda = Just "Irangamimerere"
            }

        MaritalStatus status ->
            case status of
                Divorced ->
                    { english = "Divorced"
                    , kinyarwanda = Just "Yatandukanye n'uwo bashakanye"
                    }

                Married ->
                    { english = "Married"
                    , kinyarwanda = Just "Arubatse"
                    }

                Single ->
                    { english = "Single"
                    , kinyarwanda = Just "Ingaragu"
                    }

                Widowed ->
                    { english = "Widowed"
                    , kinyarwanda = Just "Umupfakazi"
                    }

        MastitisRecommendedTreatmentHeader ->
            { english = "This patient has Mastitis"
            , kinyarwanda = Just "Uyu mubyeyi afite uburwayi bw'amabere"
            }

        MastitisRecommendedTreatmentHelper ->
            { english = "Select the best treatment option for the patient below"
            , kinyarwanda = Just "Hitamo umuti ukurikira ukwiye kuvura umurwayi"
            }

        MeasurementNoChange ->
            { english = "No Change"
            , kinyarwanda = Just "nta cyahindutse"
            }

        MeasurementGained amount ->
            { english = "Gained " ++ String.fromFloat amount
            , kinyarwanda = Just <| "Kwiyongera " ++ String.fromFloat amount
            }

        MeasurementLost amount ->
            { english = "Lost " ++ String.fromFloat amount
            , kinyarwanda = Just <| "Kwiyongera " ++ String.fromFloat amount
            }

        MedicationCausingHypertension medication ->
            case medication of
                MedicationOestrogens ->
                    { english = "Oestrogens (Family Planning)"
                    , kinyarwanda = Nothing
                    }

                MedicationSteroids ->
                    { english = "Steroids (Prednisolone)"
                    , kinyarwanda = Nothing
                    }

                MedicationAmitriptyline ->
                    { english = "Amitriptyline"
                    , kinyarwanda = Nothing
                    }

                MedicationIbuprofen ->
                    { english = "Ibuprofen (Diclofenac)"
                    , kinyarwanda = Nothing
                    }

                NoMedicationCausingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        MedicationCausingHypertensionQuestion ->
            { english = "Has the patient taken or currently take any of the following hypertension causing medications"
            , kinyarwanda = Nothing
            }

        MedicalCondition condition ->
            case condition of
                MedicalConditionHIV ->
                    { english = "HIV"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionKidneyDisease ->
                    { english = "Kidney Disease"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionPregnancy ->
                    { english = "Pregnancy"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionHypertension ->
                    { english = "Hypertension"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionPregnancyRelatedHypertension ->
                    { english = "Pregnancy Related Hypertension"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionNeuropathy ->
                    { english = "Neuropathy"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionRentalComplications ->
                    { english = "Rental Complications"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionEyeComplications ->
                    { english = "Eye Complications"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionAnemia ->
                    { english = "Anemia"
                    , kinyarwanda = Nothing
                    }

                MedicalConditionOther ->
                    { english = "Other"
                    , kinyarwanda = Nothing
                    }

                NoMedicalConditions ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        MedicalConditionQuestion ->
            { english = "Have you ever been diagnosed with any of these conditions"
            , kinyarwanda = Nothing
            }

        MedicationDistribution ->
            { english = "Medication Distribution"
            , kinyarwanda = Just "Gutanga Imiti"
            }

        MedicationTreatingDiabetes medication ->
            case medication of
                MedicationMetformin ->
                    { english = "Metformin"
                    , kinyarwanda = Nothing
                    }

                MedicationGlibenclamide ->
                    { english = "Glibenclamide"
                    , kinyarwanda = Nothing
                    }

                MedicationInsulin ->
                    { english = "Insulin"
                    , kinyarwanda = Nothing
                    }

                NoMedicationTreatingDiabetes ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        MedicationTreatingDiabetesQuestion ->
            { english = "Have you ever been diagnosed with any of these conditions"
            , kinyarwanda = Nothing
            }

        MedicationTreatingHypertension medication ->
            case medication of
                MedicationAceInhibitors ->
                    { english = "Ace-Inhibitors (Example: Captopril)"
                    , kinyarwanda = Nothing
                    }

                MedicationARBs ->
                    { english = "Angiotensine Receptor Blockers (ARBs)"
                    , kinyarwanda = Nothing
                    }

                MedicationHCTZ ->
                    { english = "HCTZ"
                    , kinyarwanda = Nothing
                    }

                MedicationCalciumChannelBlockers ->
                    { english = "Calcium Channel Blockers"
                    , kinyarwanda = Nothing
                    }

                MedicationMethyldopa ->
                    { english = "Methyldopa"
                    , kinyarwanda = Nothing
                    }

                MedicationBetaBlockers ->
                    { english = "Beta-Blockers"
                    , kinyarwanda = Nothing
                    }

                MedicationHydralazine ->
                    { english = "Hydralazine"
                    , kinyarwanda = Nothing
                    }

                NoMedicationTreatingHypertension ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        MedicationTreatingHypertensionQuestion ->
            { english = "Have you ever been diagnosed with any of these conditions"
            , kinyarwanda = Nothing
            }

        MedicalDiagnosis ->
            { english = "Medical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe na Muganga"
            }

        MedicalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisUterineMyoma ->
                    { english = "Uterine Myoma"
                    , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
                    }

                Backend.PrenatalActivity.Model.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    }

                DiagnosisCardiacDisease ->
                    { english = "Cardiac Disease"
                    , kinyarwanda = Just "Indwara z'umutima"
                    }

                DiagnosisRenalDisease ->
                    { english = "Renal Disease"
                    , kinyarwanda = Just "Indwara z'impyiko"
                    }

                DiagnosisHypertensionBeforePregnancy ->
                    { english = "Hypertension"
                    , kinyarwanda = Nothing
                    }

                Backend.PrenatalActivity.Model.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    }

                DiagnosisAsthma ->
                    { english = "Asthma"
                    , kinyarwanda = Just "Asthma (Agahema)"
                    }

                DiagnosisBowedLegs ->
                    { english = "Bowed Legs"
                    , kinyarwanda = Just "Amaguru atameze neza (yagize imitego)"
                    }

                DiagnosisKnownHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virus itera SIDA"
                    }

                DiagnosisMentalHealthHistory ->
                    { english = "History of Mental Health Problems"
                    , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
                    }

        MedicationCausesSideEffectsQuestion ->
            { english = "Did you experience adverse events of the medication"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            }

        MedicationDistributionHelperAnemia ->
            { english = "Patient shows signs of Mild - Moderate Anemia"
            , kinyarwanda = Just "Umurwayi afite amaraso make byoroheje"
            }

        MedicationDistributionHelperDiscordantPartnership ->
            { english = "This patient is part of a discordant partnership"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite"
            }

        MedicationDistributionHelperDiscordantPartnershipNoARVs ->
            { english = "This patient is part of a discordant partnership in which the partner is not on ARVs"
            , kinyarwanda = Just "Uwo babana afite ubwandu bwa Virusi itera SIDA ariko umubyeyi we ntabwo afite kandi ntago afata imiti igabanya ubukana"
            }

        MedicationDistributionHelperEarlyMastitisOrEngorgment ->
            { english = "This patient has signs of Early Mastitis or Engorgement"
            , kinyarwanda = Just "Uyu mubyeyi afite ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
            }

        MedicationDistributionHelperHIV ->
            { english = "This patient is HIV positive"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bute"
            }

        MedicationDistributionHelperMebendazole ->
            { english = "This patient is over 24 weeks EGA and has not had a dewormer in the last 6 months"
            , kinyarwanda = Just "Uyu mubyeyi atwite inda y'ibyumweru 24 kandi nta muti w'inzoka yafashe mu mezi 6 ashize"
            }

        MedicationDistributionHelperGonorrhea ->
            { english = "This patient has signs of possible Gonorrhea"
            , kinyarwanda = Just "Uyu mubyeyi agaragaza ibimenyetso by'umitezi"
            }

        MedicationDistributionHelperTrichomonasOrBacterialVaginosis ->
            { english = "This patient has signs of possible Trichomonas or Bacterial Vaginosis"
            , kinyarwanda = Just "Umubyeyii afite ibimenyetso bishobora kuba ari ibya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
            }

        MedicationDistributionHelperVitaminA ->
            { english = "This patient did not receive Vitamin A"
            , kinyarwanda = Just "Uyu mubyeyi ntiyahawe Vitamine A"
            }

        MedicationDistributionNoticeGonorrhea ->
            { english = "Note: It is also recommended to prescribe the partner"
            , kinyarwanda = Just "Icyitonderwa: Ni ngombwa kuvura uwo babana/bashakanye"
            }

        MedicationDistributionNoticeGonorrheaPartnerMed1 ->
            { english = "Ciprofloxacin (1000mg): by mouth as a single dose"
            , kinyarwanda = Just "Kunywa ikinini cya Ciplofoloxacine (1000mg) inshuro imwe"
            }

        MedicationDistributionNoticeGonorrheaPartnerMed2 ->
            { english = "Doxycycline (100mg): by mouth 2x a day for 7 days"
            , kinyarwanda = Just "Kunywa ikinini cya Doxycycline (100mg) inshuro ebyri ku munsi mu minsi irindwi"
            }

        MedicationDistributionSign sign ->
            case sign of
                Amoxicillin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Amoxicillin"
                    , kinyarwanda = Nothing
                    }

                Coartem ->
                    { english = "Coartem"
                    , kinyarwanda = Just "Kowariteme"
                    }

                ORS ->
                    { english = "Oral Rehydration Solution (ORS)"
                    , kinyarwanda = Just "SRO"
                    }

                Zinc ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Zinc"
                    , kinyarwanda = Nothing
                    }

                LemonJuiceOrHoney ->
                    { english = "Lemon Juice and/or Honey"
                    , kinyarwanda = Just "Umutobe w'indimu n'ubuki"
                    }

                Albendazole ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    }

                Mebendezole ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    }

                VitaminA ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    }

                Paracetamol ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Paracetamol"
                    , kinyarwanda = Nothing
                    }

                Tenofovir ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Tenofovir"
                    , kinyarwanda = Nothing
                    }

                Lamivudine ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Lamivudine"
                    , kinyarwanda = Nothing
                    }

                Dolutegravir ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Dolutegravir"
                    , kinyarwanda = Nothing
                    }

                TDF3TC ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "TDF + 3TC"
                    , kinyarwanda = Nothing
                    }

                Iron ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Iron"
                    , kinyarwanda = Just "Fer"
                    }

                FolicAcid ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Folic Acid"
                    , kinyarwanda = Nothing
                    }

                Ceftriaxone ->
                    { english = "Ceftriaxone"
                    , kinyarwanda = Nothing
                    }

                Azithromycin ->
                    { english = "Azithromycin"
                    , kinyarwanda = Nothing
                    }

                Metronidazole ->
                    { english = "Metronidazole"
                    , kinyarwanda = Nothing
                    }

                NoMedicationDistributionSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                NoMedicationDistributionSignsInitialPhase ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                NoMedicationDistributionSignsRecurrentPhase ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        MedicationDoxycycline ->
            -- Names of Medication, therefore,
            -- no translation is needed.
            { english = "Doxycycline"
            , kinyarwanda = Nothing
            }

        MedicationDosesMissedQuestion ->
            { english = "Did you miss any doses of medications"
            , kinyarwanda = Just "Waba hari imiti wibagiwe gufata"
            }

        MedicationForFeverPast6Hours ->
            { english = "Patient took medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Umurwayi yanyoye imiti y’umuriro mu masaha atandatu ashize"
            }

        MedicationHelpedEnding helped ->
            if helped then
                { english = "and improved"
                , kinyarwanda = Just "none yorohewe"
                }

            else
                { english = "but no improvement"
                , kinyarwanda = Just "ariko ntiyorohewe"
                }

        MedicationFeelBetterAfterTakingQuestion ->
            { english = "Do you feel better after taking medications"
            , kinyarwanda = Just "Wumva umeze neza nyuma yo gufata imiti"
            }

        MedicationForMalariaToday ->
            { english = "Patient received medication for malaria today before this visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya uyu munsi mbere yuko aza mu isuzuma"
            }

        MedicationForMalariaPastMonth ->
            { english = "Patient received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Umurwayi yahawe imiti ya malariya mu kwezi gushize mbere yuko aza mu isuzuma uyu munsi "
            }

        MedicalFormHelper ->
            { english = "Please record if the mother was diagnosed with the following medical issues"
            , kinyarwanda = Just "Andika niba umubyeyi yaragaragaweho indwara zikurikira"
            }

        MedicationForFeverPast6HoursQuestion ->
            { english = "Have you taken any medication to treat a fever in the past six hours"
            , kinyarwanda = Just "Hari imiti y'umuriro waba wafashe mu masaha atandatu ashize"
            }

        MedicationForMalariaTodayQuestion ->
            { english = "Did you receive medication for malaria today before this visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba wanyoye mbere y'uko uza kwivuza"
            }

        MedicationForMalariaWithinPastMonthQuestion ->
            { english = "Have you received medication for malaria within the past month before today's visit"
            , kinyarwanda = Just "Hari imiti ivura Maraliya waba waranyoye mukwezi gushize mbere yuko uza hano kwivuza"
            }

        MedicationHelpedQuestion ->
            { english = "Do you feel better after taking this"
            , kinyarwanda = Just "Urumva umeze neza nyuma yo kunywa iyi miti"
            }

        MedicationTaken ->
            { english = "Medication taken"
            , kinyarwanda = Just "Imiti yafashe"
            }

        MedicationTakenAsPrescribedQuestion ->
            { english = "Did you take the medication as prescribed"
            , kinyarwanda = Just "Wafashe imiti neza uko wayandikiwe na muganga"
            }

        MentalHealthHistory ->
            { english = "History of Mental Health Problems"
            , kinyarwanda = Just "Niba yaragize uburwayi bwo mumutwe"
            }

        MMHGUnit ->
            { english = "mmHG"
            , kinyarwanda = Nothing
            }

        MiddleName ->
            { english = "Middle Name"
            , kinyarwanda = Nothing
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
            }

        MissedDosesOfMedicatgion val ->
            if val == 0 then
                { english = "No missed doses of medication"
                , kinyarwanda = Just "Yafashe kandi arangiza neza imiti uko yayandikiwe"
                }

            else
                { english = "Missed " ++ String.fromInt val ++ " doses of medication"
                , kinyarwanda = Just <| "Yasimbutse gufata imiti inshuro " ++ String.fromInt val
                }

        ModeOfDelivery mode ->
            case mode of
                VaginalDelivery (Spontaneous True) ->
                    { english = "Spontaneous vaginal delivery with episiotomy"
                    , kinyarwanda = Just "Yabyaye neza ariko bamwongereye"
                    }

                VaginalDelivery (Spontaneous False) ->
                    { english = "Spontaneous vaginal delivery without episiotomy"
                    , kinyarwanda = Just "Yabyaye neza"
                    }

                VaginalDelivery WithVacuumExtraction ->
                    { english = "Vaginal delivery with vacuum extraction"
                    , kinyarwanda = Just "Yabyaye neza ariko hanifashishijwe icyuma gikurura umwana"
                    }

                CesareanDelivery ->
                    { english = "Cesarean delivery"
                    , kinyarwanda = Just "Yabyaye bamubaze"
                    }

        ModeOfDeliveryLabel ->
            { english = "Mode of delivery"
            , kinyarwanda = Just "Uburyo yabyayemo"
            }

        ModeratelyUnderweight ->
            { english = "Moderately Underweight"
            , kinyarwanda = Just "Imirire mibi yoroheje ku biro"
            }

        ModeratePreeclampsia ->
            { english = "Moderate Preeclampsia"
            , kinyarwanda = Just "Preklampusi Yoroheje"
            }

        Month ->
            { english = "Month"
            , kinyarwanda = Just "Ukwezi"
            }

        MonthAbbrev ->
            { english = "mo"
            , kinyarwanda = Just "am"
            }

        MonthSinglePlural value ->
            if value == 1 then
                { english = "1 Month"
                , kinyarwanda = Just "Ukwezi 1"
                }

            else
                { english = String.fromInt value ++ " Months"
                , kinyarwanda = Just <| "Amezi " ++ String.fromInt value
                }

        MonthsOld ->
            { english = "months old"
            , kinyarwanda = Just "Amezi"
            }

        Mother ->
            { english = "Mother"
            , kinyarwanda = Just "Umubyeyi"
            }

        MotherDemographicInformation ->
            { english = "Mother Demographic Information"
            , kinyarwanda = Nothing
            }

        MotherName name ->
            { english = "Mother/Caregiver: " ++ name
            , kinyarwanda = Just <| "Umubyeyi: " ++ name
            }

        MotherNameLabel ->
            { english = "Mother's Name"
            , kinyarwanda = Nothing
            }

        MotherNationalId ->
            { english = "Mother's National ID"
            , kinyarwanda = Nothing
            }

        Mothers ->
            { english = "Mothers"
            , kinyarwanda = Just "Ababyeyi"
            }

        MUAC ->
            { english = "MUAC"
            , kinyarwanda = Just "Ikizigira"
            }

        MuacHelper ->
            { english = "Make sure to measure at the center of the baby’s upper arm."
            , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
            }

        MyAccount ->
            { english = "My Account"
            , kinyarwanda = Just "Konti yanjye"
            }

        MyRelatedBy relationship ->
            translateMyRelatedBy relationship

        MyRelatedByQuestion relationship ->
            translateMyRelatedByQuestion relationship

        Name ->
            { english = "Name"
            , kinyarwanda = Nothing
            }

        NationalIdNumber ->
            { english = "National ID Number"
            , kinyarwanda = Just "Numero y'irangamuntu"
            }

        NCDASignQyestion sign ->
            case sign of
                NCDABornUnderweight ->
                    { english = "Was the child born underweight"
                    , kinyarwanda = Nothing
                    }

                NCDABornWithBirthDefect ->
                    { english = "Was the child born with a birth defect"
                    , kinyarwanda = Nothing
                    }

                NCDAAppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 months)"
                    , kinyarwanda = Nothing
                    }

                NCDAOngeraMNP ->
                    { english = "Did you receive and use Ongera-MNP"
                    , kinyarwanda = Nothing
                    }

                NCDAFiveFoodGroups ->
                    { english = "Does the child receive food items from the 5 food groups?"
                    , kinyarwanda = Nothing
                    }

                NCDAMealFrequency6to8Months ->
                    { english = "Feed your young child complementary foods 2 to 3 times a day"
                    , kinyarwanda = Nothing
                    }

                NCDAMealFrequency9to11Months ->
                    { english = "Feed your young child complementary foods 3 to 4 times a day"
                    , kinyarwanda = Nothing
                    }

                NCDAMealFrequency12MonthsOrMore ->
                    { english = "Feed your young child complementary foods at least 5 times a day"
                    , kinyarwanda = Nothing
                    }

                NCDASupportChildWithDisability ->
                    { english = "Provide support to a child with a disability"
                    , kinyarwanda = Nothing
                    }

                NCDAConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Nothing
                    }

                NCDAConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Nothing
                    }

                NCDAHasCleanWater ->
                    { english = "Does the house have clean water"
                    , kinyarwanda = Nothing
                    }

                NCDAHasHandwashingFacility ->
                    { english = "Does the house have a handwashing facility"
                    , kinyarwanda = Nothing
                    }

                NCDAHasToilets ->
                    { english = "Does the household have toilets"
                    , kinyarwanda = Nothing
                    }

                NCDAHasKitchenGarden ->
                    { english = "Does the house have a kitchen garden"
                    , kinyarwanda = Nothing
                    }

                NoNCDASigns ->
                    { english = "None"
                    , kinyarwanda = Nothing
                    }

        NCDActivityTitle activity ->
            case activity of
                Backend.NCDActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    }

                Backend.NCDActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    }

                Backend.NCDActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    }

                Backend.NCDActivity.Model.MedicalHistory ->
                    { english = "Medical History"
                    , kinyarwanda = Just "Amateka y'uburwayi busanzwe"
                    }

                Backend.NCDActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    }

                Backend.NCDActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

                Backend.NCDActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    }

        NCDANCServicesInstructions ->
            { english = "Refer patient to ANC services for further management of hypertension during pregnancy"
            , kinyarwanda = Nothing
            }

        NCDDangerSign sign ->
            case sign of
                Dyspnea ->
                    { english = "Acute Shortness of Breath (Dyspnea)"
                    , kinyarwanda = Nothing
                    }

                VisionChanges ->
                    { english = "Vision Changes"
                    , kinyarwanda = Nothing
                    }

                ChestPain ->
                    { english = "Chest Pain"
                    , kinyarwanda = Nothing
                    }

                FlankPain ->
                    { english = "Flank Pain"
                    , kinyarwanda = Nothing
                    }

                Hematuria ->
                    { english = "Blood in Urine (Hematuria)"
                    , kinyarwanda = Nothing
                    }

                SevereHeadaches ->
                    { english = "Severe Headaches"
                    , kinyarwanda = Nothing
                    }

                LossOfConciousness ->
                    { english = "Loss of Conciousness Since Last Visit"
                    , kinyarwanda = Nothing
                    }

                NoNCDDangerSigns ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        NCDExaminationTask task ->
            case task of
                TaskCoreExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Just "Isuzuma ryimbitse"
                    }

                TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibimenyetso by'ubuzima"
                    }

        NCDFamilyHistorySignQuestion sign ->
            case sign of
                SignHypertensionHistory ->
                    { english = "Has anyone in your family been told they have hypertension"
                    , kinyarwanda = Nothing
                    }

                SignHeartProblemHistory ->
                    { english = "Has anyone in your family been told they have a problem with their heart"
                    , kinyarwanda = Nothing
                    }

                SignDiabetesHistory ->
                    { english = "Has anyone in your family been told they have a problem with diabetes"
                    , kinyarwanda = Nothing
                    }

                NoNCDFamilyHistorySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NCDGuidanceSignQuestion sign ->
            case sign of
                ReturnInOneMonth ->
                    { english = "Have you advised the patient to return in one month for a check-up"
                    , kinyarwanda = Nothing
                    }

                NoNCDGuidanceSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NCDHealthEducationHeader ->
            { english = "Stage One Hypertension"
            , kinyarwanda = Nothing
            }

        NCDHealthEducationInstructions ->
            { english = "Council patient on lifestyle changes and the root causes of hypertension"
            , kinyarwanda = Nothing
            }

        NCDHealthEducationQuestion ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Nothing
            }

        NCDLabsCaseManagementEntryTypeResults ->
            { english = "NCD Lab Results"
            , kinyarwanda = Nothing
            }

        NCDMedicalHistoryTask task ->
            case task of
                TaskCoMorbidities ->
                    { english = "Co-Morbidities"
                    , kinyarwanda = Nothing
                    }

                TaskMedicationHistory ->
                    { english = "Medication History"
                    , kinyarwanda = Nothing
                    }

                TaskSocialHistory ->
                    { english = "Social History"
                    , kinyarwanda = Nothing
                    }

                TaskFamilyHistory ->
                    { english = "Family History"
                    , kinyarwanda = Nothing
                    }

                TaskOutsideCare ->
                    { english = "Outside Care"
                    , kinyarwanda = Nothing
                    }

        NCDNextStepsTask task ->
            case task of
                Pages.NCD.Activity.Types.TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                Pages.NCD.Activity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.Activity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    }

        NCDGroup1Symptom symptom ->
            case symptom of
                SwellingInLegs ->
                    { english = "Swelling in Legs"
                    , kinyarwanda = Nothing
                    }

                UrinaryFrequency ->
                    { english = "Urinary Frequency"
                    , kinyarwanda = Nothing
                    }

                Anxiety ->
                    { english = "Anxiety"
                    , kinyarwanda = Nothing
                    }

                WeightLoss ->
                    { english = "Weight Loss"
                    , kinyarwanda = Nothing
                    }

                Palpitations ->
                    { english = "Palpitations"
                    , kinyarwanda = Nothing
                    }

                Tremor ->
                    { english = "Tremor"
                    , kinyarwanda = Nothing
                    }

                SwellingInFace ->
                    { english = "Swelling in Face"
                    , kinyarwanda = Nothing
                    }

                SwellingInAbdomen ->
                    { english = "Swelling in Abdomen"
                    , kinyarwanda = Nothing
                    }

                DizzinessWithChangingPosition ->
                    { english = "Dizziness with Changing Position"
                    , kinyarwanda = Nothing
                    }

                MildHeadache ->
                    { english = "Mild Headache"
                    , kinyarwanda = Nothing
                    }

                NoNCDGroup1Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        NCDGroup2Symptom symptom ->
            case symptom of
                WeaknessOfOneSideOfTheBody ->
                    { english = "Weakness of One Side of the Body"
                    , kinyarwanda = Nothing
                    }

                ProblemsWithWalking ->
                    { english = "Problems with Walking"
                    , kinyarwanda = Nothing
                    }

                ProblemsWithTalking ->
                    { english = "Problems with Talking"
                    , kinyarwanda = Nothing
                    }

                DecreasedVision ->
                    { english = "Decreased Vision"
                    , kinyarwanda = Nothing
                    }

                BlurryVision ->
                    { english = "Blurry Vision"
                    , kinyarwanda = Nothing
                    }

                IncreasedFatigueWithDailyActivities ->
                    { english = "Increased Fatigue with Daily Activities"
                    , kinyarwanda = Nothing
                    }

                ShortOfBreathWhenLayingDown ->
                    { english = "Short of Breath When Laying Down"
                    , kinyarwanda = Nothing
                    }

                ShortOfBreathAtNight ->
                    { english = "Short of Breath at Night"
                    , kinyarwanda = Nothing
                    }

                KidneyProblems ->
                    { english = "Kidney Problems"
                    , kinyarwanda = Nothing
                    }

                NCDIncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Nothing
                    }

                NoNCDGroup2Symptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        NCDPainSymptom symptom ->
            case symptom of
                PainFlank ->
                    { english = "Flank"
                    , kinyarwanda = Nothing
                    }

                PainLowerBack ->
                    { english = "Lower Back"
                    , kinyarwanda = Nothing
                    }

                PainFeet ->
                    { english = "Feet"
                    , kinyarwanda = Nothing
                    }

                PainNeck ->
                    { english = "Neck"
                    , kinyarwanda = Nothing
                    }

                PainAbdomen ->
                    { english = "Abdomen"
                    , kinyarwanda = Nothing
                    }

                NoNCDPainSymptoms ->
                    { english = "None of the Above"
                    , kinyarwanda = Nothing
                    }

        NCDProgressReport ->
            { english = "NCD Progress Report"
            , kinyarwanda = Nothing
            }

        NCDRecurrentActivitiesTitle activity ->
            case activity of
                Backend.NCDActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    }

                Backend.NCDActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

        NCDRecurrentNextStepsTask task ->
            case task of
                Pages.NCD.RecurrentActivity.Types.TaskMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.NCD.RecurrentActivity.Types.TaskReferral ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    }

        NCDRiskFactor value ->
            case value of
                RiskFactorSmokeCigarettes ->
                    { english = "Smokes Cigarettes"
                    , kinyarwanda = Nothing
                    }

                RiskFactorConsumeSalt ->
                    { english = "Adds Salt to Diet"
                    , kinyarwanda = Nothing
                    }

                RiskFactorHypertensionHistory ->
                    { english = "Family History of Hypertension"
                    , kinyarwanda = Nothing
                    }

                RiskFactorHearProblemHistory ->
                    { english = "Family History of Heart Problems"
                    , kinyarwanda = Nothing
                    }

                RiskFactorDiabetesHistory ->
                    { english = "Family History of Diabetes"
                    , kinyarwanda = Nothing
                    }

        NCDSocialHistoryFoodQuestion ->
            { english = "What foods do you eat most"
            , kinyarwanda = Nothing
            }

        NCDSocialHistoryFoodQuestionInstructions ->
            { english = "Please check the most fitting group"
            , kinyarwanda = Nothing
            }

        NCDSocialHistorySignQuestion sign ->
            case sign of
                SignDrinkAlcohol ->
                    { english = "Do you drink any alcoholic beverages"
                    , kinyarwanda = Nothing
                    }

                SignSmokeCigarettes ->
                    { english = "Do you smoke cigarettes"
                    , kinyarwanda = Nothing
                    }

                SignConsumeSalt ->
                    { english = "Do you add salt to your food"
                    , kinyarwanda = Nothing
                    }

                SignDifficult4TimesAYear ->
                    { english = "Would it be difficult for you to come to the health center 4 times a year"
                    , kinyarwanda = Nothing
                    }

                SignHelpWithTreatmentAtHome ->
                    { english = "Are there people at home who can help you with treatment"
                    , kinyarwanda = Nothing
                    }

                NoNCDSocialHistorySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        Neck ->
            { english = "Neck"
            , kinyarwanda = Just "Ijosi"
            }

        NeckCPESign option ->
            case option of
                EnlargedThyroid ->
                    { english = "Enlarged Thyroid"
                    , kinyarwanda = Just "Umwingo"
                    }

                EnlargedLymphNodes ->
                    { english = "Enlarged Lymph Nodes"
                    , kinyarwanda = Just "Inturugunyu/Amatakara"
                    }

                NormalNeck ->
                    translationSet Normal

        NegativeLabel ->
            { english = "Negative"
            , kinyarwanda = Just "Nta bwandu afite"
            }

        Never ->
            { english = "Never"
            , kinyarwanda = Nothing
            }

        Next ->
            { english = "Next"
            , kinyarwanda = Just "Ibikurikiyeho"
            }

        NextAppointment ->
            { english = "Next Appointment"
            , kinyarwanda = Just "Itariki yo kugarukaho"
            }

        NextDue ->
            { english = "Next Due"
            , kinyarwanda = Just "itariki azahabwaho urukingo rukurikira"
            }

        NextDoseDue ->
            { english = "Next Dose Due"
            , kinyarwanda = Nothing
            }

        NextImmunisationVisit ->
            { english = "Next immunization visit"
            , kinyarwanda = Nothing
            }

        NextPediatricVisit ->
            { english = "Next pediatric visit"
            , kinyarwanda = Nothing
            }

        NextSteps ->
            { english = "Next Steps"
            , kinyarwanda = Just "Ibikurikiyeho"
            }

        NextStepsTask isChw task ->
            case task of
                NextStepsIsolation ->
                    if isChw then
                        { english = "Isolate Patient"
                        , kinyarwanda = Just "Shyira umurwayi mu kato"
                        }

                    else
                        { english = "Monitor at Home"
                        , kinyarwanda = Just "Gukurikiranira umurwayi mu rugo"
                        }

                NextStepsContactHC ->
                    { english = "Contact Health Center"
                    , kinyarwanda = Just "Menyesha ikigo nderabuzima"
                    }

                NextStepsCall114 ->
                    { english = "Call 114"
                    , kinyarwanda = Just "Hamagara 114"
                    }

                NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.AcuteIllness.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        }

                    else
                        { english = "Refer to Hospital"
                        , kinyarwanda = Just "Ohereza ku Bitaro"
                        }

                Pages.AcuteIllness.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

                Pages.AcuteIllness.Activity.Types.NextStepsContactTracing ->
                    { english = "Contact Tracing"
                    , kinyarwanda = Just "Gushakisha abahuye n'uwanduye"
                    }

                NextStepsSymptomsReliefGuidance ->
                    -- We qualify it as Medication distribution, to keep
                    -- consistant with other types of Covid steps.
                    translationSet MedicationDistribution

        No ->
            { english = "No"
            , kinyarwanda = Just "Oya"
            }

        NoActivitiesCompleted ->
            { english = "No activities are entirely completed for the attending participants."
            , kinyarwanda = Just "Nta gikorwa cyarangiye cyose kubitabiriye."
            }

        NoActivitiesPending ->
            { english = "All activities are completed for the attending participants."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            }

        NoActivitiesCompletedForThisParticipant ->
            { english = "No activities are completed for this participant."
            , kinyarwanda = Just "Nta gikorwa cyarangiye kubitabiriye."
            }

        NoActivitiesPendingForThisParticipant ->
            { english = "All activities are completed for this participant."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            }

        NoContactReason reason ->
            case reason of
                ReasonNoAnswer ->
                    { english = "Did not answer"
                    , kinyarwanda = Just "Ntago yitabye"
                    }

                ReasonWrongContactInfo ->
                    { english = "Wrong contact information"
                    , kinyarwanda = Just "Amakuru atariyo"
                    }

                ReasonDeclinedFollowUp ->
                    { english = "Declined Follow Up"
                    , kinyarwanda = Just "Yanze gukurikiranwa"
                    }

        NoGroupsFound ->
            { english = "No groups found."
            , kinyarwanda = Nothing
            }

        NoMatchesFound ->
            { english = "No matches found"
            , kinyarwanda = Just "Ibyo wifuza ntibiboneste"
            }

        NormalRange ->
            { english = "Normal Range"
            , kinyarwanda = Just "Ibimeze neza"
            }

        NoTreatmentAdministered ->
            { english = "No treatment administered"
            , kinyarwanda = Just "Nta muti watanzwe"
            }

        NoTreatmentRecorded ->
            { english = "No treatment recorded"
            , kinyarwanda = Just "Nta muti yanditswe"
            }

        NutritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            }

        ReasonForNonReferral reason ->
            case reason of
                ClientRefused ->
                    { english = "Client refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    }

                NoAmbulance ->
                    { english = "No ambulance available"
                    , kinyarwanda = Just "Nta mbangukiragutabara ihari"
                    }

                ClientUnableToAffordFees ->
                    { english = "Client unable to afford fees"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    }

                ClientAlreadyInCare ->
                    { english = "Client already in care"
                    , kinyarwanda = Just "Umukiriya ari kwitabwaho"
                    }

                ReasonForNonReferralNotIndicated ->
                    { english = "Not indicated"
                    , kinyarwanda = Just "Ntibyasabwe"
                    }

                ReasonForNonReferralOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoReasonForNonReferral ->
                    { english = "No Reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    }

        AdministrationNote note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Nta miti iri mu bubiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy"
                    , kinyarwanda = Just "Uyu muti usanzwe umutera ifurutwa"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    }

                NonAdministrationHomeBirth ->
                    { english = "Home Birth"
                    , kinyarwanda = Nothing
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                AdministeredToday ->
                    { english = "Administered Today"
                    , kinyarwanda = Nothing
                    }

                AdministeredPreviously ->
                    { english = "Already Received"
                    , kinyarwanda = Just "Byamaze kwakirwa"
                    }

        AdministrationNoteForPrenatalImmunisation note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Patient Declined"
                    , kinyarwanda = Just "Umurwayi yanze"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                -- Other options are not relevant for Immunisation.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        AdministrationNoteForWellChildImmunisation note ->
            case note of
                NonAdministrationLackOfStock ->
                    { english = "Out of Stock"
                    , kinyarwanda = Just "Byashize mu bubiko"
                    }

                NonAdministrationKnownAllergy ->
                    { english = "Known Allergy or Reaction"
                    , kinyarwanda = Just "Agira ingaruka zizwi kubera uru rukingo/umuti"
                    }

                NonAdministrationPatientDeclined ->
                    { english = "Mother / Caregiver Declined"
                    , kinyarwanda = Just "Umubyeyi / Umurezi yanze"
                    }

                NonAdministrationPatientUnableToAfford ->
                    { english = "Patient Unable to Afford"
                    , kinyarwanda = Just "Nta bushobozi bwo kwishyura afite"
                    }

                NonAdministrationTooIll ->
                    { english = "Too Sick"
                    , kinyarwanda = Just "Ararembye"
                    }

                NonAdministrationOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                -- Other options are not relevant for Immunisation.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NoParticipantsCompleted ->
            { english = "No participants have completed all their activities yet."
            , kinyarwanda = Just "Ntagikorwa nakimwe kirarangira kubitabiriye."
            }

        NoParticipantsPending ->
            { english = "All attending participants have completed their activities."
            , kinyarwanda = Just "Abaje bose barangirijwe"
            }

        NoParticipantsCompletedForThisActivity ->
            { english = "No participants have completed this activity yet."
            , kinyarwanda = Just "Ntawaje warangirijwe kukorerwa."
            }

        NoParticipantsPendingForThisActivity ->
            { english = "All attending participants have completed this activitity."
            , kinyarwanda = Just "Ababje bose barangirijwe."
            }

        Normal ->
            { english = "Normal"
            , kinyarwanda = Just "Bimeze neza/Nta kibazo gihari"
            }

        NoChildrenRegisteredInTheSystem ->
            { english = "No children registered in the system"
            , kinyarwanda = Just "Ntamwana wanditswe muriyi sisiteme"
            }

        NoParticipantsFound ->
            { english = "No participants found"
            , kinyarwanda = Just "Ntamuntu ugaragaye"
            }

        NotAvailable ->
            { english = "not available"
            , kinyarwanda = Just "Ntibiboneste"
            }

        NotConnected ->
            { english = "Not Connected"
            , kinyarwanda = Just "Ntamurandasi"
            }

        NotFollowingRecommendationQuestion ->
            { english = "Why recommendations were not followed"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            }

        NotTaken ->
            { english = "Not taken"
            , kinyarwanda = Just "Nta bipimo byafashwe"
            }

        NumberOfAbortions ->
            { english = "Number of Abortions"
            , kinyarwanda = Just "Umubare w'inda zavuyemo"
            }

        NumberOfChildrenUnder5 ->
            { english = "Number of Children under 5"
            , kinyarwanda = Just "Umubare w'abana bari munsi y'imyaka 5"
            }

        NumberOfCSections ->
            { english = "Number of C-Sections"
            , kinyarwanda = Just "Umubare w'inshuro yabazwe"
            }

        NumberOfLiveChildren ->
            { english = "Number of Live Children"
            , kinyarwanda = Just "Umubare w'abana bariho"
            }

        NumberOfStillbirthsAtTerm ->
            { english = "Number of Stillbirths at Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda bashyitse"
            }

        NumberOfStillbirthsPreTerm ->
            { english = "Number of Stillbirths pre Term"
            , kinyarwanda = Just "Umubare w'abapfiriye mu nda badashyitse"
            }

        NutritionActivityHelper activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe gupima ikizigira cy'akaboko"
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    }

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

        NutritionActivityTitle activity ->
            case activity of
                Backend.NutritionActivity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                Backend.NutritionActivity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                Backend.NutritionActivity.Model.Nutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

                Backend.NutritionActivity.Model.Photo ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                Backend.NutritionActivity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

                Backend.NutritionActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

        NutritionAssessment assessment ->
            case assessment of
                AssesmentAcuteMalnutritionModerate ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi yoroheje ije vuba"
                    }

                AssesmentAcuteMalnutritionSevere ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Just "Imirire  mibi ikabije ije vuba"
                    }

                AssesmentUnderweightModerate ->
                    { english = "Moderately Underweight"
                    , kinyarwanda = Just "Imirire mibi yoroheje ku biro"
                    }

                AssesmentUnderweightSevere ->
                    { english = "Severely Underweight"
                    , kinyarwanda = Just "Imirire mibi ikabije ku biro"
                    }

                AssesmentDangerSignsNotPresent ->
                    { english = "Without Danger Signs"
                    , kinyarwanda = Just "Nta bimenyetso mpuruza"
                    }

                AssesmentDangerSignsPresent ->
                    { english = "With Danger Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    }

                AssesmentMalnutritionSigns _ ->
                    { english = "Malnutrition Signs"
                    , kinyarwanda = Just "Ifite ibimenyetso mpuruza"
                    }

                AssesmentConsecutiveWeightLoss ->
                    { english = "Consecutive Weight Loss"
                    , kinyarwanda = Just "Gutakaza ibiro mu buryo bwikurikiranije"
                    }

                NoNutritionAssessment ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        NutritionAssessmentTask task ->
            case task of
                TaskHeight ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                TaskHeadCircumference ->
                    { english = "Head Circumference"
                    , kinyarwanda = Just "Umuzenguruko w'umutwe"
                    }

                TaskMuac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                TaskNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

                TaskWeight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

        NutritionCaringOption option ->
            case option of
                CaredByParent ->
                    { english = "Parent"
                    , kinyarwanda = Just "Umubyeyi"
                    }

                CaredByGrandparent ->
                    { english = "Grandparent"
                    , kinyarwanda = Just "Nyirakuru/Sekuru"
                    }

                CaredBySibling ->
                    { english = "Sibling"
                    , kinyarwanda = Just "Umuvandimwe"
                    }

                CaredByNeighbor ->
                    { english = "Neighbor"
                    , kinyarwanda = Just "Umuturanyi"
                    }

                CaredByHouseHelper ->
                    { english = "House helper"
                    , kinyarwanda = Just "Umukozi wo mu rugo"
                    }

                CaredByDaycare ->
                    { english = "Daycare"
                    , kinyarwanda = Just "Irerero"
                    }

        NutritionFeedingSignQuestion sign ->
            case sign of
                ReceiveSupplement ->
                    { english = "Did you receive food supplement"
                    , kinyarwanda = Just "Waba warahawe inyongeramirire"
                    }

                RationPresentAtHome ->
                    { english = "Is the ration of the food supplement present in the home"
                    , kinyarwanda = Just "Haba hari inyongeramirire usigaranye mu nzu"
                    }

                EnoughTillNextSession ->
                    { english = "Is the available food supplement enough to last until the next health center session"
                    , kinyarwanda = Just "Iyo nyongeramiriee ufite yaba ihagije kugeza igihe uzasubirira ku kigonderabuzima"
                    }

                SupplementShared ->
                    { english = "Is the food supplement being shared or eaten only by the sick child"
                    , kinyarwanda = Just "Ese inyongeramirire yaba ifatwa n'umwana urwaye gusa cyangwa yaba ayisangira n'abandi"
                    }

                EncouragedToEat ->
                    { english = "Does someone help / encourage the sick child to eat"
                    , kinyarwanda = Just "Hari umuntu waba afasha cyangwa ashishikariza umwana kurya"
                    }

                RefusingToEat ->
                    { english = "Is the child refusing to eat"
                    , kinyarwanda = Just "Ese umwana yanga kurya"
                    }

                FeedingSignBreastfeeding ->
                    { english = "Is the child currently breastfeeding (for children < 2)"
                    , kinyarwanda = Just "Umwana yaba yonka (ku bana bari munsi y'imyaka 2)"
                    }

                CleanWaterAvailable ->
                    { english = "Is clean water available"
                    , kinyarwanda = Just "Ese mazi asukuye arahari"
                    }

                EatenWithWater ->
                    { english = "Is water given to the child when eating the food supplement"
                    , kinyarwanda = Just "Ese umwana yaba ahabwa amazi yo kunwa igihe afata inyongeramirire"
                    }

                NoNutritionFeedingSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NutritionFoodSecuritySignQuestion sign ->
            case sign of
                HouseholdGotFood ->
                    { english = "Does the household currently have food available"
                    , kinyarwanda = Just "Ese ubu urugo rufite ibyo kurya"
                    }

                NoNutritionFoodSecuritySigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NutritionHelper ->
            { english = "Explain to the mother how to check the malnutrition signs for their own child."
            , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye."
            }

        NutritionHygieneSignQuestion sign ->
            case sign of
                SoapInTheHouse ->
                    { english = "Is there soap for washing in the house"
                    , kinyarwanda = Just "Ese mu rugo haba hari isabune yo koga"
                    }

                WashHandsBeforeFeeding ->
                    { english = "Do the caregiver and child wash hands before the child is fed"
                    , kinyarwanda = Just "Ese umurezi n'umwana bakaraba intoki mbere y'uko umwana agaburirwa"
                    }

                FoodCovered ->
                    { english = "Is the food / RUTF covered and free from flies"
                    , kinyarwanda = Just "Ese ibiryo/RUTUFU birapfundikiye kandi nta sazi zibiriho"
                    }

                NoNutritionHygieneSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        NutritionNextStepsTask task ->
            case task of
                Measurement.Model.NextStepsSendToHC ->
                    { english = "Send to Health Center"
                    , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                    }

                Measurement.Model.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                NextStepContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    }

                NextStepFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

        NutritionSupplementType type_ ->
            case type_ of
                FortifiedPorridge ->
                    { english = "Fortified Porridge"
                    , kinyarwanda = Just "Igikoma kirimo Imyunyu ngugu na Vitamin"
                    }

                Rutf ->
                    { english = "RUTF"
                    , kinyarwanda = Just "RUTUFU"
                    }

                Ongera ->
                    { english = "Ongera intungamubiri at the village level / CHW"
                    , kinyarwanda = Just "Ongera Intungamubiri mu mudugudu/Ku mujyanama w'Ubuzima"
                    }

                TherapeuticMilk ->
                    { english = "Therapeutic Milk"
                    , kinyarwanda = Just "Amata avura"
                    }

                NoNutritionSupplementType ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        NitritionSigns ->
            { english = "Nutrition Signs"
            , kinyarwanda = Just "Ibimenyetso by'imirire"
            }

        ObstetricalDiagnosis ->
            { english = "Obstetrical Diagnosis"
            , kinyarwanda = Just "Uburwayi bwemejwe n'inzobere mu gusuzuma abagore batwite"
            }

        ObstetricalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisRhNegative ->
                    { english = "Patient is RH Negative"
                    , kinyarwanda = Nothing
                    }

                DiagnosisModerateUnderweight ->
                    { english = "Moderate underweight"
                    , kinyarwanda = Just "Ibiro bike bidakabije ugendeye ku myaka"
                    }

                DiagnosisSevereUnderweight ->
                    { english = "Severe underweight"
                    , kinyarwanda = Just "Afite ibiro bikie bikabije"
                    }

                DiagnosisOverweight ->
                    { english = "Overweight"
                    , kinyarwanda = Nothing
                    }

                DiagnosisObese ->
                    { english = "Obese"
                    , kinyarwanda = Just "Kubyibuha gukabije"
                    }

                DisgnosisPeripheralEdema ->
                    { english = "Peripheral Edema"
                    , kinyarwanda = Nothing
                    }

                DiagnosisFetusBreech ->
                    { english = "Fetus is in breech"
                    , kinyarwanda = Nothing
                    }

                DiagnosisFetusTransverse ->
                    { english = "Fetus is transverse"
                    , kinyarwanda = Nothing
                    }

                DiagnosisBreastExamination ->
                    { english = "Breast exam showed"
                    , kinyarwanda = Nothing
                    }

                DiagnosisHypotension ->
                    { english = "Hypotension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisPregnancyInducedHypertension ->
                    { english = "Pregnancy-induced hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisPreeclampsiaHighRisk ->
                    { english = "High Risk for Preeclampsia"
                    , kinyarwanda = Nothing
                    }

        OK ->
            { english = "OK"
            , kinyarwanda = Just "Nibyo, yego"
            }

        Old ->
            { english = "old"
            , kinyarwanda = Just "imyaka"
            }

        On ->
            { english = "On"
            , kinyarwanda = Just "Ku itariki"
            }

        OneVisit ->
            { english = "One visit"
            , kinyarwanda = Just "Inshuro imwe"
            }

        OnceYouEndTheEncounter ->
            { english = "Once you end the Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe cyose urangije igikorwa ,nta bushobozi wongera kugira bwo guhindura ibyo winjije cyangwa amakuru."
            }

        OnceYouEndYourGroupEncounter ->
            { english = "Once you end your Group Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Just "Igihe ushoze igikorwa, ntabwo ushobora guhindura cg wongeremo andi makuru."
            }

        OngoingTreatmentTask task ->
            case task of
                OngoingTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    }

        OnlySickChild ->
            { english = "Only Sick Child"
            , kinyarwanda = Just "Umwana urwaye gusa"
            }

        Or ->
            { english = "or"
            , kinyarwanda = Just "cyangwa"
            }

        PackagesPerMonth ->
            { english = "packages / month"
            , kinyarwanda = Just "Amapaki ku kwezi"
            }

        Page ->
            { english = "Page"
            , kinyarwanda = Just "Paji"
            }

        Page404 ->
            { english = "404 page"
            , kinyarwanda = Just "404 paji"
            }

        PageNotFoundMsg ->
            { english = "Sorry, nothing found in this URL."
            , kinyarwanda = Just "Mutwihanganire ntabwo ubufasha mwasabye mubashije kuboneka."
            }

        Pallor ->
            { english = "Pallor"
            , kinyarwanda = Just "Kweruruka (k'urugingo rw'umubiri)"
            }

        Para ->
            { english = "Para"
            , kinyarwanda = Just "Imbyaro"
            }

        ParacetamolPrescriptionForAdult ->
            { english = "Every 4-6 hours as needed. Not to exceed 4g in 24h."
            , kinyarwanda = Nothing
            }

        ParentsAliveAndHealthyQuestion ->
            { english = "Are both parents alive and healthy"
            , kinyarwanda = Just "Ese ababyeyi bombi bariho kandi bafite ubuzima bwiza"
            }

        PaleConjuctiva ->
            { english = "Pale Conjuctiva"
            , kinyarwanda = Just "Ibihenehene byeruruka"
            }

        PartialPlacentaPreviousDelivery ->
            { english = "Partial Placenta in previous delivery"
            , kinyarwanda = Just "Ubwo aheruka kubyara iya nyuma ntiyavuyeyo  yose (yaje igice)"
            }

        ParticipantDirectory ->
            { english = "Participant Directory"
            , kinyarwanda = Just "Ububiko bw'amakuru y'umurwayi"
            }

        Participants ->
            { english = "Participants"
            , kinyarwanda = Just "Ubwitabire"
            }

        ParticipantReviewed ->
            { english = "I have reviewed and understand the above."
            , kinyarwanda = Nothing
            }

        ParticipantSignature ->
            { english = "Participant Signature"
            , kinyarwanda = Nothing
            }

        ParticipantSummary ->
            { english = "Participant Summary"
            , kinyarwanda = Just "Umwirondoro w’Umwana"
            }

        ParticipantDemographicInformation ->
            { english = "Participant Demographic Information"
            , kinyarwanda = Just "Umwirondoro w'umugenerwabikorwa"
            }

        ParticipantInformation ->
            { english = "Participant Information"
            , kinyarwanda = Nothing
            }

        PartnerHivTestResult ->
            { english = "What was the partners HIV Test result"
            , kinyarwanda = Just "Ni ikihe gisubizo cy'ubwandu bwa Virusi itera SIDA kuwo babana?"
            }

        PartnerReceivedHivCounseling ->
            { english = "Did partner receive HIV Counseling during this pregnancy"
            , kinyarwanda = Just "Umugabo yahawe ubujyanama kuri Virusi itera SIDA? "
            }

        PartnerReceivedHivTesting ->
            { english = "Did partner receive HIV Testing during this pregnancy"
            , kinyarwanda = Just "Umugabo  yasuzumwe Virusi itera SIDA?"
            }

        PastDiagnosisReportReason ->
            { english = "As a result of entering lab results from past encounter"
            , kinyarwanda = Nothing
            }

        PatientDiagnosedWithLabel ->
            { english = "The patient has been diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwo"
            }

        PatientExhibitAnyFindings ->
            { english = "Does the patient exhibit any of these findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bikurikira"
            }

        PatientExhibitAnyRespiratoryFindings ->
            { english = "Does the patient exhibit any of these Respiratory findings"
            , kinyarwanda = Just "Umurwayi agaragaza bimwe muri ibi bimenyetso by'ubuhumekero"
            }

        PatientGotAnyDangerSigns ->
            { english = "Does the patient have any of these danger signs"
            , kinyarwanda = Nothing
            }

        PatientGotAnySymptoms ->
            { english = "Does the patient have any of these symptoms"
            , kinyarwanda = Just "Umurwayi yaba afite bimwe muri ibi bimenyetso"
            }

        PatientGotPainAnywhewre ->
            { english = "Does the patient have pain anywhere"
            , kinyarwanda = Nothing
            }

        PatientGotDiabetesHeader ->
            { english = "This patient has Diabetes"
            , kinyarwanda = Nothing
            }

        PatientGotDiabetesByGlucoseHeader fasting value ->
            if fasting then
                { english = "This patient has Diabetes with fasting glucose levels of " ++ " " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Nothing
                }

            else
                { english = "This patient has Diabetes with non-fasting glucose levels of " ++ " " ++ String.fromFloat value ++ " mg/dL"
                , kinyarwanda = Nothing
                }

        PatientGotDiabetesByUrineDip value ->
            { english = "This patient has Diabetes with a urine dip glucose levels of " ++ value
            , kinyarwanda = Nothing
            }

        PatientProgress ->
            { english = "Patient Progress"
            , kinyarwanda = Just "Uruhererekane rw'ibyakorewe umubyeyi"
            }

        PatientRecord ->
            { english = "Patient Record"
            , kinyarwanda = Just "Amakuru y'Umurwayi"
            }

        PatientInformation ->
            { english = "Patient Information"
            , kinyarwanda = Just "Amakuru k'umurwayi"
            }

        PatientIsolatedQuestion isChw ->
            if isChw then
                { english = "Have you isolated the patient"
                , kinyarwanda = Just "Washyize umurwayi mu kato"
                }

            else
                { english = "Is the patient able to self-isolate at home"
                , kinyarwanda = Just "Umurwayi ashobora kwishyira mu kato ka wenyine mu rugo"
                }

        PatientNotYetSeenAtHCLabel ->
            { english = " has not yet been seen at the health center for this pregnancy"
            , kinyarwanda = Just " ntiyigeze asuzumwa ku kigo nderabuzima kuri iyi nda atwite"
            }

        PatientRecordFilter filter ->
            case filter of
                Pages.PatientRecord.Model.FilterAcuteIllness ->
                    { english = "Acute Illness"
                    , kinyarwanda = Just "Uburwayi butunguranye"
                    }

                Pages.PatientRecord.Model.FilterAntenatal ->
                    { english = "Antenatal Care"
                    , kinyarwanda = Just "Isuzuma ku mugore utwite"
                    }

                FilterDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Just "Umwirondoro"
                    }

                FilterFamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    }

        PauseEncounter ->
            { english = "Pause Encounter"
            , kinyarwanda = Just "Igikorwa cyahagaritswe"
            }

        PatientShowsNoSignsOfCovid ->
            { english = "Patient shows no signs of Covid"
            , kinyarwanda = Nothing
            }

        PediatricVisit ->
            { english = "Pediatric Visit"
            , kinyarwanda = Nothing
            }

        PediatricCareMilestone milestone ->
            case milestone of
                Milestone6Weeks ->
                    { english = "6 weeks"
                    , kinyarwanda = Nothing
                    }

                Milestone14Weeks ->
                    { english = "14 weeks"
                    , kinyarwanda = Nothing
                    }

                Milestone6Months ->
                    { english = "6 months"
                    , kinyarwanda = Nothing
                    }

                Milestone9Months ->
                    { english = "9 months"
                    , kinyarwanda = Nothing
                    }

                Milestone12Months ->
                    { english = "12 months"
                    , kinyarwanda = Nothing
                    }

                Milestone15Months ->
                    { english = "15 months"
                    , kinyarwanda = Nothing
                    }

                Milestone18Months ->
                    { english = "18 months"
                    , kinyarwanda = Nothing
                    }

                Milestone2Years ->
                    { english = "2 years"
                    , kinyarwanda = Nothing
                    }

                Milestone3Years ->
                    { english = "3 years"
                    , kinyarwanda = Nothing
                    }

                Milestone4Years ->
                    { english = "4 years"
                    , kinyarwanda = Nothing
                    }

        People ->
            { english = "People"
            , kinyarwanda = Just "Abantu"
            }

        PersistentStorage authorized ->
            if authorized then
                { english = "Persistent storage has been authorized. The browser will not delete locally cached data without your approval."
                , kinyarwanda = Just "Ububiko buhoraho bwaremejwe,amakuru wabitse ntabwo yatsibama udatanze uburenganzira/utabyemeje"
                }

            else
                { english = "Persistent storage has not been authorized. The browser may delete locally cached data if storage runs low."
                , kinyarwanda = Just "Ibikwa ry'amakuru ntabwo remejwe. Sisiteme mushakisha ukoreramo ishobora kubisiba umwanya ubaye muto."
                }

        Person ->
            { english = "Person"
            , kinyarwanda = Just "Umuntu"
            }

        PersonHasBeenSaved ->
            { english = "Person has been saved"
            , kinyarwanda = Just "Amakuru kuri uyu muntu yabitswe"
            }

        PertinentSymptoms ->
            { english = "Pertinent Symptoms"
            , kinyarwanda = Just " Ibimenyetso by'ingenzi"
            }

        PhotosTransferStatus ->
            { english = "Photos Transfer Status"
            , kinyarwanda = Nothing
            }

        PhysicalExam ->
            { english = "Physical Exam"
            , kinyarwanda = Just "Gusuzuma umurwayi"
            }

        PhysicalExamTask task ->
            case task of
                PhysicalExamVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    }

                PhysicalExamCoreExam ->
                    { english = "Core Exam"
                    , kinyarwanda = Just "Ikizamini cy'ingenzi"
                    }

                PhysicalExamMuac ->
                    { english = "Muac"
                    , kinyarwanda = Just "Ikizigira"
                    }

                PhysicalExamAcuteFindings ->
                    { english = "Acute Findings"
                    , kinyarwanda = Just "Ibimenyetso biziyeho"
                    }

                PhysicalExamNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

        PlaceholderEnterHeight ->
            { english = "Enter height here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            }

        PlaceholderEnterMUAC ->
            { english = "Enter MUAC here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            }

        PlaceholderEnterParticipantName ->
            { english = "Enter participant name here"
            , kinyarwanda = Just "Andika izina ry'umurwayi hano"
            }

        PlaceholderEnterWeight ->
            { english = "Enter weight here…"
            , kinyarwanda = Just "Andika ibiro hano…"
            }

        PlaceholderSearchContactName ->
            { english = "Search contact name here"
            , kinyarwanda = Just "Shakisha izina ry'uwo bahuye"
            }

        PleaseCall ->
            { english = "Please call"
            , kinyarwanda = Nothing
            }

        PleaseContact ->
            { english = "Please contact"
            , kinyarwanda = Nothing
            }

        PleaseSelectGroup ->
            { english = "Please select the relevant Group for the new encounter"
            , kinyarwanda = Nothing
            }

        PleaseSync ->
            { english = "Please sync data for selected Health Center."
            , kinyarwanda = Nothing
            }

        PositiveLabel ->
            { english = "Positive"
            , kinyarwanda = Just "Afite ubwandu"
            }

        PostpartumEncounter ->
            { english = "Postpartum Encounter"
            , kinyarwanda = Just "Igikorwa cya nyuma yo kubyara"
            }

        PostpartumHealingProblem problem ->
            case problem of
                NormalPostpartumHealing ->
                    { english = "Healing Normally"
                    , kinyarwanda = Just "Ari gukira neza"
                    }

                HealingProblemSwelling ->
                    { english = "Swelling"
                    , kinyarwanda = Just "Harabyimbye"
                    }

                HealingProblemDischarge ->
                    { english = "Discharge"
                    , kinyarwanda = Just "Harasohoka ibintu bidasanzwe"
                    }

                HealingProblemReleaseOfSutures ->
                    { english = "Release (lâchage) of sutures"
                    , kinyarwanda = Just "Indodo zavuyemo"
                    }

                HealingProblemHematoma ->
                    { english = "Hematoma"
                    , kinyarwanda = Just "Igisebe cyajemo amaraso"
                    }

                HealingProblemBruising ->
                    { english = "Bruising"
                    , kinyarwanda = Just "imfunira/ahantu hasa n'umukara kubera amaraso atasohotse mu mubiri"
                    }

        PostpartumHealingProblemQuestion ->
            { english = "What issues are presented"
            , kinyarwanda = Just "Ni ibihe bibazo byagaragaye"
            }

        PostpartumChildDangerSign sign ->
            case sign of
                PostpartumChildInabilityToSuckle ->
                    { english = "Inability to Suckle"
                    , kinyarwanda = Just "Ntashobora konka"
                    }

                PostpartumChildParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    }

                PostpartumChildLabouredBreathing ->
                    { english = "Laboured or Rapid Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye cg guhumeka vuba vuba"
                    }

                PostpartumChildAbnormalTemperature ->
                    { english = "High (Fever) or Low Temperature"
                    , kinyarwanda = Just "Igipimo cy'ubushyuhe kiri hejuru cg kiri hasi"
                    }

                PostpartumChildInactiveNoMovement ->
                    { english = "Inactive or No Movement"
                    , kinyarwanda = Just "Uruhinja ntacyo rwumva cg ntirunyeganyega"
                    }

                PostpartumChildBodyTurnedYellow ->
                    { english = "Whole Body Has Turned Yellow"
                    , kinyarwanda = Just "Umubiri wose wabaye umuhondo"
                    }

                NoPostpartumChildDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    }

        PostpartumMotherDangerSign sign ->
            case sign of
                PostpartumMotheUterineBleeding ->
                    { english = "Excessive Uterinal Bleeding"
                    , kinyarwanda = Just "Umubyeyi ava bikabije cyane"
                    }

                PostpartumMotherFever ->
                    { english = "High Temperature / Fever"
                    , kinyarwanda = Just "Guhinda umuriro mwinshi/Umuriro"
                    }

                PostpartumMotherMigraine ->
                    { english = "Migraine"
                    , kinyarwanda = Just "Umutwe umurya cyane"
                    }

                PostpartumMotherParalysis ->
                    { english = "Paralysis"
                    , kinyarwanda = Just "Igice cy'umubiri kidakora"
                    }

                PostpartumMotherAcuteAbdominalPain ->
                    { english = "Acute Abdominal Pain"
                    , kinyarwanda = Just "Kuribwa mu nda cyane"
                    }

                PostpartumMotherLabouredBreathing ->
                    { english = "Laboured Breathing"
                    , kinyarwanda = Just "Guhumeka bigoranye"
                    }

                NoPostpartumMotherDangerSigns ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta kimenyetso na kimwe"
                    }

        Predecessor predecessor ->
            case predecessor of
                PredecessorFather ->
                    { english = "Father"
                    , kinyarwanda = Nothing
                    }

                PredecessorMother ->
                    { english = "Mother"
                    , kinyarwanda = Nothing
                    }

                PredecessorGrandFather ->
                    { english = "Grand-Father"
                    , kinyarwanda = Nothing
                    }

                PredecessorGrandMother ->
                    { english = "Grand-Mother"
                    , kinyarwanda = Nothing
                    }

                NoPredecessors ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PreeclampsiaPreviousPregnancy ->
            { english = "Preeclampsia in previous pregnancy "
            , kinyarwanda = Just "Ubushize yagize ibimenyetso bibanziriza guhinda umushyitsi"
            }

        PregnancyConclusion ->
            { english = "Pregnancy Conclusion"
            , kinyarwanda = Just "Iherezo ry'Inda"
            }

        PregnancyStart ->
            { english = "Pregnancy Start"
            , kinyarwanda = Just "Itangira ryo Gutwita"
            }

        PregnancyTestResult result ->
            case result of
                PregnancyTestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Aratwite"
                    }

                PregnancyTestNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Ntago Atwite"
                    }

                PregnancyTestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    }

                PregnancyTestUnableToConduct ->
                    { english = "Unable to conduct test"
                    , kinyarwanda = Just "Ikizamini nticyakozwe"
                    }

        PregnancyTrimester trimester ->
            case trimester of
                FirstTrimester ->
                    { english = "First Trimester"
                    , kinyarwanda = Just "Igihembwe cya mbere"
                    }

                SecondTrimester ->
                    { english = "Second Trimester"
                    , kinyarwanda = Just "Igihembwe cya kabiri"
                    }

                ThirdTrimester ->
                    { english = "Third Trimester"
                    , kinyarwanda = Just "Igihembwe cya gatatu"
                    }

        PregnancyUrineTest ->
            { english = "Urine Pregnancy Test"
            , kinyarwanda = Just "Ikizamini cy'inkari gisuzuma ko umugore atwite"
            }

        PrenatalActivityTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso mpuruza"
                    }

                Backend.PrenatalActivity.Model.Examination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    }

                Backend.PrenatalActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro"
                    }

                History ->
                    { english = "History"
                    , kinyarwanda = Just "Amateka y'ibyamubayeho"
                    }

                PregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Just "Igihe inda imaze"
                    }

                PrenatalPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                Backend.PrenatalActivity.Model.Laboratory ->
                    { english = "Laboratory"
                    , kinyarwanda = Just "Ibizamini"
                    }

                Backend.PrenatalActivity.Model.HealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                BirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Just "Gutegura gahunda yo kubyara"
                    }

                Backend.PrenatalActivity.Model.NextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

                Backend.PrenatalActivity.Model.PregnancyOutcome ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Just "Iherezo ry'inda"
                    }

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    { english = "Malaria Prevention"
                    , kinyarwanda = Nothing
                    }

                Backend.PrenatalActivity.Model.Medication ->
                    { english = "Medication"
                    , kinyarwanda = Nothing
                    }

                Backend.PrenatalActivity.Model.SymptomReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    }

                PrenatalTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    }

                MaternalMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    }

                PrenatalImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    }

                Backend.PrenatalActivity.Model.Breastfeeding ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Just "Konsa"
                    }

                SpecialityCare ->
                    { english = "Specialty Care"
                    , kinyarwanda = Just "Ubuvuzi bw'inzobere"
                    }

                PostpartumTreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    }

        PrenatalRecurrentActivitiesTitle activity ->
            case activity of
                Backend.PrenatalActivity.Model.LabResults ->
                    { english = "Lab Results"
                    , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe"
                    }

                Backend.PrenatalActivity.Model.RecurrentNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

                RecurrentExamination ->
                    { english = "Examination"
                    , kinyarwanda = Just "Gusuzuma"
                    }

        PrenatalAssesment assesment ->
            case assesment of
                AssesmentNormalPregnancy ->
                    { english = "Routine Pregnancy Follow Up"
                    , kinyarwanda = Just "Gukurikirana Umubyeyi Utwite Bisanzwe"
                    }

                AssesmentHighRiskPregnancy ->
                    { english = "High Risk Pregnancy"
                    , kinyarwanda = Just "Inda Ibangamiwe n'ibibazo Bikomeye"
                    }

        PrenatalDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosis DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
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
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza udukoko dutera virusi ya SIDA mu maraso"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Neurosyphilis"
                    , kinyarwanda = Just "Mburugu yageze mu bwonko"
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Mild to Moderate Anemia"
                    , kinyarwanda = Just "Amaraso Macye byoroheje"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Severe Anemia"
                    , kinyarwanda = Just "Amaraso Macye Cyane"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Severe Anemia with Complications"
                    , kinyarwanda = Just "Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    }

                DiagnosisMiscarriage ->
                    { english = "Miscarriage"
                    , kinyarwanda = Just "Inda yavuyemo"
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Molar Pregnancy"
                    , kinyarwanda = Just "Atwite amahuri"
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Placenta Previa"
                    , kinyarwanda = Just "Ingobyi iri hasi ku nkondo y'umura"
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Placental Abruption"
                    , kinyarwanda = Just "Ingobyi yomotse hakiri kare"
                    }

                DiagnosisUterineRupture ->
                    { english = "Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi yaturitse"
                    }

                DiagnosisObstructedLabor ->
                    { english = "Obstructed Labor"
                    , kinyarwanda = Just "Inda yanze kuvuka "
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Post Abortion Sepsis"
                    , kinyarwanda = Just "Afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Ectopic Pregnancy"
                    , kinyarwanda = Just "Yasamiye hanze y'umura"
                    }

                DiagnosisPROM ->
                    { english = "Premature Rupture of Membranes (PROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    }

                DiagnosisPPROM ->
                    { english = "Preterm Premature Rupture of Membranes (PPROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Persistent Heartburn"
                    , kinyarwanda = Just "Ikirungurira gihoraho"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Deep Vein Thrombosis"
                    , kinyarwanda = Just "Gufatana(Kuvura) gukabije kw'amaraso"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Intense Pelvic Pain"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent Pelvic Pain"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Urinary Tract Infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Urinary Tract Infection Continued"
                    , kinyarwanda = Just "Indwara y'ubwandu bukomeje bw'umuyoboro w'inkari"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Pyelonephritis"
                    , kinyarwanda = Just "Indwara yo kubyimba impyiko"
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis Continued"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea Continued"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis Continued"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (indwara y'igisukari)"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no utwite"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        PrenatalDiagnosisForProgressReport diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertensionImmediate ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisForProgressReport DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
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
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi Itera SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Suspected Neurosyphilis"
                    , kinyarwanda = Just "Arakekwaho Mburugu yageze mu bwonko"
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Malaria Continued"
                    , kinyarwanda = Just "Uburwayi bwa Malariya buracyagaragara"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Malaria with Anemia Continued"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye bikigaragara"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Anemia (Mild to Moderate)"
                    , kinyarwanda = Just "Amaraso Macye (byoroheje)"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Anemia (Severe)"
                    , kinyarwanda = Just "Amaraso Macye (bikabije)"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Anemia (Severe with Complications)"
                    , kinyarwanda = Just "Amaraso Macye (Bikabije n'Ibibazo Bishamikiyeho)"
                    }

                DiagnosisMiscarriage ->
                    { english = "Possible Miscarriage"
                    , kinyarwanda = Just "Ashobora kuba yavanyemo inda"
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Possible Molar Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba atwite amahuri"
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Possible Placenta Previa"
                    , kinyarwanda = Just "Ingobyi ishobora kuba iri hasi ku nkondo y'umura"
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Possible Placental Abruption"
                    , kinyarwanda = Just "Ingobyi ishobora kuba yomotse hakiri kare"
                    }

                DiagnosisUterineRupture ->
                    { english = "Possible Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi ishobora kuha yaturitse"
                    }

                DiagnosisObstructedLabor ->
                    { english = "Possible Obstructed Labor"
                    , kinyarwanda = Just "Inda ishobora kuba yanze kuvuka "
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Possible Post Abortion Sepsis"
                    , kinyarwanda = Just "Ashobora kuba afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Possible Ectopic Pregnancy"
                    , kinyarwanda = Just "Ashobora kuba yarasamiye hanze y'umura"
                    }

                DiagnosisPROM ->
                    { english = "PROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    }

                DiagnosisPPROM ->
                    { english = "PPROM"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn in pregnancy"
                    , kinyarwanda = Just "Ikirungurira mu gihe umubyeyi atwite"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Heartburn in pregnancy (persistent)"
                    , kinyarwanda = Just "Ikirungurira gihoraho mu gihe umubyeyi atwite"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Possible DVT"
                    , kinyarwanda = Just "Ashobora kuba afite ibibazo by'imitsi, bituma amaraso adatembera neza mu mubiri"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Severe pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda igihe umubyeyi atwite"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Persistent pelvic pain in pregnancy"
                    , kinyarwanda = Just "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Lower urinary tract infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Lower urinary tract infection (continued)"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari wo hasi bukomeje kugaragara"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Possible Pyelonephritis"
                    , kinyarwanda = Just "Ashobora kuba afite Indwara yo kubyimba impyiko"
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Candidiasis (continued)"
                    , kinyarwanda = Just "Kandidoze ikomeje kugaragara"
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Gonorrhea (continued)"
                    , kinyarwanda = Just "Umutezi ukomeje kugaragara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Trichomonas or Bacterial Vaginosis (continued)"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Possible Active Tuberculosis"
                    , kinyarwanda = Just "Ashobora kuba afite Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete yatewe no gutwita"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Rhesus Negative"
                    , kinyarwanda = Just "Rezisi negatifu"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    }

                DiagnosisOther ->
                    { english = "Received a diagnosis from a different health care facility - please follow up with patient"
                    , kinyarwanda = Just "Yabwiwe uburwayi n'irindi vuriro - Gerageza ukurikirane umurwayi"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)/infegisiyo"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        PrenatalDiagnosisNonUrgentMessage diagnosis ->
            case diagnosis of
                DiagnosisHIV ->
                    { english = "Patient has tested positive for HIV"
                    , kinyarwanda = Just "Afite ubwandu bwa Virusi itera SIDA"
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Patient has a detectable HIV Viral Load"
                    , kinyarwanda = Just "Umurwayi agaragaza  udukoko dutera virusi ya SIDA mu maraso"
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Patient is HIV Negative with a discordant partner"
                    , kinyarwanda = Just "Umwe mubashakanye afite ubwandu bwa virusi itera SIDA"
                    }

                DiagnosisSyphilis ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Patient has tested positive for Syphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu"
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Patient has tested positive for Syphilis and shows signs of Neurosyphilis"
                    , kinyarwanda = Just "Afite ubwandu bw'indwara ya Mburugu kandi afite ibimenyetso bigaragaza ko yageze mu bwonko"
                    }

                DiagnosisHepatitisB ->
                    { english = "Patient has tested positive for Hepatitis B"
                    , kinyarwanda = Just "Afite ubwandu bw'umwijima wo mu bwoko bwa B"
                    }

                DiagnosisMalaria ->
                    { english = "Patient has tested positive for Malaria"
                    , kinyarwanda = Just "Afite ubwandu bwa Malariya"
                    }

                DiagnosisMalariaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya ikomeje kugaragara"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Patient has tested positive for Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite Malariya n'amaraso macye"
                    }

                DiagnosisMalariaWithAnemiaMedicatedContinued ->
                    { english = "Patient has tested positive for persistent Malaria with Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso make bikomeje kugaragara"
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Patient has tested positive for Malaria with Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi afite ubwandu bwa Malariya n'amaraso macye cyane"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Patient shows signs of Mild to Moderate Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'amaraso Macye byoroheje"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Patient shows signs of Severe Anemia"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Amaraso Macye Cyane"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Patient has tested positive for Severe Anemia with Complications"
                    , kinyarwanda = Just "Umubyeyi afite Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    }

                DiagnosisChronicHypertensionImmediate ->
                    { english = "Patient shows signs of Chronic Hypertension"
                    , kinyarwanda = Just "Agaragaza ibimenyetso by'indwara y'umuvuduko w'amaraso imaze igihe kirekire"
                    }

                DiagnosisChronicHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisChronicHypertensionImmediate

                DiagnosisGestationalHypertensionImmediate ->
                    { english = "Patient shows signs of Pregnancy-Induced Hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisGestationalHypertensionAfterRecheck ->
                    translationSet <| PrenatalDiagnosisNonUrgentMessage DiagnosisGestationalHypertensionImmediate

                DiagnosisModeratePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    }

                DiagnosisModeratePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso byoroheje bya Preklampusi"
                    }

                DiagnosisSeverePreeclampsiaInitialPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    }

                DiagnosisSeverePreeclampsiaRecurrentPhase ->
                    { english = "Patient shows signs of Severe Preeclampsia"
                    , kinyarwanda = Just "Agaragaza ibimenyetso bikabije bya Preklampusi"
                    }

                DiagnosisHeartburn ->
                    { english = "Patient shows signs of Persistent Heartburn"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho"
                    }

                DiagnosisHeartburnPersistent ->
                    { english = "Patient shows signs of Persistent Heartburn that is not responding to treatment"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ikirungurira gihoraho ariko imiti itari kuvura"
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Patient shows signs of Deep Vein Thrombosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo Gufatana(Kuvura) gukabije kw'amaraso"
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Patient shows signs of Intense Pelvic Pain"
                    , kinyarwanda = Just "mubyeyi agaragaza ibimenyetso by'ububabare bukabije mu kiziba cy'inda"
                    }

                DiagnosisPelvicPainContinued ->
                    { english = "Patient shows signs of Persistent Pelvic Pain"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ububabare buhoraho mu kiziba cy'inda"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Patient shows signs of Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari"
                    }

                DiagnosisUrinaryTractInfectionContinued ->
                    { english = "Patient shows signs of Persistant Urinary Tract Infection"
                    , kinyarwanda = Just "Umurwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
                    }

                DiagnosisPyelonephritis ->
                    { english = "Patient shows signs of Pyelonephritis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Indwara yo kubyimba impyiko"
                    }

                DiagnosisCandidiasis ->
                    { english = "Patient shows signs of a Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo"
                    }

                DiagnosisCandidiasisContinued ->
                    { english = "Patient shows signs of a Persistant Yeast infection"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya infegisiyo ikomeje kugaragara"
                    }

                DiagnosisGonorrhea ->
                    { english = "Patient shows signs of Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi"
                    }

                DiagnosisGonorrheaContinued ->
                    { english = "Patient shows signs of Persistant Gonorrhea"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'ndwara y'umutezi ikomeje kugaragara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Patient shows signs of Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    }

                DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                    { english = "Patient shows signs of Persistant Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Tirikomonasi cyangwa Mikorobe zo mu nda ibyara ikomeje kugaragara"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
                    { english = "Patient shows signs of Tuberculosis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'Igituntu"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
                    { english = "Patient shows signs of Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete"
                    }

                Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
                    { english = "Patient shows signs of Gestational Diabetes"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso bya Diyabete yatewe no gutwita"
                    }

                DiagnosisRhesusNegative ->
                    { english = "Patient has Rh-Negative status"
                    , kinyarwanda = Just "Umubyeyi afite Rezisi Negatifu"
                    }

                DiagnosisHyperemesisGravidumBySymptoms ->
                    { english = "Patient shows signs of Hyperemesis Gravidum"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije k'umugore utwite"
                    }

                DiagnosisSevereVomitingBySymptoms ->
                    { english = "Patient shows signs of Severe Vomiting"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuruka bikabije"
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionPossible ->
                    { english = "Patient shows signs of possible depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka ko yagira indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Patient shows signs of fairly high possibility of depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko bishoboka cyane ko afite indwara y'agahinda gakabije"
                    }

                DiagnosisDepressionProbable ->
                    { english = "Patient shows signs of probable depression"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byuko ashobora kuba afite indwara y'agahinda gakabije"
                    }

                DiagnosisSuicideRisk ->
                    { english = "Patient shows signs of being a suicide risk"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Patient shows signs of Urinary Incontinence"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kutabasha kunyara"
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Patient shows signs of Infection"
                    , kinyarwanda = Just "Umubyei agaragaza ibimenyetso bya infegisiyo"
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Patient shows signs of Excessive Bleeding"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuva cyane"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Patient shows signs of Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bwo kubyimba amabere bwaje kare cyane"
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Patient shows signs of Mastitis"
                    , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso by'uburwayi bw'amabere"
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                -- Non Not Urgent diagnoses.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalEncounterType encounterType ->
            case encounterType of
                NurseEncounter ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

                NursePostpartumEncounter ->
                    { english = "Postpartum"
                    , kinyarwanda = Just "Igihe cya nyuma cyo kubyara"
                    }

                ChwFirstEncounter ->
                    { english = "First Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya mbere umugore utwite"
                    }

                ChwSecondEncounter ->
                    { english = "Second Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya kabiri umugore utwite"
                    }

                ChwThirdPlusEncounter ->
                    { english = "Third Antenatal Visit"
                    , kinyarwanda = Just "Gusura ku nshuro ya gatatu umugore utwite"
                    }

                ChwPostpartumEncounter ->
                    { english = "Postpartum"
                    , kinyarwanda = Just "Igihe cya nyuma cyo kubyara"
                    }

        PrenatalFlankPainSign sign ->
            case sign of
                FlankPainLeftSide ->
                    { english = "Left side"
                    , kinyarwanda = Just "Uruhande rw'ibumoso"
                    }

                FlankPainRightSide ->
                    { english = "Right side"
                    , kinyarwanda = Just "Uruhande rw'iburyo"
                    }

                FlankPainBothSides ->
                    { english = "Both sides"
                    , kinyarwanda = Just "Impande zose"
                    }

                NoFlankPain ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        PrenatalHealthEducationSignsDiagnosis isInitial date sign ->
            case sign of
                EducationNauseaVomiting ->
                    if isInitial then
                        { english = "Nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent nausea + vomiting in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Isesemi + kuruka  bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationLegCramps ->
                    if isInitial then
                        { english = "Leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent leg cramps in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ibinya mu maguru bikomeza kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationLowBackPain ->
                    if isInitial then
                        { english = "Lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent lower back pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara umugongo wo hasi bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationConstipation ->
                    if isInitial then
                        { english = "Constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent constipation in pregnacy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kwituma impatwe bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationVaricoseVeins ->
                    if isInitial then
                        { english = "Varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent varicose veins during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubyimba kw'imitsi (imigarura) y'amaraso bikomeje igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationLegPainRedness ->
                    if isInitial then
                        { english = "Leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent leg pain during pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Kubabara akaguru kamwe bikomeje kugaragara igihe umugore atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                EducationPelvicPain ->
                    if isInitial then
                        { english = "Pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                    else
                        { english = "Persistent pelvic pain in pregnancy - provided health education on " ++ date
                        , kinyarwanda = Just <| "Ububabare buhoraho mu kiziba cy'inda igihe umubyeyi atwite - inyigisho ku buzima zatanzwe ku wa " ++ date
                        }

                -- Other signs do not reflect a diagnosis.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalHealthEducationLabel sign ->
            case sign of
                EducationNauseaVomiting ->
                    { english = "Nausea and Vomiting"
                    , kinyarwanda = Just "Iseseme no kuruka"
                    }

                EducationLegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    }

                EducationLowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    }

                EducationConstipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    }

                EducationHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    }

                EducationVaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    }

                EducationLegPainRedness ->
                    { english = "Leg Pain or Redness"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    }

                EducationPelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    }

                EducationSaferSex ->
                    { english = "Safer Sex Practices"
                    , kinyarwanda = Just "Imibonano mpuzabitsina ikingiye"
                    }

                EducationMentalHealth ->
                    { english = "Maternal Mental Health"
                    , kinyarwanda = Just "Ubuzima bwo mu mutwe ku mugore utwite"
                    }

                EducationEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    }

                EducationMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalHealthEducationAppropriateProvided ->
            { english = "Have you provided the appropriate health education to the patient"
            , kinyarwanda = Just "Wahaye umubyeyi inyigisho zabugenewe ku buzima"
            }

        PrenatalHealthEducationQuestion isChw sign ->
            case sign of
                EducationExpectations ->
                    { english = "Have you provided health education and anticipatory guidance on what to expect during the pregnancy"
                    , kinyarwanda = Just "Watanze inyigisho z'ubuzima k'umugore utwite unamusobanurira ibishobora kumubaho"
                    }

                EducationVisitsReview ->
                    { english = "Have you reviewed anticipated visits by the CHW and to the health center with the mother"
                    , kinyarwanda = Just "Waba waganiriye n'umubyeyi ibyerekeye gusurwa n'umujyanama w'ubuzima cyangwa kujya ku kigonderabuzima"
                    }

                EducationWarningSigns ->
                    { english = "Have you provided health education and anticipatory guidance on pregnancy warning signs"
                    , kinyarwanda = Just "Watanze inyigisho ku bimenyetso mpuruza k'umugore utwite nuko yakwitwara aramuste agize kimwe muribyo"
                    }

                EducationHemorrhaging ->
                    { english = "Have you provided education on post-partum hemorrhaging"
                    , kinyarwanda = Just "Watanze inyigisho ku kimenyesto cyo kuva cyane nyuma yo kubyara"
                    }

                EducationFamilyPlanning ->
                    if isChw then
                        { english = "Have you provided education on family planning"
                        , kinyarwanda = Just "Watanze inyigisho zijyanye no kuboneza urubyaro"
                        }

                    else
                        { english = "Have you counseled the patient on family planning options"
                        , kinyarwanda = Just "Waba wagiriye inama umurwayi (umubyeyi) uburyo bwo kuboneza urubyaro"
                        }

                EducationBreastfeeding ->
                    { english = "Have you provided education on breastfeeding"
                    , kinyarwanda = Just "Watanze inyigisho ku birebana no konsa"
                    }

                EducationImmunization ->
                    { english = "Have you provided education on immunizations"
                    , kinyarwanda = Just "Watanze inyigisho zijyanye na gahunda yo gukingiza"
                    }

                EducationHygiene ->
                    { english = "Have you provided education on hygiene"
                    , kinyarwanda = Just "Watanze inyigisho ku bijyanye n'isuku"
                    }

                EducationPositiveHIV ->
                    { english = "Have you counseled patient on positive HIV test meaning"
                    , kinyarwanda = Just "Waba wasobanuriye umurwayi (umubyeyi) icyo bisibanuye kugira ibisubizo biri positifu ku bwandu bw'agakoko gatera SIDA"
                    }

                EducationSaferSexHIV ->
                    { english = "Have you counseled patient on safer sex practices"
                    , kinyarwanda = Just "Wagiriye inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
                    }

                EducationPartnerTesting ->
                    { english = "Have you encouraged the patient’s partner to get tested"
                    , kinyarwanda = Just "Waba washishikarije umubyueyi kubwira uwo babana kwipimisha"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalHealthEducationDiabetesInform ->
            { english = "Counsel patient on healthy nutrition and exercise practices"
            , kinyarwanda = Just "Igisha umubyeyi ku mirire myiza no gukora imyitozo ngororamubiri"
            }

        PrenatalHealthEducationHivDetectableViralLoadInform ->
            { english = "Instruct the patient on the importance of strict adherence to their medication and the dangers of transmission to their child during labor and delivery"
            , kinyarwanda = Just "Igisha umubyeyi akamaro ku gufata imiti neza ndetse n'ingaruka zo kuba yakwanduza umwana mu gihe abyara"
            }

        PrenatalHealthEducationNauseaAndVomitingAdvise ->
            { english = "Advise the patient that small amounts of chamomile tea, ginger, and Vitamin B6 can help relieve these symptoms if these are available to the patient"
            , kinyarwanda = Just "Gira umubyeyi inama ko gufata icyayi cya Chamomile, tangawizi na vitamini B6 byagabanya ibimenyetso afite igihe byaba bihari"
            }

        PrenatalHealthEducationNauseaAndVomitingInform ->
            { english = "Inform the patient that the symptoms of nausea and vomiting usually resolve on their own in the second half of pregnancy"
            , kinyarwanda = Just "Menyesha umubyeyi ko ibimenyetso byo kugira iseseme no kuruka bigenda bigabanuka uko inda igenda ikura ( kumezi ane, atanu cyangwa atandatu)"
            }

        PrenatalHealthEducationLegCrampsInform ->
            { english = "Instruct the patient that the following may help relieve cramping in the legs"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira bishobora kugabanya ibinya mu maguru"
            }

        PrenatalHealthEducationLowBackPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent lower back pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho mu gihe atwite igabanya uburibwe bw'umugongo wo hasi"
            }

        PrenatalHealthEducationConstipationInform ->
            { english = "Instruct the patient that increasing the intake of fruits, vegetables, high fiber foods, and water can help relieve constipation symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko kurya imbuto, imboga, ibiryo bisukura umubiri (fibre) no kunywa amazi birinda kunanairwa kwituma"
            }

        PrenatalHealthEducationHeartburnInform ->
            { english = "Instruct the patient that the following may help relieve heartburn"
            , kinyarwanda = Just "Sobanurira umubyeyi ko ibi bikurikira bifasha mu kugabanya ikirungurira"
            }

        PrenatalHealthEducationVaricoseVeinsInform ->
            { english = "Instruct the patient that compression stockings (tight socks or leggings) and elevating their legs will help reduce varicose veins"
            , kinyarwanda = Just "Igisha umubyeyi ko kwambara ibintu bimufashe ku maguru (amasogisi,..) no gusegura amaguru igihe aryamye bizamurinda kubyimba kw'imitsi"
            }

        PrenatalHealthEducationLegPainRednessInform ->
            { english = "Instruct the patient that regular exercise and stretching can relieve leg pain or redness"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngororamubiri ihoraho izamurinda kuribwa amaguru ndetse no kuba yatukuara"
            }

        PrenatalHealthEducationPelvicPainInform ->
            { english = "Instruct the patient that regular exercise during pregnancy will help prevent pelvic pain"
            , kinyarwanda = Just "Igisha umubyeyi ko imyitozo ngoraramubiri ihoraho izamurinda kuribwa mu kiziba cy'inda"
            }

        PrenatalHealthEducationSaferSexInform ->
            { english = "Counsel patient on safer sex practices"
            , kinyarwanda = Just "Gira inama umubyeyi ku bijyanye no gukora imibonano mpuzabitsina ikingiye"
            }

        PrenatalHealthEducationEarlyMastitisOrEngorgmentInform ->
            { english = "Instruct the patient that the following may help relieve symptoms"
            , kinyarwanda = Just "Igisha umubyeyi ko ibi bikurikira byamufasha kugabanya uburibwe"
            }

        PrenatalHealthEducationMentalHealthInform ->
            { english = "Provide information to support patients mental well being during pregnancy"
            , kinyarwanda = Just "Tanga inama zafasha umubyeyi utwite kubungabunga ubuzima bwo mu mutwe"
            }

        PrenatalNCDProgramHeaderPrefix ->
            { english = "This patient was diagnosed with"
            , kinyarwanda = Just "Umurwayi yasuzumwe uburwayi bwa"
            }

        PrenatalNCDProgramHeaderSuffix ->
            { english = "during her pregnancy"
            , kinyarwanda = Just "mu gihe yari atwite"
            }

        PrenatalNCDProgramInstructions ->
            { english = "Refer patient to NCD services for further management"
            , kinyarwanda = Just "Ohereza umurwayi muri serivisi y'indwara zitandura bamwiteho byimbitse"
            }

        PrenatalNextStepsTask isChw task ->
            case task of
                Pages.Prenatal.Activity.Types.NextStepsAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Just "Kwemeza itariki yo kugaruka"
                    }

                Pages.Prenatal.Activity.Types.NextStepsFollowUp ->
                    { english = "CHW Follow Up"
                    , kinyarwanda = Just "Isura ry'umujyanama w'ubuzima"
                    }

                Pages.Prenatal.Activity.Types.NextStepsSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        }

                    else
                        { english = "Referral"
                        , kinyarwanda = Just "Kohereza"
                        }

                Pages.Prenatal.Activity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                Pages.Prenatal.Activity.Types.NextStepsNewbornEnrolment ->
                    { english = "Newborn Enrollment"
                    , kinyarwanda = Just "Kwandika uruhinja"
                    }

                Pages.Prenatal.Activity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.Activity.Types.NextStepsWait ->
                    { english = "Wait"
                    , kinyarwanda = Just "Tegereza"
                    }

        PrenatalRecurrentNextStepsTask task ->
            case task of
                Pages.Prenatal.RecurrentActivity.Types.NextStepsSendToHC ->
                    { english = "Referral"
                    , kinyarwanda = Just "Kohereza"
                    }

                Pages.Prenatal.RecurrentActivity.Types.NextStepsMedicationDistribution ->
                    translationSet MedicationDistribution

                Pages.Prenatal.RecurrentActivity.Types.NextStepsHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

        PrenatalARVProgramInstructions forPostpartum ->
            if forPostpartum then
                { english = "Refer patient to ARV services for further management"
                , kinyarwanda = Just "Ohereza umurwayi muri serivisi itanga imiti igabanya ubukana bwa Virusi itera SIDA bamwiteho byimbiste"
                }

            else
                { english = "Refer patient to ARV services for assessment of ARVs"
                , kinyarwanda = Just "Ohereza umurwayi muri serivise itanga imiti igabanya ubukana kugirango hasuzumwe neza ibijyanye n'imiti igabanya ubukana bwa Virusi itera SIDA"
                }

        PrenatalHIVSignQuestion sign ->
            case sign of
                HIVProgramHC ->
                    { english = "Does the health center have a ARV services program"
                    , kinyarwanda = Just "Ikigonderabuzima cyaba gifite service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                PartnerHIVPositive ->
                    { english = "Is the patients partner HIV positive"
                    , kinyarwanda = Just "Umugabo we yaba afite virusi itera SIDA"
                    }

                PartnerTakingARV ->
                    { english = "Is the patients partner taking ARVs"
                    , kinyarwanda = Just "Umugabo we yaba fata imiti igabanya ubukana bwa virusi itera SIDA"
                    }

                PartnerSurpressedViralLoad ->
                    { english = "Does the partner have a surpressed viral load"
                    , kinyarwanda = Just "Umugabo we yaba atakigaragaza ingano ya virusi mu maraso"
                    }

                NoPrenatalHIVSign ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalImmunisationTask task ->
            case task of
                Pages.Prenatal.Activity.Types.TaskTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    }

        PrenatalImmunisationDescription task ->
            case task of
                VaccineTetanus ->
                    { english = "The Tetanus vaccine prevents the patient from getting Tetanus which causes muscle spasms, fever, high blood pressure, and death."
                    , kinyarwanda = Just "Urukingo rw'agakwega rurinda umwana kwandura indwara y'agakwega (Tetanosi) itera kugagara kw'imitsi, umuriro, umuvuduko w'amaraso ndetse n'urupfu."
                    }

        PrenatalImmunisationHeader task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    }

        PrenatalImmunisationHistory task ->
            case task of
                VaccineTetanus ->
                    { english = "Tetanus History"
                    , kinyarwanda = Just "Amakuru ku ndwara y'Agakwega"
                    }

        PrenatalLaboratoryBloodGroupLabel ->
            { english = "Blood Group"
            , kinyarwanda = Just "Ubwoko bw'Amaraso"
            }

        PrenatalLaboratoryBloodGroupTestResult ->
            { english = "Blood Group Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cy'ubwoko bw'amaraso"
            }

        PrenatalLaboratoryBloodGroup value ->
            case value of
                BloodGroupA ->
                    { english = "A"
                    , kinyarwanda = Just "Ubwoko bwa A"
                    }

                BloodGroupB ->
                    { english = "B"
                    , kinyarwanda = Just "Ubwoko bwa B"
                    }

                BloodGroupAB ->
                    { english = "AB"
                    , kinyarwanda = Just "Ubwoko bwa AB"
                    }

                BloodGroupO ->
                    { english = "O"
                    , kinyarwanda = Just "Ubwoko bwa O"
                    }

        PrenatalLaboratoryRhesusLabel ->
            { english = "Rhesus"
            , kinyarwanda = Just "Rezisi"
            }

        PrenatalLaboratoryRhesusTestResult ->
            { english = "Rhesus Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini cya Rezisi"
            }

        PrenatalLaboratoryRhesus value ->
            case value of
                RhesusPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite Resisi pisitifu"
                    }

                RhesusNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Afite Resisi negatifu"
                    }

        PrenatalLaboratoryProteinLabel ->
            { english = "Protein"
            , kinyarwanda = Just "Proteyine"
            }

        PrenatalLaboratoryProteinTestResult ->
            { english = "Protein Test Result"
            , kinyarwanda = Just "Ibisubizo bya proteyine"
            }

        PrenatalLaboratoryProteinValue value ->
            case value of
                Protein0 ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    }

                ProteinPlus1 ->
                    { english = "+1"
                    , kinyarwanda = Nothing
                    }

                ProteinPlus2 ->
                    { english = "+2"
                    , kinyarwanda = Nothing
                    }

                ProteinPlus3 ->
                    { english = "+3"
                    , kinyarwanda = Nothing
                    }

                ProteinPlus4 ->
                    { english = "+4"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryPHLabel ->
            { english = "pH"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryPHTestResult ->
            { english = "pH Test Result"
            , kinyarwanda = Just "Igisubizo cya pH"
            }

        PrenatalLaboratoryPHValue value ->
            case value of
                Ph40 ->
                    { english = "4.0"
                    , kinyarwanda = Nothing
                    }

                Ph45 ->
                    { english = "4.5"
                    , kinyarwanda = Nothing
                    }

                Ph50 ->
                    { english = "5.0"
                    , kinyarwanda = Nothing
                    }

                Ph60 ->
                    { english = "6.0"
                    , kinyarwanda = Nothing
                    }

                Ph65 ->
                    { english = "6.5"
                    , kinyarwanda = Nothing
                    }

                Ph70 ->
                    { english = "7.0"
                    , kinyarwanda = Nothing
                    }

                Ph75 ->
                    { english = "7.5"
                    , kinyarwanda = Nothing
                    }

                Ph80 ->
                    { english = "8.0"
                    , kinyarwanda = Nothing
                    }

                Ph85 ->
                    { english = "8.5"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryGlucoseLabel ->
            { english = "Glucose"
            , kinyarwanda = Just "Isukari"
            }

        PrenatalLaboratoryGlucoseTestResult ->
            { english = "Glucose Test Result"
            , kinyarwanda = Just "Ibisubizo by'isukari mu nkari"
            }

        PrenatalLaboratoryGlucoseValue value ->
            case value of
                Glucose0 ->
                    { english = "0"
                    , kinyarwanda = Nothing
                    }

                GlucosePlus1 ->
                    { english = "+1"
                    , kinyarwanda = Nothing
                    }

                GlucosePlus2 ->
                    { english = "+2"
                    , kinyarwanda = Nothing
                    }

                GlucosePlus3 ->
                    { english = "+3"
                    , kinyarwanda = Nothing
                    }

                GlucosePlus4 ->
                    { english = "+4"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryLeukocytesLabel ->
            { english = "Leukocytes"
            , kinyarwanda = Just "Uturemangingo twera"
            }

        PrenatalLaboratoryLeukocytesTestResult ->
            { english = "Leukocytes Test Result"
            , kinyarwanda = Just "Igisubizo k'uturemangingo twera"
            }

        PrenatalLaboratoryLeukocytesValue value ->
            case value of
                LeukocytesNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta Kibazo afite"
                    }

                LeukocytesSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Insoro zera nke zigaragara mu nkari (+)"
                    }

                LeukocytesMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Insoro zera ziringaniye zigaragara mu nkari (++)"
                    }

                LeukocytesLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Insoro zera nyinshi zigaragara mu nkari (+++)"
                    }

        PrenatalLaboratoryNitriteLabel ->
            { english = "Nitrite"
            , kinyarwanda = Just "Umunyu wapimwe mu nkari"
            }

        PrenatalLaboratoryNitriteTestResult ->
            { english = "Nitrite Test Result"
            , kinyarwanda = Just "Ibisubizo kumunyu wapimwe mu nkari"
            }

        PrenatalLaboratoryNitriteValue value ->
            case value of
                NitriteNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta kibazo afite"
                    }

                NitritePlus ->
                    { english = "+"
                    , kinyarwanda = Nothing
                    }

                NitritePlusPlus ->
                    { english = "++"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryUrobilinogenLabel ->
            { english = "Urobilinogen"
            , kinyarwanda = Just "urobilinogene (mu nkari)"
            }

        PrenatalLaboratoryUrobilinogenTestResult ->
            { english = "Urobilinogen Test Result"
            , kinyarwanda = Just "Igisubizo cya urobilinogene (mu nkari)"
            }

        PrenatalLaboratoryUrobilinogenValue value ->
            case value of
                Urobilinogen002 ->
                    { english = "0-0.2"
                    , kinyarwanda = Nothing
                    }

                Urobilinogen10 ->
                    { english = "1"
                    , kinyarwanda = Nothing
                    }

                Urobilinogen20 ->
                    { english = "2"
                    , kinyarwanda = Nothing
                    }

                Urobilinogen40 ->
                    { english = "4"
                    , kinyarwanda = Nothing
                    }

                Urobilinogen80 ->
                    { english = "8"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryHaemoglobinLabel ->
            { english = "Hemoglobin"
            , kinyarwanda = Just "Ingano y'Amaraso"
            }

        PrenatalLaboratoryHaemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo by'ikizamini gipima ingano y'amaraso"
            }

        PrenatalLaboratoryHaemoglobinValue value ->
            case value of
                HaemoglobinNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta kibazo afite"
                    }

                HaemoglobinNonHemolyzedTrace ->
                    { english = "Non-Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura nkeya zidashwanyutse"
                    }

                HaemoglobinNonHemolyzedModerate ->
                    { english = "Non-Hemolyzed Moderate"
                    , kinyarwanda = Just "Insoro  zitukura  ziri mu rugero zidashwanyutse"
                    }

                HaemoglobinHemolyzedTrace ->
                    { english = "Hemolyzed Trace"
                    , kinyarwanda = Just "Insoro zitukura zashwanyutse"
                    }

                HaemoglobinSmall ->
                    { english = "Small"
                    , kinyarwanda = Just "Ikigero gito cy'amaraso agaragara mu nkari"
                    }

                HaemoglobinModerate ->
                    { english = "Moderate"
                    , kinyarwanda = Just "Ikigero kiringaniye cy'amaraso agaragara mu nkari"
                    }

                HaemoglobinLarge ->
                    { english = "Large"
                    , kinyarwanda = Just "Ikigero kinini cy'amaraso (hemoglobini)  agaragara mu nkari"
                    }

        PrenatalLaboratoryKetoneLabel ->
            { english = "Ketone Test"
            , kinyarwanda = Just "Ikizamini cya Ketone mu nkari"
            }

        PrenatalLaboratoryKetoneTestResult ->
            { english = "Ketone Test Result"
            , kinyarwanda = Just "Igisubizo cya Ketone (mu nkari)"
            }

        PrenatalLaboratoryKetoneValue value ->
            case value of
                KetoneNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta Kibazo afite"
                    }

                Ketone5 ->
                    { english = "5"
                    , kinyarwanda = Nothing
                    }

                Ketone10 ->
                    { english = "10"
                    , kinyarwanda = Nothing
                    }

                Ketone15 ->
                    { english = "15"
                    , kinyarwanda = Nothing
                    }

                Ketone40 ->
                    { english = "40"
                    , kinyarwanda = Nothing
                    }

                Ketone80 ->
                    { english = "80"
                    , kinyarwanda = Nothing
                    }

                Ketone100 ->
                    { english = "100"
                    , kinyarwanda = Nothing
                    }

        PrenatalLaboratoryBilirubinLabel ->
            { english = "Bilirubin"
            , kinyarwanda = Just "Bililibine (mu nkari)"
            }

        PrenatalLaboratoryBilirubinTestResult ->
            { english = "Bilirubin Test Result"
            , kinyarwanda = Just "Igisubizo cya Bililibine (mu nkari)"
            }

        PrenatalLaboratoryBilirubinValue value ->
            case value of
                BilirubinNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta kibazo afite"
                    }

                BilirubinSmall ->
                    { english = "Small (+)"
                    , kinyarwanda = Just "Byoroheje"
                    }

                BilirubinMedium ->
                    { english = "Medium (++)"
                    , kinyarwanda = Just "Bikabije"
                    }

                BilirubinLarge ->
                    { english = "Large (+++)"
                    , kinyarwanda = Just "Bikabije cyane"
                    }

        PrenatalLaboratoryHemoglobinTestResult ->
            { english = "Hemoglobin Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'amaraso"
            }

        PrenatalLaboratoryRandomBloodSugarTestResult ->
            { english = "Random Blood Sugar Test Result"
            , kinyarwanda = Just "Igisubizo ku kizamini gipima ingano y'isukari mu maraso"
            }

        PrenatalLaboratoryHIVPCRTestResult ->
            { english = "HIV PCR Test Result"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryHIVPCRViralLoadStatusQuestion ->
            { english = "Are there less than 20 copies/mm3"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryCreatinineLabel ->
            { english = "Creatinine"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryBUNLabel ->
            { english = "BUN"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryALTLabel ->
            { english = "ALT"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryALSLabel ->
            { english = "ALS"
            , kinyarwanda = Nothing
            }

        PrenatalLaboratoryPregnancyLabel ->
            { english = "Pregnancy"
            , kinyarwanda = Nothing
            }

        LaboratoryTest value ->
            case value of
                TestBloodGpRs ->
                    { english = "Blood Group"
                    , kinyarwanda = Just "Ubwoko bw'Amaraso"
                    }

                TestHemoglobin ->
                    { english = "Hemoglobin"
                    , kinyarwanda = Just "Ingano y'Amaraso"
                    }

                TestHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    }

                TestRandomBloodSugar ->
                    { english = "Blood Sugar"
                    , kinyarwanda = Just "Ingano y'isukari mu Maraso"
                    }

                TestSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    }

                TestUrineDipstick ->
                    { english = "Urine Dipstick"
                    , kinyarwanda = Just "Ikizamini cy'inkari"
                    }

                TestVitalsRecheck ->
                    { english = "Vitals Recheck"
                    , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
                    }

                TestHIVPCR ->
                    { english = "HIV PCR"
                    , kinyarwanda = Just "PCR ipima Virusi itera SIDA"
                    }

                TestCreatinine ->
                    { english = "Creatinine"
                    , kinyarwanda = Nothing
                    }

                TestLiverFunction ->
                    { english = "Liver Function"
                    , kinyarwanda = Nothing
                    }

        PrenatalLabsCaseManagementEntryTypeResults ->
            { english = "ANC Lab Results"
            , kinyarwanda = Just "Ibisubizo by'Ibizamini Byafashwe ku mugore utwite"
            }

        PrenatalLabsCaseManagementEntryTypeVitals ->
            { english = "Vitals Recheck"
            , kinyarwanda = Just "Gusubiramo ibipimo by'ubuzima"
            }

        LabsEntryState state ->
            case state of
                LabsEntryPending ->
                    { english = "Pending"
                    , kinyarwanda = Just "Birategerejwe"
                    }

                LabsEntryClosingSoon ->
                    { english = "Closing Soon"
                    , kinyarwanda = Just "Birafunga vuba"
                    }

        PrenatalLabsHistoryCompletedQuestion ->
            { english = "Have you updated all results that have been returned for this patient"
            , kinyarwanda = Just "Waba wujuje ibisubizo byose byaba byabonetse kuri uyu mubyeyi"
            }

        PrenatalLabsHistoryInstructions ->
            { english = "Please update all outstanding lab results before proceeding"
            , kinyarwanda = Just "Gerageza gushyiramo ibisubizo byose mbere yo gukomeza"
            }

        PrenatalLabsHistoryLabel ->
            { english = "This patient has pending lab results"
            , kinyarwanda = Just "Umubyeyi afite ibizamini bitarabonerwa ibisubizo"
            }

        PrenatalMentalHealthQuestion question ->
            case question of
                MentalHealthQuestion1 ->
                    { english = "I have been able to laugh and see the funny side of things"
                    , kinyarwanda = Just "Njya nshobora guseka kandi nkabona ibintu mu buryo bwiza"
                    }

                MentalHealthQuestion2 ->
                    { english = "I have looked forward with enjoyment to things"
                    , kinyarwanda = Just "Nategereje ko ibintu nezerewe"
                    }

                MentalHealthQuestion3 ->
                    { english = "I have blamed myself unnecessarily when things went wrong"
                    , kinyarwanda = Just "Njya niciraga urubanza iyo ibintu byabaga byagenze nabi"
                    }

                MentalHealthQuestion4 ->
                    { english = "I have been anxious or worried for no good reason"
                    , kinyarwanda = Just "Njya mpangayika nta mpamvu igaragara"
                    }

                MentalHealthQuestion5 ->
                    { english = "I have felt scared or panicky for no very good reason"
                    , kinyarwanda = Just "Njya ngira ubwoba cyangwa nkakuka umutima nta mpamvu ifatika"
                    }

                MentalHealthQuestion6 ->
                    { english = "Things have been getting on top of me"
                    , kinyarwanda = Just "Ibintu bijya bindenga "
                    }

                MentalHealthQuestion7 ->
                    { english = "I have been so unhappy that I have had difficulty sleeping"
                    , kinyarwanda = Just "Njya numva mbabaye ku buryo ngira ikibazo cyo kudasinzira"
                    }

                MentalHealthQuestion8 ->
                    { english = "I have felt sad or miserable"
                    , kinyarwanda = Just "Njya numva mbabaye cyangwa mfite ishavu "
                    }

                MentalHealthQuestion9 ->
                    { english = "I have been so unhappy that I have been crying"
                    , kinyarwanda = Just "Njya numva mbabaye cyane ku buryo ndira"
                    }

                MentalHealthQuestion10 ->
                    { english = "The thought of harming myself has occurred to me"
                    , kinyarwanda = Just "Ibitekerezo byo kwigirira nabi bijya binzamo"
                    }

        PrenatalMentalHealthOptionForQuestion question option ->
            case question of
                MentalHealthQuestion1 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I always could"
                            , kinyarwanda = Just "Buri gihe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not quite so much now"
                            , kinyarwanda = Just "Ubu ntago ari cyane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely not so much now"
                            , kinyarwanda = Just "Ntago ari cyane na gato"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            }

                MentalHealthQuestion2 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "As much as I ever did"
                            , kinyarwanda = Just "Nk'ibisanzwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Rather less than I used to"
                            , kinyarwanda = Just "Byaraganutse ugereranyije nuko byari bisanzwe"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Definitely less than I used to"
                            , kinyarwanda = Just "Gake cyane ugereranyije n‘uko byari bisanzwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Hardly at all"
                            , kinyarwanda = Just "Habe na mba"
                            }

                MentalHealthQuestion3 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Si cyane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, some of the time"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, akenshi"
                            }

                MentalHealthQuestion4 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, not at all"
                            , kinyarwanda = Just "No, nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, very often"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            }

                MentalHealthQuestion5 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not much"
                            , kinyarwanda = Just "Oya, ntago ari cyane"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite a lot"
                            , kinyarwanda = Just "Yego, akenshi"
                            }

                MentalHealthQuestion6 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, I have been coping as well as ever"
                            , kinyarwanda = Just "Oya, ndabyakira nk'ibisanzwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, most of the time I have coped quite well"
                            , kinyarwanda = Just "Oya, akenshi njya nshoboraga kubyakira neza"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes I haven’t been coping as well as usual"
                            , kinyarwanda = Just "Yes, rimwe na rimwe ntago njya mbyakira neza"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time I haven’t been able to cope"
                            , kinyarwanda = Just "Yego, kenshi na kenshi ntago njya mbyakira neza"
                            }

                MentalHealthQuestion7 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "No, not very often"
                            , kinyarwanda = Just "Oya, ntago ari kenshi"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, sometimes"
                            , kinyarwanda = Just "Yego, rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            }

                MentalHealthQuestion8 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Not at all"
                            , kinyarwanda = Just "Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Not very often"
                            , kinyarwanda = Just "Ntago ari kenshi"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, kenshi na kenshi"
                            }

                MentalHealthQuestion9 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "No, never"
                            , kinyarwanda = Just "Oya, Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Only occasionally"
                            , kinyarwanda = Just "Gisa rimwe na riwme"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "yego, kenshi"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, most of the time"
                            , kinyarwanda = Just "Yego, Kemshi na kenshi"
                            }

                MentalHealthQuestion10 ->
                    case option of
                        MentalHealthQuestionOption0 ->
                            { english = "Never"
                            , kinyarwanda = Just "Nta na rimwe"
                            }

                        MentalHealthQuestionOption1 ->
                            { english = "Hardly ever"
                            , kinyarwanda = Just "Gake gashoboka"
                            }

                        MentalHealthQuestionOption2 ->
                            { english = "Sometimes"
                            , kinyarwanda = Just "rimwe na rimwe"
                            }

                        MentalHealthQuestionOption3 ->
                            { english = "Yes, quite often"
                            , kinyarwanda = Just "Yego, kenshi"
                            }

        PrenatalMentalHealthSpecialistHelper ->
            { english = "Refer patient to mental health specialist for further evaluation"
            , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
            }

        PrenatalMentalHealthSpecialistQuestion ->
            { english = "Does this health center have a mental health specialist available"
            , kinyarwanda = Just "Iki kigo nderabuzima gifite umuganga w'inzobere ku buzima bwo mu mutwe"
            }

        PrenatalMentalHealthWarningPopupMessage ->
            { english = "Patient shows signs of being a suicide risk"
            , kinyarwanda = Just "Umubyeyi agaragaza ibimenyetso byo kuba afite ibyago byo kwiyahura"
            }

        PrenatalMentalHealthWarningPopupInstructions ->
            { english = "Contact mental health specialist immediately"
            , kinyarwanda = Just "Ihutire kureba umuganga w'inzobere mu buzima bwo mu mutwe"
            }

        OutsideCareSignQuestion sign ->
            case sign of
                SeenAtAnotherFacility ->
                    { english = "Have you been seen at another facility since your last visit"
                    , kinyarwanda = Just "Waba hari irindi vuriro wagiyeho nyuma yo kuva hano"
                    }

                GivenNewDiagnoses ->
                    { english = "Were you given a new diagnosis"
                    , kinyarwanda = Just "Haba hari ubundi burwayi bagusanzemo"
                    }

                GivenMedicine ->
                    { english = "Were you given medicine"
                    , kinyarwanda = Just "Waba warahawe imiti"
                    }

                PlannedFollowUpCareWithSpecialist ->
                    { english = "Do you have follow up care planned with a specialist"
                    , kinyarwanda = Just "Waba ufite gahunda yo gukurikiranwa n'umuganga w'inzobere"
                    }

                -- There's not question for this sign.
                NoOutsideCareSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        OutsideCareMedicationDosage medication ->
            case medication of
                OutsideCareMedicationQuinineSulphate ->
                    { english = "3 x a day for 7 days"
                    , kinyarwanda = Just "inshuri 3 ku munsi mu minsi 7"
                    }

                OutsideCareMedicationCoartem ->
                    { english = "4 tablets by mouth 2x a day for 7 days"
                    , kinyarwanda = Just "Kunywa ibinini 4 inshuro 2 ku munsi mu minsi 7"
                    }

                OutsideCareMedicationPenecilin1 ->
                    { english = "IM x 1"
                    , kinyarwanda = Just "IM inshuro 1"
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 mu cyumweru mu byumweru 3"
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "Kunywa inshuro 4 ku munsi mu minsi 14"
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "Kunywa ibinini 4 ku munsi"
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    }

                OutsideCareMedicationMethyldopa2 ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 2 ku munsi"
                    }

                OutsideCareMedicationMethyldopa3 ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    }

                OutsideCareMedicationMethyldopa4 ->
                    { english = "by mouth 4x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 4 ku munsi"
                    }

                OutsideCareMedicationCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 2 ku munsi"
                    }

                OutsideCareMedicationAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 1 ku munsi"
                    }

                OutsideCareMedicationTDF3TC ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    }

                OutsideCareMedicationDolutegravir ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro imwe ku munsi"
                    }

                OutsideCareMedicationIron1 ->
                    { english = "one tab by mouth 1x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 1 ku munsi"
                    }

                OutsideCareMedicationIron2 ->
                    { english = "one tab by mouth 2x a day"
                    , kinyarwanda = Just "Kunywa ikinini kimwe inshuro 2 ku munsi"
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "Mu kanwa inshuro 3 ku munsi"
                    }

                -- Dosage is not applicable for other options.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        OutsideCareMedicationLabel medication ->
            case medication of
                OutsideCareMedicationQuinineSulphate ->
                    { english = "Quinine Sulphate per os (10 mg/kg/dose)"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationCoartem ->
                    { english = "Coartem"
                    , kinyarwanda = Just "Kowaritemu"
                    }

                NoOutsideCareMedicationForMalaria ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

                OutsideCareMedicationPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    }

                OutsideCareMedicationPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    }

                OutsideCareMedicationErythromycin ->
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    }

                OutsideCareMedicationAzithromycin ->
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    }

                OutsideCareMedicationCeftriaxon ->
                    { english = "Ceftriaxone (1g)"
                    , kinyarwanda = Nothing
                    }

                NoOutsideCareMedicationForSyphilis ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

                OutsideCareMedicationMethyldopa2 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationMethyldopa3 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationMethyldopa4 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationCarvedilol ->
                    { english = "Carvedilol (6.25mg)"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    }

                NoOutsideCareMedicationForHypertension ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta ns kimwe"
                    }

                OutsideCareMedicationTDF3TC ->
                    { english = "TDF+3TC"
                    , kinyarwanda = Nothing
                    }

                OutsideCareMedicationDolutegravir ->
                    { english = "Dolutegravir (50mg)"
                    , kinyarwanda = Nothing
                    }

                NoOutsideCareMedicationForHIV ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

                OutsideCareMedicationIron1 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    }

                OutsideCareMedicationIron2 ->
                    { english = "Iron (60mg)"
                    , kinyarwanda = Just "Fer (60mg)"
                    }

                OutsideCareMedicationFolicAcid ->
                    { english = "Folic Acid (400IU)"
                    , kinyarwanda = Nothing
                    }

                NoOutsideCareMedicationForAnemia ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        PrenatalPhotoHelper ->
            { english = "Take a picture of the mother's belly. Then you and the mother will see how the belly has grown!"
            , kinyarwanda = Just "Fata ifoto y'inda y'umubyeyi hanyuma uyimwereke arebe uko yakuze/yiyongereye."
            }

        PrenatalSymptom value ->
            case value of
                BurningWithUrination ->
                    { english = "Burning with Urination"
                    , kinyarwanda = Just "Kunyara Ukababara"
                    }

                AbnormalVaginalDischarge ->
                    { english = "Abnormal Vaginal Discharge"
                    , kinyarwanda = Just "Gusohora ibintu bidasanzwe mu gitsina"
                    }

                NauseaAndVomiting ->
                    { english = "Nausea and Vomiting"
                    , kinyarwanda = Just "Iseseme no kuruka"
                    }

                Heartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    }

                LegCramps ->
                    { english = "Leg Cramps"
                    , kinyarwanda = Just "Ibinya mu maguru"
                    }

                LowBackPain ->
                    { english = "Lower Back Pain"
                    , kinyarwanda = Just "Kubabara umugongo wo hasi"
                    }

                CoughContinuous ->
                    { english = "Cough for >2 weeks"
                    , kinyarwanda = Just "Inkorora irengeje ibyumweru 2"
                    }

                PelvicPain ->
                    { english = "Pelvic Pain"
                    , kinyarwanda = Just "Kubabara mu kiziba cy'inda"
                    }

                Constipation ->
                    { english = "Constipation"
                    , kinyarwanda = Just "Kwituma impatwe"
                    }

                VaricoseVeins ->
                    { english = "Varicose Veins"
                    , kinyarwanda = Just "Kubyimba kw'imitsi (imigarura) y'amaraso"
                    }

                LegPainRedness ->
                    { english = "Leg Pain or Redness (One Leg)"
                    , kinyarwanda = Just "Kubabara akaguru kamwe cyangwa gutukura ku kuguru kumwe"
                    }

                PostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    }

                PostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    }

                PostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kuribwa Umutwe"
                    }

                PostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "Umunaniro"
                    }

                PostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Guhinda Umuriro"
                    }

                PostpartumPerinealPainOrDischarge ->
                    { english = "Perineal pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    }

                NoPrenatalSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        PrenatalSymptomQuestion value ->
            case value of
                SymptomQuestionDizziness ->
                    { english = "Are you experiencing dizziness"
                    , kinyarwanda = Just "Waba ujya ugira isereri"
                    }

                SymptomQuestionLowUrineOutput ->
                    { english = "Are you experiencing low urine output"
                    , kinyarwanda = Just "Waba ujya unyara inkari nkeya"
                    }

                SymptomQuestionDarkUrine ->
                    { english = "Are you experiencing dark urine"
                    , kinyarwanda = Just "Waba ujya unyara inkari zijimye"
                    }

                SymptomQuestionPelvicPainHospitalization ->
                    { english = "Is there severe pain that requires referral to hospital"
                    , kinyarwanda = Just "Waba ufite ububabare bukabije busaba koherezwa ku bitaro"
                    }

                SymptomQuestionLegPainRednessLeft ->
                    { english = "On which side are you experiencing leg pain or redness"
                    , kinyarwanda = Just "Ni uruhe ruhande rw'ukuguru ruribwa cyangwa rutukuye"
                    }

                SymptomQuestionLegPainful ->
                    { english = "Is the leg painful"
                    , kinyarwanda = Just "Ubabara ukuguru"
                    }

                SymptomQuestionLegSwollen ->
                    { english = "Is the leg swollen"
                    , kinyarwanda = Just "Ukuguru kurabyimbye"
                    }

                SymptomQuestionLegWarm ->
                    { english = "Is the leg red or warm to the touch"
                    , kinyarwanda = Just "Ukuguru kuratukuye cyangwa kurashyushye iyo ukozeho"
                    }

                SymptomQuestionNightSweats ->
                    { english = "Do you have night sweats"
                    , kinyarwanda = Just "Waba ubira ibyuya nijoro"
                    }

                SymptomQuestionBloodInSputum ->
                    { english = "Do you have blood in sputum"
                    , kinyarwanda = Just "Waba ugira ikororwa kirimo amaraso"
                    }

                SymptomQuestionWeightLoss ->
                    { english = "Do you have weight loss"
                    , kinyarwanda = Just "Waba waratakaje ibiro"
                    }

                SymptomQuestionSevereFatigue ->
                    { english = "Do you have severe fatigue"
                    , kinyarwanda = Just "Waba ugira umunaniro ukabije"
                    }

                SymptomQuestionVaginalItching ->
                    { english = "Do you experience vaginal itching"
                    , kinyarwanda = Just "Waba ufite uburyaryate mu gitsina"
                    }

                SymptomQuestionVaginalDischarge ->
                    { english = "Do you experience vaginal discharge"
                    , kinyarwanda = Just "Ujya ubona ibintu bidasanzwe biva mu gitsina"
                    }

                SymptomQuestionFrequentUrination ->
                    { english = "Do you experience urinating frequently"
                    , kinyarwanda = Just "Waba ujya kunyara buri kanya"
                    }

                SymptomQuestionFlankPain ->
                    { english = "Do you experience flank pain"
                    , kinyarwanda = Just "Waba ujya uribwa mu ibondo"
                    }

                SymptomQuestionPartnerUrethralDischarge ->
                    { english = "Does your partner have urethral discharge"
                    , kinyarwanda = Just "Umugabo wawe ajya agira ibintu bidasanzwe biva mu gitsina"
                    }

                NoSymptomQuestions ->
                    { english = "None"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        PrenatalSymptomQuestionsHeader ->
            { english = "The patient has noted symptoms that require follow up questions"
            , kinyarwanda = Just "Umubyeyi yagaragaje ibimenyetso bisaba ibindi bibazo"
            }

        TestExecutionNote note ->
            case note of
                TestNoteRunToday ->
                    { english = "Run Today"
                    , kinyarwanda = Just "Ikizamini cyakozwe uyu munsi"
                    }

                TestNoteRunPreviously ->
                    { english = "Run Previously"
                    , kinyarwanda = Just "Ikizamimi cyakozwe ubushize"
                    }

                TestNoteLackOfReagents ->
                    { english = "Lack of Reagents"
                    , kinyarwanda = Just "Kubura kw'ibikoresho byo gupima"
                    }

                TestNoteLackOfOtherSupplies ->
                    { english = "Lack of Other Supplies"
                    , kinyarwanda = Just "Kubura kw'ibindi bikoresho bicyenerwa mu gupima"
                    }

                TestNoteNoEquipment ->
                    { english = "No Equipment"
                    , kinyarwanda = Just "Nta gikoresho gihari"
                    }

                TestNoteBrokenEquipment ->
                    { english = "Broken Equipment"
                    , kinyarwanda = Just "Igikoresho gipima cyarangiritse"
                    }

                TestNoteNotIndicated ->
                    { english = "Not Indicated"
                    , kinyarwanda = Just "Ikizamini nticyasabwe"
                    }

                TestNoteKnownAsPositive ->
                    { english = "Known as Positive"
                    , kinyarwanda = Just "Asanzwe afite ubwandu"
                    }

        TestResult result ->
            case result of
                TestPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite ubwandu"
                    }

                TestNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                TestIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    }

        PrenatalUrineDipstickTestLabel variant ->
            case variant of
                VariantShortTest ->
                    { english = "Urine Dipstick Short"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo bike"
                    }

                VariantLongTest ->
                    { english = "Urine Dipstick Long"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    }

        PrenatalUrineDipstickTestVariant variant ->
            case variant of
                VariantShortTest ->
                    { english = "Short Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo bike"
                    }

                VariantLongTest ->
                    { english = "Long Dip"
                    , kinyarwanda = Just "Ikizamini gitanga ibisubizo byinshi"
                    }

        PrenatalVaccineLabel value ->
            case value of
                VaccineTetanus ->
                    { english = "Tetanus"
                    , kinyarwanda = Just "Agakwega"
                    }

        PreTerm ->
            { english = "Pre Term"
            , kinyarwanda = Just "Inda itaragera igihe"
            }

        PregnancyConcludedLabel ->
            { english = "or Pregnancy Concluded"
            , kinyarwanda = Just "Cyangwa Iherezo ry'inda"
            }

        PregnancyOutcomeLabel ->
            { english = "Pregnancy Outcome"
            , kinyarwanda = Just "Iherezo ry'inda"
            }

        PregnancyOutcome outcome ->
            case outcome of
                OutcomeLiveAtTerm ->
                    { english = "Live Birth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Kubyara umwana muzima/Ushyitse (ku byumweru 38 kuzamura)"
                    }

                OutcomeLivePreTerm ->
                    { english = "Live Birth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Kubyara mwana udashyitse (munsi y'ibyumweru 38)"
                    }

                OutcomeStillAtTerm ->
                    { english = "Stillbirth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda bageze igihe cyo kuvuka (ku byumweru 38 kuzamura)"
                    }

                OutcomeStillPreTerm ->
                    { english = "Stillbirth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda batagejeje igihe cyo kuvuka (munsi y'ibyumweru 38)"
                    }

                OutcomeAbortions ->
                    { english = "Abortions (before 24 weeks EGA)"
                    , kinyarwanda = Just "Kuvanamo inda (mbere y'ibyumweru 24)"
                    }

        PreviousCSectionScar ->
            { english = "Previous C-section scar"
            , kinyarwanda = Just "Inkovu yaho babaze ubushize"
            }

        PreviousDelivery ->
            { english = "Previous Delivery"
            , kinyarwanda = Just "Kubyara guheruka"
            }

        PreviousDeliveryPeriods period ->
            case period of
                LessThan18Month ->
                    { english = "Less than 18 month ago"
                    , kinyarwanda = Just "Munsi y'amezi 18 ashize"
                    }

                MoreThan5Years ->
                    { english = "More than 5 years ago"
                    , kinyarwanda = Just "Hejuru y'imyaka itanu ishize"
                    }

                Neither ->
                    { english = "Neither"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        PreviousFloatMeasurement value ->
            { english = "Previous measurement: " ++ String.fromFloat value
            , kinyarwanda = Just <| "Ibipimo by'ubushize: " ++ String.fromFloat value
            }

        PreviousMeasurementNotFound ->
            { english = "No previous measurement on record"
            , kinyarwanda = Just "Nta gipimo cy'ubushize cyanditswe"
            }

        PriorTreatmentTask task ->
            case task of
                TreatmentReview ->
                    { english = "Treatment Review"
                    , kinyarwanda = Just "Kureba imiti yahawe"
                    }

        Profession ->
            { english = "Profession"
            , kinyarwanda = Nothing
            }

        Programs ->
            { english = "Programs"
            , kinyarwanda = Just "Porogaramu"
            }

        ProgressPhotos ->
            { english = "Progress Photos"
            , kinyarwanda = Just "Uko amafoto agenda ahinduka"
            }

        ProgressReport ->
            { english = "Progress Report"
            , kinyarwanda = Just "Raporo y’ibyakozwe"
            }

        ProgressReports ->
            { english = "Progress Reports"
            , kinyarwanda = Just "Raporo z’ibyakozwe"
            }

        ProgressTimeline ->
            { english = "Progress Timeline"
            , kinyarwanda = Just "Uko inda igenda ikura"
            }

        ProgressTrends ->
            { english = "Progress Trends"
            , kinyarwanda = Just "Uko ibipimo bigenda bizamuka"
            }

        ProvideHealthEducationAndInstructToIsolate ->
            { english = "Provide health education and instruct them to self isolate at home"
            , kinyarwanda = Nothing
            }

        PrenatalParticipant ->
            { english = "Antenatal Participant"
            , kinyarwanda = Just "Umubyeyi witabiriye kwipimisha inda"
            }

        PrenatalParticipants ->
            { english = "Antenatal Participants"
            , kinyarwanda = Just "Ababyeyi bitabiriye kwipimisha inda"
            }

        PreTermPregnancy ->
            { english = "Number of Pre-term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima badashyitse"
            }

        TestDate ->
            { english = "Date of Test"
            , kinyarwanda = Just "Itariki y'Ikizamini"
            }

        TestName ->
            { english = "Test name"
            , kinyarwanda = Just "Izina ry'ikizamini"
            }

        TestPerformedQuestion ->
            { english = "Were you able to perform the test"
            , kinyarwanda = Just "Waba wakoze ikizamini"
            }

        TestPerformedTodayQuestion ->
            { english = "Did you perform this test today"
            , kinyarwanda = Just "Waba wakoze iki kizamini uyu munsi"
            }

        TestPrerequisiteQuestion value ->
            case value of
                PrerequisiteFastFor12h ->
                    { english = "Was this test performed before a meal"
                    , kinyarwanda = Just "Umurwayi yafatiwe iki kizamini mbere yo kurya"
                    }

                NoTestPrerequisites ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    }

        TestVariantUrineDipstickQuestion ->
            { english = "Which type of urine dipstick test was run"
            , kinyarwanda = Just "Ni ikihe kizamini cy'inkari cyakozwe"
            }

        TestResultQuestion ->
            { english = "What were the results of the test"
            , kinyarwanda = Just "Ibisubizo by'ikizamini byabaye ibihe"
            }

        PriorDiagnosis ->
            { english = "Prior Diagnosis"
            , kinyarwanda = Just "Uburwayi yagize/yigeze kurwara"
            }

        ProvidedHealthEducationAction ->
            { english = "Provided health education and anticipatory guidance"
            , kinyarwanda = Nothing
            }

        ProvideHealthEducation ->
            { english = "Provide health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            }

        ProvideHealthEducationShort ->
            { english = "Provide health education and anticipatory guidance"
            , kinyarwanda = Just "Tanga inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            }

        ProvidedPreventionEducationQuestion ->
            { english = "Have you provided health education and anticipatory guidance for the prevention of"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho ku kwirinda"
            }

        ProvidedPreventionEducationQuestionShort ->
            { english = "Have you provided health education and anticipatory guidance"
            , kinyarwanda = Just "Mwatanze inyigisho ku buzima n' umurongo ngenderwaho"
            }

        ProvidedSymtomReliefGuidanceQuestion ->
            { english = "Have you provided the guidance for symptom relief"
            , kinyarwanda = Just "Wamusobanuriye ibijyanye n'imiti itangwa mukuvura ibimenyesto"
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Just "Intara"
            }

        ReasonForCSection ->
            { english = "Reason for C-section"
            , kinyarwanda = Nothing
            }

        ReasonForNotBreastfeeding reason ->
            case reason of
                NotBreastfeedingBreastPain ->
                    { english = "Breast pain"
                    , kinyarwanda = Just "Ububabare bw'amabere"
                    }

                NotBreastfeedingBreastRedness ->
                    { english = "Breast redness"
                    , kinyarwanda = Just "Amabere aratukuye"
                    }

                NotBreastfeedingLowMilkProduction ->
                    { english = "Low milk production"
                    , kinyarwanda = Just "Amashereka adahagije"
                    }

                NotBreastfeedingProblemsLatching ->
                    { english = "Problems latching"
                    , kinyarwanda = Just "Ibibazo byo konka"
                    }

                NotBreastfeedingMedicalProblems ->
                    { english = "Medical Problems"
                    , kinyarwanda = Just "Ibibazo by'uburwayi"
                    }

                NotBreastfeedingPersonalChoice ->
                    { english = "Personal Choice"
                    , kinyarwanda = Just "Amahitamo ye bwite"
                    }

                NotBreastfeedingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        ReasonForNotIsolating reason ->
            case reason of
                NoSpace ->
                    { english = "No space available at home or clinic"
                    , kinyarwanda = Just "Nta mwanya uboneka mu rugo cyangwa mu ivuriro"
                    }

                TooIll ->
                    { english = "Too ill to leave alone"
                    , kinyarwanda = Just "Umurwayi ararembye ntagomba gusigara wenyine"
                    }

                CanNotSeparateFromFamily ->
                    { english = "Unable to separate from family"
                    , kinyarwanda = Just "Ntibishoboka kumutandukanya n'umuryango"
                    }

                OtherReason ->
                    { english = "Other"
                    , kinyarwanda = Just "Ikindi"
                    }

                IsolationReasonNotApplicable ->
                    { english = "Not Applicable "
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    }

        ReasonForNotProvidingHealthEducation reason ->
            case reason of
                PatientNeedsEmergencyReferral ->
                    { english = "Patient needs an emergency referral"
                    , kinyarwanda = Just "Umurwayi akeneye kwoherezwa ku ivuriro byihutirwa"
                    }

                ReceivedEmergencyCase ->
                    { english = "Received an emergency case to treat"
                    , kinyarwanda = Just "Nakiriye undi murwayi ukeneye kuvurwa byihutirwa"
                    }

                LackOfAppropriateEducationUserGuide ->
                    { english = "Lack of appropriate education user guide"
                    , kinyarwanda = Just "Nta mfashanyigisho yabugenewe ihari"
                    }

                PatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    }

                PatientTooIll ->
                    { english = "Patient too ill"
                    , kinyarwanda = Just "Umurwayi ararembye"
                    }

                NoReasonForNotProvidingHealthEducation ->
                    { english = "No reason"
                    , kinyarwanda = Just "Nta mpamvu"
                    }

        ReasonForNotTaking reason ->
            case reason of
                NotTakingAdverseEvent ->
                    { english = "Adverse event"
                    , kinyarwanda = Just "Ibintu bidasanzwe (bitewe n'imiti wafashe)"
                    }

                NotTakingNoMoney ->
                    { english = "No money for medication"
                    , kinyarwanda = Just "Nta mafaranga yo kwishyura imiti afite"
                    }

                NotTakingMemoryProblems ->
                    { english = "Memory problems"
                    , kinyarwanda = Just "Ibibazo byo kwibagirwa"
                    }

                NotTakingOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoReasonForNotTakingSign ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        ReceivedDewormingPill ->
            { english = "Has the mother received deworming pill"
            , kinyarwanda = Just "Umubyeyi yahawe ikinini cy'inzoka"
            }

        ReceivedFolicAcid ->
            { english = "Have you received Folic Acid"
            , kinyarwanda = Just "Wahawe ibinini bya Folic Acid"
            }

        ReceivedIronFolicAcid ->
            { english = "Has the mother received iron and folic acid supplement"
            , kinyarwanda = Just "Umubyeyi yahawe ibinini bya Fer cg Folic Acid byongera amaraso"
            }

        ReceivedMebendazole ->
            { english = "Has the mother received Mebendazole in the last 6 months"
            , kinyarwanda = Just "Ububyeyi yahawe umuti wa Mebendazole mu mezi 6 ashize"
            }

        ReceivedMosquitoNet ->
            { english = "Has the mother received a mosquito net"
            , kinyarwanda = Just "Umubyeyi yahawe inzitiramubu"
            }

        ReceivedVitaminA ->
            { english = "Have you received Vitamin A"
            , kinyarwanda = Just "Wahawe Vitamine A"
            }

        Recommendation114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "Send Patient to the nearest health center"
                    , kinyarwanda = Just "Ohereza umurwayi ku kigo nderabuzima kikwegereye"
                    }

                SendToRRTCenter ->
                    { english = "Send patient to the Rapid Response Team center"
                    , kinyarwanda = Just "Ohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    }

                SendToHospital ->
                    { english = "Send patient to the nearest hospital"
                    , kinyarwanda = Just "Ohereza umurwayi ku bitaro bikwegereye"
                    }

                OtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoneNoAnswer ->
                    { english = "No answer"
                    , kinyarwanda = Just "Nta Gisubizo cyabonetse"
                    }

                NoneBusySignal ->
                    { english = "Busy Signal"
                    , kinyarwanda = Just "Umurongo bawuvugiragaho"
                    }

                NoneOtherRecommendation114 ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

        RecommendationSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Team will come to village"
                    , kinyarwanda = Just "Itsinda rizaza mu mudugudu"
                    }

                SendToSiteWithForm ->
                    { english = "Advised to send patient to site with referral form"
                    , kinyarwanda = Just "Nagiriwe inama yo kohereza umurwayi ku rwego rubishinzwe yitwaje impapuro zimwohereza"
                    }

                OtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                NoneSentWithForm ->
                    { english = "No response. Sent patient with referral form."
                    , kinyarwanda = Just "Nta gisubizo. Nohereje umurwayi yitwaje impapuro zimwohereza."
                    }

                NonePatientRefused ->
                    { english = "Patient refused"
                    , kinyarwanda = Just "Umurwayi yabyanze"
                    }

                NoneOtherRecommendationSite ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    }

                RecommendationSiteNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    }

        Recommended ->
            { english = "Recommended"
            , kinyarwanda = Just "Imiti yemewe"
            }

        RecommendedButNotGivenDueTo ->
            { english = "recommended but not given due to"
            , kinyarwanda = Nothing
            }

        RecommendedSymptomRelief ->
            { english = "Recommended Symptom Relief"
            , kinyarwanda = Just "Imiti yemewe mukuvura ibimenyesto"
            }

        RecommendedTreatmentSignDosage sign ->
            case sign of
                TreatmentPenecilin1 ->
                    { english = "IM x 1"
                    , kinyarwanda = Just "IM inshuro 1"
                    }

                TreatmentPenecilin3 ->
                    { english = "IM 1x a week for 3 weeks"
                    , kinyarwanda = Just "IM inshuro 1 buri cyumweru mu byumweru 3"
                    }

                TreatmentErythromycin ->
                    { english = "by mouth 4x a day for 14 days"
                    , kinyarwanda = Just "mu kanwa inshuro enye ku munsi mu minsi 14"
                    }

                TreatmentAzithromycin ->
                    { english = "4 tabs by mouth x one day"
                    , kinyarwanda = Just "ibinini 4 abinywe mu kanwa umunsi umwe"
                    }

                TreatmentCeftriaxon ->
                    { english = "IM daily x 10 days"
                    , kinyarwanda = Just "IM buri munsi mu minsi 10"
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "1 tablet by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "kunywa ikinini 1 inshuro ku munsi mu minsi 7"
                    }

                TreatmentNitrofurantoin ->
                    { english = "by mouth 2x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi mu minsi 7"
                    }

                TreatmentAmoxicillin ->
                    { english = "by mouth 3x a day for 7 days"
                    , kinyarwanda = Just "mu kanwa inshuro 3 ku munsi mu minsi 7"
                    }

                TreatmentClotrimaxazole200 ->
                    { english = "vaginally every night x 3 night"
                    , kinyarwanda = Just "mu gitsina buri joro mu majoro 3"
                    }

                TreatmentClotrimaxazole500 ->
                    { english = "vaginally one time"
                    , kinyarwanda = Just "inshuro imwe mu gitsina"
                    }

                TreatmentMethyldopa2 ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi"
                    }

                TreatmentMethyldopa3 ->
                    { english = "by mouth 3x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 3 ku munsi"
                    }

                TreatmentMethyldopa4 ->
                    { english = "by mouth 4x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 4 ku munsi"
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "by mouth 2x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 2 ku munsi"
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "by mouth 1x a day"
                    , kinyarwanda = Just "mu kanwa inshuro 1 ku munsi"
                    }

                TreatmentHydrochlorothiazide ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Nothing
                    }

                TreatmentAmlodipine ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Nothing
                    }

                TreatmentNifedipine ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Nothing
                    }

                TreatmentCaptopril ->
                    { english = "1 tablet by mouth 3 times a day"
                    , kinyarwanda = Nothing
                    }

                TreatmentLisinopril ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Nothing
                    }

                TreatmentAtenlol ->
                    { english = "1 tablet by mouth daily"
                    , kinyarwanda = Nothing
                    }

                TreatmentCloxacillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "Kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    }

                TreatmentMastitisAmoxicillin ->
                    { english = "2 capsules by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "Kunywa ibinini bibiri inshuri 3 ku munsi mu minsi 7"
                    }

                TreatmentPenecilinV ->
                    { english = "2 tablets by mouth 3 times a day for 7 days"
                    , kinyarwanda = Just "Ibinini 2 mu kanwa inshuri 3 ku munsi mu minsi 7"
                    }

                TreatmentParacetamol ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "Ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    }

                TreatmentIbuprofen ->
                    { english = "1 tablet by mouth 3 times a day for 5 days"
                    , kinyarwanda = Just "Ikinini 1 mu kanwa inshuri 3 ku munsi mu minsi 5"
                    }

                TreatmentMetformin1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide1m1e ->
                    { english = "1 tablet by mouth twice a day"
                    , kinyarwanda = Nothing
                    }

                TreatmentMetformin2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide2m1e ->
                    { english = "2 tablets by mouth in the morning and 1 tablet by mouth in the evening"
                    , kinyarwanda = Nothing
                    }

                TreatmentMetformin2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide2m2e ->
                    { english = "2 tablets by mouth twice a day"
                    , kinyarwanda = Nothing
                    }

                -- Dosage is not applicable for other options.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        RecommendedTreatmentSignLabelForProgressReport sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "Quinine Sulphate - per os 10 mg/kg/dose, 3 times a day for 7 days"
                    , kinyarwanda = Nothing
                    }

                TreatmentCoartem ->
                    { english = "Coartem - 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Nothing
                    }

                _ ->
                    translationSet (RecommendedTreatmentSignLabel sign)

        RecommendedTreatmentSignLabel sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "Give quinine sulphate per os 10 mg/kg/dose, 3 times a day for 7 days"
                    , kinyarwanda = Just "Tanga umuti wa Kinini mu kanwa: 10mg ku kilo, gatatu ku munsi, mu minsi irindwi"
                    }

                TreatmentCoartem ->
                    { english = "Give Coartem 4 tablets by mouth twice per day x 3 days"
                    , kinyarwanda = Just "Tanga AL (Kowaritemu) ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    }

                TreatmentWrittenProtocols ->
                    { english = "GI complications: followed Written Protocols"
                    , kinyarwanda = Just "Afite ibibazo by'urwungano ngogozi: Kurikiza amabwiriza"
                    }

                TreatmentReferToHospital ->
                    { english = "Severe Malaria: Stabilize and Refer to Hospital"
                    , kinyarwanda = Just "Afite Malaria y'Igikatu: Tanga umuti w'ibanze uhite umwoherza ku bitaro"
                    }

                NoTreatmentForMalaria ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    }

                TreatmentPenecilin1 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    }

                TreatmentPenecilin3 ->
                    { english = "Penicillin (2.4 million units)"
                    , kinyarwanda = Just "Penisilini (Miliyoni 2.4)"
                    }

                TreatmentErythromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Erythromycin (500mg)"
                    , kinyarwanda = Just "Erythromicine (500mg)"
                    }

                TreatmentAzithromycin ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Azithromycin (2g)"
                    , kinyarwanda = Just "Azithromycine (2g)"
                    }

                TreatmentCeftriaxon ->
                    -- Names of Medication, therefore,
                    -- no translation is needed.
                    { english = "Ceftriaxone (1g)"
                    , kinyarwanda = Nothing
                    }

                NoTreatmentForSyphilis ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    }

                TreatmentMethyldopa2 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMethyldopa3 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMethyldopa4 ->
                    { english = "Methyldopa (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "Carvedilol (6.25mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentHydrochlorothiazide ->
                    { english = "Hydrochlorothiazide (12.5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentAmlodipine ->
                    { english = "Amlodipine (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentNifedipine ->
                    { english = "Nifedipine (20mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentCaptopril ->
                    { english = "Captopril (25mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentLisinopril ->
                    { english = "Lisinopril (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentAtenlol ->
                    { english = "Atenlol (125mg)"
                    , kinyarwanda = Nothing
                    }

                NoTreatmentForHypertension ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    }

                TreatmentAluminiumHydroxide ->
                    { english = "Aluminium Hydroxide (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentHealthEducationForHeartburn ->
                    { english = "Not dispensing medicine. Follow health education protocols."
                    , kinyarwanda = Just "Witanga umuti. Kurikiza amabwiriza ajyanye n'inyigisho z'buzima."
                    }

                TreatmentNitrofurantoin ->
                    { english = "Nitrofurantoin (100mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentAmoxicillin ->
                    { english = "Amoxicillin (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentClotrimaxazole200 ->
                    { english = "Clotrimaxazole (200mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentClotrimaxazole500 ->
                    { english = "Clotrimaxazole (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentCloxacillin ->
                    { english = "Cloxacillin (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMastitisAmoxicillin ->
                    { english = "Amoxicillin (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentPenecilinV ->
                    { english = "Penicillin V (250mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentParacetamol ->
                    { english = "Paracetamol (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentIbuprofen ->
                    { english = "Ibuprofen (400mg)"
                    , kinyarwanda = Nothing
                    }

                NoTreatmentForMastitis ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    }

                TreatmentMetformin1m1e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide1m1e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMetformin2m1e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide2m1e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMetformin2m2e ->
                    { english = "Metformin (500mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide2m2e ->
                    { english = "Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentMetformin2m2eGlipenclamide1m1e ->
                    { english = "Metformin (500mg), Glipenclamide (5mg)"
                    , kinyarwanda = Nothing
                    }

                TreatmentGlipenclamide2m2eMetformin1m1e ->
                    { english = "Glipenclamide (5mg), Metformin (500mg)"
                    , kinyarwanda = Nothing
                    }

                NoTreatmentForDiabetes ->
                    { english = "No Treatment Administered"
                    , kinyarwanda = Just "Nta muti watanzwe"
                    }

        RecordAcuteIllnessOutcome ->
            { english = "Record Acute Illness Outcome"
            , kinyarwanda = Just "Andika iherezo ry'indwara ifatiyeho"
            }

        RecordPregnancyOutcome ->
            { english = "Record Pregnancy Outcome"
            , kinyarwanda = Just "Andika iherezo ry'inda"
            }

        RectalHemorrhoids ->
            { english = "Rectal Hemorrhoids"
            , kinyarwanda = Just "Kubyimba kw'imitsi y'ishyira(rectum)/Hemoroyide"
            }

        RecurringHighSeverityAlert alert ->
            case alert of
                Backend.PrenatalActivity.Model.BloodPressure ->
                    { english = "Blood Pressure"
                    , kinyarwanda = Just "Umuvuduko w'amaraso"
                    }

        ReferredPatientToFacilityQuestion facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Have you referred the patient to the health center"
                    , kinyarwanda = Just "Waba wohereje umurwayi ku kigo nderabuzima"
                    }

                FacilityHospital ->
                    { english = "Have you referred the patient to the hospital"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Have you referred the patient to the specialist"
                    , kinyarwanda = Just "Waba wohereje umubyeyi ku muganaga w'inzobere"
                    }

                FacilityARVProgram ->
                    { english = "Have you referred the patient to the ARV services"
                    , kinyarwanda = Just "Waba wohere umubyeyi muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Have you referred the patient to NCD services"
                    , kinyarwanda = Just "Waba wohereje umubyeyi muri service y'indwara zitandura"
                    }

                FacilityANCServices ->
                    { english = "Have you referred the patient to ANC services"
                    , kinyarwanda = Nothing
                    }

        ReferredToFacility facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Referred to health center"
                    , kinyarwanda = Just "Yoherejwe ku kigo nderabuzima"
                    }

                FacilityHospital ->
                    { english = "Referred to hospital"
                    , kinyarwanda = Just "Yoherejwe ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Referred to mental health specialist"
                    , kinyarwanda = Nothing
                    }

                FacilityARVProgram ->
                    { english = "Referred to ARV services"
                    , kinyarwanda = Just "Yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Referred to NCD services"
                    , kinyarwanda = Just "Yoherejwe muri service y'indwara zitandura"
                    }

                FacilityANCServices ->
                    { english = "Referred to ANC services"
                    , kinyarwanda = Nothing
                    }

        ReferredToFacilityNot facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Not referred to health center"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku kigo nderabuzima"
                    }

                FacilityHospital ->
                    { english = "Not referred to hospital"
                    , kinyarwanda = Just "Ntabwo yoherejwe ku bitaro"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Not referred to mental health specialist"
                    , kinyarwanda = Just "Ntabwo yoherejwe kwa muganga w'inzobere w'ubuzima bwo mu mutwe"
                    }

                FacilityARVProgram ->
                    { english = "Not referred to ARV services"
                    , kinyarwanda = Just "Ntago yoherejwe muri service itanga imiti igabanya ubukana bwa Virusi itera SIDA"
                    }

                FacilityNCDProgram ->
                    { english = "Not referred to NCD services"
                    , kinyarwanda = Just "Ntabwo yoherejwe muri service y'indwara zitandura"
                    }

                FacilityANCServices ->
                    { english = "Not referred to ANC services"
                    , kinyarwanda = Nothing
                    }

        ReferredToFacilityPostpartum facility ->
            case facility of
                FacilityARVProgram ->
                    { english = "referred to ARV services for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise itanga imiti igabanya ubukana bwa Virusi itera SIDA kugirango akurikiranwe nyuma yo kubyara"
                    }

                FacilityNCDProgram ->
                    { english = "referred to NCD program for post-partum management"
                    , kinyarwanda = Just "yoherejwe muri serivise y'indwara zitandura kugirango akurikiranwe nyuma yo kubyara"
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        ReferToHospitalForFurtherEvaluation ->
            { english = "Refer patient to hospital for further evaluation"
            , kinyarwanda = Nothing
            }

        ReferToProgramAction ->
            { english = "Refer patient to appropriate nutrition program"
            , kinyarwanda = Nothing
            }

        ReferToProgramQuestion ->
            { english = "Did you direct the patient to attend the next program session"
            , kinyarwanda = Nothing
            }

        Register ->
            { english = "Register"
            , kinyarwanda = Nothing
            }

        RegisterContactHelper ->
            { english = "Not the contact you were looking for?"
            , kinyarwanda = Just "Ntabwo ari uwo washakishaga?"
            }

        RegisterParticipantHelper ->
            { english = "Not the participant you were looking for?"
            , kinyarwanda = Nothing
            }

        RegisterNewContact ->
            { english = "Register a new contact"
            , kinyarwanda = Just "Andika umuntu mushya wahuye n'umurwayi"
            }

        RegisterNewParticipant ->
            { english = "Register a new participant"
            , kinyarwanda = Just "Andika umurwayi mushya"
            }

        RegistratingHealthCenter ->
            { english = "Registrating Health Center"
            , kinyarwanda = Just "Izina ry'ikigo nderabuzima umugenerwabikorwa abarizwamo"
            }

        RegistrationSuccessful ->
            { english = "Registration Successful"
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulParticipantAdded ->
            { english = "The participant has been added to E-Heza."
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulSuggestAddingChild ->
            { english = "The participant has been added to E-Heza. Would you like to add a child for this participant?"
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulSuggestAddingMother ->
            { english = "The participant has been added to E-Heza. Would you like to add a mother for this participant?"
            , kinyarwanda = Nothing
            }

        RelationSuccessful ->
            { english = "Relation Successful"
            , kinyarwanda = Nothing
            }

        RelationSuccessfulChildWithMother ->
            { english = "Child succesfully assocoated with mother."
            , kinyarwanda = Nothing
            }

        RelationSuccessfulMotherWithChild ->
            { english = "Mother succesfully assocoated with child."
            , kinyarwanda = Nothing
            }

        RenalDisease ->
            { english = "Renal Disease"
            , kinyarwanda = Just "Indwara z'impyiko"
            }

        RemainingForDownloadLabel ->
            { english = "Remaining for Download"
            , kinyarwanda = Just "Ibisigaye gukurwa kuri seriveri"
            }

        RemainingForUploadLabel ->
            { english = "Remaining for Upload"
            , kinyarwanda = Just "Ibisigaye koherezwa kuri seriveri"
            }

        RemainingTotalToUpload ->
            { english = "Remaining to upload, in total"
            , kinyarwanda = Nothing
            }

        ReportAge age ->
            { english = "Age: " ++ age
            , kinyarwanda = Just <| "Imyaka: " ++ age
            }

        ReportDOB dob ->
            { english = "DOB: " ++ dob
            , kinyarwanda = Just <| "Itariki y'amavuko: " ++ dob
            }

        ReportRemaining remaining ->
            { english = String.fromInt remaining ++ " remaning"
            , kinyarwanda = Just <| String.fromInt remaining ++ " iyibutswa rya raporo"
            }

        ReportResultsOfContactsSearch total ->
            case total of
                1 ->
                    { english = "There is 1 contract that matches your search."
                    , kinyarwanda = Just "Hagaragaye umuntu 1 uhuye nuwo washakaga."
                    }

                _ ->
                    { english = "There are " ++ String.fromInt total ++ " contacts that match your search."
                    , kinyarwanda = Just <| "Hagaragaye abantu " ++ String.fromInt total ++ " bahuje nibyo ushakisha."
                    }

        ReportResultsOfParticipantsSearch total ->
            case total of
                1 ->
                    { english = "There is 1 participant that matches your search."
                    , kinyarwanda = Just "Hari umujyenerwabikorwa 1 uhuye nuwo washatse"
                    }

                _ ->
                    { english = "There are " ++ String.fromInt total ++ " participants that match your search."
                    , kinyarwanda = Just <| "Hari abagenerwabikorwa " ++ String.fromInt total ++ " bahuye nuwo ushaka mu ishakiro"
                    }

        Reports ->
            { english = "Reports"
            , kinyarwanda = Just "Raporo"
            }

        RecentAndUpcomingGroupEncounters ->
            { english = "Recent and upcoming Group Encounters"
            , kinyarwanda = Just "Ahabarizwa amatsinda aheruka gukorerwa n'agiye gukorerwa"
            }

        ReportCompleted { pending, completed } ->
            { english = String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ " / " ++ String.fromInt (pending + completed) ++ " Raporo irarangiye"
            }

        ResolveMonth short month ->
            translateMonth month short

        ResolveMonthYY year short month ->
            translateMonthYY month year short

        RespiratoryDistress ->
            { english = "Respiratory Distress"
            , kinyarwanda = Just "Ahumeka bimugoye"
            }

        RespiratoryRate ->
            { english = "Respiratory Rate"
            , kinyarwanda = Just "Inshuro ahumeka"
            }

        ResponsePeriod period ->
            case period of
                LessThan30Min ->
                    { english = "Less than 30 min"
                    , kinyarwanda = Just "Munsi y'iminota mirongo itatu"
                    }

                Between30min1Hour ->
                    { english = "30 min - 1 hour"
                    , kinyarwanda = Just "Hagati y’iminota mirongo itatu n’isaha"
                    }

                Between1Hour2Hour ->
                    { english = "1 hour - 2 hours"
                    , kinyarwanda = Just "Hagati y'isaha n'amasaha abiri"
                    }

                Between2Hour1Day ->
                    { english = "2 hours - 1 day"
                    , kinyarwanda = Just "Hagati y'amasaha abiri n'umunsi"
                    }

                ResponsePeriodNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    }

        Result ->
            { english = "Result"
            , kinyarwanda = Just "Igisubizo"
            }

        ResultOfContacting114 recommendation ->
            case recommendation of
                SendToHealthCenter ->
                    { english = "114 recommended to send patient to the nearest health center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku kigo nderabuzima kinyegereye"
                    }

                SendToRRTCenter ->
                    { english = "114 recommended to send patient to Rapid Response Team center"
                    , kinyarwanda = Just "Ku 114 Bangiriye inama yo kohereza umurwayi ku itsinda rishinzwe gutanga ubuvuzi bwihuse"
                    }

                SendToHospital ->
                    { english = "114 recommended to send patient to the nearest hospital"
                    , kinyarwanda = Just "Ku 114 bangiriye inama yo kohereza umurwayi ku bitaro binyegereye"
                    }

                OtherRecommendation114 ->
                    { english = "114 did not recommended to send patient to site"
                    , kinyarwanda = Just "Ku 114 bansabye kutohereza umurwayi"
                    }

                NoneNoAnswer ->
                    { english = "Not able to talk to 114 - no answer"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- nta gisubizo"
                    }

                NoneBusySignal ->
                    { english = "Not able to talk to 114 - busy signal"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- umurongo bawuvugiragaho"
                    }

                NoneOtherRecommendation114 ->
                    { english = "Not able to talk to 114 - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana ku 114- Izindi mpamvu"
                    }

        ResultOfContactingRecommendedSite recommendation ->
            case recommendation of
                TeamComeToVillage ->
                    { english = "Site recommendation: Team will come to village"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Itsinda rizaza mu mudugudu"
                    }

                SendToSiteWithForm ->
                    { english = "Site recommendation: Send patient to site with referral form"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Twohereze umurwayi yitwaje impapuro zimwohereza"
                    }

                OtherRecommendationSite ->
                    { english = "Site recommendation: Other"
                    , kinyarwanda = Just "Imyanzuro y’urwego rubishinzwe: Ibindi"
                    }

                NoneSentWithForm ->
                    { english = "Not able to talk to site due - no response. Sent patient with referral form"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera- nta gisubizo cyabonetse. Nohereje umurwayi yitwaje impapuro zimwohereza"
                    }

                NonePatientRefused ->
                    { english = "Did not talk to site as patient has refused"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe kubera umurwayi yanze"
                    }

                NoneOtherRecommendationSite ->
                    { english = "Not able to talk to site - other reason"
                    , kinyarwanda = Just "Ntibyakunze kuvugana n’urwego rubishinzwe- Izindi mpamvu"
                    }

                RecommendationSiteNotApplicable ->
                    { english = "Not Applicable"
                    , kinyarwanda = Just "Ibi ntibikorwa"
                    }

        ResultsMissing ->
            { english = "Results Missing"
            , kinyarwanda = Just "Ibisubizo Ntibihari"
            }

        ResultsPending ->
            { english = "Results Pending"
            , kinyarwanda = Just "Ibisubizo birategerejwe"
            }

        Retry ->
            { english = "Retry"
            , kinyarwanda = Just "Kongera kugerageza"
            }

        ReviewCaseWith144Respondent ->
            { english = "Review case with 114 Respondent"
            , kinyarwanda = Just "Ongera ukore isuzuma ufatanije n’ukwitabye kuri 114"
            }

        Reviewed ->
            { english = "Reviewed"
            , kinyarwanda = Just "Byarebwe"
            }

        ReviewPriorDiagnosis ->
            { english = "Review Prior Diagnosis"
            , kinyarwanda = Just "Kureba uburwayi yagize/yigeze kurwara"
            }

        RhNegative ->
            { english = "RH Negative"
            , kinyarwanda = Just "Ubwoko bw'amaraso ni Negatifu"
            }

        Right ->
            { english = "Right"
            , kinyarwanda = Just "Iburyo"
            }

        RiskFactorAlert factor ->
            case factor of
                FactorNumberOfCSections number ->
                    if number == 1 then
                        { english = "1 previous C-section"
                        , kinyarwanda = Just "Yabazwe inshuro imwe ubushize"
                        }

                    else
                        { english = String.fromInt number ++ " previous C-sections"
                        , kinyarwanda = Just <| String.fromInt number ++ " ubushize yarabazwe"
                        }

                FactorCSectionInPreviousDelivery ->
                    { english = "C-section in previous delivery"
                    , kinyarwanda = Just "Yarabazwe ku nda ishize"
                    }

                FactorCSectionReason ->
                    { english = "C-section in previous delivery due to"
                    , kinyarwanda = Just "Ubushize yabazwe abyara kubera"
                    }

                FactorPreviousDeliveryPeriod ->
                    { english = "Previous delivery"
                    , kinyarwanda = Just "kubyara guheruka"
                    }

                FactorSuccessiveAbortions ->
                    { english = "Patient experienced successive abortions"
                    , kinyarwanda = Just "Umubyeyi yavanyemo inda zikurikiranye"
                    }

                FactorSuccessivePrematureDeliveries ->
                    { english = "Patient experienced successive preterm deliveries"
                    , kinyarwanda = Just "Umubyeyi yabyaye inda zidashyitse zikurikiranye"
                    }

                FactorStillbornPreviousDelivery ->
                    { english = "Stillbirth in previous delivery"
                    , kinyarwanda = Just "Ubushize yabyaye umwana upfuye(wapfiriye mu nda)"
                    }

                FactorBabyDiedOnDayOfBirthPreviousDelivery ->
                    { english = "Live Birth but the baby died the same day in previous delivery"
                    , kinyarwanda = Just "Aheruka kubyara umwana muzima apfa uwo munsi"
                    }

                FactorPartialPlacentaPreviousDelivery ->
                    { english = "Patient had partial placenta in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize iya nyuma ntiyavutse yose/yaje igice"
                    }

                FactorSevereHemorrhagingPreviousDelivery ->
                    { english = "Patient experienced severe hemorrhage in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yaravuye cyane/bikabije ku nda y'ubushize"
                    }

                FactorPreeclampsiaPreviousPregnancy ->
                    { english = "Patient had preeclampsia in previous pregnancy"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso bibanziriza kugagara ku nda y'ubushize"
                    }

                FactorConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Just "Ubushize mubyeyi yagize ibimenyetso byo kugagara/Guhinda umushyitsi abyara"
                    }

                FactorConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions and resulted in becoming unconscious after delivery"
                    , kinyarwanda = Just "Umubyeyi yagize ibimenyetso byo kugagara nyuma yo kubyara bimuviramo kutumva/guta ubwenge"
                    }

                FactorIncompleteCervixPreviousPregnancy ->
                    { english = "Patient had an Incomplete Cervix in previous pregnancy"
                    , kinyarwanda = Just "Ku nda y'ubushize inkondo y'umura ntiyashoboye kwifunga neza"
                    }

                FactorVerticalCSectionScar ->
                    { english = "Vertical C-Section Scar"
                    , kinyarwanda = Just "Inkovu yo kubagwa irahagaze"
                    }

                FactorGestationalDiabetesPreviousPregnancy ->
                    { english = "Patient had Gestational Diabetes in previous pregnancy"
                    , kinyarwanda = Just "Ubushize umubyeyi yagize indwara ya Diyabete itewe no gutwita"
                    }

        RiskFactors ->
            { english = "Risk Factors"
            , kinyarwanda = Just "Abashobora kwibasirwa n'indwara runaka (kubera impamvu zitandukanye:kuba atwite..)"
            }

        SachetsPerDayHelper weight recommendation ->
            { english = "The recommended amount for a " ++ String.fromFloat weight ++ " kg child is " ++ String.fromFloat recommendation ++ " sachets a day"
            , kinyarwanda = Just <| "Amasashe yemewe ku mwana w'ibiro " ++ String.fromFloat weight ++ " ni " ++ String.fromFloat recommendation ++ " ku munsi"
            }

        SachetsPerDayQuestion ->
            { english = "How many sachets of supplement is given to the child per day"
            , kinyarwanda = Just "Ni amasashe angahe ahabwa umwana ku munsi"
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            }

        SaveAndNext ->
            { english = "Save & Next"
            , kinyarwanda = Just "Bika & ukomeze"
            }

        SaveAndRecordOutcome ->
            { english = "Save & Record Outcome"
            , kinyarwanda = Just "Bika & Andika iherezo ry'uburwayi"
            }

        SavedMoneyQuestion ->
            { english = "Have you saved money for use at the health center while you give birth"
            , kinyarwanda = Just "Wazigamye amafaranga yo gukoresha ku kigo nderabuzima igihe cyo kubyara"
            }

        SaveError ->
            { english = "Save Error"
            , kinyarwanda = Just "Kubika error (ikosa mu kubika)"
            }

        ScheduleFollowUp ->
            { english = "Schedule Follow Up"
            , kinyarwanda = Nothing
            }

        Search ->
            { english = "Search"
            , kinyarwanda = Nothing
            }

        SearchByName ->
            { english = "Search by Name"
            , kinyarwanda = Just "Gushakisha izina"
            }

        SearchEhezaForExistingParticipants ->
            { english = "Search E-Heza to see if the contact already exists"
            , kinyarwanda = Just "Reba muri E-heza niba abo bahuye basanzwe barimo"
            }

        SearchExistingParticipants ->
            { english = "Search Existing Participants"
            , kinyarwanda = Just "Gushaka abagenerwabikorwa basanzwe muri sisiteme"
            }

        SearchHelper ->
            { english = "Search to see if the participant already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Shakisha kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba atagaragara, mwandike nku mushya."
            }

        SearchHelperFamilyMember ->
            { english = "Search to see if the additional family member already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Just "Kanda ku Ishakiro kugirango urebe niba umugenerwabikorwa asanzwe ari muri E-Heza. Niba uwo muntu atagaragara mu ishakiro, mwandike nk'umugenerwabikorwa mushya."
            }

        SecondName ->
            { english = "Second Name"
            , kinyarwanda = Just "Izina ry'umuryango"
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Just "Umurenge"
            }

        SeeDosageScheduleByWeight ->
            { english = "See dosage schedule by Weight"
            , kinyarwanda = Nothing
            }

        SeeLabResults ->
            { english = "See Lab Results"
            , kinyarwanda = Just "Reba Ibisubizo by'Ibizamini Byafashwe"
            }

        SeeMore ->
            { english = "See More"
            , kinyarwanda = Just "Reba Ibindi"
            }

        SelectAntenatalVisit ->
            { english = "Select an Antenatal Visit"
            , kinyarwanda = Just "Hitamo inshuro aje kwipimishaho inda"
            }

        SelectAllDiagnoses ->
            { english = "Select all diagnoses"
            , kinyarwanda = Just "Hitamo uburwayi bwose bwagaragaye"
            }

        SelectAllSigns ->
            { english = "Select all signs that are present"
            , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite"
            }

        SelectDangerSigns ->
            { english = "Please select one or more of the danger signs the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            }

        SelectDate ->
            { english = "Select Date"
            , kinyarwanda = Just "Hitamo Itariki"
            }

        SelectedFamilyPlanningMethod ->
            { english = "Selected Family Planning Method"
            , kinyarwanda = Just "Uburyo bwo kuboneza urubyaro bwatoranijwe"
            }

        SelectIllnessSymptoms ->
            { english = "Please select one or more symptoms the patient is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cyangwa byinshi mu bimenyetso umurwayi afite"
            }

        SelectPostpartumChildDangerSigns ->
            { english = "Please select one or more of the danger signs the child is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umwana  yaba afite?"
            }

        SelectPostpartumMotherDangerSigns ->
            { english = "Please select one or more of the danger signs the mother is experiencing"
            , kinyarwanda = Just "Hitamo kimwe cg byinshi mu bimenyetso mpuruza umubyeyi yaba afite"
            }

        SelectedProgram ->
            { english = "Selected Program"
            , kinyarwanda = Just "Porogaramu Yatoranyijwe"
            }

        SelectedVillage ->
            { english = "Selected Village"
            , kinyarwanda = Just "Umudugudu Watoranyijwe"
            }

        SelectEncounterType ->
            { english = "Select an encounter type"
            , kinyarwanda = Just "Hitamo ubwoko bw'icyiciro cyo gukorera"
            }

        SelectLanguage ->
            { english = "Select language"
            , kinyarwanda = Nothing
            }

        SelectExistingAcuteIllness ->
            { english = "Select Existing Acute Illness"
            , kinyarwanda = Just "Hitamo Indwara ifatiyeho iheruka kuvurwa"
            }

        SelectExistingAcuteIllnessToRecordOutcome ->
            { english = "Select Existing Acute Illness to Record Outcome"
            , kinyarwanda = Just "Hitamo indwara ifatiyeho iheruka kuvurwa kugira ngo wandike iherezo ryayo"
            }

        SelectGroup ->
            { english = "Select Group..."
            , kinyarwanda = Just "Hitamo itsinda ryawe..."
            }

        SelectProgram ->
            { english = "Select Program"
            , kinyarwanda = Just "Hitamo porogaramu"
            }

        SelectYourGroup ->
            { english = "Select your Group"
            , kinyarwanda = Just "Hitamo itsinda ryawe"
            }

        SelectYourHealthCenter ->
            { english = "Select your Health Center"
            , kinyarwanda = Just "Hitamo ikigo nderabuzima"
            }

        SelectYourVillage ->
            { english = "Select your village"
            , kinyarwanda = Just "Hitamo umudugudu wawe"
            }

        SelectedHCDownloading ->
            { english = "Downloading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            }

        SelectedHCNotSynced ->
            { english = "Data is not synced"
            , kinyarwanda = Nothing
            }

        SelectedHCSyncing ->
            { english = "Data is syncing"
            , kinyarwanda = Nothing
            }

        SelectedHCUploading ->
            { english = "Uploading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActive ->
            { english = "The app is installed on this device."
            , kinyarwanda = Just "Apulikasiyo muri icyi cyuma cy'inkoranabuhanga yinjijwe."
            }

        ServiceWorkerCurrent ->
            { english = "You have the current version of the app."
            , kinyarwanda = Just "Ufite apulikasiyo nshya igezweho uyu munsi"
            }

        ServiceWorkerCheckForUpdates ->
            { english = "Check for updates"
            , kinyarwanda = Just "Kugenzura ibyavuguruwe"
            }

        ServiceWorkerInstalling ->
            { english = "A new version of the app has been detected and is being downloaded. You can continue to work while this is in progress."
            , kinyarwanda = Nothing
            }

        ServiceWorkerInstalled ->
            { english = "A new version of the app has been downloaded."
            , kinyarwanda = Just "Gufungura verisio nshyashya byarangiye."
            }

        ServiceWorkerSkipWaiting ->
            { english = "Activate new version of the app"
            , kinyarwanda = Just "Gufungura verisio nshyashya"
            }

        ServiceWorkerRestarting ->
            { english = "The app should reload momentarily with the new version."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActivating ->
            { english = "A new version of the app is preparing itself for use."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActivated ->
            { english = "A new version of the app is ready for use."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRedundant ->
            { english = "An error occurred installing a new version of the app."
            , kinyarwanda = Nothing
            }

        ServiceWorkerInactive ->
            { english = "The app is not yet installed on this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegNotAsked ->
            { english = "We have not yet attempted to install the app on this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegLoading ->
            { english = "Installation of the app on this device is progressing."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegErr ->
            { english = "There was an error installing the app on this device. To try again, reload this page."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegSuccess ->
            { english = "The app was successfully registered with this device."
            , kinyarwanda = Just "Igikorwa cyo gushyira apulikasiyo kuri iki gikoresho cy'ikoranabuhanga cyagenze neza."
            }

        ServiceWorkerStatus ->
            { english = "Deployment Status"
            , kinyarwanda = Just "Ibijyanye no kuvugurura no kongerera ubushobozi sisiteme"
            }

        SevereAcuteMalnutrition ->
            { english = "Severe acute malnutrition"
            , kinyarwanda = Just "Imirire mibi ikabije imaze igihe gito"
            }

        SevereHemorrhagingPreviousDelivery ->
            { english = "Severe Hemorrhaging in previous delivery (>500 ml)"
            , kinyarwanda = Just "Ubushize yavuye cyane akimara kubyara hejuru ya Ml 500"
            }

        Shared ->
            { english = "Shared"
            , kinyarwanda = Just "Ayisangira n'abandi"
            }

        SignOnDoorPostedQuestion ->
            { english = "Have you posted signs on the door indicating that the space is an isolation area"
            , kinyarwanda = Just "Waba washyize ibimenyetso ku rugi byerekana ko iki cyumba ari ikijyamo abantu bari mu kato"
            }

        SocialHistoryHivTestingResult result ->
            case result of
                ResultHivPositive ->
                    { english = "Positive"
                    , kinyarwanda = Just "Afite Ubwandu"
                    }

                ResultHivNegative ->
                    { english = "Negative"
                    , kinyarwanda = Just "Nta bwandu afite"
                    }

                ResultHivIndeterminate ->
                    { english = "Indeterminate"
                    , kinyarwanda = Just "Ntibisobanutse"
                    }

                NoHivTesting ->
                    { english = "No HIV Testing"
                    , kinyarwanda = Just "Ntibibonetse"
                    }

        SpecialityCareHeaderPrefix ->
            { english = "You were diagnosed with"
            , kinyarwanda = Just "Wasuzumwe uburwayi bwa"
            }

        SpecialityCareHeaderSuffix ->
            { english = "during your pregnancy"
            , kinyarwanda = Just "Mu gihe wari utwite"
            }

        SpecialityCareSignQuestion sign ->
            case sign of
                EnrolledToARVProgram ->
                    { english = "Are you currently enrolled in ARV services at the health center"
                    , kinyarwanda = Just "Waba wanditswe muri serivise itanaga imiti igabanya ubukana bwa Vurusi itera SIDA ku kigo nderabuzima"
                    }

                EnrolledToNCDProgram ->
                    { english = "Are you currently enrolled in NCD services at the health center"
                    , kinyarwanda = Just "Waba usanzwe wanditse muri serivisi y'indwara zitandura ku kigo nderabusima"
                    }

                NoSpecialityCareSigns ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        StillbornPreviousDelivery ->
            { english = "Stillborn in previous delivery"
            , kinyarwanda = Just "Aheruka kubyara umwana upfuye"
            }

        SubsequentEncounter ->
            { english = "Subsequent Encounter"
            , kinyarwanda = Just "Igikorwa gikurikiyeho"
            }

        SubsequentAntenatalVisit ->
            { english = "Subsequent Antenatal Visit"
            , kinyarwanda = Just "Igihe cyo kongera kwipimisha inda"
            }

        SubsequentEncounterReferral encounterType ->
            if encounterType == AcuteIllnessEncounterCHW then
                { english = "CHW Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku mujyanama w'ubuzima"
                }

            else
                { english = "Health Center Referral"
                , kinyarwanda = Just "Kohereza umurwayi ku kigo nderabuzima"
                }

        SuccessiveAbortions ->
            { english = "Successive Abortions"
            , kinyarwanda = Just "Inda zavuyemo zikurikiranye"
            }

        SuccessivePrematureDeliveries ->
            { english = "Successive Premature Deliveries"
            , kinyarwanda = Just "Inda zavutse zidashyitse zikurikiranye"
            }

        SuspectedCovid19CaseAlert ->
            { english = "Suspected COVID-19 case"
            , kinyarwanda = Just "Acyekwaho kwandura COVID-19"
            }

        SuspectedCovid19CaseAlertHelper ->
            { english = "Please isolate immediately from family and contact health center"
            , kinyarwanda = Just "Mutandukanye n'umuryango we byihuse uhite umenyesha Ikigo nderabuzima"
            }

        SuspectedCovid19CaseIsolate ->
            { english = "Isolate immediately from family"
            , kinyarwanda = Just "Mutandukanye ako kanya n'umuryango we umushyire mu kato"
            }

        SuspectedCovid19CaseContactHC ->
            { english = "Contact health center immediately"
            , kinyarwanda = Just "Menyesha ikigo nderabuzima ako kanya"
            }

        SuspectedCovid19CasePerformRapidTest ->
            { english = "Perform a rapid test immediately"
            , kinyarwanda = Just "Kora ikizamini nonaha"
            }

        SuspectedCovid19CaseReferToHCForTesting ->
            { english = "Refer to Health Center for testing"
            , kinyarwanda = Nothing
            }

        SymptomRelief type_ ->
            case type_ of
                SymptomReliefParacetamol ->
                    { english = "Paracetamol for Fever"
                    , kinyarwanda = Just "Umuti wa Paracetamoro ugabanya umuriro"
                    }

                SymptomReliefVitaminC ->
                    { english = "Effervescent Vitamin C tablets"
                    , kinyarwanda = Just "Ibinini bya Vitamin C"
                    }

                SymptomReliefPaidoterineSyrup ->
                    { english = "Paidoterin syrup as a decongestant"
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero"
                    }

                SymptomReliefCoughMixture ->
                    { english = "Cough mixtures such as Ascoril, Bronchalene, etc."
                    , kinyarwanda = Just "Umuti wa Siro Pedotere ku ndwara z'imyanya y'ubuhumekero, n'indi"
                    }

        Symptoms ->
            { english = "Symptoms"
            , kinyarwanda = Just "Ibimenyetso"
            }

        SymptomsAtFirstEncounter ->
            { english = "Symptoms at first encounter"
            , kinyarwanda = Just "Ibimenyetso ku isuzuma rya mbere"
            }

        SymptomsGeneralSign sign ->
            case sign of
                BodyAches ->
                    { english = "Body Aches"
                    , kinyarwanda = Just "Ububabare bw'umubiri wose"
                    }

                Chills ->
                    { english = "Chills"
                    , kinyarwanda = Just "Gutengurwa"
                    }

                SymptomGeneralFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    }

                Headache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kubabara umutwe"
                    }

                NightSweats ->
                    { english = "Night Sweats"
                    , kinyarwanda = Just "Kubira ibyuya nijoro"
                    }

                Lethargy ->
                    { english = "Lethargy"
                    , kinyarwanda = Just "Guhwera"
                    }

                PoorSuck ->
                    { english = "Poor Suck"
                    , kinyarwanda = Just "Yonka nta mbaraga"
                    }

                UnableToDrink ->
                    { english = "Unable to Drink"
                    , kinyarwanda = Just "Ntashobora kunywa"
                    }

                UnableToEat ->
                    { english = "Unable to Eat"
                    , kinyarwanda = Just "Ntashobora kurya"
                    }

                IncreasedThirst ->
                    { english = "Increased Thirst"
                    , kinyarwanda = Just "Afite inyota cyane"
                    }

                DryMouth ->
                    { english = "Dry/Sticky Mouth"
                    , kinyarwanda = Just "Iminwa yumye"
                    }

                SevereWeakness ->
                    { english = "Severe Weakness"
                    , kinyarwanda = Just "Yacitse intege cyane"
                    }

                YellowEyes ->
                    { english = "Yellow Eyes"
                    , kinyarwanda = Just "Amaso y'umuhondo"
                    }

                CokeColoredUrine ->
                    { english = "Coca-Cola Colored Urine"
                    , kinyarwanda = Just "Inkari zisa na kokakola"
                    }

                SymptomsGeneralConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    }

                SpontaneousBleeding ->
                    { english = "Spontaneous Bleeding"
                    , kinyarwanda = Just "Kuva amaraso bitunguranye"
                    }

                NoSymptomsGeneral ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        SymptomsGISign sign ->
            case sign of
                SymptomGIAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    }

                BloodyDiarrhea ->
                    { english = "Bloody Diarrhea"
                    , kinyarwanda = Just "Arituma amaraso"
                    }

                Nausea ->
                    { english = "Nausea"
                    , kinyarwanda = Just "Afite iseseme"
                    }

                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea - >3 liquid stools in the last 24 hours"
                    , kinyarwanda = Just "Nta maraso yituma- yituma ibyoroshye inshuro zirenze 3 mu masaha 24"
                    }

                Vomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Araruka"
                    }

                NoSymptomsGI ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        SymptomsGISignAbbrev sign ->
            case sign of
                NonBloodyDiarrhea ->
                    { english = "Non-Bloody Diarrhea"
                    , kinyarwanda = Just "Nta maraso yituma"
                    }

                _ ->
                    translationSet (SymptomsGISign sign)

        SymptomsRespiratorySign sign ->
            case sign of
                BloodInSputum ->
                    { english = "Blood in Sputum"
                    , kinyarwanda = Just "Amaraso mu gikororwa"
                    }

                Cough ->
                    { english = "Cough"
                    , kinyarwanda = Just "Inkorora"
                    }

                NasalCongestion ->
                    { english = "Nasal Congestion"
                    , kinyarwanda = Just "Gufungana mu mazuru"
                    }

                ShortnessOfBreath ->
                    { english = "Shortness of Breath"
                    , kinyarwanda = Just "Guhumeka nabi"
                    }

                SoreThroat ->
                    { english = "Sore Throat"
                    , kinyarwanda = Just "Kubabara mu muhogo"
                    }

                LossOfSmell ->
                    { english = "Loss of Smell"
                    , kinyarwanda = Just "Kudahumurirwa"
                    }

                StabbingChestPain ->
                    { english = "Stabbing Chest Pain"
                    , kinyarwanda = Just "Kubabara mu gatuza"
                    }

                NoSymptomsRespiratory ->
                    { english = "None of the above"
                    , kinyarwanda = Just "Nta na kimwe mu byavuzwe haruguru"
                    }

        SymptomsTask task ->
            case task of
                SymptomsGeneral ->
                    { english = "General"
                    , kinyarwanda = Just "Ibimenyesto rusange"
                    }

                SymptomsRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Just "Ubuhumekero"
                    }

                SymptomsGI ->
                    { english = "GI"
                    , kinyarwanda = Just "Urwungano ngogozi"
                    }

        SyphilisRecommendedTreatmentHeader ->
            { english = "This patient has tested positive for Syphilis"
            , kinyarwanda = Just "Uyu murwayi afite ubwandu bwa Mburugu"
            }

        SyphilisRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            }

        SyphilisRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            }

        SyphilisRecommendedTreatmentWarning ->
            { english = "If Erythromycin or Azithromycin used, must treat newborn immediately after delivery (does not cross into placenta)"
            , kinyarwanda = Just "Niba ari Erythromicine cg Azithromycine wakoresheje, ugomba kuvura uruhinja rukivuka (Uyu muti ntiwinjira mu ngobyi y'umwana)"
            }

        GroupEncounterClosed ->
            { english = "Group Encounter closed"
            , kinyarwanda = Nothing
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
            }

        GroupEncounterLoading ->
            { english = "Loading Group Encounter"
            , kinyarwanda = Just "Gufungura icyiciro cyo gukorera"
            }

        GroupEncounterUnauthorized ->
            { english = "Group Encounter unauthorized"
            , kinyarwanda = Nothing
            }

        GroupEncounterUnauthorized2 ->
            { english =
                """You are not authorized to view this health assessment.
        Please contact the Ihangane project for further
        instructions."""
            , kinyarwanda = Nothing
            }

        SendPatientToFacility facility ->
            case facility of
                FacilityHealthCenter ->
                    { english = "Send patient to the health center"
                    , kinyarwanda = Just "Ohereza umurwayi ku kigo nderabuzima"
                    }

                FacilityHospital ->
                    { english = "Send patient to the hospital"
                    , kinyarwanda = Just "Ohereza umurwayi kwa muganga"
                    }

                FacilityMentalHealthSpecialist ->
                    { english = "Refer patient to mental health specialist for further evaluation"
                    , kinyarwanda = Just "Ohereza umubyeyi ku muganga w'inzobere ku buzima bwo mu mutwe kugirango hakorwe isuzuma ryimbitse"
                    }

                FacilityARVProgram ->
                    { english = "Direct patient to the appropriate location"
                    , kinyarwanda = Just "Yobora umurwayi ahantu habugenewe"
                    }

                FacilityNCDProgram ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

                FacilityANCServices ->
                    translationSet (SendPatientToFacility FacilityARVProgram)

        ShowAll ->
            { english = "Show All"
            , kinyarwanda = Just "Erekana amazina yose"
            }

        StartEncounter ->
            { english = "Start an encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            }

        StartEndDate ->
            { english = "Start - End"
            , kinyarwanda = Nothing
            }

        StrartNewAcuteIllnessHelper ->
            { english = "If existing Acute Illness is not part of the list above, start a new encounter"
            , kinyarwanda = Just "Niba Indwara ifatiyeho iheruka kuvurwa itagaragara ku rutonde rwavuzwe haruguru , tangira isuzuma rishya"
            }

        StartDate ->
            { english = "Start Date"
            , kinyarwanda = Just "Itariki utangireyeho"
            }

        EndDate ->
            { english = "End Date"
            , kinyarwanda = Just "Itariki urangirijeho"
            }

        StartSyncing ->
            { english = "Start Syncing"
            , kinyarwanda = Just "Tangira uhuze amakuru kuri seriveri"
            }

        StatusLabel ->
            { english = "Status"
            , kinyarwanda = Just "Uko bihagaze kugeza ubu"
            }

        StopSyncing ->
            { english = "Stop Syncing"
            , kinyarwanda = Just "Tangira gukura amakuru kuri seriveri"
            }

        Submit ->
            { english = "Submit"
            , kinyarwanda = Nothing
            }

        Success ->
            { english = "Success"
            , kinyarwanda = Just "Byagezweho"
            }

        SyncGeneral ->
            { english = "Sync Status (General)"
            , kinyarwanda = Just "Ibijyanye no guhuza amakuru yafashwe n'igikoresho cy'ikoranabuhanga n'abitse kuri seriveri"
            }

        TabletSinglePlural value ->
            if value == "1" then
                { english = "1 tablet"
                , kinyarwanda = Just "Ikinini cyimwe"
                }

            else
                { english = value ++ " tablets"
                , kinyarwanda = Just <| "ibinini " ++ value
                }

        TakenCareOfBy ->
            { english = "Taken care of by"
            , kinyarwanda = Nothing
            }

        TakingMedicationAsPrescribed taking ->
            if taking then
                { english = "Taking medication as prescribed"
                , kinyarwanda = Just "Yafashe imiti uko yayandikiwe"
                }

            else
                { english = "Not taking medication as prescribed because of"
                , kinyarwanda = Just "Ntabwo yafashe imiti uko yayandikiwe kubera ko"
                }

        TasksCompleted completed total ->
            { english = String.fromInt completed ++ "/" ++ String.fromInt total ++ " Tasks Completed"
            , kinyarwanda = Just <| String.fromInt completed ++ "/" ++ String.fromInt total ++ " Ibikorwa byarangiye"
            }

        TelephoneNumber ->
            { english = "Telephone Number"
            , kinyarwanda = Just "Numero ya telefoni"
            }

        Term ->
            { english = "Term"
            , kinyarwanda = Just "Inda igeze igihe"
            }

        TermPregnancy ->
            { english = "Number of Term Pregnancies (Live Birth)"
            , kinyarwanda = Just "Umubare w'abavutse ari bazima bashyitse"
            }

        ThisActionCannotBeUndone ->
            { english = "This action cannot be undone."
            , kinyarwanda = Nothing
            }

        ThisGroupHasNoMothers ->
            { english = "This Group has no mothers assigned to it."
            , kinyarwanda = Just "Iki cyiciro nta mubyeyi cyagenewe."
            }

        Time ->
            { english = "Time"
            , kinyarwanda = Nothing
            }

        To ->
            { english = "to"
            , kinyarwanda = Just "kuri"
            }

        ToThePatient ->
            { english = "to the patient"
            , kinyarwanda = Just "ku murwayi"
            }

        Training ->
            { english = "Training"
            , kinyarwanda = Nothing
            }

        TrainingGroupEncounterCreateSuccessMessage ->
            { english = "Training encounters were created."
            , kinyarwanda = Nothing
            }

        TrainingGroupEncounterDeleteSuccessMessage ->
            { english = "Training encounters were deleted."
            , kinyarwanda = Nothing
            }

        TransportationPlanQuestion ->
            { english = "Have you planned for transportation to and from the health center to give birth"
            , kinyarwanda = Just "Waba warateganije uburyo uzagera ku kigo nderabuzima ugiye kubyara ndetse n'uburyo uzavayo nyuma yo kubyara"
            }

        TraveledToCOVID19CountryQuestion ->
            { english = "Have you traveled to any country or district in Rwanda known to have COVID-19 in the past 14 days"
            , kinyarwanda = Just "Waba waragiye mu gihugu cyangwa mu karere mu Rwanda bizwi ko hagaragayemo ubwandu bwa Covid 19 mu minsi 14 ishize"
            }

        TravelHistory ->
            { english = "Travel History"
            , kinyarwanda = Just "Amukuru ku ngendo"
            }

        TreatedWith ->
            { english = "Treated with"
            , kinyarwanda = Just "Bivurwa na"
            }

        TreatedWithNot ->
            { english = "Not treated with"
            , kinyarwanda = Just "Ntibivurwa na"
            }

        Treatment ->
            { english = "Treatment"
            , kinyarwanda = Just "Ubuvuzi"
            }

        TreatmentDetailsAnemia ->
            { english = "At the previous visit you were given Iron (120mg), one 60mg tablet to be taken 2x a day for 3 months and Folic Acid (400 IU) to be taken daily for 3 months."
            , kinyarwanda = Just "Mu isura riheruka wahawe umuti (Ubutare or Feri) wongera amaraso(120mg), miligarama 60 inshuro ebyiri ku munsi mu mezi atatu na Acide folike(400 UI)inshuro imwe ku munsi mu miezi atatu."
            }

        TreatmentDetailsHIV dolutegravir arvs ->
            if dolutegravir && arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day and Doltegravir (50mg) to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine ikinini kimwe ku munsi na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                }

            else if dolutegravir then
                { english = "At the previous visit you were given Doltegravir (50mg), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura rishize wahawe ikinini cya Dulutogaraviri(50mg), ikinini kimwe ku munsi."
                }

            else if arvs then
                { english = "At the previous visit you were given TDF + 3TC (1 tablet), to be taken by mouth 1x a day."
                , kinyarwanda = Just "Mu isura riheruka wahawe ikinini cya Tenofoviri na Lamividine na Dulutogaraviri (50mg), ikinini kimwe ku munsi."
                }

            else
                { english = ""
                , kinyarwanda = Nothing
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
                    }

                TreatmentMethyldopa3 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 3x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro eshatu ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    }

                TreatmentMethyldopa4 ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    }

                TreatmentHypertensionAddCarvedilol ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    }

                TreatmentHypertensionAddAmlodipine ->
                    { english = "At the previous visit you were given Methyldopa (250mg), to be taken by mouth 4x a day and Carvedilol (6.25mg), to be taken by mouth 2x a day and Amlodipine (5mg), by mouth 1x a day for " ++ diagnosis ++ "."
                    , kinyarwanda = Just <| "Mu isura riheruka wahawe Metilidopa (250mg), mu kanwa Inshuro enye ku munsi na Karuvedilole (5.25mg), mu kanwa inshuro 2 ku munsi na Amlodipine (5mg), mu kanwa inshuro imwe ku munsi ku ndwara ya " ++ diagnosis ++ "."
                    }

                -- All others are not Hypertension treatments.
                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        TreatmentDetailsMalaria sign ->
            case sign of
                TreatmentQuinineSulphate ->
                    { english = "At the previous visit you were given Quinine Sulphate per os 10 mg/kg/dose, to be taken 3 times a day for 7 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe umuti wa Kinini 10mg ku kilo, gatatu ku munsi mu minsi irindwi."
                    }

                TreatmentCoartem ->
                    { english = "At the previous visit you were given Coartem, 4 tablets to be taken by mouth twice per day x 3 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe AL (Kowaritemu), ibibini bine (4) byo kunywa mu kanwa inshuri ebyiri ku munsi mu minsi itatu."
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        TreatmentDetailsSyphilis sign ->
            case sign of
                TreatmentPenecilin1 ->
                    { english = "At the previous visit you were given Penicillin (2.4 million units), IM x 1."
                    , kinyarwanda = Just "Mu isura rishize wahawe Penisilini (inite Miliyoni 2.4 ), IM inshuro 1."
                    }

                TreatmentPenecilin3 ->
                    { english = "At the previous visit you were given Penicillin (2.4 million units), IM 1x a week for 3 weeks."
                    , kinyarwanda = Just "Mu isura rishize wahawe Penisilini (inite Miliyoni 2.4 ), IM inshuro 1, IM inshuro 1 buri cyumweru mu byumweru 3."
                    }

                TreatmentErythromycin ->
                    { english = "At the previous visit you were given Erythromycin (500mg), by mouth 4x a day for 14 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Erythromicine (500mg), mu kanwa inshuro enye ku munsi mu minsi 14."
                    }

                TreatmentAzithromycin ->
                    { english = "At the previous visit you were given Azithromycin (2g), 4 tabs by mouth x one day."
                    , kinyarwanda = Just "Mu isura rishize wahawe Azithromycine (2g), Ibinini 4 abinywe mu kanwa umunsi umwe."
                    }

                TreatmentCeftriaxon ->
                    { english = "At the previous visit you were given Ceftriaxone (1g), IM daily x 10 days."
                    , kinyarwanda = Just "Mu isura rishize wahawe Ceftriaxone (1g), IM buri munsi mu minsi 10."
                    }

                _ ->
                    { english = ""
                    , kinyarwanda = Nothing
                    }

        TreatmentReviewQuestionAdverseEvents ->
            { english = "Have you experienced any adverse events"
            , kinyarwanda = Just "Waba hari ibintu wabonye bidasanzwe(bitewe n'imiti wafashe)"
            }

        TreatmentReviewQuestionAdverseEventsHospitalization ->
            { english = "The patient had an adverse reaction to the medication. Would you like to refer them to the hospital as next step"
            , kinyarwanda = Just "Umurwayi yabonye ibintu bidasanzwe byatewe n'imiti yahawe. Waba ushaka kumwhoreza ku bitaro nk'igikorwa gikurikiyeho"
            }

        TreatmentReviewQuestionMedicationByPMTCT ->
            { english = "Did you receive medicine from PMTCT"
            , kinyarwanda = Just "Wahawe imiti muri PMTCT"
            }

        TreatmentReviewQuestionMissedDoses ->
            { english = "Have you missed any doses"
            , kinyarwanda = Just "Haba hari imiti wasimbutse gufata"
            }

        TreatmentReviewQuestionStillTaking ->
            { english = "Are you still taking this medication"
            , kinyarwanda = Just "Uracyari gufata imiti"
            }

        TreatmentReviewQuestionStillTakingForHIV ->
            { english = "Are you still taking ARVs"
            , kinyarwanda = Just "Uracyari gufata imiti igabanya ubukana bwa virusi itera SIDA"
            }

        TreatmentReviewTask forModeratePreeclamsia task ->
            case task of
                TreatmentReviewPrenatalMedication ->
                    { english = "Prenatal Medication"
                    , kinyarwanda = Just "Imiti yo gufata mu gihe utwite"
                    }

                TreatmentReviewHIV ->
                    { english = "HIV Medication"
                    , kinyarwanda = Just "Imiti ya Virusi Itera SIDA"
                    }

                TreatmentReviewHypertension ->
                    if forModeratePreeclamsia then
                        { english = "Moderate Preeclamsia Medication"
                        , kinyarwanda = Just "Imiti Preklampusi Yoroheje"
                        }

                    else
                        { english = "Hypertension Medication"
                        , kinyarwanda = Just "Imiti y'Umuvuduko w'Amaraso"
                        }

                TreatmentReviewMalaria ->
                    { english = "Malaria Medication"
                    , kinyarwanda = Just "Imiti ya Malariya"
                    }

                TreatmentReviewAnemia ->
                    { english = "Anemia Medication"
                    , kinyarwanda = Just "Imiti ivura indwara y'Amaraso make"
                    }

                TreatmentReviewSyphilis ->
                    { english = "Syphilis Medication"
                    , kinyarwanda = Just "Imiti ya Mburugu"
                    }

        TreatmentReviewWarningPopupMessage ->
            { english = "Patient non-adherent"
            , kinyarwanda = Just "Uyu murwayi ntabwo yubahiriza gahunda yo kunywa imiti uko bisabwa"
            }

        TreatmentReviewWarningPopupInstructions ->
            { english = "Further evaluation necessary"
            , kinyarwanda = Just "Gusuzuma byimbitse"
            }

        TrySyncing ->
            { english = "Try syncing with backend"
            , kinyarwanda = Just "Gerageza guhuza amakuru y'iki gikoresho cy'ikoranabuhanga n'abakoze E-Heza"
            }

        TuberculosisPast ->
            { english = "Tuberculosis in the past"
            , kinyarwanda = Just "Yigeze kurwara igituntu"
            }

        TuberculosisPresent ->
            { english = "Tuberculosis in the present"
            , kinyarwanda = Just "Arwaye igituntu"
            }

        TuberculosisInstructions ->
            { english = "Follow TB protocols"
            , kinyarwanda = Just "Kurikiza amabwiriza yo kuvura igitintu"
            }

        TuberculosisInstructionsFollowed ->
            { english = "followed TB protocols"
            , kinyarwanda = Just "Hakurikijwe amabwiriza yo kuvura igitintu"
            }

        TuberculosisWarning ->
            { english = "Patient is high risk for active Tuberculosis"
            , kinyarwanda = Just "Umubyeyi afite ibyago byinshi byo kuba afite igituntu"
            }

        TwoVisits ->
            { english = "Two visits"
            , kinyarwanda = Just "Inshuro ebyiri"
            }

        Type ->
            { english = "Type"
            , kinyarwanda = Just "Ubwoko bw'Urukingo"
            }

        UbudeheLabel ->
            { english = "Ubudehe"
            , kinyarwanda = Nothing
            }

        UbudeheNumber ubudehe ->
            case ubudehe of
                Ubudehe1 ->
                    { english = "1"
                    , kinyarwanda = Nothing
                    }

                Ubudehe2 ->
                    { english = "2"
                    , kinyarwanda = Nothing
                    }

                Ubudehe3 ->
                    { english = "3"
                    , kinyarwanda = Nothing
                    }

                Ubudehe4 ->
                    { english = "4"
                    , kinyarwanda = Nothing
                    }

        UndeterminedDiagnoses ->
            { english = "Undetermined Diagnoses"
            , kinyarwanda = Just "Uburwayi ntibusobanutse"
            }

        UndeterminedDiagnosisMessage ->
            { english = "undetermined diagnosis - followed Post-Partum Protocols"
            , kinyarwanda = Just "Uburwayi ntibusobanutse - hakurikijwe mabwiriza yo kwita ku mubyeyi wabyaye"
            }

        UnitCopiesPerMM3 ->
            { english = "copies/mm3"
            , kinyarwanda = Just "Kopi/mm3"
            }

        UnitGramsPerDeciliter ->
            { english = "g/dL"
            , kinyarwanda = Nothing
            }

        UnitInternationalUnitsPerLiter ->
            { english = "IU/L"
            , kinyarwanda = Nothing
            }

        UnitMilliGramsPerDeciliter ->
            { english = "mg/dL"
            , kinyarwanda = Nothing
            }

        Unknown ->
            { english = "Unknown"
            , kinyarwanda = Just "Ntabizi"
            }

        Update ->
            { english = "Update"
            , kinyarwanda = Just "Kuvugurura"
            }

        UpdateError ->
            { english = "Update Error"
            , kinyarwanda = Just "ikosa mwivugurura"
            }

        Uploading ->
            { english = "Uploading"
            , kinyarwanda = Nothing
            }

        UrinaryTractInfectionRecommendedTreatmentHeader ->
            { english = "This patient shows signs of Urinary Tract Infection"
            , kinyarwanda = Just "Uyu murwayi agaragaza ibimenyetso by'indwara y'ubwandu bw'umuyoboro w'inkari buhoraho"
            }

        UrinaryTractInfectionRecommendedTreatmentHelper ->
            { english = "Select the medication and dosage you will administer to the patient"
            , kinyarwanda = Just "Hitamo umuti ugiye guha umurwayi n'uburyo bwo kuwufata"
            }

        UrinaryTractInfectionRecommendedTreatmentInstructions ->
            { english = "Ensure the patient is not allergic to the medication before prescribing"
            , kinyarwanda = Just "Menya neza ko umurwayi adafite aleriji ku miti mbere yo kuyimwandikira"
            }

        UterineMyoma ->
            { english = "Uterine Myoma"
            , kinyarwanda = Just "Ibibyimba byo mu mura/Nyababyeyi"
            }

        VaccinationCatchUpRequiredQuestion ->
            { english = "Are there previous immunizations that are not in E-Heza that need to be recorded"
            , kinyarwanda = Nothing
            }

        VaccinationStatus status ->
            case status of
                StatusBehind ->
                    { english = "Behind"
                    , kinyarwanda = Just "Ntibyakozwe"
                    }

                StatusCompleted ->
                    { english = "Completed"
                    , kinyarwanda = Just "Byarakozwe"
                    }

                StatusUpToDate ->
                    { english = "Up To Date"
                    , kinyarwanda = Just "Biri ku gihe"
                    }

        VaccinationNoDosesAdministered ->
            { english = "There are no recorded immunizations for this patient"
            , kinyarwanda = Just "Nta makuru ku nkigo agaragara"
            }

        VaccineDoseAdministeredPreviouslyPrenatalQuestion vaccineType ->
            { english = "Did the patient receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Uyu mubyeyi yaba yarabonye urukingo rw'" ++ vaccineType ++ "bakaba batarabyanditse"
            }

        VaccineDoseAdministeredPreviouslyWellChildQuestion vaccineType ->
            { english = "Did the child receive any " ++ vaccineType ++ " immunizations prior to today that are not recorded above"
            , kinyarwanda = Just <| "Umwana yaba yarabonye " ++ vaccineType ++ " bakaba batarabyanditse"
            }

        VaccineDoseAdministeredTodayPrenatalQuestion vaccineType ->
            { english = "Will the patient receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umubyeyi arahabwa urukingo rw'" ++ vaccineType ++ " uyu munsi"
            }

        VaccineDoseAdministeredTodayWellChildQuestion vaccineType ->
            { english = "Will the child receive the " ++ vaccineType ++ " immunization today"
            , kinyarwanda = Just <| "Umwana arahabwa " ++ vaccineType ++ " uyu munsi"
            }

        VaccineType vaccineType ->
            case vaccineType of
                WellChildVaccine wellChildVaccineType ->
                    case wellChildVaccineType of
                        VaccineBCG ->
                            { english = "BCG Bacilius Calmette - Guérin Vaccine (BCG)"
                            , kinyarwanda = Just "Urukingo rw'igituntu"
                            }

                        VaccineOPV ->
                            { english = "Oral Polio Vaccine (OPV)"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                            }

                        VaccineDTP ->
                            { english = "DTP - HepB - Hib Vaccine"
                            , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                            }

                        VaccinePCV13 ->
                            { english = "Pneumoccocal Vaccine (PCV 13)"
                            , kinyarwanda = Just "Urukingo rw'umusonga"
                            }

                        VaccineRotarix ->
                            { english = "Rotavirus (Rotarix) Vaccine"
                            , kinyarwanda = Just "Urukingo rw'impiswi"
                            }

                        VaccineIPV ->
                            { english = "Inactivated Polio Vaccine"
                            , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                            }

                        VaccineMR ->
                            { english = "Measles-Rubella Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                            }

                        VaccineHPV ->
                            { english = "HPV Vaccine"
                            , kinyarwanda = Just "Urukingo rw'Inkondo y'umura"
                            }

                PrenatalVaccine prenatalVaccineType ->
                    case prenatalVaccineType of
                        VaccineTetanus ->
                            { english = "Tetanus"
                            , kinyarwanda = Just "Agakwega"
                            }

        VaginalExamination ->
            { english = "Vaginal Examination"
            , kinyarwanda = Just "Isuzuma ry'imyanya ndangagitsina"
            }

        VaginalExamSign sign ->
            case sign of
                FoulSmellingLochia ->
                    { english = "Foul Smelling Lochia"
                    , kinyarwanda = Just "Ibisanza binuka"
                    }

                ExcessiveVaginalBleeding ->
                    { english = "Bleeding"
                    , kinyarwanda = Just "Kuva"
                    }

                NormalVaginalExam ->
                    { english = "Normal"
                    , kinyarwanda = Just "Bisanzwe"
                    }

        ValidationErrors ->
            { english = "Validation Errors"
            , kinyarwanda = Nothing
            }

        -- As in, the version the app
        Version ->
            { english = "Version"
            , kinyarwanda = Nothing
            }

        View ->
            { english = "View"
            , kinyarwanda = Nothing
            }

        ViewProgressReport ->
            { english = "View Progress Report"
            , kinyarwanda = Just "Raporo y’ibyakozwe"
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Just "Umudugudu"
            }

        VitaminAWarningPopupMessage ->
            { english = "Patient did not recieve Vitamin A"
            , kinyarwanda = Nothing
            }

        WaitForVitalsRecheckHelper ->
            { english = "Patient needs to return in 2 hours to confirm blood pressure. Instruct the patient to wait until called for further testing."
            , kinyarwanda = Just "Umurwayi agomba kugaruka mu masaha 2 kugira ngo twemeze neza umuvuduko w'amaraso. Saba umurwayi kwihangana kugeza umuhamagaye kugira ngo yongere asuzumwe."
            }

        WaitForLabsResultsHelper ->
            { english = "Patient has labs pending. Instruct the patient to wait until called for lab results and further diagnoses."
            , kinyarwanda = Just "Umurwayi afite ibisubizo ategereje bya Laboratwari. Musabe gutegereza kugeza umuhamagaye ngo afate ibisubizo anamenye indwara afite."
            }

        WaitInstructions ->
            { english = "To proceed with more encounters while you wait for test results or a vitals recheck, touch \"Pause Encounter\" below to leave this encounter. You can return to it from the case management screen."
            , kinyarwanda = Just "Kugira ngo ukomeze ufate andi masuzuma menshi mu gihe utegereje ibisubizo cyangwa se gusubiramo ibipimo by'ubuzima, kanda mu nsi hano ahanditse ngo \" Ba uhagaritse igikorwa\" kugira ngo usohoke kuri iri suzuma. Ushobora kurigarukaho unyuze ku kibaho cyo Kuvura uburwayi."
            }

        Warning ->
            { english = "Warning"
            , kinyarwanda = Just "Impuruza"
            }

        WasFbfDistirbuted activity ->
            case activity of
                ChildActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    }

                MotherActivity _ ->
                    { english = "If distributed amount is not as per guidelines, select the reason"
                    , kinyarwanda = Just "Niba ingano ya FBF yatanzwe idahuye n’amabwiriza, hitamo impamvu"
                    }

        WeekSinglePlural value ->
            if value == 1 then
                { english = "1 Week"
                , kinyarwanda = Just "1 Icyumweru"
                }

            else
                { english = String.fromInt value ++ " Weeks"
                , kinyarwanda = Just <| String.fromInt value ++ " Ibyumweru"
                }

        Weight ->
            { english = "Weight"
            , kinyarwanda = Just "Ibiro"
            }

        WelcomeUser name ->
            { english = "Welcome " ++ name
            , kinyarwanda = Just <| "Murakaza neza " ++ name
            }

        WellChildActivityTitle activity ->
            case activity of
                WellChildDangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Just "Ibimenyetso Mpuruza"
                    }

                WellChildNutritionAssessment ->
                    { english = "Nutrition Assessment"
                    , kinyarwanda = Just "Gusuzuma imirire"
                    }

                WellChildECD ->
                    { english = "ECD"
                    , kinyarwanda = Just "Kwita ku mikurire y'abana bato"
                    }

                WellChildMedication ->
                    { english = "Medication"
                    , kinyarwanda = Just "Gufata imiti"
                    }

                WellChildPregnancySummary ->
                    { english = "History"
                    , kinyarwanda = Just "Amateka y'ibyamubayeho"
                    }

                WellChildImmunisation ->
                    { english = "Immunizations"
                    , kinyarwanda = Just "Ikingira"
                    }

                WellChildNextSteps ->
                    { english = "Next Steps"
                    , kinyarwanda = Just "Ibikurikiyeho"
                    }

                WellChildPhoto ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

        WellChildDangerSignsTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskSymptomsReview ->
                    { english = "Symptom Review"
                    , kinyarwanda = Just "Kureba ibimenyetso by'uburwayi"
                    }

                Pages.WellChild.Activity.Types.TaskVitals ->
                    { english = "Vitals"
                    , kinyarwanda = Just "Ibipimo by'ubuzima"
                    }

        WellChildEncounterPopup popupType ->
            case popupType of
                PopupDangerSigns ->
                    { english = "Child shows signs of acute illness. Please close this encounter and continue in an “Acute Illness” encounter immediately."
                    , kinyarwanda = Nothing
                    }

                PopupECD ecdPopupType ->
                    case ecdPopupType of
                        ChildBehind ->
                            { english = "Child is behind on ECD milestones. Continue to monitor the child and provide anticipatory guidance to the caregiver."
                            , kinyarwanda = Nothing
                            }

                        ReferToSpecialist ->
                            { english = "Child is behind on ECD milestones. Refer the child to a specialist."
                            , kinyarwanda = Nothing
                            }

        WellChildECDMilestoneForDiagnosisPane encounterType ->
            case encounterType of
                Milestone6Weeks ->
                    { english = "1.5 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone14Weeks ->
                    { english = "3.5 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone6Months ->
                    { english = "6 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone9Months ->
                    { english = "9 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone12Months ->
                    { english = "12 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone15Months ->
                    { english = "15 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone18Months ->
                    { english = "18 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone2Years ->
                    { english = "24 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone3Years ->
                    { english = "36 Mo"
                    , kinyarwanda = Nothing
                    }

                Milestone4Years ->
                    { english = "48 Mo"
                    , kinyarwanda = Nothing
                    }

        WellChildMacrocephalyWarning ->
            { english = "Child shows signs of macrocephaly, follow hydrocephalus protocol. Please refer to a specialist if concerned for genetic syndrome or other problems."
            , kinyarwanda = Nothing
            }

        WellChildMicrocephalyWarning ->
            { english = "Child shows signs of microcephaly. Monitor for developmental, nutritional, and genetic problems.  Please refer to a specialist if concerned for genetic syndrome or other problems."
            , kinyarwanda = Nothing
            }

        WellChildImmunisationDescription task ->
            case task of
                VaccineBCG ->
                    { english = "BCG protects your child from getting the worst complications of tuberculosis, which can affect the lungs and could be deadly for young children."
                    , kinyarwanda = Just "Urukingo rw'igituntu rurinda umwana ibyago byo kuba yakwandura igituntu, ndeste nibyago byashamikiraho bishobora kwibasira ibihaha, ibi bikaba byanahitana umwana akiri muto."
                    }

                VaccineDTP ->
                    { english = "Prevents the child from getting lockjaw (Tetanus), whooping cough (Pertussis), liver failure (Hepatitis B), breathing problems and fever (Diptheria)."
                    , kinyarwanda = Just "Rurinda umwana indwara ya agakwega, kokolishe, umwijima wo mubwoko bwa B, n'ibibazo, ibibazo byo guhumeka n'umuriro (Akaniga)."
                    }

                VaccineHPV ->
                    { english = "HPV prevents certain types of cancer from developing in your child."
                    , kinyarwanda = Just "Rurinda umwana kurwara zimwe muri kanseri"
                    }

                VaccineIPV ->
                    { english = "Is the final vaccine to prevent Polio in children. IPV boosts the effects of the previous polio vaccines your child received."
                    , kinyarwanda = Just "Ni urukingo rwa nyuma rw'imbasa ku bana, rwongerera imbaraga / rushimangira inkingo z'imbasa yabonye mbere."
                    }

                VaccineMR ->
                    { english = "Prevents the child from contracting a highly contagious viral infection that causes a fever, lesions, and diarrhea. MR is very dangerous for pregnant women, causing miscarriage or birth defects. Vaccinating your child prevents the spread of the disease in the community."
                    , kinyarwanda = Nothing
                    }

                VaccineOPV ->
                    { english = "OPV prevents the child from contracting the Polio Virus, which affects the spinal cord and can cause paralysis."
                    , kinyarwanda = Just "Uru rukingo rurinda umwana kwandura Virusi itera indwara y'imbasa, iyo virusi ifata ururenda ruba mu ruti rw'umugongo bigatera umwana ubumuga bw'ingingo (Amaguru cg amaboko)."
                    }

                VaccinePCV13 ->
                    { english = "Protects against any disease caused by a specific bacteria that can lead to lung infections."
                    , kinyarwanda = Just "Rurinda umwana indwara ziterwa n'udukoko twangiza ibihaha."
                    }

                VaccineRotarix ->
                    { english = "Protects against diarrhea caused by the Rotavirus. Diarrhea is the 3rd leading cause of death of children in Rwanda."
                    , kinyarwanda = Just "Rurinda umwana impiswi ziterwa n'udukoko twa rotavirusi. Impiswi ni impamvu ya gatatu itera imfu z'abana mu Rwanda."
                    }

        WellChildImmunisationDosage task ->
            case task of
                VaccineBCG ->
                    { english = "There is one dose of BCG and it is given at birth."
                    , kinyarwanda = Just "Urukingo rw'igituntu rutangwa inshuro imwe umwana akimara kuvuka."
                    }

                VaccineDTP ->
                    { english = "There are 3 doses of DTP-HepB-Hib - 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa inshuro eshatu inkingo zikurikira:(urukingo rw'agakwega, Hepatite yo mubwoko bwa B, nigihuka) yujuje ibyumweru 6, ibyumweru 10, no ku byumweru 14."
                    }

                VaccineHPV ->
                    { english = "There are 2 doses of HPV - at 12 years and 12.5 years."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'inkondo y'umura inshuro 2 - ku myaka 12 n'imyaka 12.5."
                    }

                VaccineIPV ->
                    { english = "There is only one dose of the inactivated vaccine at 14 weeks."
                    , kinyarwanda = Just "Uru rukingo aruhabwa inshuro imwe gusa ku byumweru 14."
                    }

                VaccineMR ->
                    { english = "There are 2 doses of Measles-Rubella - at 9 months and 15 months."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'Iseru na Rubeyole inshuro 2: Afite Amezi 9, n'amezi 15."
                    }

                VaccineOPV ->
                    { english = "There are 4 doses of OPV - at birth, 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'imbasa inshuro 4:Akivuka, ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    }

                VaccinePCV13 ->
                    { english = "There are 3 doses of PCV 13 - 6 weeks, 10 weeks, and 14 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'umusonga inshuro 3:Ku byumweru 6, ku byumweru 10 no ku byumweru 14."
                    }

                VaccineRotarix ->
                    { english = "There are 2 doses of Rotarix - 6 weeks and 10 weeks."
                    , kinyarwanda = Just "Umwana ahabwa urukingo rw'impiswi inshuro 2:Ku byumweru 6, no ku byumweru 10."
                    }

        WellChildImmunisationHeader task ->
            case task of
                VaccineBCG ->
                    { english = "Bacillus Calmette - Guérin (BCG)"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    }

                VaccineDTP ->
                    { english = "Diptheria, Hepatitis B, Tetanus, and Pertussis"
                    , kinyarwanda = Just "Urukingo rwa Kokorishi, Agakwega (Tetanosi), Akaniga,indwara zifata imyanya y'ubuhumekero, Umwijima wo mu bwoko bwa B"
                    }

                VaccineHPV ->
                    { english = "Human Papillomavirus (HPV)"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    }

                VaccineIPV ->
                    { english = "Inactivated Polio Vaccine (IPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella (MR)"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    }

                VaccineOPV ->
                    { english = "Oral Polio Vaccine (OPV)"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu kanwa"
                    }

                VaccinePCV13 ->
                    { english = "Pneumococcal Vaccine (PCV 13)"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    }

                VaccineRotarix ->
                    { english = "Rotavirus Vaccine (Rotarix)"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    }

        WellChildImmunisationHistory task ->
            case task of
                VaccineBCG ->
                    { english = "BCG History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'igituntu"
                    }

                VaccineDTP ->
                    { english = "DTP - HepB - Hib History"
                    , kinyarwanda = Just "Amakuru kuri DTP - HepB - Hib"
                    }

                VaccineHPV ->
                    { english = "HPV History"
                    , kinyarwanda = Just "Amakuru ku rukingo rw'inkondo y'umura"
                    }

                VaccineIPV ->
                    { english = "IPV History"
                    , kinyarwanda = Just "Amakuru k' Urukingo rw'imbasa rutangwa mu rushinge"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella History"
                    , kinyarwanda = Just "amakuru k'Urukingo rw'Iseru na Rubeyole"
                    }

                VaccineOPV ->
                    { english = "OPV History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'imbasa rutangwa mu kanwa"
                    }

                VaccinePCV13 ->
                    { english = "PCV 13 History"
                    , kinyarwanda = Just "Amakuru k'urukingo rw'umusonga"
                    }

                VaccineRotarix ->
                    { english = "Rotarix History"
                    , kinyarwanda = Just "Amakuru k'Urukingo rw'impiswi"
                    }

        WellChildImmunisationTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    }

                Pages.WellChild.Activity.Types.TaskDTP ->
                    { english = "DTP - HepB - Hib"
                    , kinyarwanda = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Inkondo y'Umura"
                    }

                Pages.WellChild.Activity.Types.TaskIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    }

                Pages.WellChild.Activity.Types.TaskMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    }

                Pages.WellChild.Activity.Types.TaskOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    }

                Pages.WellChild.Activity.Types.TaskPCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    }

                Pages.WellChild.Activity.Types.TaskRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    }

                Pages.WellChild.Activity.Types.TaskOverview ->
                    { english = "Overview"
                    , kinyarwanda = Just "Ishusho Rusange"
                    }

        WellChildMedicationTask task ->
            case task of
                Pages.WellChild.Activity.Types.TaskAlbendazole ->
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskMebendezole ->
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    }

                Pages.WellChild.Activity.Types.TaskVitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    }

        WellChildNextStepsTask isChw task ->
            case task of
                TaskContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Just "Impamvu zateye uburwayi"
                    }

                TaskHealthEducation ->
                    { english = "Health Education"
                    , kinyarwanda = Just "Inyigisho ku buzima"
                    }

                TaskSendToHC ->
                    if isChw then
                        { english = "Send to Health Center"
                        , kinyarwanda = Just "Ohereza Ku kigo nderabuzima"
                        }

                    else
                        { english = "Refer to Program"
                        , kinyarwanda = Nothing
                        }

                TaskFollowUp ->
                    { english = "Follow Up"
                    , kinyarwanda = Just "Gukurikirana umurwayi"
                    }

                TaskNextVisit ->
                    { english = "Next Visit"
                    , kinyarwanda = Nothing
                    }

        WellChildSymptom symptom ->
            case symptom of
                SymptomBreathingProblems ->
                    { english = "Breathing problems"
                    , kinyarwanda = Just "Ibibazo bijyanye no guhumeka"
                    }

                SymptomConvulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Just "Kugagara"
                    }

                SymptomLethargyOrUnresponsiveness ->
                    { english = "Lethargy or unresponsiveness"
                    , kinyarwanda = Just "Gucika intege cyane"
                    }

                SymptomDiarrhea ->
                    { english = "Diarrhea"
                    , kinyarwanda = Just "Impiswi"
                    }

                SymptomVomiting ->
                    { english = "Vomiting"
                    , kinyarwanda = Just "Kuruka"
                    }

                SymptomUmbilicalCordRedness ->
                    { english = "Umbilical Cord Redness"
                    , kinyarwanda = Just "Guhisha k'umukondo"
                    }

                SymptomStiffNeckOrBulgingFontanelle ->
                    { english = "Stiff neck or bulging fontanelle"
                    , kinyarwanda = Just "Kugagara ibikanu cyangwa igihorihori kibyimbye"
                    }

                SymptomSevereEdema ->
                    { english = "Severe Edema"
                    , kinyarwanda = Just "Kubyimba bikabije (Cyane ibirenge cg intonki)"
                    }

                SymptomPalmoplantarPallor ->
                    { english = "Palmoplantar pallor"
                    , kinyarwanda = Just "Kweruruka mu biganza no mu bworo bw'ibirenge"
                    }

                SymptomHistoryOfFever ->
                    { english = "History of fever"
                    , kinyarwanda = Just "Amakuru yerekeye umuriro yagize mu bihe byashize"
                    }

                SymptomBabyTiresQuicklyWhenFeeding ->
                    { english = "Baby tires quickly when feeding"
                    , kinyarwanda = Just "Umwana ahita ananirwa iyo atangiye kurya"
                    }

                SymptomCoughingOrTearingWhileFeeding ->
                    { english = "Coughing/tearing while feeding (<6 months)"
                    , kinyarwanda = Just "Gukorora/Kwiriza iyo atangiye kurya (munsi y'amezi 6)"
                    }

                SymptomRigidMusclesOrJawClenchingPreventingFeeding ->
                    { english = "Rigid muscles/jaw clenching that prevents feeding"
                    , kinyarwanda = Just "Imikaya ireze/amatama afunganye bikamubuza kurya"
                    }

                ExcessiveSweatingWhenFeeding ->
                    { english = "Excessive sweating when feeding"
                    , kinyarwanda = Just "Kubira ibyuya byinshi iyo ari kurya"
                    }

                NoWellChildSymptoms ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta na kimwe"
                    }

        WellChildVaccineLabel vaccineType ->
            case vaccineType of
                VaccineBCG ->
                    { english = "BCG"
                    , kinyarwanda = Just "Urukingo rw'igituntu"
                    }

                VaccineDTP ->
                    { english = "DTP - HepB - Hib"
                    , kinyarwanda = Nothing
                    }

                VaccineHPV ->
                    { english = "HPV"
                    , kinyarwanda = Just "Urukingo rw'Unkondo y'Umura"
                    }

                VaccineIPV ->
                    { english = "IPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa rutangwa mu rushinge"
                    }

                VaccineMR ->
                    { english = "Measles-Rubella"
                    , kinyarwanda = Just "Urukingo rw'Iseru na Rubeyole"
                    }

                VaccineOPV ->
                    { english = "OPV"
                    , kinyarwanda = Just "Urukingo rw'imbasa"
                    }

                VaccinePCV13 ->
                    { english = "PCV 13"
                    , kinyarwanda = Just "Urukingo rw'umusonga"
                    }

                VaccineRotarix ->
                    { english = "Rotarix"
                    , kinyarwanda = Just "Urukingo rw'impiswi"
                    }

        WhatDoYouWantToDo ->
            { english = "What do you want to do?"
            , kinyarwanda = Just "Urashaka gukora iki?"
            }

        WhatType ->
            { english = "What type"
            , kinyarwanda = Just "Ubuhe bwoko"
            }

        WhatWasTheirResponse ->
            { english = "What was their response"
            , kinyarwanda = Just "Ni iki bagusubije"
            }

        WhoCaresForTheChildDuringTheDay ->
            { english = "Who cares for the child during the day"
            , kinyarwanda = Just "Ni inde wita ku mwana ku manywa"
            }

        WhoInFamilyHasCondition ->
            { english = "Who in the family has this condition"
            , kinyarwanda = Nothing
            }

        WhyNot ->
            { english = "Why not"
            , kinyarwanda = Just "Kubera iki"
            }

        WhyDifferentFbfAmount activity ->
            case activity of
                ChildActivity _ ->
                    { english = "Select why child received a different amount of FBF"
                    , kinyarwanda = Nothing
                    }

                MotherActivity _ ->
                    { english = "Select why mother received a different amount of FBF"
                    , kinyarwanda = Nothing
                    }

        WrittenProtocolsFollowed ->
            { english = "Written protocols followed"
            , kinyarwanda = Just "Amabwiriza yanditse yakurikijwe"
            }

        Year ->
            { english = "Year"
            , kinyarwanda = Just "Umwaka"
            }

        YearsOld int ->
            { english = String.fromInt int ++ " years old"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt int
            }

        Yes ->
            { english = "Yes"
            , kinyarwanda = Just "Yego"
            }

        YouAreNotAnAdmin ->
            { english = "You are not logged in as an Administrator."
            , kinyarwanda = Nothing
            }

        YourGroupEncounterHasBeenSaved ->
            { english = "Your Group Encounter has been saved."
            , kinyarwanda = Nothing
            }

        ZScoreHeadCircumferenceForAge ->
            { english = "Z-Score Head Circumference for Age: "
            , kinyarwanda = Just "Z-score ku muzenguruko w'umutwe ugereranije n'imyaka afite: "
            }

        ZScoreHeightForAge ->
            { english = "Z-Score Height for Age: "
            , kinyarwanda = Just "Z-score Uburebure ku myaka: "
            }

        ZScoreMuacForAge ->
            { english = "MUAC for Age: "
            , kinyarwanda = Just "MUAC ku myaka: "
            }

        ZScoreWeightForAge ->
            { english = "Z-Score Weight for Age: "
            , kinyarwanda = Just "Z-score Ibiro ku myaka: "
            }

        ZScoreWeightForHeight ->
            { english = "Z-Score Weight for Height: "
            , kinyarwanda = Just "Z-score Ibiro ku uburebure: "
            }


translateMyRelatedBy : MyRelatedBy -> TranslationSet String
translateMyRelatedBy relationship =
    case relationship of
        MyChild ->
            { english = "Child"
            , kinyarwanda = Just "Umwana"
            }

        MyParent ->
            { english = "Parent"
            , kinyarwanda = Nothing
            }

        MyCaregiven ->
            { english = "Care given"
            , kinyarwanda = Nothing
            }

        MyCaregiver ->
            { english = "Caregiver"
            , kinyarwanda = Nothing
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
            }

        MyParent ->
            { english = "is the child of"
            , kinyarwanda = Just "ni umwana wa"
            }

        MyCaregiven ->
            { english = "is the caregiver for"
            , kinyarwanda = Just "ni umurezi wa"
            }

        MyCaregiver ->
            { english = "is given care by"
            , kinyarwanda = Just "arerwa na"
            }


translateActivePage : Page -> TranslationSet String
translateActivePage page =
    case page of
        DevicePage ->
            { english = "Device Status"
            , kinyarwanda = Just "Uko igikoresho cy'ikoranabuhanga gihagaze"
            }

        PinCodePage ->
            { english = "PIN Code"
            , kinyarwanda = Just "Umubare w'ibanga"
            }

        PageNotFound url ->
            { english = "Missing"
            , kinyarwanda = Just "Ibibura"
            }

        ServiceWorkerPage ->
            { english = "Deployment"
            , kinyarwanda = Nothing
            }

        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    { english = "Clinical"
                    , kinyarwanda = Nothing
                    }

                ClinicsPage _ ->
                    { english = "Groups"
                    , kinyarwanda = Just "Itsinda"
                    }

                ClinicalProgressReportPage _ _ ->
                    { english = "Clinical Progress Report"
                    , kinyarwanda = Just "Erekana raporo yibyavuye mu isuzuma"
                    }

                CreatePersonPage _ _ ->
                    { english = "Create Person"
                    , kinyarwanda = Nothing
                    }

                DashboardPage dashboardPage ->
                    { english = "Dashboards"
                    , kinyarwanda = Nothing
                    }

                GlobalCaseManagementPage ->
                    { english = "Case Management"
                    , kinyarwanda = Just "Gukurikirana Umurwayi"
                    }

                DemographicsReportPage _ _ ->
                    { english = "Demographics Report"
                    , kinyarwanda = Just "Raporo y'umwirondoro"
                    }

                EditPersonPage _ ->
                    { english = "Edit Person"
                    , kinyarwanda = Nothing
                    }

                MyAccountPage ->
                    { english = "My Account"
                    , kinyarwanda = Just "Compte"
                    }

                PersonPage _ _ ->
                    { english = "Person"
                    , kinyarwanda = Nothing
                    }

                PersonsPage _ _ ->
                    { english = "Participant Directory"
                    , kinyarwanda = Just "Ububiko bw'amakuru y'umurwayi"
                    }

                PrenatalParticipantPage _ _ ->
                    { english = "Antenatal Participant"
                    , kinyarwanda = Nothing
                    }

                IndividualEncounterParticipantsPage encounterType ->
                    case encounterType of
                        AcuteIllnessEncounter ->
                            { english = "Acute Illness Participants"
                            , kinyarwanda = Just "Abagaragweho n'uburwayi butunguranye"
                            }

                        AntenatalEncounter ->
                            { english = "Antenatal Participants"
                            , kinyarwanda = Nothing
                            }

                        InmmunizationEncounter ->
                            { english = "Inmmunization Participants"
                            , kinyarwanda = Nothing
                            }

                        NutritionEncounter ->
                            { english = "Nutrition Participants"
                            , kinyarwanda = Nothing
                            }

                        HomeVisitEncounter ->
                            { english = "Home Visit Participants"
                            , kinyarwanda = Nothing
                            }

                        WellChildEncounter ->
                            { english = "Standard Pediatric Visit Participant"
                            , kinyarwanda = Nothing
                            }

                        NCDEncounter ->
                            { english = "NCD Participants"
                            , kinyarwanda = Nothing
                            }

                RelationshipPage _ _ _ ->
                    { english = "Relationship"
                    , kinyarwanda = Nothing
                    }

                SessionPage sessionId sessionPage ->
                    case sessionPage of
                        ActivitiesPage ->
                            { english = "Activities"
                            , kinyarwanda = Just "Ibikorwa"
                            }

                        ActivityPage activityType ->
                            { english = "Activity"
                            , kinyarwanda = Just "Igikorwa"
                            }

                        AttendancePage ->
                            { english = "Attendance"
                            , kinyarwanda = Just "Ubwitabire"
                            }

                        ParticipantsPage ->
                            { english = "Participants"
                            , kinyarwanda = Just "Abagenerwabikorwa"
                            }

                        ChildPage childId ->
                            { english = "Child"
                            , kinyarwanda = Just "Umwana"
                            }

                        MotherPage motherId ->
                            { english = "Mother"
                            , kinyarwanda = Just "Umubyeyi"
                            }

                        NextStepsPage childId _ ->
                            { english = "Next Steps"
                            , kinyarwanda = Just "Ibikurikiyeho"
                            }

                        ProgressReportPage childId ->
                            { english = "Progress Report"
                            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
                            }

                PrenatalEncounterPage _ ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Just "Isuzuma k’umugore utwite"
                    }

                PrenatalActivityPage _ _ ->
                    { english = "Antenatal Activity"
                    , kinyarwanda = Nothing
                    }

                PrenatalRecurrentEncounterPage _ ->
                    { english = "Antenatal Recurrent Encounter"
                    , kinyarwanda = Just "Kwipimisha inda bigaruka"
                    }

                PrenatalRecurrentActivityPage _ _ ->
                    { english = "Antenatal Recurrent Activity"
                    , kinyarwanda = Just "Igikorwa cyo kwipimisha inda bigaruka"
                    }

                IndividualEncounterTypesPage ->
                    { english = "Encounter Types"
                    , kinyarwanda = Nothing
                    }

                PregnancyOutcomePage _ _ ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Nothing
                    }

                NutritionParticipantPage _ _ ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    }

                NutritionEncounterPage _ ->
                    { english = "Nutrition Encounter"
                    , kinyarwanda = Just "Isuzuma ry’imirire"
                    }

                NutritionActivityPage _ _ ->
                    { english = "Nutrition Activity"
                    , kinyarwanda = Nothing
                    }

                NutritionProgressReportPage _ ->
                    { english = "Nutrition Progress Report"
                    , kinyarwanda = Nothing
                    }

                AcuteIllnessParticipantPage _ _ ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Isuzuma  ry'uburwayi butunguranye"
                    }

                AcuteIllnessEncounterPage _ ->
                    { english = "Acute Illness Encounter"
                    , kinyarwanda = Just "Isuzuma  ry'uburwayi butunguranye"
                    }

                AcuteIllnessActivityPage _ _ ->
                    { english = "Acute Illness Activity"
                    , kinyarwanda = Just "Igikorwa cyo kuvura uburwayi butunguranye"
                    }

                AcuteIllnessProgressReportPage _ _ ->
                    { english = "Acute Illness Progress Report"
                    , kinyarwanda = Just "Raporo y’ibyakozwe ku ndwara zifatiyeho"
                    }

                AcuteIllnessOutcomePage _ ->
                    { english = "Acute Illness Outcome"
                    , kinyarwanda = Just "Iherezo ry'indwara ifatiyeho"
                    }

                HomeVisitEncounterPage _ ->
                    { english = "Home Visit Encounter"
                    , kinyarwanda = Just "Gusura abarwayi mu rugo"
                    }

                HomeVisitActivityPage _ _ ->
                    { english = "Home Visit Activity"
                    , kinyarwanda = Nothing
                    }

                WellChildParticipantPage _ _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    }

                WellChildEncounterPage _ ->
                    { english = "Standard Pediatric Visit Encounter"
                    , kinyarwanda = Just "Isura risanzwe ry'Umwana"
                    }

                WellChildActivityPage _ _ ->
                    { english = "Standard Pediatric Visit Activity"
                    , kinyarwanda = Nothing
                    }

                WellChildProgressReportPage _ ->
                    { english = "Progress Report"
                    , kinyarwanda = Nothing
                    }

                NCDParticipantPage _ _ ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Nothing
                    }

                NCDEncounterPage _ ->
                    { english = "NCD Encounter"
                    , kinyarwanda = Nothing
                    }

                NCDActivityPage _ _ ->
                    { english = "NCD Activity"
                    , kinyarwanda = Nothing
                    }

                NCDRecurrentEncounterPage _ ->
                    { english = "NCD Recurrent Encounter"
                    , kinyarwanda = Nothing
                    }

                NCDRecurrentActivityPage _ _ ->
                    { english = "NCD Recurrent Activity"
                    , kinyarwanda = Nothing
                    }

                NCDProgressReportPage _ ->
                    { english = "NCD Progress Report"
                    , kinyarwanda = Nothing
                    }

                TraceContactPage _ ->
                    { english = "Trace Contact"
                    , kinyarwanda = Nothing
                    }

                PatientRecordPage _ _ ->
                    { english = "Patient Record"
                    , kinyarwanda = Just "Amakuru y'Umurwayi"
                    }

                PrenatalLabsHistoryPage _ _ _ ->
                    { english = "Labs History"
                    , kinyarwanda = Just "Amakuru ku Bizamini byafashwe"
                    }


translateAdherence : Adherence -> TranslationSet String
translateAdherence adherence =
    case adherence of
        PrescribedAVRs ->
            { english = "Ask the mother to name or describe her prescribed AVRs. Can she correctly describe her medication?"
            , kinyarwanda = Just "Saba umubyeyi kuvuga izina ry’imiti igabanya ubukana bamuhaye. Ese abashije kuyivuga neza?"
            }

        CorrectDosage ->
            { english = "Can she tell you the correct dosage?"
            , kinyarwanda = Just "Yaba abasha kukubwira neza uburyo ayifata?"
            }

        TimeOfDay ->
            { english = "Can she tell you the correct time of day to make her ARVs?"
            , kinyarwanda = Just "Yaba abasha kukubwira amasaha ayifatiraho buri munsi?"
            }

        Adhering ->
            { english = "Based on your conversations with her, do you think she is adhering to her ARV regimen?"
            , kinyarwanda = Just "Ugendeye ku kiganiro mwagiranye, utekereza ko ari gufata imiti ye neza?"
            }


translateCounselingTimingHeading : CounselingTiming -> TranslationSet String
translateCounselingTimingHeading timing =
    case timing of
        Entry ->
            { english = "Entry Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama ku ntangiriro:"
            }

        MidPoint ->
            { english = "Mid Program Review Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama hagati mu gusubiramo gahunda:"
            }

        Exit ->
            { english = "Exit Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama kumuntu usohotse muri gahunda:"
            }

        BeforeMidpoint ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            }

        BeforeExit ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            }


translateChartPhrase : ChartPhrase -> TranslationSet String
translateChartPhrase phrase =
    case phrase of
        AgeCompletedMonthsYears ->
            { english = "Age (completed months and years)"
            , kinyarwanda = Just "Imyaka uzuza amezi n'imyaka"
            }

        AgeWeeks ->
            { english = "Age (weeks)"
            , kinyarwanda = Nothing
            }

        Birth ->
            { english = "Birth"
            , kinyarwanda = Just "kuvuka"
            }

        ChartAgeRange range ->
            case range of
                RangeBirthToThirteenWeeks ->
                    { english = "Birth to 13-weeks (z-scores)"
                    , kinyarwanda = Nothing
                    }

                RangeBirthToTwoYears ->
                    { english = "Birth to 2-years (z-scores)"
                    , kinyarwanda = Just "kuvuka (Kuva avutse)  kugeza ku myaka 2 Z-score"
                    }

                RangeBirthToFiveYears ->
                    { english = "Birth to 5-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 0-5"
                    }

                RangeFiveToTenYears ->
                    { english = "5 to 10-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-10"
                    }

                RangeFiveToNineteenYears ->
                    { english = "5 to 19-years (z-scores)"
                    , kinyarwanda = Just "Imyaka 5-19"
                    }

        HeadCircumferenceCm ->
            { english = "Head Circumference (cm)"
            , kinyarwanda = Nothing
            }

        HeadCircumferenceForAge gender ->
            case gender of
                Male ->
                    { english = "Head Circumference Boys"
                    , kinyarwanda = Nothing
                    }

                Female ->
                    { english = "Head Circumference Girls"
                    , kinyarwanda = Nothing
                    }

        HeightCm ->
            { english = "Height (cm)"
            , kinyarwanda = Just "Uburebure cm"
            }

        HeightForAge gender ->
            case gender of
                Male ->
                    { english = "Height-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    }

                Female ->
                    { english = "Height-For-Age Girls"
                    , kinyarwanda = Just "Uburebure ku myaka/ umukobwa"
                    }

        LengthCm ->
            { english = "Length (cm)"
            , kinyarwanda = Just "Uburebure cm"
            }

        LengthForAge gender ->
            case gender of
                Male ->
                    { english = "Length-For-Age Boys"
                    , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
                    }

                Female ->
                    { english = "Length-For-Age Girls"
                    , kinyarwanda = Just "uburebure ku myaka UMUKOBWA"
                    }

        Months ->
            { english = "Months"
            , kinyarwanda = Just "Amezi"
            }

        OneYear ->
            { english = "1 year"
            , kinyarwanda = Just "Umwaka umwe"
            }

        WeightForAge gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Age Boys"
                    , kinyarwanda = Just "Ibiro ku myaka umuhungu"
                    }

                Female ->
                    { english = "Weight-For-Age Girls"
                    , kinyarwanda = Just "ibiro ku myaka umukobwa"
                    }

        WeightForLength gender ->
            case gender of
                Male ->
                    { english = "Weight-For-Height Boys"
                    , kinyarwanda = Just "Ibiro ku Uburebure umuhungu"
                    }

                Female ->
                    { english = "Weight-For-Height Girls"
                    , kinyarwanda = Just "ibiro ku uburebure umukobwa"
                    }

        WeightKg ->
            { english = "Weight (kg)"
            , kinyarwanda = Just "Ibiro kg"
            }

        YearsPlural value ->
            { english = String.fromInt value ++ " years"
            , kinyarwanda = Just <| "Imyaka " ++ String.fromInt value
            }

        ZScoreChartsAvailableAt ->
            { english = "Z-score charts available at"
            , kinyarwanda = Just "Raporo ku mikurire y'umwana"
            }


translateDashboard : Dashboard -> TranslationSet String
translateDashboard trans =
    case trans of
        BeneficiariesLabel ->
            { english = "FBF Beneficiaries"
            , kinyarwanda = Nothing
            }

        AcuteIllnessDiagnosed ->
            { english = "Acute Illness Diagnosed"
            , kinyarwanda = Just "Uburwayi bufatiyeho bwasuzumwe"
            }

        BeneficiariesTableColumnLabel label ->
            case label of
                New ->
                    { english = "New beneficiaries to program"
                    , kinyarwanda = Nothing
                    }

                Missed ->
                    { english = "Missed session by beneficiaries"
                    , kinyarwanda = Nothing
                    }

                Malnourished ->
                    { english = "Malnourished beneficiaries"
                    , kinyarwanda = Nothing
                    }

                Total ->
                    { english = "Total beneficiaries in program"
                    , kinyarwanda = Nothing
                    }

        BeneficiariesTableLabel ->
            { english = "Grouped by age (Months)"
            , kinyarwanda = Nothing
            }

        BoysFilterLabel ->
            { english = "Boys"
            , kinyarwanda = Just "Umuhungu"
            }

        CallsTo114 ->
            { english = "Calls to 114"
            , kinyarwanda = Just "Inshuro bahamagaye 114"
            }

        CaseManagementFirstWordHelper ->
            { english = "Review"
            , kinyarwanda = Nothing
            }

        CaseManagementHelper ->
            { english = "list of malnourished children"
            , kinyarwanda = Nothing
            }

        CaseManagementLabel ->
            { english = "Case Management"
            , kinyarwanda = Just "Gukurikirana Umurwayi"
            }

        ChildrenWhoDied ->
            { english = "Children Who Died"
            , kinyarwanda = Nothing
            }

        CompletedProgramLabel ->
            { english = "Completed Program"
            , kinyarwanda = Nothing
            }

        CurrentPregnancies ->
            { english = "Currently Pregnant Women"
            , kinyarwanda = Just "Abagore basanzwe batwite"
            }

        CommunityLevelCases ->
            { english = "Community Level Cases"
            , kinyarwanda = Just "Umubare w'ababonetse ku rwego rw'umudugudu"
            }

        ComplicatedMalariaReferredToHC ->
            { english = "Complicated Malaria Referred to HC"
            , kinyarwanda = Just "Abarwaye Malariya y'ikigatu boherejwe ku Kigo Nderabuzima"
            }

        ComplicatedGIInfectionsReferredToHc ->
            { english = "Complicated GI Infections Referred to Health Center"
            , kinyarwanda = Just "Uburwayi bwo munda bukomeye bwoherejwe ku kigo nderabuzima"
            }

        DiagnosisUndetermined ->
            { english = "Diagnosis Undetermined"
            , kinyarwanda = Just "Uburwayi budasobanutse"
            }

        DiagnosedCases ->
            { english = "Diagnosed Cases"
            , kinyarwanda = Just "Umubare w'indwara zavuwe"
            }

        FamilyPlanningLabel ->
            { english = "Family Planning"
            , kinyarwanda = Just "Kuboneza Urubyaro"
            }

        FamilyPlanningOutOfWomen { total, useFamilyPlanning } ->
            { english = String.fromInt useFamilyPlanning ++ " out of " ++ String.fromInt total ++ " women"
            , kinyarwanda = Nothing
            }

        FamilyThatMoved ->
            { english = "Families Who Moved"
            , kinyarwanda = Nothing
            }

        FeversByCause ->
            { english = "Fevers by Cause"
            , kinyarwanda = Just "Impamvu zateye umuriro"
            }

        FeverCause cause ->
            case cause of
                FeverCauseCovid19 ->
                    { english = "COVID-19"
                    , kinyarwanda = Nothing
                    }

                FeverCauseMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    }

                FeverCauseRespiratory ->
                    { english = "Respiratory"
                    , kinyarwanda = Nothing
                    }

                FeverCauseGI ->
                    { english = "Gastrointeritis"
                    , kinyarwanda = Just "Indwara yo mu nda"
                    }

                FeverCauseUnknown ->
                    { english = "Unknown"
                    , kinyarwanda = Just "Ntibizwi"
                    }

        FeverOfUnknownOrigin ->
            { english = " Fever of Unknown Origin"
            , kinyarwanda = Just "Umuriro utazwi icyawuteye"
            }

        Filter filter ->
            case filter of
                Stunting ->
                    { english = "Stunting"
                    , kinyarwanda = Just "Igwingira"
                    }

                Underweight ->
                    { english = "Underweight"
                    , kinyarwanda = Just "Ibiro bidahagije"
                    }

                Wasting ->
                    { english = "Wasting"
                    , kinyarwanda = Just "Kunanuka Bikabije"
                    }

                Dashboard.MUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    }

                MissedSession ->
                    { english = "Missed Sessions"
                    , kinyarwanda = Nothing
                    }

        FilterProgramType filterProgramType ->
            case filterProgramType of
                FilterAllPrograms ->
                    { english = "All"
                    , kinyarwanda = Nothing
                    }

                FilterProgramAchi ->
                    { english = "ACHI"
                    , kinyarwanda = Nothing
                    }

                FilterProgramFbf ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    }

                FilterProgramPmtct ->
                    { english = "PMTCT"
                    , kinyarwanda = Nothing
                    }

                FilterProgramSorwathe ->
                    { english = "Sorwathe"
                    , kinyarwanda = Nothing
                    }

                FilterProgramCommunity ->
                    { english = "Community"
                    , kinyarwanda = Nothing
                    }

        Filters ->
            { english = "Filters"
            , kinyarwanda = Just "Guhitamo"
            }

        GirlsFilterLabel ->
            { english = "Girls"
            , kinyarwanda = Just "Umukobwa"
            }

        GoodNutritionLabel ->
            { english = "% Good nutrition"
            , kinyarwanda = Just "% Abafite imirire myiza"
            }

        HomeDeliveries ->
            { english = "Home Deliveries"
            , kinyarwanda = Just "Ababyariye mu Rugo"
            }

        HealthFacilityDeliveries ->
            { english = "Health Facility Deliveries"
            , kinyarwanda = Just "Ababyariye ku Ivuriro"
            }

        HealthCenterReferrals ->
            { english = "Health Center Referrals"
            , kinyarwanda = Just "Aboherejwe ku kigo nderabuzima"
            }

        IncidenceOf ->
            { english = "Incidence of"
            , kinyarwanda = Just "Umubare w'abana bashya bafite"
            }

        LastUpdated ->
            { english = "Last updated"
            , kinyarwanda = Just "Ivugurura riheruka"
            }

        LoadingDataGeneral ->
            { english = "Loading dashboard stats..."
            , kinyarwanda = Nothing
            }

        Moderate ->
            { english = "Moderate"
            , kinyarwanda = Nothing
            }

        MissedSessionsLabel ->
            { english = "Missed Session"
            , kinyarwanda = Nothing
            }

        ModeratelyMalnourished ->
            { english = "Moderately Malnourished"
            , kinyarwanda = Nothing
            }

        MothersInANC ->
            { english = "Mothers in ANC"
            , kinyarwanda = Just "Ababyeyi bari muri serivisi ikurikirana abagore batwite"
            }

        NewCasesLabel ->
            { english = "New Cases"
            , kinyarwanda = Nothing
            }

        NewCasesPerMonth ->
            { english = "New cases per month"
            , kinyarwanda = Just "Abashya bagaragaye mu kwezi"
            }

        NewPregnancy ->
            { english = "New Identified Pregnancies"
            , kinyarwanda = Just "Abagore bashya batwite"
            }

        NewBeneficiaries ->
            { english = "New Beneficiaries"
            , kinyarwanda = Nothing
            }

        NewbornsInCare ->
            { english = "Newborns in Care"
            , kinyarwanda = Just "Impinja zikurikiranwa"
            }

        NoDataGeneral ->
            { english = "No data for this health center."
            , kinyarwanda = Nothing
            }

        NoDataForPeriod ->
            { english = "No data for the selected period."
            , kinyarwanda = Just "Nta bipimo bigaragara muri iki gihe wahisemo"
            }

        PatientsManagedAtHome ->
            { english = "Managed at Home"
            , kinyarwanda = Just "Abavuriwe mu Rugo"
            }

        PatientCurrentlyUnderCare ->
            { english = "Currently Under Care"
            , kinyarwanda = Just "Abacyitabwaho"
            }

        PercentageLabel period ->
            case period of
                Dashboard.OneYear ->
                    { english = "from last year"
                    , kinyarwanda = Just "Guhera umwaka ushize"
                    }

                Dashboard.ThisMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    }

                Dashboard.LastMonth ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "from last month"
                    , kinyarwanda = Nothing
                    }

        PeriodFilter period ->
            case period of
                Dashboard.OneYear ->
                    { english = "1 year"
                    , kinyarwanda = Nothing
                    }

                Dashboard.ThisMonth ->
                    { english = "This month"
                    , kinyarwanda = Nothing
                    }

                Dashboard.LastMonth ->
                    { english = "Last month"
                    , kinyarwanda = Nothing
                    }

                Dashboard.ThreeMonthsAgo ->
                    { english = "Three months"
                    , kinyarwanda = Nothing
                    }

        ProgramType ->
            { english = "Program Type"
            , kinyarwanda = Nothing
            }

        ResolvedCases ->
            { english = " Resolved Cases: Currently in Care"
            , kinyarwanda = Just "Abavuwe: Bacyitabwaho"
            }

        Severe ->
            { english = "Severe"
            , kinyarwanda = Nothing
            }

        SeverelyMalnourished ->
            { english = "Severely Malnourished"
            , kinyarwanda = Nothing
            }

        StatisticsFirstWordHelper ->
            { english = "See"
            , kinyarwanda = Nothing
            }

        StatisticsHelper ->
            { english = "statistics for this month"
            , kinyarwanda = Nothing
            }

        SubFilter filter ->
            case filter of
                FilterTotal ->
                    { english = "Total"
                    , kinyarwanda = Nothing
                    }

                FilterModerate ->
                    { english = "Moderate"
                    , kinyarwanda = Nothing
                    }

                FilterSevere ->
                    { english = "Severe"
                    , kinyarwanda = Nothing
                    }

        SyncNotice ->
            { english = "If the dashboard statistics doesn't load shortly, please sync data from the backend."
            , kinyarwanda = Nothing
            }

        TotalBeneficiaries ->
            { english = "Total Beneficiaries"
            , kinyarwanda = Just "Umubare w'abana bose bafite"
            }

        TotalMalnourished ->
            { english = "Total Malnourished"
            , kinyarwanda = Nothing
            }

        TotalEncountersLabel ->
            { english = "Total encounters completed"
            , kinyarwanda = Just "Ibikorwa byose byarangiye"
            }

        TotalAssessment ->
            { english = "Total # of Assessments"
            , kinyarwanda = Just "Umubare wose w'Amasuzuma Yakozwe"
            }

        UncomplicatedMalariaByChws ->
            { english = "Uncomplicated Malaria Managed by CHWs"
            , kinyarwanda = Just "Abarwaye Malariya yorohejwe yavuwe n'abajyanama b'ubuzima"
            }

        UncomplicatedMalariaInPregnancyReferredToHc ->
            { english = "Uncomplicated Malaria in Pregnancy Referred to HC"
            , kinyarwanda = Just "Ababyeyi batwite bafite Malariya yoroheje boherejwe ku kigo nderabuzima"
            }

        UncomplicatedGIInfectionByCHWS ->
            { english = "Uncomplicated GI Infections Managed by CHWs"
            , kinyarwanda = Just "Uburwayi bwo mu nda bworoheje bwavuwe n'abajyanama w'ubuzima"
            }

        UseFamilyPlanning ->
            { english = "use family planning"
            , kinyarwanda = Nothing
            }

        Within4MonthsOfDueDate ->
            { english = "Within 4 Months of Due Date"
            , kinyarwanda = Just "Inda ibura amezi 4 ngo ivuke"
            }

        WithDangerSigns ->
            { english = "With Danger Signs"
            , kinyarwanda = Just "Abafite Ibimenyetso Mpuruza"
            }


translateLoginPhrase : LoginPhrase -> TranslationSet String
translateLoginPhrase phrase =
    case phrase of
        CheckingCachedCredentials ->
            { english = "Checking cached credentials"
            , kinyarwanda = Nothing
            }

        ForgotPassword1 ->
            { english = "Forgot your password?"
            , kinyarwanda = Just "Wibagiwe ijambo ry'ibanga?"
            }

        ForgotPassword2 ->
            { english = "Call The Ihangane Project at +250 788 817 542"
            , kinyarwanda = Just "Hamagara The Ihangane Project kuri +250 788 817 542(Hamagara kumushinga wa ihangane"
            }

        LoggedInAs ->
            { english = "Logged in as"
            , kinyarwanda = Just "Kwinjira nka"
            }

        LoginRejected method ->
            case method of
                ByAccessToken ->
                    { english = "Your access token has expired. You will need to sign in again."
                    , kinyarwanda = Just "Igihe cyo gukoresha sisitemu cyarangiye . Ongera winjore muri sisitemu"
                    }

                ByPassword ->
                    { english = "The server rejected your username or password."
                    , kinyarwanda = Just "Seriveri yanze ijambo ryo kwinjira cg ijambo ry'ibanga"
                    }

        LoginError error ->
            translateHttpError error

        LoginToSyncHealthCenters ->
            { english = "Please log in before syncing health centers"
            , kinyarwanda = Nothing
            }

        Logout ->
            { english = "Logout"
            , kinyarwanda = Just "Gufunga"
            }

        LogoutInProgress ->
            { english = "Logout in progress ..."
            , kinyarwanda = Just "sisitemi irikwifunga"
            }

        LogoutFailed ->
            { english = "Logout Failed"
            , kinyarwanda = Just "Gufunga byanze"
            }

        Password ->
            { english = "Password"
            , kinyarwanda = Just "Ijambo ry'ibanga"
            }

        PinCode ->
            { english = "PIN code"
            , kinyarwanda = Nothing
            }

        PinCodeRejected ->
            { english = "Your PIN code was not recognized."
            , kinyarwanda = Just "Umubare wawe w'ibanga ntabwo uzwi."
            }

        SignIn ->
            { english = "Sign In"
            , kinyarwanda = Just "Kwinjira"
            }

        SignOut ->
            { english = "Sign Out"
            , kinyarwanda = Just "Gusohoka muri sisiteme"
            }

        Username ->
            { english = "Username"
            , kinyarwanda = Just "Izina ryo kwinjira"
            }

        WorkOffline ->
            { english = "Work Offline"
            , kinyarwanda = Just "Gukora nta internet"
            }

        YouMustLoginBefore ->
            { english = "You must sign in before you can access the"
            , kinyarwanda = Just "Ugomba kubanza kwinjira muri sisitemi mbere yuko ubona"
            }


translateMonth : Month -> Bool -> TranslationSet String
translateMonth month short =
    case month of
        Jan ->
            if short then
                { english = "Jan"
                , kinyarwanda = Just "Mut"
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ugu"
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                }


translateMonthYY : Month -> Int -> Bool -> TranslationSet String
translateMonthYY month year short =
    translateMonth month short
        |> (\set ->
                { english = set.english ++ "-" ++ Debug.toString year
                , kinyarwanda = Maybe.map (\kinyarwanda -> kinyarwanda ++ "-" ++ Debug.toString year) set.kinyarwanda
                }
           )


translateHttpError : Http.Error -> TranslationSet String
translateHttpError error =
    case error of
        Http.NetworkError ->
            { english = "Something went wrong. Please refresh the page and try again. If problem persisits, please contact system administrator."
            , kinyarwanda = Just "Hari ikitagenze neza. Ongera ugerageze ukoraho, niba ikibazo gikomeje hamagara umuyobozi wa sisiteme."
            }

        Http.Timeout ->
            { english = "The request to the server timed out."
            , kinyarwanda = Just "Ibyo wasabye kuri seriveri byarengeje igihe."
            }

        Http.BadUrl url ->
            { english = "URL is not valid: " ++ url
            , kinyarwanda = Nothing
            }

        Http.BadStatus response ->
            { english = "The server indicated the following error:"
            , kinyarwanda = Just "Aya makosa yagaragaye hamagara kuri seriveri:"
            }

        Http.BadPayload message response ->
            { english = "The server responded with data of an unexpected type."
            , kinyarwanda = Nothing
            }


translateValidationError : ValidationError -> TranslationSet String
translateValidationError id =
    case id of
        DigitsOnly ->
            { english = "should contain only digit characters"
            , kinyarwanda = Nothing
            }

        InvalidBirthDate ->
            { english = "is invalid"
            , kinyarwanda = Nothing
            }

        InvalidBirthDateForAdult ->
            { english = "is invalid - adult should at least 13 years old"
            , kinyarwanda = Nothing
            }

        InvalidBirthDateForChild ->
            { english = "is invalid - child should be below the age of 13"
            , kinyarwanda = Nothing
            }

        InvalidHmisNumber ->
            { english = "is invalid - child should be between 1 and 15"
            , kinyarwanda = Nothing
            }

        LengthError correctLength ->
            { english = "should contain " ++ String.fromInt correctLength ++ " characters"
            , kinyarwanda = Nothing
            }

        LettersOnly ->
            { english = "should contain only letter characters"
            , kinyarwanda = Nothing
            }

        RequiredField ->
            { english = "is a required field"
            , kinyarwanda = Just "ni ngombwa kuhuzuza"
            }

        UnknownGroup ->
            { english = "is not a known Group"
            , kinyarwanda = Nothing
            }

        UnknownProvince ->
            { english = "is not a known province"
            , kinyarwanda = Nothing
            }

        UnknownDistrict ->
            { english = "is not a known district"
            , kinyarwanda = Nothing
            }

        UnknownSector ->
            { english = "is not a known sector"
            , kinyarwanda = Nothing
            }

        UnknownCell ->
            { english = "is not a known cell"
            , kinyarwanda = Nothing
            }

        UnknownVillage ->
            { english = "is not a known village"
            , kinyarwanda = Nothing
            }

        DecoderError err ->
            { english = "Decoder error: " ++ err
            , kinyarwanda = Nothing
            }


translateFormError : ErrorValue ValidationError -> TranslationSet String
translateFormError error =
    case error of
        Empty ->
            { english = "should not be empty"
            , kinyarwanda = Just "igomba kuzuzwa"
            }

        InvalidString ->
            { english = "is not a valid string"
            , kinyarwanda = Just "Ntibyemewe kwandikama inyuguti"
            }

        InvalidEmail ->
            { english = "is not a valid email"
            , kinyarwanda = Nothing
            }

        InvalidFormat ->
            { english = "is not a valid format"
            , kinyarwanda = Nothing
            }

        InvalidInt ->
            { english = "is not a valid integer"
            , kinyarwanda = Nothing
            }

        InvalidFloat ->
            { english = "is not a valid number"
            , kinyarwanda = Nothing
            }

        InvalidBool ->
            { english = "is not a valid boolean"
            , kinyarwanda = Nothing
            }

        SmallerIntThan int ->
            { english = "must be smaller than " ++ String.fromInt int
            , kinyarwanda = Nothing
            }

        GreaterIntThan int ->
            { english = "must be larger than " ++ String.fromInt int
            , kinyarwanda = Nothing
            }

        SmallerFloatThan float ->
            { english = "must be smaller than " ++ String.fromFloat float
            , kinyarwanda = Nothing
            }

        GreaterFloatThan float ->
            { english = "must be larger than " ++ String.fromFloat float
            , kinyarwanda = Nothing
            }

        ShorterStringThan int ->
            { english = "must have fewer than " ++ String.fromInt int ++ " characters"
            , kinyarwanda = Nothing
            }

        LongerStringThan int ->
            { english = "must have more than " ++ String.fromInt int ++ " characters"
            , kinyarwanda = Nothing
            }

        NotIncludedIn ->
            { english = "was not among the valid options"
            , kinyarwanda = Nothing
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
            }
