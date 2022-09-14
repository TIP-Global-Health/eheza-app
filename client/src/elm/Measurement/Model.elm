module Measurement.Model exposing (..)

{-| These modules manage the UI for the various measurements relating to a
participant.
-}

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.ParticipantConsent.Model exposing (..)
import Date exposing (Unit(..))
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Translate.Model exposing (Language)


{-| The strategy here, at least for now, is this:

  - The values in the `Model` here reflect what is entered in the UI. So, they
    are updated on every key-press etc.

  - The `update` function takes a parameter which represents the actual data.
    It updates that parameter only when the value has actually been saved.

So, basically we're doing pure UI here ... all other concerns are handled via
the `OutMsg` returned to the caller, which the caller is expected to do something
useful with.

This means that we need to be able to initialize our UI state here from some
backend state in order to perform an edit -- it's the caller's job to handle
that.

Ideally, we'll eventually use `Restful.RestfulData` to track underlying
data, UI edits, validation, and update status all in one type. If we had that,
we'd wouldn't really need our own model here (and we'd avoid some synchronization
issues) since the data itself would encapsulate an editor state.

-}
type alias ModelChild =
    { height : String
    , muac : String
    , nutrition : NutritionValue
    , photo : Maybe PhotoUrl
    , weight : String
    , counseling : Maybe ( CounselingTiming, EverySet CounselingTopicId )
    , fbfForm : FbfForm
    , contributingFactorsForm : ContributingFactorsForm
    , followUpForm : FollowUpForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    }


type alias ModelMother =
    { familyPlanningSigns : EverySet FamilyPlanningSign
    , participantConsent : ParticipantFormUI
    , lactationForm : LactationForm
    , fbfForm : FbfForm
    }


emptyModelChild : ModelChild
emptyModelChild =
    { height = ""
    , muac = ""
    , nutrition = emptyNutritionValue
    , photo = Nothing
    , weight = ""
    , counseling = Nothing
    , fbfForm = FbfForm Nothing Nothing
    , contributingFactorsForm = emptyContributingFactorsForm
    , followUpForm = emptyFollowUpForm
    , healthEducationForm = emptyHealthEducationForm
    , sendToHCForm = emptySendToHCForm
    }


emptyModelMother : ModelMother
emptyModelMother =
    { familyPlanningSigns = EverySet.empty
    , participantConsent = emptyParticipantFormUI
    , lactationForm = LactationForm Nothing
    , fbfForm = FbfForm Nothing Nothing
    }


type alias FbfForm =
    { distributedAmount : Maybe Float
    , distributionNotice : Maybe DistributionNotice
    }


emptyFbfForm : FbfForm
emptyFbfForm =
    FbfForm Nothing Nothing


type alias ContributingFactorsForm =
    { signs : Maybe (List ContributingFactorsSign)
    }


emptyContributingFactorsForm : ContributingFactorsForm
emptyContributingFactorsForm =
    ContributingFactorsForm Nothing


type alias FollowUpForm =
    { option : Maybe FollowUpOption

    -- We do not display this. Using it when saving.
    , assesment : Maybe (EverySet NutritionAssessment)
    , resolutionDate : Maybe NominalDate
    }


emptyFollowUpForm : FollowUpForm
emptyFollowUpForm =
    FollowUpForm Nothing Nothing Nothing


type alias HealthEducationForm =
    { educationForDiagnosis : Maybe Bool
    , reasonForNotProvidingHealthEducation : Maybe ReasonForNotProvidingHealthEducation
    }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    HealthEducationForm Nothing Nothing


type alias SendToHCForm =
    { handReferralForm : Maybe Bool
    , referToHealthCenter : Maybe Bool
    , accompanyToHealthCenter : Maybe Bool
    , enrollToNutritionProgram : Maybe Bool
    , referToNutritionProgram : Maybe Bool
    , reasonForNotSendingToHC : Maybe ReasonForNonReferral
    }


emptySendToHCForm : SendToHCForm
emptySendToHCForm =
    { handReferralForm = Nothing
    , referToHealthCenter = Nothing
    , accompanyToHealthCenter = Nothing
    , enrollToNutritionProgram = Nothing
    , referToNutritionProgram = Nothing
    , reasonForNotSendingToHC = Nothing
    }


{-| The UI for participant consent forms for a particular mother.

  - `expected` tracks which forms we expect to deal with for that mother.

  - `view` tracks which form we're looking at for the mother. If `Nothing`,
    we're looking at a list of the forms.

  - `progress` tracks the state of the UI for each particular form.

-}
type alias ParticipantFormUI =
    { expected : Dict ParticipantFormId ParticipantForm
    , view : Maybe ParticipantFormId
    , progress : Dict ParticipantFormId ParticipantFormProgress
    }


emptyParticipantFormUI : ParticipantFormUI
emptyParticipantFormUI =
    { expected = Dict.empty
    , view = Nothing
    , progress = Dict.empty
    }


type alias ParticipantFormProgress =
    { counselorSigned : Bool
    , participantSigned : Bool
    }


{-| The starting point for the UI where we haven't
obtained a consent yet.
-}
emptyParticipantFormProgress : ParticipantFormProgress
emptyParticipantFormProgress =
    { counselorSigned = False
    , participantSigned = False
    }


{-| The starting point for the UI when we have obtained
a consent.
-}
completedParticipantFormProgress : ParticipantFormProgress
completedParticipantFormProgress =
    { counselorSigned = True
    , participantSigned = True
    }


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FileId =
    Int


{-| Represents the "file" that DropZone gives us when
the upload is complete. There are several things we
could get from this ... for now, just the location.
-}
type alias DropZoneFile =
    { url : String
    }


type MsgChild
    = SelectNutritionSign Bool ChildNutritionSign
    | SelectCounselingTopic Bool CounselingTopicId
    | SendOutMsgChild OutMsgChild
    | SetDistributedAmountForChild String
    | SetDistributoinNoticeForChild DistributionNotice
    | UpdateHeight String
    | UpdateMuac String
    | UpdateWeight String
    | DropZoneComplete DropZoneFile
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNonReferral ReasonForNonReferral
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SetContributingFactorsSign ContributingFactorsSign
    | SetFollowUpOption FollowUpOption


type MsgMother
    = SelectFamilyPlanningSign Bool FamilyPlanningSign
    | SelectLactationSign LactationSign Bool
    | ViewParticipantForm (Maybe ParticipantFormId)
    | SetCounselorSigned ParticipantFormId Bool
    | SetDistributedAmountForMother String
    | SetDistributoinNoticeForMother DistributionNotice
    | SetParticipantSigned ParticipantFormId Bool
    | SendOutMsgMother OutMsgMother


{-| This is sort of the "opposite" of `Msg`. Instead of representing messages
which we can handle, it represents messages we **can't** handle, and would
like the caller to take care of.

The `Maybe` IDs indicate whether we're trying to update an exsiting value, vs.
creating a new one.

-}
type OutMsgChild
    = FetchIndividualNutritionData PersonId
    | SaveHeight (Maybe HeightId) HeightInCm
    | SaveWeight (Maybe WeightId) WeightInKg
    | SaveMuac (Maybe MuacId) MuacInCm
    | SaveCounselingSession (Maybe CounselingSessionId) CounselingTiming (EverySet CounselingTopicId)
    | SaveNutrition (Maybe ChildNutritionId) NutritionValue
    | SavePhoto (Maybe PhotoId) PhotoUrl
    | SaveChildFbf (Maybe ChildFbfId) FbfValue
    | SaveContributingFactors (Maybe ContributingFactorsId) (EverySet ContributingFactorsSign)
    | SaveFollowUp (Maybe FollowUpId) FollowUpValue
    | SaveHealthEducation (Maybe GroupHealthEducationId) HealthEducationValue
    | SaveSendToHC (Maybe GroupSendToHCId) SendToHCValue


type OutMsgMother
    = SaveAttendance (Maybe AttendanceId) Bool
    | SaveFamilyPlanningSigns (Maybe FamilyPlanningId) (EverySet FamilyPlanningSign)
    | SaveCompletedForm (Maybe ParticipantConsentId) ParticipantFormId Language
    | SaveLactation (Maybe LactationId) (EverySet LactationSign)
    | SaveMotherFbf (Maybe MotherFbfId) FbfValue


type NextStepsTask
    = NextStepsSendToHC
    | NextStepsHealthEducation
    | NextStepContributingFactors
    | NextStepFollowUp


type alias HeightForm =
    { height : Maybe Float
    , heightDirty : Bool
    }


emptyHeightForm : HeightForm
emptyHeightForm =
    HeightForm Nothing False


type alias MuacForm =
    { muac : Maybe Float
    , muacDirty : Bool
    }


emptyMuacForm : MuacForm
emptyMuacForm =
    MuacForm Nothing False


type alias NutritionForm =
    { signs : Maybe (List ChildNutritionSign)

    -- We do not display this. Using it when saving.
    , assesment : Maybe (EverySet NutritionAssessment)
    }


emptyNutritionForm : NutritionForm
emptyNutritionForm =
    NutritionForm Nothing Nothing


type alias PhotoForm =
    { url : Maybe PhotoUrl
    }


emptyPhotoForm : PhotoForm
emptyPhotoForm =
    PhotoForm Nothing


type alias WeightForm =
    { weight : Maybe Float
    , weightDirty : Bool
    }


emptyWeightForm : WeightForm
emptyWeightForm =
    WeightForm Nothing False


type alias VitalsForm =
    { sysBloodPressure : Maybe Float
    , sysBloodPressureDirty : Bool
    , diaBloodPressure : Maybe Float
    , diaBloodPressureDirty : Bool
    , heartRate : Maybe Int
    , heartRateDirty : Bool
    , respiratoryRate : Maybe Int
    , respiratoryRateDirty : Bool
    , bodyTemperature : Maybe Float
    , bodyTemperatureDirty : Bool
    , sysRepeated : Maybe Float
    , sysRepeatedDirty : Bool
    , diaRepeated : Maybe Float
    , diaRepeatedDirty : Bool
    }


emptyVitalsForm : VitalsForm
emptyVitalsForm =
    { sysBloodPressure = Nothing
    , sysBloodPressureDirty = False
    , diaBloodPressure = Nothing
    , diaBloodPressureDirty = False
    , heartRate = Nothing
    , heartRateDirty = False
    , respiratoryRate = Nothing
    , respiratoryRateDirty = False
    , bodyTemperature = Nothing
    , bodyTemperatureDirty = False
    , sysRepeated = Nothing
    , sysRepeatedDirty = False
    , diaRepeated = Nothing
    , diaRepeatedDirty = False
    }


type alias VitalsFormConfig msg =
    { setIntInputMsg : (Maybe Int -> VitalsForm -> VitalsForm) -> String -> msg
    , setFloatInputMsg : (Maybe Float -> VitalsForm -> VitalsForm) -> String -> msg
    , sysBloodPressurePreviousValue : Maybe Float
    , diaBloodPressurePreviousValue : Maybe Float
    , heartRatePreviousValue : Maybe Float
    , respiratoryRatePreviousValue : Maybe Float
    , bodyTemperaturePreviousValue : Maybe Float
    , birthDate : Maybe NominalDate
    , formClass : String
    , mode : VitalsFormMode
    , invokationModule : InvokationModule
    }


type VitalsFormMode
    = VitalsFormBasic
    | VitalsFormFull
    | VitalsFormRepeated


type InvokationModule
    = InvokationModulePrenatal
    | InvokationModuleAcuteIllness
    | InvokationModuleWellChild
    | InvokationModuleNCD


type alias VaccinationForm msg =
    { administeredDoses : Maybe (EverySet VaccineDose)
    , administeredDosesDirty : Bool
    , administrationDates : Maybe (EverySet NominalDate)

    -- This is the note for suggesed dose for encounter.
    -- There are situations where there will be no suggested dose,
    -- due to the ability to update previous doses.
    -- In this case, we'll set 'AdministeredPreviously' value.
    , administrationNote : Maybe AdministrationNote
    , administrationNoteDirty : Bool

    -- Form inner functionality inputs
    , viewMode : VaccinationFormViewMode
    , updatePreviousVaccines : Maybe Bool
    , willReceiveVaccineToday : Maybe Bool
    , vaccinationUpdateDate : Maybe NominalDate
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


type VaccinationFormViewMode
    = ViewModeInitial
    | ViewModeVaccinationUpdate VaccineDose


emptyVaccinationForm : VaccinationForm msg
emptyVaccinationForm =
    { administeredDoses = Nothing
    , administeredDosesDirty = False
    , administrationDates = Nothing
    , administrationNote = Nothing
    , administrationNoteDirty = False
    , viewMode = ViewModeInitial
    , updatePreviousVaccines = Nothing
    , willReceiveVaccineToday = Nothing
    , vaccinationUpdateDate = Nothing
    , dateSelectorPopupState = Nothing
    }


type alias VaccinationFormDynamicContentAndTasksConfig msg =
    { birthDate : NominalDate
    , expectedDoses : List VaccineDose
    , dosesFromPreviousEncountersData : List ( VaccineDose, NominalDate )
    , dosesFromCurrentEncounterData : List ( VaccineDose, NominalDate )
    , setVaccinationFormViewModeMsg : VaccinationFormViewMode -> msg
    , setUpdatePreviousVaccinesMsg : VaccineDose -> Bool -> msg
    , setWillReceiveVaccineTodayMsg : VaccineDose -> Bool -> msg
    , setAdministrationNoteMsg : AdministrationNote -> msg
    , setVaccinationUpdateDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setVaccinationUpdateDateMsg : NominalDate -> msg
    , saveVaccinationUpdateDateMsg : VaccineDose -> msg
    , deleteVaccinationUpdateDateMsg : VaccineDose -> NominalDate -> msg
    , nextVaccinationDataForVaccine : NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
    , getIntervalForVaccine : VaccineDose -> ( Int, Unit )
    , firstDoseExpectedFrom : NominalDate
    }


type alias CorePhysicalExamForm =
    { brittleHair : Maybe Bool
    , paleConjuctiva : Maybe Bool
    , neck : Maybe (List NeckCPESign)
    , heart : Maybe HeartCPESign
    , heartMurmur : Maybe Bool
    , lungs : Maybe (List LungsCPESign)
    , abdomen : Maybe (List AbdomenCPESign)
    , hands : Maybe (List HandsCPESign)
    , legs : Maybe (List LegsCPESign)
    }


emptyCorePhysicalExamForm : CorePhysicalExamForm
emptyCorePhysicalExamForm =
    { brittleHair = Nothing
    , paleConjuctiva = Nothing
    , neck = Nothing
    , heart = Nothing
    , heartMurmur = Nothing
    , lungs = Nothing
    , abdomen = Nothing
    , hands = Nothing
    , legs = Nothing
    }


type alias CorePhysicalExamFormConfig msg =
    { setBoolInputMsg : (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) -> Bool -> msg
    , setNeckMsg : NeckCPESign -> msg
    , setHeartMsg : HeartCPESign -> msg
    , setLungsMsg : LungsCPESign -> msg
    , setAbdomenMsg : AbdomenCPESign -> msg
    , setHandsMsg : HandsCPESign -> msg
    , setLegsMsg : LegsCPESign -> msg
    }


type alias FamilyPlanningForm =
    { signs : Maybe (List FamilyPlanningSign)
    }


emptyFamilyPlanningForm : FamilyPlanningForm
emptyFamilyPlanningForm =
    FamilyPlanningForm Nothing


type alias OutsideCareForm diagnosis =
    { seenAtAnotherFacility : Maybe Bool
    , givenNewDiagnosis : Maybe Bool
    , givenMedicine : Maybe Bool
    , plannedFollowUp : Maybe Bool
    , diagnoses : Maybe (List diagnosis)
    , diagnosesDirty : Bool
    , malariaMedications : Maybe (List OutsideCareMedication)
    , hypertensionMedications : Maybe (List OutsideCareMedication)
    , syphilisMedications : Maybe (List OutsideCareMedication)
    , hivMedications : Maybe (List OutsideCareMedication)
    , anemiaMedications : Maybe (List OutsideCareMedication)
    }


emptyOutsideCareForm : OutsideCareForm diagnosis
emptyOutsideCareForm =
    { seenAtAnotherFacility = Nothing
    , givenNewDiagnosis = Nothing
    , givenMedicine = Nothing
    , plannedFollowUp = Nothing
    , diagnoses = Nothing
    , diagnosesDirty = False
    , malariaMedications = Nothing
    , hypertensionMedications = Nothing
    , syphilisMedications = Nothing
    , hivMedications = Nothing
    , anemiaMedications = Nothing
    }


type OutsideCareStep
    = OutsideCareStepDiagnoses
    | OutsideCareStepMedications


type LaboratoryTask
    = TaskBloodGpRsTest
    | TaskHemoglobinTest
    | TaskHepatitisBTest
    | TaskHIVTest
    | TaskMalariaTest
    | TaskRandomBloodSugarTest
    | TaskSyphilisTest
    | TaskUrineDipstickTest
    | TaskHIVPCRTest
    | TaskCompletePreviousTests


type alias HIVTestForm msg =
    { knownAsPositive : Maybe Bool
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , testResult : Maybe TestResult
    , hivProgramHC : Maybe Bool
    , hivProgramHCDirty : Bool
    , partnerHIVPositive : Maybe Bool
    , partnerHIVPositiveDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyHIVTestForm : HIVTestForm msg
emptyHIVTestForm =
    { knownAsPositive = Nothing
    , testPerformed = Nothing
    , testPerformedDirty = False
    , testPerformedToday = Nothing
    , testPerformedTodayDirty = False
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , hivProgramHC = Nothing
    , hivProgramHCDirty = False
    , partnerHIVPositive = Nothing
    , partnerHIVPositiveDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    , dateSelectorPopupState = Nothing
    }


type alias MalariaTestForm msg =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , testResult : Maybe TestResult
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyMalariaTestForm : MalariaTestForm msg
emptyMalariaTestForm =
    MalariaTestForm Nothing False Nothing False Nothing False Nothing False Nothing Nothing


type alias UrineDipstickForm msg =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , testVariant : Maybe TestVariant
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyUrineDipstickForm : UrineDipstickForm msg
emptyUrineDipstickForm =
    UrineDipstickForm Nothing False Nothing False Nothing Nothing False Nothing False Nothing


type alias RandomBloodSugarForm msg =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , patientFasted : Maybe Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyRandomBloodSugarForm : RandomBloodSugarForm msg
emptyRandomBloodSugarForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , patientFasted = Nothing
    , testPerformedToday = Nothing
    , testPerformedTodayDirty = False
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , dateSelectorPopupState = Nothing
    }


type alias NonRDTForm msg =
    { knownAsPositive : Maybe Bool
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyNonRDTForm : NonRDTForm msg
emptyNonRDTForm =
    NonRDTForm Nothing Nothing False Nothing False Nothing False Nothing False Nothing


type alias ContentAndTasksLaboratoryTestInitialConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestForm msg -> HIVTestForm msg) -> Bool -> msg
    , setHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVTestResultMsg : String -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setSyphilisTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHepatitisBTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm msg -> MalariaTestForm msg) -> Bool -> msg
    , setMalariaTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestResultMsg : String -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickForm msg -> UrineDipstickForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestVariantMsg : TestVariant -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionNoteMsg : TestExecutionNote -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarForm msg -> RandomBloodSugarForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionNoteMsg : TestExecutionNote -> msg
    , noOpMsg : msg
    }


type alias ContentAndTasksForPerformedLaboratoryTestConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestForm msg -> HIVTestForm msg) -> Bool -> msg
    , setHIVTestExecutionDateMsg : NominalDate -> msg
    , setHIVTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setSyphilisTestExecutionDateMsg : NominalDate -> msg
    , setSyphilisTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHepatitisBTestExecutionDateMsg : NominalDate -> msg
    , setHepatitisBTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm msg -> MalariaTestForm msg) -> Bool -> msg
    , setMalariaTestExecutionDateMsg : NominalDate -> msg
    , setMalariaTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionDateMsg : NominalDate -> msg
    , setBloodGpRsTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickForm msg -> UrineDipstickForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionDateMsg : NominalDate -> msg
    , setUrineDipstickTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionDateMsg : NominalDate -> msg
    , setHemoglobinTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarForm msg -> RandomBloodSugarForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionDateMsg : NominalDate -> msg
    , setRandomBloodSugarTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionDateMsg : NominalDate -> msg
    , setHIVPCRTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , noOpMsg : msg
    }
