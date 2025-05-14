module Measurement.Model exposing (..)

{-| These modules manage the UI for the various measurements relating to a
participant.
-}

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.ParticipantConsent.Model exposing (..)
import Backend.Person.Model exposing (Person)
import Date exposing (Unit)
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
    , photo : Maybe ImageUrl
    , weight : String
    , counseling : Maybe ( CounselingTiming, EverySet CounselingTopicId )
    , fbfForm : FbfForm
    , contributingFactorsForm : ContributingFactorsForm
    , followUpForm : NutritionFollowUpForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    , ncdaData : NCDAData
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
    , followUpForm = emptyNutritionFollowUpForm
    , healthEducationForm = emptyHealthEducationForm
    , sendToHCForm = emptySendToHCForm
    , ncdaData = emptyNCDAData
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
    , resolutionDate : Maybe NominalDate
    }


emptyFollowUpForm : FollowUpForm
emptyFollowUpForm =
    { option = Nothing
    , resolutionDate = Nothing
    }


type alias NutritionFollowUpForm =
    { option : Maybe FollowUpOption

    -- We do not display this. Using it when saving.
    , assesment : Maybe (EverySet NutritionAssessment)
    , resolutionDate : Maybe NominalDate
    }


emptyNutritionFollowUpForm : NutritionFollowUpForm
emptyNutritionFollowUpForm =
    NutritionFollowUpForm Nothing Nothing Nothing


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
    | SetUpdateANCVisits Bool
    | ToggleANCVisitDate NominalDate
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetChildReceivesVitaminA ReceiveOption
    | SetStuntingLevel StuntingLevel
    | SetWeight String
    | SetMuac String
    | SetNCDAHelperState (Maybe NCDASign)
    | SetNCDAFormStep NCDAStep


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
    = NoOp
    | FetchIndividualNutritionData PersonId
    | SaveHeight (Maybe HeightId) HeightInCm
    | SaveWeight (Maybe WeightId) WeightInKg
    | SaveMuac (Maybe MuacId) MuacInCm
    | SaveCounselingSession (Maybe CounselingSessionId) CounselingTiming (EverySet CounselingTopicId)
    | SaveNutrition (Maybe ChildNutritionId) NutritionValue
    | SavePhoto (Maybe PhotoId) ImageUrl
    | SaveChildFbf (Maybe ChildFbfId) FbfValue
    | SaveContributingFactors (Maybe ContributingFactorsId) (EverySet ContributingFactorsSign)
    | SaveFollowUp (Maybe FollowUpId) NutritionFollowUpValue
    | SaveHealthEducation (Maybe GroupHealthEducationId) HealthEducationValue
    | SaveSendToHC (Maybe GroupSendToHCId) SendToHCValue
    | SaveNCDA (Maybe GroupNCDAId) NCDAValue


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
    { url : Maybe ImageUrl
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
    , suggestDoseToday : Bool
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
    { step : OutsideCareStep
    , seenAtAnotherFacility : Maybe Bool
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
    { step = OutsideCareStepDiagnoses
    , seenAtAnotherFacility = Nothing
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
    | TaskPregnancyTest
    | TaskCreatinineTest
    | TaskLiverFunctionTest
    | TaskLipidPanelTest
    | TaskHbA1cTest
    | TaskPartnerHIVTest
    | TaskCompletePreviousTests


type alias ContentAndTasksLaboratoryTestInitialConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestForm msg -> HIVTestForm msg) -> Bool -> msg
    , setHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVTestResultMsg : String -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setSyphilisTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHepatitisBTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm -> MalariaTestForm) -> Bool -> msg
    , setMalariaTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestResultMsg : String -> msg
    , setBloodSmearResultMsg : String -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickTestForm msg -> UrineDipstickTestForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestVariantMsg : TestVariant -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionNoteMsg : TestExecutionNote -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarTestForm msg -> RandomBloodSugarTestForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPregnancyTestFormBoolInputMsg : (Bool -> PregnancyTestForm msg -> PregnancyTestForm msg) -> Bool -> msg
    , setPregnancyTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPregnancyTestResultMsg : String -> msg
    , setCreatinineTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setCreatinineTestExecutionNoteMsg : TestExecutionNote -> msg
    , setLiverFunctionTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLiverFunctionTestExecutionNoteMsg : TestExecutionNote -> msg
    , setLipidPanelTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLipidPanelTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHbA1cTestFormBoolInputMsg : (Bool -> HbA1cTestForm msg -> HbA1cTestForm msg) -> Bool -> msg
    , setHbA1cTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPartnerHIVTestFormBoolInputMsg : (Bool -> PartnerHIVTestForm -> PartnerHIVTestForm) -> Bool -> msg
    , setPartnerHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPartnerHIVTestResultMsg : String -> msg
    , noOpMsg : msg
    }


type alias ContentAndTasksLaboratoryUniversalTestInitialConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestUniversalForm -> HIVTestUniversalForm) -> Bool -> msg
    , setHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> SyphilisTestForm -> SyphilisTestForm) -> Bool -> msg
    , setSyphilisTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> HepatitisBTestForm -> HepatitisBTestForm) -> Bool -> msg
    , setHepatitisBTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm -> MalariaTestForm) -> Bool -> msg
    , setMalariaTestExecutionNoteMsg : TestExecutionNote -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> BloodGpRsTestForm -> BloodGpRsTestForm) -> Bool -> msg
    , setBloodGpRsTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickTestUniversalForm -> UrineDipstickTestUniversalForm) -> Bool -> msg
    , setUrineDipstickTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestVariantMsg : TestVariant -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> HemoglobinTestForm -> HemoglobinTestForm) -> Bool -> msg
    , setHemoglobinTestExecutionNoteMsg : TestExecutionNote -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarTestUniversalForm -> RandomBloodSugarTestUniversalForm) -> Bool -> msg
    , setRandomBloodSugarTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> HIVPCRTestForm -> HIVPCRTestForm) -> Bool -> msg
    , setHIVPCRTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPregnancyTestFormBoolInputMsg : (Bool -> PregnancyTestForm msg -> PregnancyTestForm msg) -> Bool -> msg
    , setPregnancyTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPregnancyTestResultMsg : String -> msg
    , setCreatinineTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setCreatinineTestExecutionNoteMsg : TestExecutionNote -> msg
    , setLiverFunctionTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLiverFunctionTestExecutionNoteMsg : TestExecutionNote -> msg
    , setLipidPanelTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLipidPanelTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHbA1cTestFormBoolInputMsg : (Bool -> HbA1cTestForm msg -> HbA1cTestForm msg) -> Bool -> msg
    , setHbA1cTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPartnerHIVTestFormBoolInputMsg : (Bool -> PartnerHIVTestForm -> PartnerHIVTestForm) -> Bool -> msg
    , setPartnerHIVTestExecutionNoteMsg : TestExecutionNote -> msg
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
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm -> MalariaTestForm) -> Bool -> msg
    , setMalariaTestExecutionDateMsg : NominalDate -> msg
    , setMalariaTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionDateMsg : NominalDate -> msg
    , setBloodGpRsTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickTestForm msg -> UrineDipstickTestForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionDateMsg : NominalDate -> msg
    , setUrineDipstickTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionDateMsg : NominalDate -> msg
    , setHemoglobinTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarTestForm msg -> RandomBloodSugarTestForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionDateMsg : NominalDate -> msg
    , setRandomBloodSugarTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setRandomBloodSugarResultMsg : String -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionDateMsg : NominalDate -> msg
    , setHIVPCRTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setPregnancyTestFormBoolInputMsg : (Bool -> PregnancyTestForm msg -> PregnancyTestForm msg) -> Bool -> msg
    , setPregnancyTestExecutionDateMsg : NominalDate -> msg
    , setPregnancyTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setCreatinineTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setCreatinineTestExecutionDateMsg : NominalDate -> msg
    , setCreatinineTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setLiverFunctionTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLiverFunctionTestExecutionDateMsg : NominalDate -> msg
    , setLiverFunctionTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setLipidPanelTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLipidPanelTestExecutionDateMsg : NominalDate -> msg
    , setLipidPanelTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHbA1cTestFormBoolInputMsg : (Bool -> HbA1cTestForm msg -> HbA1cTestForm msg) -> Bool -> msg
    , setHbA1cTestExecutionDateMsg : NominalDate -> msg
    , setHbA1cTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHbA1cTestResultMsg : String -> msg
    , setPartnerHIVTestFormBoolInputMsg : (Bool -> PartnerHIVTestForm -> PartnerHIVTestForm) -> Bool -> msg
    , setPartnerHIVTestExecutionDateMsg : NominalDate -> msg
    , setPartnerHIVTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , noOpMsg : msg
    }


type alias ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg =
    { setHIVTestResultMsg : String -> msg
    , setSyphilisTestResultMsg : String -> msg
    , setIllnessSymptomMsg : IllnessSymptom -> msg
    , setHepatitisBTestResultMsg : String -> msg
    , setMalariaTestResultMsg : String -> msg
    , setBloodSmearResultMsg : String -> msg
    , setBloodGroupMsg : String -> msg
    , setRhesusMsg : String -> msg
    , setHemoglobinCountMsg : String -> msg
    , setRandomBloodSugarResultMsg : String -> msg
    , setHIVViralLoadMsg : String -> msg
    , setHIVViralLoadUndetectableMsg : Bool -> msg
    , setProteinMsg : String -> msg
    , setPHMsg : String -> msg
    , setGlucoseMsg : String -> msg
    , setLeukocytesMsg : String -> msg
    , setNitriteMsg : String -> msg
    , setUrobilinogenMsg : String -> msg
    , setHaemoglobinMsg : String -> msg
    , setKetoneMsg : String -> msg
    , setBilirubinMsg : String -> msg
    , setPartnerHIVTestResultMsg : String -> msg
    , setPregnancyTestFormBoolInputMsg : (Bool -> PregnancyTestForm msg -> PregnancyTestForm msg) -> Bool -> msg
    , setPregnancyTestExecutionDateMsg : NominalDate -> msg
    , setPregnancyTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setCreatinineTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setCreatinineTestExecutionDateMsg : NominalDate -> msg
    , setCreatinineTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setLiverFunctionTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLiverFunctionTestExecutionDateMsg : NominalDate -> msg
    , setLiverFunctionTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setLipidPanelTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setLipidPanelTestExecutionDateMsg : NominalDate -> msg
    , setLipidPanelTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHbA1cTestFormBoolInputMsg : (Bool -> HbA1cTestForm msg -> HbA1cTestForm msg) -> Bool -> msg
    , setHbA1cTestExecutionDateMsg : NominalDate -> msg
    , setHbA1cTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHbA1cTestResultMsg : String -> msg
    , noOpMsg : msg
    }


type alias ContentAndTasksLaboratoryResultConfig msg encounterId =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVResultForm -> HIVResultForm) -> Bool -> msg
    , setHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setPartnerHIVTestFormBoolInputMsg : (Bool -> PartnerHIVResultForm -> PartnerHIVResultForm) -> Bool -> msg
    , setPartnerHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> SyphilisResultForm encounterId -> SyphilisResultForm encounterId) -> Bool -> msg
    , setSyphilisTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> HepatitisBResultForm encounterId -> HepatitisBResultForm encounterId) -> Bool -> msg
    , setHepatitisBTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaResultForm -> MalariaResultForm) -> Bool -> msg
    , setMalariaTestExecutionNoteMsg : TestExecutionNote -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> BloodGpRsResultForm encounterId -> BloodGpRsResultForm encounterId) -> Bool -> msg
    , setBloodGpRsTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickResultForm -> UrineDipstickResultForm) -> Bool -> msg
    , setUrineDipstickTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> HemoglobinResultForm -> HemoglobinResultForm) -> Bool -> msg
    , setHemoglobinTestExecutionNoteMsg : TestExecutionNote -> msg
    , setRandomBloodSugarTestFormBoolInputMsg :
        (Bool
         -> RandomBloodSugarResultForm encounterId
         -> RandomBloodSugarResultForm encounterId
        )
        -> Bool
        -> msg
    , setRandomBloodSugarTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> HIVPCRResultForm -> HIVPCRResultForm) -> Bool -> msg
    , setHIVPCRTestExecutionNoteMsg : TestExecutionNote -> msg

    -- , setPregnancyTestFormBoolInputMsg : (Bool -> PregnancyTestForm msg -> PregnancyTestForm msg) -> Bool -> msg
    -- , setPregnancyTestExecutionNoteMsg : TestExecutionNote -> msg
    -- , setPregnancyTestResultMsg : String -> msg
    -- , setCreatinineTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    -- , setCreatinineTestExecutionNoteMsg : TestExecutionNote -> msg
    -- , setLiverFunctionTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    -- , setLiverFunctionTestExecutionNoteMsg : TestExecutionNote -> msg
    -- , setLipidPanelTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    -- , setLipidPanelTestExecutionNoteMsg : TestExecutionNote -> msg
    -- , setHbA1cTestFormBoolInputMsg : (Bool -> HbA1cTestForm msg -> HbA1cTestForm msg) -> Bool -> msg
    -- , setHbA1cTestExecutionNoteMsg : TestExecutionNote -> msg
    , noOpMsg : msg
    }



-- Universal Lab forms    - start


type alias BloodGpRsTestForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , bloodGroup : Maybe BloodGroup
    , bloodGroupDirty : Bool
    , rhesus : Maybe Rhesus
    , rhesusDirty : Bool
    }


emptyBloodGpRsTestForm : BloodGpRsTestForm
emptyBloodGpRsTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , bloodGroup = Nothing
    , bloodGroupDirty = False
    , rhesus = Nothing
    , rhesusDirty = False
    }


type alias BloodGpRsResultForm encounterId =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , bloodGroup : Maybe BloodGroup
    , bloodGroupDirty : Bool
    , rhesus : Maybe Rhesus
    , rhesusDirty : Bool
    , originatingEncounter : Maybe encounterId
    }


emptyBloodGpRsResultForm : BloodGpRsResultForm encounterId
emptyBloodGpRsResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , bloodGroup = Nothing
    , bloodGroupDirty = False
    , rhesus = Nothing
    , rhesusDirty = False
    , originatingEncounter = Nothing
    }


type alias HemoglobinTestForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , hemoglobinCount : Maybe Float
    , hemoglobinCountDirty : Bool
    }


emptyHemoglobinTestForm : HemoglobinTestForm
emptyHemoglobinTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , hemoglobinCount = Nothing
    , hemoglobinCountDirty = False
    }


type alias HemoglobinResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , hemoglobinCount : Maybe Float
    , hemoglobinCountDirty : Bool
    }


emptyHemoglobinResultForm : HemoglobinResultForm
emptyHemoglobinResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , hemoglobinCount = Nothing
    , hemoglobinCountDirty = False
    }


type alias HepatitisBTestForm =
    { knownAsPositive : Maybe Bool

    -- If true, test will be performed today.
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    }


emptyHepatitisBTestForm : HepatitisBTestForm
emptyHepatitisBTestForm =
    { knownAsPositive = Nothing
    , testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , testResultDirty = False
    }


type alias HepatitisBResultForm encounterId =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , originatingEncounter : Maybe encounterId
    }


emptyHepatitisBResultForm : HepatitisBResultForm encounterId
emptyHepatitisBResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , testResult = Nothing
    , testResultDirty = False
    , originatingEncounter = Nothing
    }


type alias HIVPCRTestForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , hivViralLoadStatus : Maybe ViralLoadStatus
    , hivViralLoadStatusDirty : Bool
    , hivViralLoad : Maybe Float
    , hivViralLoadDirty : Bool
    }


emptyHIVPCRTestForm : HIVPCRTestForm
emptyHIVPCRTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , hivViralLoadStatus = Nothing
    , hivViralLoadStatusDirty = False
    , hivViralLoad = Nothing
    , hivViralLoadDirty = False
    }


type alias HIVPCRResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , hivViralLoadStatus : Maybe ViralLoadStatus
    , hivViralLoadStatusDirty : Bool
    , hivViralLoad : Maybe Float
    , hivViralLoadDirty : Bool
    }


emptyHIVPCRResultForm : HIVPCRResultForm
emptyHIVPCRResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , hivViralLoadStatus = Nothing
    , hivViralLoadStatusDirty = False
    , hivViralLoad = Nothing
    , hivViralLoadDirty = False
    }


type alias RandomBloodSugarTestUniversalForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , patientFasted : Maybe Bool
    , sugarCount : Maybe Float
    , sugarCountDirty : Bool
    }


emptyRandomBloodSugarTestUniversalForm : RandomBloodSugarTestUniversalForm
emptyRandomBloodSugarTestUniversalForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , patientFasted = Nothing
    , sugarCount = Nothing
    , sugarCountDirty = False
    }


type alias RandomBloodSugarResultForm encounterId =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , sugarCount : Maybe Float
    , sugarCountDirty : Bool
    , originatingEncounter : Maybe encounterId
    }


emptyRandomBloodSugarResultForm : RandomBloodSugarResultForm encounterId
emptyRandomBloodSugarResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , sugarCount = Nothing
    , sugarCountDirty = False
    , originatingEncounter = Nothing
    }


type alias SyphilisTestForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , symptoms : Maybe (List IllnessSymptom)
    , symptomsDirty : Bool
    }


emptySyphilisTestForm : SyphilisTestForm
emptySyphilisTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , testResultDirty = False
    , symptoms = Nothing
    , symptomsDirty = False
    }


type alias SyphilisResultForm encounterId =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , symptoms : Maybe (List IllnessSymptom)
    , symptomsDirty : Bool
    , originatingEncounter : Maybe encounterId
    }


emptySyphilisResultForm : SyphilisResultForm encounterId
emptySyphilisResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , testResult = Nothing
    , testResultDirty = False
    , symptoms = Nothing
    , symptomsDirty = False
    , originatingEncounter = Nothing
    }


type alias UrineDipstickTestUniversalForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testVariant : Maybe TestVariant
    , testVariantDirty : Bool
    , protein : Maybe ProteinValue
    , proteinDirty : Bool
    , ph : Maybe PHValue
    , phDirty : Bool
    , glucose : Maybe GlucoseValue
    , glucoseDirty : Bool
    , leukocytes : Maybe LeukocytesValue
    , leukocytesDirty : Bool
    , nitrite : Maybe NitriteValue
    , nitriteDirty : Bool
    , urobilinogen : Maybe UrobilinogenValue
    , urobilinogenDirty : Bool
    , haemoglobin : Maybe HaemoglobinValue
    , haemoglobinDirty : Bool
    , ketone : Maybe KetoneValue
    , ketoneDirty : Bool
    , bilirubin : Maybe BilirubinValue
    , bilirubinDirty : Bool
    }


emptyUrineDipstickTestUniversalForm : UrineDipstickTestUniversalForm
emptyUrineDipstickTestUniversalForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testVariant = Nothing
    , testVariantDirty = False
    , protein = Nothing
    , proteinDirty = False
    , ph = Nothing
    , phDirty = False
    , glucose = Nothing
    , glucoseDirty = False
    , leukocytes = Nothing
    , leukocytesDirty = False
    , nitrite = Nothing
    , nitriteDirty = False
    , urobilinogen = Nothing
    , urobilinogenDirty = False
    , haemoglobin = Nothing
    , haemoglobinDirty = False
    , ketone = Nothing
    , ketoneDirty = False
    , bilirubin = Nothing
    , bilirubinDirty = False
    }


type alias UrineDipstickResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , testVariant : Maybe TestVariant
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , protein : Maybe ProteinValue
    , proteinDirty : Bool
    , ph : Maybe PHValue
    , phDirty : Bool
    , glucose : Maybe GlucoseValue
    , glucoseDirty : Bool
    , leukocytes : Maybe LeukocytesValue
    , leukocytesDirty : Bool
    , nitrite : Maybe NitriteValue
    , nitriteDirty : Bool
    , urobilinogen : Maybe UrobilinogenValue
    , urobilinogenDirty : Bool
    , haemoglobin : Maybe HaemoglobinValue
    , haemoglobinDirty : Bool
    , ketone : Maybe KetoneValue
    , ketoneDirty : Bool
    , bilirubin : Maybe BilirubinValue
    , bilirubinDirty : Bool
    }


emptyUrineDipstickResultForm : UrineDipstickResultForm
emptyUrineDipstickResultForm =
    { runConfirmedByLabTech = Nothing
    , testVariant = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , protein = Nothing
    , proteinDirty = False
    , ph = Nothing
    , phDirty = False
    , glucose = Nothing
    , glucoseDirty = False
    , leukocytes = Nothing
    , leukocytesDirty = False
    , nitrite = Nothing
    , nitriteDirty = False
    , urobilinogen = Nothing
    , urobilinogenDirty = False
    , haemoglobin = Nothing
    , haemoglobinDirty = False
    , ketone = Nothing
    , ketoneDirty = False
    , bilirubin = Nothing
    , bilirubinDirty = False
    }


type alias HIVTestUniversalForm =
    { knownAsPositive : Maybe Bool
    , -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , hivProgramHC : Maybe Bool
    , hivProgramHCDirty : Bool
    , partnerHIVPositive : Maybe Bool
    , partnerHIVPositiveDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    }


emptyHIVTestUniversalForm : HIVTestUniversalForm
emptyHIVTestUniversalForm =
    { knownAsPositive = Nothing
    , testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , testResultDirty = False
    , hivProgramHC = Nothing
    , hivProgramHCDirty = False
    , partnerHIVPositive = Nothing
    , partnerHIVPositiveDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    }


type alias HIVResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , hivProgramHC : Maybe Bool
    , hivProgramHCDirty : Bool
    , partnerHIVPositive : Maybe Bool
    , partnerHIVPositiveDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    }


emptyHIVResultForm : HIVResultForm
emptyHIVResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , testResult = Nothing
    , testResultDirty = False
    , hivProgramHC = Nothing
    , hivProgramHCDirty = False
    , partnerHIVPositive = Nothing
    , partnerHIVPositiveDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    }


type alias MalariaTestForm =
    { -- If true, test will be performed today.
      testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool

    -- Holds the date of Malaria test execution.
    -- If Malaria test was not performed, but Blood smear was,
    -- will hold the date of blood smear.
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , bloodSmearTaken : Maybe Bool
    , bloodSmearTakenDirty : Bool
    , bloodSmearResult : Maybe BloodSmearResult
    , bloodSmearResultDirty : Bool
    }


emptyMalariaTestForm : MalariaTestForm
emptyMalariaTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , testResultDirty = False
    , bloodSmearTaken = Nothing
    , bloodSmearTakenDirty = False
    , bloodSmearResult = Nothing
    , bloodSmearResultDirty = False
    }


type alias MalariaResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , testResultDirty : Bool

    -- Set to True, if Malaria test was not
    -- taken, and blood smear was ordered at lab.
    , bloodSmearTaken : Bool
    , bloodSmearResult : Maybe BloodSmearResult
    , bloodSmearResultDirty : Bool
    }


emptyMalariaResultForm : MalariaResultForm
emptyMalariaResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , testResult = Nothing
    , testResultDirty = False
    , bloodSmearTaken = False
    , bloodSmearResult = Nothing
    , bloodSmearResultDirty = False
    }


type alias PartnerHIVTestForm =
    { knownAsPositive : Maybe Bool

    -- If true, test will be performed today.
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool

    -- Test specific fields.
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    }


emptyPartnerHIVTestForm : PartnerHIVTestForm
emptyPartnerHIVTestForm =
    { knownAsPositive = Nothing
    , testPerformed = Nothing
    , testPerformedDirty = False
    , immediateResult = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , testResultDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    }


type alias PartnerHIVResultForm =
    { runConfirmedByLabTech : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , testResult : Maybe TestResult
    , testResultDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    }


emptyPartnerHIVResultForm : PartnerHIVResultForm
emptyPartnerHIVResultForm =
    { runConfirmedByLabTech = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , testResult = Nothing
    , testResultDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    }



-- Universal Lab forms    - end


type alias HIVTestForm msg =
    { knownAsPositive : Maybe Bool
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
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
    , immediateResult = Nothing
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


type alias UrineDipstickTestForm msg =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , immediateResult : Maybe Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , testVariant : Maybe TestVariant
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyUrineDipstickTestForm : UrineDipstickTestForm msg
emptyUrineDipstickTestForm =
    UrineDipstickTestForm Nothing False Nothing Nothing False Nothing Nothing False Nothing False Nothing


type alias RandomBloodSugarTestForm msg =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , patientFasted : Maybe Bool
    , immediateResult : Maybe Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    , sugarCount : Maybe Float
    , sugarCountDirty : Bool
    }


emptyRandomBloodSugarTestForm : RandomBloodSugarTestForm msg
emptyRandomBloodSugarTestForm =
    { testPerformed = Nothing
    , testPerformedDirty = False
    , patientFasted = Nothing
    , immediateResult = Nothing
    , testPerformedToday = Nothing
    , testPerformedTodayDirty = False
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , dateSelectorPopupState = Nothing

    -- We need this, since RandomBloodSugar result can be
    -- entered both  immediately, and from case management.
    , sugarCount = Nothing
    , sugarCountDirty = False
    }


type alias PregnancyTestForm msg =
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
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    }


emptyPregnancyTestForm : PregnancyTestForm msg
emptyPregnancyTestForm =
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


type alias CreatinineResultForm =
    { executionNote : Maybe TestExecutionNote
    , executionDate : Maybe NominalDate
    , creatinineResult : Maybe Float
    , bunResult : Maybe Float
    }


emptyCreatinineResultForm : CreatinineResultForm
emptyCreatinineResultForm =
    CreatinineResultForm Nothing Nothing Nothing Nothing


type alias LiverFunctionResultForm =
    { executionNote : Maybe TestExecutionNote
    , executionDate : Maybe NominalDate
    , altResult : Maybe Float
    , astResult : Maybe Float
    }


emptyLiverFunctionResultForm : LiverFunctionResultForm
emptyLiverFunctionResultForm =
    LiverFunctionResultForm Nothing Nothing Nothing Nothing


type alias NCDAData =
    { form : NCDAForm
    , helperState : Maybe NCDASign
    }


emptyNCDAData : NCDAData
emptyNCDAData =
    { form = emptyNCDAForm
    , helperState = Nothing
    }


type alias NCDAForm =
    { step : Maybe NCDAStep

    -- Step 1.
    , updateANCVisits : Maybe Bool
    , ancVisitsDates : Maybe (EverySet NominalDate)
    , supplementsDuringPregnancy : Maybe Bool
    , takenSupplementsPerGuidance : Maybe Bool
    , bornWithBirthDefect : Maybe Bool
    , birthWeight : Maybe WeightInGrm

    -- Step 2.
    , stuntingLevel : Maybe StuntingLevel
    , stuntingLevelNotTaken : Maybe Bool
    , weight : Maybe WeightInKg
    , weightNotTaken : Maybe Bool
    , muac : Maybe MuacInCm
    , muacNotTaken : Maybe Bool
    , showsEdemaSigns : Maybe Bool

    -- Step 3.
    , childBehindOnVaccination : Maybe Bool
    , childReceivesVitaminA : Maybe ReceiveOption
    , childReceivesDewormer : Maybe Bool
    , ongeraMNP : Maybe Bool
    , takingOngeraMNP : Maybe Bool
    , childReceivesECD : Maybe Bool

    -- Step 4.
    , fiveFoodGroups : Maybe Bool
    , breastfedForSixMonths : Maybe Bool
    , appropriateComplementaryFeeding : Maybe Bool
    , mealsAtRecommendedTimes : Maybe Bool

    -- Step 5.
    , childReceivesFBF : Maybe Bool
    , childTakingFBF : Maybe Bool
    , beneficiaryCashTransfer : Maybe Bool
    , receivingCashTransfer : Maybe Bool
    , conditionalFoodItems : Maybe Bool
    , treatedForAcuteMalnutrition : Maybe Bool
    , childWithDisability : Maybe Bool
    , receivingSupport : Maybe Bool
    , childGotDiarrhea : Maybe Bool

    -- Step 6.
    , hasCleanWater : Maybe Bool
    , hasHandwashingFacility : Maybe Bool
    , hasToilets : Maybe Bool
    , hasKitchenGarden : Maybe Bool
    , insecticideTreatedBednets : Maybe Bool
    }


emptyNCDAForm : NCDAForm
emptyNCDAForm =
    { step = Nothing

    -- Step 1.
    , updateANCVisits = Nothing
    , ancVisitsDates = Nothing
    , supplementsDuringPregnancy = Nothing
    , takenSupplementsPerGuidance = Nothing
    , bornWithBirthDefect = Nothing
    , birthWeight = Nothing

    -- Step 2.
    , stuntingLevel = Nothing
    , stuntingLevelNotTaken = Nothing
    , weight = Nothing
    , weightNotTaken = Nothing
    , muac = Nothing
    , muacNotTaken = Nothing
    , showsEdemaSigns = Nothing

    -- Step 3.
    , childBehindOnVaccination = Nothing
    , childReceivesVitaminA = Nothing
    , childReceivesDewormer = Nothing
    , ongeraMNP = Nothing
    , takingOngeraMNP = Nothing
    , childReceivesECD = Nothing

    -- Step 4.
    , fiveFoodGroups = Nothing
    , breastfedForSixMonths = Nothing
    , appropriateComplementaryFeeding = Nothing
    , mealsAtRecommendedTimes = Nothing

    -- Step 5.
    , childReceivesFBF = Nothing
    , childTakingFBF = Nothing
    , beneficiaryCashTransfer = Nothing
    , receivingCashTransfer = Nothing
    , conditionalFoodItems = Nothing
    , treatedForAcuteMalnutrition = Nothing
    , childWithDisability = Nothing
    , receivingSupport = Nothing
    , childGotDiarrhea = Nothing

    -- Step 6.
    , hasCleanWater = Nothing
    , hasHandwashingFacility = Nothing
    , hasToilets = Nothing
    , hasKitchenGarden = Nothing
    , insecticideTreatedBednets = Nothing
    }


type NCDAStep
    = NCDAStepAntenatalCare
    | NCDAStepNutritionAssessment
    | NCDAStepUniversalInterventions
    | NCDAStepNutritionBehavior
    | NCDAStepTargetedInterventions
    | NCDAStepInfrastructureEnvironment


type GroupOfFoods
    = Staples
    | Legumes
    | DairyProducts
    | AnimalSourceFoods
    | Eggs
    | FruitsVegetables
    | BreastMilk
    | MealsWithEdibleOil


type alias LipidPanelResultForm =
    { executionNote : Maybe TestExecutionNote
    , executionDate : Maybe NominalDate
    , unitOfMeasurement : Maybe UnitOfMeasurement
    , totalCholesterolResult : Maybe Float
    , totalCholesterolResultDirty : Bool
    , ldlCholesterolResult : Maybe Float
    , ldlCholesterolResultDirty : Bool
    , hdlCholesterolResult : Maybe Float
    , hdlCholesterolResultDirty : Bool
    , triglyceridesResult : Maybe Float
    , triglyceridesResultDirty : Bool
    }


emptyLipidPanelResultForm : LipidPanelResultForm
emptyLipidPanelResultForm =
    { executionNote = Nothing
    , executionDate = Nothing
    , unitOfMeasurement = Nothing
    , totalCholesterolResult = Nothing
    , totalCholesterolResultDirty = False
    , ldlCholesterolResult = Nothing
    , ldlCholesterolResultDirty = False
    , hdlCholesterolResult = Nothing
    , hdlCholesterolResultDirty = False
    , triglyceridesResult = Nothing
    , triglyceridesResultDirty = False
    }


type alias HbA1cTestForm msg =
    { gotResultsPreviously : Maybe Bool
    , executionNote : Maybe TestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
    , hba1cResult : Maybe Float
    , hba1cResultDirty : Bool
    }


emptyHbA1cTestForm : HbA1cTestForm msg
emptyHbA1cTestForm =
    { gotResultsPreviously = Nothing
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , dateSelectorPopupState = Nothing
    , hba1cResult = Nothing
    , hba1cResultDirty = False
    }


type alias NCDAContentConfig msg =
    { -- Indicates if NCDA activity was performed at Health center,
      -- or by CHW (during Child Scoreboard encounter).
      atHealthCenter : Bool

    -- Indications if display of tasks tray is required or not.
    , showTasksTray : Bool

    -- Required data, which is resolved from previous encounters.
    , pregnancySummary : Maybe PregnancySummaryValue
    , ncdaNeverFilled : Bool
    , ncdaNotFilledAfterAgeOfSixMonths : Bool

    -- ANC Visit actions.
    , setUpdateANCVisitsMsg : Bool -> msg
    , toggleANCVisitDateMsg : NominalDate -> msg

    -- Other actions.
    , setBoolInputMsg : (Bool -> NCDAForm -> NCDAForm) -> Bool -> msg
    , setBirthWeightMsg : String -> msg
    , setChildReceivesVitaminAMsg : ReceiveOption -> msg
    , setStuntingLevelMsg : StuntingLevel -> msg
    , setWeightMsg : String -> msg
    , setMuacMsg : String -> msg
    , setStepMsg : NCDAStep -> msg
    , setHelperStateMsg : Maybe NCDASign -> msg
    , saveMsg : msg
    }


minimalNumberOfANCVisits : Int
minimalNumberOfANCVisits =
    4


type alias VaccinationProgressDict =
    Dict WellChildVaccineType (Dict VaccineDose NominalDate)


type VaccinationStatus
    = StatusBehind
    | StatusCompleted
    | StatusUpToDate


type ImmunisationTask
    = TaskBCG
    | TaskDTP
    | TaskDTPStandalone
    | TaskHPV
    | TaskIPV
    | TaskMR
    | TaskOPV
    | TaskPCV13
    | TaskRotarix
    | TaskOverview


type alias NutritionFeedingForm =
    { receiveSupplement : Maybe Bool
    , rationPresentAtHome : Maybe Bool
    , enoughTillNextSession : Maybe Bool
    , supplementShared : Maybe Bool
    , encouragedToEat : Maybe Bool
    , refusingToEat : Maybe Bool
    , breastfeeding : Maybe Bool
    , cleanWaterAvailable : Maybe Bool
    , eatenWithWater : Maybe Bool
    , supplementType : Maybe NutritionSupplementType
    , sachetsPerDay : Maybe Float
    }


emptyNutritionFeedingForm : NutritionFeedingForm
emptyNutritionFeedingForm =
    { receiveSupplement = Nothing
    , rationPresentAtHome = Nothing
    , enoughTillNextSession = Nothing
    , supplementShared = Nothing
    , encouragedToEat = Nothing
    , refusingToEat = Nothing
    , breastfeeding = Nothing
    , cleanWaterAvailable = Nothing
    , eatenWithWater = Nothing
    , supplementType = Nothing
    , sachetsPerDay = Nothing
    }


type alias NutritionHygieneForm =
    { soapInTheHouse : Maybe Bool
    , washHandsBeforeFeeding : Maybe Bool
    , foodCovered : Maybe Bool
    , mainWaterSource : Maybe MainWaterSource
    , waterPreparationOption : Maybe WaterPreparationOption
    }


emptyNutritionHygieneForm : NutritionHygieneForm
emptyNutritionHygieneForm =
    { soapInTheHouse = Nothing
    , washHandsBeforeFeeding = Nothing
    , foodCovered = Nothing
    , mainWaterSource = Nothing
    , waterPreparationOption = Nothing
    }


type alias NutritionFoodSecurityForm =
    { householdGotFood : Maybe Bool
    , mainIncomeSource : Maybe MainIncomeSource
    }


emptyNutritionFoodSecurityForm : NutritionFoodSecurityForm
emptyNutritionFoodSecurityForm =
    NutritionFoodSecurityForm Nothing Nothing


type alias NutritionCaringForm =
    { caringOption : Maybe CaringOption
    , parentHealth : Maybe Bool
    , childClean : Maybe Bool
    }


emptyNutritionCaringForm : NutritionCaringForm
emptyNutritionCaringForm =
    NutritionCaringForm Nothing Nothing Nothing


type alias OngoingTreatmentReviewForm =
    { takenAsPrescribed : Maybe Bool
    , missedDoses : Maybe Bool
    , feelingBetter : Maybe Bool
    , sideEffects : Maybe Bool
    , reasonForNotTaking : Maybe ReasonForNotTaking
    , reasonForNotTakingDirty : Bool
    , totalMissedDoses : Maybe Int
    , totalMissedDosesDirty : Bool
    , adverseEvents : Maybe (List AdverseEvent)
    , adverseEventsDirty : Bool
    }


emptyOngoingTreatmentReviewForm : OngoingTreatmentReviewForm
emptyOngoingTreatmentReviewForm =
    { takenAsPrescribed = Nothing
    , missedDoses = Nothing
    , feelingBetter = Nothing
    , sideEffects = Nothing
    , reasonForNotTaking = Nothing
    , reasonForNotTakingDirty = False
    , totalMissedDoses = Nothing
    , totalMissedDosesDirty = False
    , adverseEvents = Nothing
    , adverseEventsDirty = False
    }


type alias MedicationAdministrationForm =
    { medicationAdministered : Maybe Bool
    , reasonForNonAdministration : Maybe AdministrationNote
    }


emptyMedicationAdministrationForm : MedicationAdministrationForm
emptyMedicationAdministrationForm =
    MedicationAdministrationForm Nothing Nothing


type alias MedicationAdministrationFormConfig msg =
    { medication : MedicationDistributionSign
    , setMedicationAdministeredMsg : Bool -> msg
    , setReasonForNonAdministration : AdministrationNote -> msg
    , resolveDosageAndIconFunc : Language -> NominalDate -> Person -> Maybe ( String, String, String )
    }
