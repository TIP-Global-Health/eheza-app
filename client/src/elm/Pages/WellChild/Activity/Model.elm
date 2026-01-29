module Pages.WellChild.Activity.Model exposing (DangerSignsData, HeadCircumferenceForm, HomeVisitData, ImmunisationData, MedicationData, Model, Msg(..), NextStepsData, NextVisitForm, NutritionAssessmentData, PregnancySummaryForm, SymptomsReviewForm, WarningPopupType(..), WellChildECDForm, WellChildVaccinationForm, emptyModel, medicationTasks)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (ContributingFactorsForm, DropZoneFile, HealthEducationForm, HeightForm, ImmunisationTask, MedicationAdministrationForm, MuacForm, NCDAData, NCDAForm, NCDAStep, NutritionCaringForm, NutritionFeedingForm, NutritionFollowUpForm, NutritionFoodSecurityForm, NutritionForm, NutritionHygieneForm, PhotoForm, SendToHCForm, VaccinationForm, VaccinationFormViewMode, VitalsForm, WeightForm, emptyContributingFactorsForm, emptyHealthEducationForm, emptyHeightForm, emptyMedicationAdministrationForm, emptyMuacForm, emptyNCDAData, emptyNutritionCaringForm, emptyNutritionFeedingForm, emptyNutritionFollowUpForm, emptyNutritionFoodSecurityForm, emptyNutritionForm, emptyNutritionHygieneForm, emptyPhotoForm, emptySendToHCForm, emptyVaccinationForm, emptyVitalsForm, emptyWeightForm)
import Pages.Page exposing (Page)
import Pages.WellChild.Activity.Types exposing (DangerSignsTask, MedicationTask(..), NutritionAssessmentTask)


type Msg
    = NoOp
    | SetActivePage Page
    | SetWarningPopupState (Maybe WarningPopupType)
      -- PREGNANCY SUMMARY
    | SetExpectedDateConcluded Date
    | SetExpectedDateConcludedSelectorState (Maybe (DateSelectorConfig Msg))
    | SetPregnancySummaryBoolInput (Bool -> PregnancySummaryForm -> PregnancySummaryForm) Bool
    | SetPregnancySummaryNumberInput (String -> PregnancySummaryForm -> PregnancySummaryForm) String
    | SetDeliveryComplication DeliveryComplication
    | SetBirthDefect BirthDefect
    | SavePregnancySummary PersonId (Maybe ( WellChildPregnancySummaryId, WellChildPregnancySummary ))
      -- DANGER SIGNS
    | SetActiveDangerSignsTask DangerSignsTask
    | SetSymptom WellChildSymptom
    | SaveSymptomsReview PersonId (Maybe ( WellChildSymptomsReviewId, WellChildSymptomsReview )) (Maybe DangerSignsTask)
    | SetVitalsIntInput (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( WellChildVitalsId, WellChildVitals )) (Maybe DangerSignsTask)
      -- NUTRITION ASSESMENT
    | SetActiveNutritionAssessmentTask NutritionAssessmentTask
    | SetHeight String
    | SaveHeight PersonId (Maybe ( WellChildHeightId, WellChildHeight )) (Maybe NutritionAssessmentTask)
    | SetHeadCircumference String
    | ToggleHeadCircumferenceNotTaken
    | CloseHeadCircumferencePopup PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | PreSaveHeadCircumference PersonId (Maybe Float) (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | SaveHeadCircumference PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | SetMuac String
    | SaveMuac PersonId (Maybe ( WellChildMuacId, WellChildMuac )) (Maybe NutritionAssessmentTask)
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( WellChildNutritionId, WellChildNutrition )) (EverySet NutritionAssessment) (Maybe NutritionAssessmentTask)
    | SetWeight String
    | SaveWeight PersonId (Maybe ( WellChildWeightId, WellChildWeight )) (Maybe NutritionAssessmentTask)
      -- IMMUNISATION
    | SetActiveImmunisationTask ImmunisationTask
    | SetVaccinationFormViewMode WellChildVaccineType VaccinationFormViewMode
    | SetUpdatePreviousVaccines WellChildVaccineType VaccineDose Bool
    | SetWillReceiveVaccineToday WellChildVaccineType VaccineDose Bool
    | SetAdministrationNote WellChildVaccineType AdministrationNote
    | SetVaccinationUpdateDateSelectorState WellChildVaccineType (Maybe (DateSelectorConfig Msg))
    | SetVaccinationUpdateDate WellChildVaccineType NominalDate
    | SaveVaccinationUpdateDate WellChildVaccineType VaccineDose
    | DeleteVaccinationUpdateDate WellChildVaccineType VaccineDose NominalDate
    | SaveBCGImmunisation PersonId (Maybe ( WellChildBCGImmunisationId, WellChildBCGImmunisation )) (Maybe ImmunisationTask)
    | SaveDTPImmunisation PersonId (Maybe ( WellChildDTPImmunisationId, WellChildDTPImmunisation )) (Maybe ImmunisationTask)
    | SaveDTPStandaloneImmunisation PersonId (Maybe ( WellChildDTPStandaloneImmunisationId, WellChildDTPStandaloneImmunisation )) (Maybe ImmunisationTask)
    | SaveHPVImmunisation PersonId (Maybe ( WellChildHPVImmunisationId, WellChildHPVImmunisation )) (Maybe ImmunisationTask)
    | SaveIPVImmunisation PersonId (Maybe ( WellChildIPVImmunisationId, WellChildIPVImmunisation )) (Maybe ImmunisationTask)
    | SaveMRImmunisation PersonId (Maybe ( WellChildMRImmunisationId, WellChildMRImmunisation )) (Maybe ImmunisationTask)
    | SaveOPVImmunisation PersonId (Maybe ( WellChildOPVImmunisationId, WellChildOPVImmunisation )) (Maybe ImmunisationTask)
    | SavePCV13Immunisation PersonId (Maybe ( WellChildPCV13ImmunisationId, WellChildPCV13Immunisation )) (Maybe ImmunisationTask)
    | SaveRotarixImmunisation PersonId (Maybe ( WellChildRotarixImmunisationId, WellChildRotarixImmunisation )) (Maybe ImmunisationTask)
      -- ECD
    | SetECDBoolInput (Bool -> WellChildECDForm -> WellChildECDForm) Bool
    | SaveECD PersonId (Maybe ( WellChildECDId, WellChildECD ))
      -- MEDICATION
    | SetActiveMedicationTask MedicationTask
    | SetAlbendazoleAdministered Bool
    | SetAlbendazoleReasonForNonAdministration AdministrationNote
    | SaveAlbendazole PersonId (Maybe ( WellChildAlbendazoleId, WellChildAlbendazole )) (Maybe MedicationTask)
    | SetMebendezoleAdministered Bool
    | SetMebendezoleReasonForNonAdministration AdministrationNote
    | SaveMebendezole PersonId (Maybe ( WellChildMebendezoleId, WellChildMebendezole )) (Maybe MedicationTask)
    | SetVitaminAAdministered Bool
    | SetVitaminAReasonForNonAdministration AdministrationNote
    | SaveVitaminA PersonId (Maybe ( WellChildVitaminAId, WellChildVitaminA )) (Maybe MedicationTask)
      -- NEXT STEPS
    | SetActiveNextStepsTask Pages.WellChild.Activity.Types.NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetEnrollToNutritionProgram Bool
    | SetReferToNutritionProgram Bool
    | SetReasonForNonReferral ReasonForNonReferral
    | SaveSendToHC PersonId (Maybe ( WellChildSendToHCId, WellChildSendToHC )) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SaveHealthEducation PersonId (Maybe ( WellChildHealthEducationId, WellChildHealthEducation )) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
    | SetContributingFactorsSign ContributingFactorsSign
    | SaveContributingFactors PersonId (Maybe ( WellChildContributingFactorsId, WellChildContributingFactors )) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( WellChildFollowUpId, WellChildFollowUp )) (EverySet NutritionAssessment) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
    | SaveNextVisit PersonId (Maybe ( WellChildNextVisitId, WellChildNextVisit )) (Maybe NominalDate) (Maybe NominalDate) (Maybe NominalDate) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
      -- PHOTO
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe WellChildPhotoId) ImageUrl
      -- NCDA
    | SetUpdateANCVisits Bool
    | ToggleANCVisitDate NominalDate
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SetBirthWeight String
    | SetChildReceivesVitaminA ReceiveOption
    | SetStuntingLevel StuntingLevel
    | SetWeightForNCDA String
    | SetMuacForNCDA String
    | SetNCDAFormStep NCDAStep
    | SetNCDAHelperState (Maybe NCDASign)
    | SaveNCDA PersonId (Maybe ( WellChildNCDAId, WellChildNCDA ))
      -- HOME VISIT
    | SetActiveHomeVisitTask Pages.WellChild.Activity.Types.HomeVisitTask
    | SetFeedingBoolInput (Bool -> NutritionFeedingForm -> NutritionFeedingForm) Bool
    | SetNutritionSupplementType NutritionSupplementType
    | SetSachetsPerDay String
    | SaveFeeding PersonId (Maybe ( WellChildFeedingId, WellChildFeeding )) (Maybe Pages.WellChild.Activity.Types.HomeVisitTask)
    | SetHygieneBoolInput (Bool -> NutritionHygieneForm -> NutritionHygieneForm) Bool
    | SetMainWaterSource MainWaterSource
    | SaveHygiene PersonId (Maybe ( WellChildHygieneId, WellChildHygiene )) (Maybe Pages.WellChild.Activity.Types.HomeVisitTask)
    | SetFoodSecurityBoolInput (Bool -> NutritionFoodSecurityForm -> NutritionFoodSecurityForm) Bool
    | SetMainIncomeSource MainIncomeSource
    | SetWaterPreparationOption WaterPreparationOption
    | SaveFoodSecurity PersonId (Maybe ( WellChildFoodSecurityId, WellChildFoodSecurity )) (Maybe Pages.WellChild.Activity.Types.HomeVisitTask)
    | SetParentsAliveAndHealthy Bool
    | SetChildClean Bool
    | SetNutritionCaringOption CaringOption
    | SaveNutritionCaring PersonId (Maybe ( WellChildCaringId, WellChildCaring )) (Maybe Pages.WellChild.Activity.Types.HomeVisitTask)


type alias Model =
    { pregnancySummaryForm : PregnancySummaryForm
    , dangerSignsData : DangerSignsData
    , nutritionAssessmentData : NutritionAssessmentData
    , immunisationData : ImmunisationData
    , ecdForm : WellChildECDForm
    , medicationData : MedicationData
    , nextStepsData : NextStepsData
    , photoForm : PhotoForm
    , ncdaData : NCDAData
    , homeVisitData : HomeVisitData
    , warningPopupState : Maybe WarningPopupType
    }


emptyModel : Model
emptyModel =
    { pregnancySummaryForm = emptyPregnancySummaryForm
    , dangerSignsData = emptyDangerSignsData
    , nutritionAssessmentData = emptyNutritionAssessmentData
    , immunisationData = emptyImmunisationData
    , ecdForm = emptyWellChildECDForm
    , medicationData = emptyMedicationData
    , nextStepsData = emptyNextStepsData
    , photoForm = emptyPhotoForm
    , ncdaData = emptyNCDAData
    , homeVisitData = emptyHomeVisitData
    , warningPopupState = Nothing
    }


type WarningPopupType
    = PopupNutritionAssessment (List NutritionAssessment)
    | PopupMacrocephaly PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | PopupMicrocephaly PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)


type alias PregnancySummaryForm =
    { expectedDateConcluded : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , deliveryComplicationsPresent : Maybe Bool
    , deliveryComplications : Maybe (List DeliveryComplication)
    , apgarScoresAvailable : Maybe Bool
    , apgarOneMin : Maybe Float
    , apgarFiveMin : Maybe Float
    , apgarDirty : Bool
    , birthWeight : Maybe WeightInGrm
    , birthLengthAvailable : Maybe Bool
    , birthLength : Maybe HeightInCm
    , birthLengthDirty : Bool
    , birthDefectsPresent : Maybe Bool
    , birthDefects : Maybe (List BirthDefect)
    }


emptyPregnancySummaryForm : PregnancySummaryForm
emptyPregnancySummaryForm =
    { expectedDateConcluded = Nothing
    , dateSelectorPopupState = Nothing
    , deliveryComplicationsPresent = Nothing
    , deliveryComplications = Nothing
    , apgarScoresAvailable = Nothing
    , apgarOneMin = Nothing
    , apgarFiveMin = Nothing
    , apgarDirty = False
    , birthWeight = Nothing
    , birthLengthAvailable = Nothing
    , birthLength = Nothing
    , birthLengthDirty = False
    , birthDefectsPresent = Nothing
    , birthDefects = Nothing
    }


type alias DangerSignsData =
    { symptomsReviewForm : SymptomsReviewForm
    , vitalsForm : VitalsForm
    , activeTask : Maybe DangerSignsTask
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { symptomsReviewForm = emptySymptomsReviewForm
    , vitalsForm = emptyVitalsForm
    , activeTask = Nothing
    }


type alias SymptomsReviewForm =
    { symptoms : Maybe (List WellChildSymptom)
    }


emptySymptomsReviewForm : SymptomsReviewForm
emptySymptomsReviewForm =
    SymptomsReviewForm Nothing


type alias NutritionAssessmentData =
    { heightForm : HeightForm
    , headCircumferenceForm : HeadCircumferenceForm
    , muacForm : MuacForm
    , nutritionForm : NutritionForm
    , weightForm : WeightForm
    , activeTask : Maybe NutritionAssessmentTask
    }


emptyNutritionAssessmentData : NutritionAssessmentData
emptyNutritionAssessmentData =
    { heightForm = emptyHeightForm
    , headCircumferenceForm = emptyHeadCircumferenceForm
    , muacForm = emptyMuacForm
    , nutritionForm = emptyNutritionForm
    , weightForm = emptyWeightForm
    , activeTask = Nothing
    }


type alias HeadCircumferenceForm =
    { headCircumference : Maybe Float
    , headCircumferenceDirty : Bool
    , measurementNotTaken : Maybe Bool
    }


emptyHeadCircumferenceForm : HeadCircumferenceForm
emptyHeadCircumferenceForm =
    HeadCircumferenceForm Nothing False Nothing


type alias ImmunisationData =
    { bcgForm : WellChildVaccinationForm
    , dtpForm : WellChildVaccinationForm
    , dtpStandaloneForm : WellChildVaccinationForm
    , hpvForm : WellChildVaccinationForm
    , ipvForm : WellChildVaccinationForm
    , mrForm : WellChildVaccinationForm
    , opvForm : WellChildVaccinationForm
    , pcv13Form : WellChildVaccinationForm
    , rotarixForm : WellChildVaccinationForm
    , activeTask : Maybe ImmunisationTask
    }


type alias WellChildVaccinationForm =
    VaccinationForm Msg


emptyImmunisationData : ImmunisationData
emptyImmunisationData =
    { bcgForm = emptyVaccinationForm
    , dtpForm = emptyVaccinationForm
    , dtpStandaloneForm = emptyVaccinationForm
    , hpvForm = emptyVaccinationForm
    , ipvForm = emptyVaccinationForm
    , mrForm = emptyVaccinationForm
    , opvForm = emptyVaccinationForm
    , pcv13Form = emptyVaccinationForm
    , rotarixForm = emptyVaccinationForm
    , activeTask = Nothing
    }


type alias WellChildECDForm =
    { followMothersEyes : Maybe Bool
    , moveArmsAndLegs : Maybe Bool
    , raiseHandsUp : Maybe Bool
    , smile : Maybe Bool
    , rollSideways : Maybe Bool
    , bringHandsToMouth : Maybe Bool
    , holdHeadWithoutSupport : Maybe Bool
    , holdAndShakeToys : Maybe Bool
    , reactToSuddenSounds : Maybe Bool
    , useConsonantSounds : Maybe Bool
    , respondToSoundWithSound : Maybe Bool
    , turnHeadWhenCalled : Maybe Bool
    , sitWithoutSupport : Maybe Bool
    , smileBack : Maybe Bool
    , rollTummyToBack : Maybe Bool
    , reachForToys : Maybe Bool
    , useSimpleGestures : Maybe Bool
    , standOnTheirOwn : Maybe Bool
    , copyDuringPlay : Maybe Bool
    , sayMamaDada : Maybe Bool
    , canHoldSmallObjects : Maybe Bool
    , looksWhenPointedAt : Maybe Bool
    , useSingleWords : Maybe Bool
    , walkWithoutHelp : Maybe Bool
    , playPretend : Maybe Bool
    , pointToThingsOfInterest : Maybe Bool
    , useShortPhrases : Maybe Bool
    , interestedInOtherChildren : Maybe Bool
    , followSimlpeInstructions : Maybe Bool
    , kickBall : Maybe Bool
    , pointAtNamedObjects : Maybe Bool
    , dressThemselves : Maybe Bool
    , washHandsGoToToiled : Maybe Bool
    , knowsColorsAndNumbers : Maybe Bool
    , useMediumPhrases : Maybe Bool
    , playMakeBelieve : Maybe Bool
    , followThreeStepInstructions : Maybe Bool
    , standOnOneFootFiveSeconds : Maybe Bool
    , useLongPhrases : Maybe Bool
    , shareWithOtherChildren : Maybe Bool
    , countToTen : Maybe Bool
    }


emptyWellChildECDForm : WellChildECDForm
emptyWellChildECDForm =
    { followMothersEyes = Nothing
    , moveArmsAndLegs = Nothing
    , raiseHandsUp = Nothing
    , smile = Nothing
    , rollSideways = Nothing
    , bringHandsToMouth = Nothing
    , holdHeadWithoutSupport = Nothing
    , holdAndShakeToys = Nothing
    , reactToSuddenSounds = Nothing
    , useConsonantSounds = Nothing
    , respondToSoundWithSound = Nothing
    , turnHeadWhenCalled = Nothing
    , sitWithoutSupport = Nothing
    , smileBack = Nothing
    , rollTummyToBack = Nothing
    , reachForToys = Nothing
    , useSimpleGestures = Nothing
    , standOnTheirOwn = Nothing
    , copyDuringPlay = Nothing
    , sayMamaDada = Nothing
    , canHoldSmallObjects = Nothing
    , looksWhenPointedAt = Nothing
    , useSingleWords = Nothing
    , walkWithoutHelp = Nothing
    , playPretend = Nothing
    , pointToThingsOfInterest = Nothing
    , useShortPhrases = Nothing
    , interestedInOtherChildren = Nothing
    , followSimlpeInstructions = Nothing
    , kickBall = Nothing
    , pointAtNamedObjects = Nothing
    , dressThemselves = Nothing
    , washHandsGoToToiled = Nothing
    , knowsColorsAndNumbers = Nothing
    , useMediumPhrases = Nothing
    , playMakeBelieve = Nothing
    , followThreeStepInstructions = Nothing
    , standOnOneFootFiveSeconds = Nothing
    , useLongPhrases = Nothing
    , shareWithOtherChildren = Nothing
    , countToTen = Nothing
    }


type alias MedicationData =
    { albendazoleForm : MedicationAdministrationForm
    , mebendezoleForm : MedicationAdministrationForm
    , vitaminAForm : MedicationAdministrationForm
    , activeTask : Maybe MedicationTask
    }


emptyMedicationData : MedicationData
emptyMedicationData =
    { albendazoleForm = emptyMedicationAdministrationForm
    , mebendezoleForm = emptyMedicationAdministrationForm
    , vitaminAForm = emptyMedicationAdministrationForm
    , activeTask = Nothing
    }


medicationTasks : List MedicationTask
medicationTasks =
    [ TaskAlbendazole, TaskMebendezole, TaskVitaminA ]


type alias NextStepsData =
    { contributingFactorsForm : ContributingFactorsForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    , followUpForm : NutritionFollowUpForm
    , nextVisitForm : NextVisitForm
    , activeTask : Maybe Pages.WellChild.Activity.Types.NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { contributingFactorsForm = emptyContributingFactorsForm
    , healthEducationForm = emptyHealthEducationForm
    , followUpForm = emptyNutritionFollowUpForm
    , sendToHCForm = emptySendToHCForm
    , nextVisitForm = emptyNextVisitForm
    , activeTask = Nothing
    }


type alias NextVisitForm =
    { immunisationDate : Maybe NominalDate
    , asapImmunisationDate : Maybe NominalDate
    , pediatricVisitDate : Maybe NominalDate
    , resolutionDate : Maybe NominalDate
    }


emptyNextVisitForm : NextVisitForm
emptyNextVisitForm =
    NextVisitForm Nothing Nothing Nothing Nothing


type alias HomeVisitData =
    { feedingForm : NutritionFeedingForm
    , hygieneForm : NutritionHygieneForm
    , foodSecurityForm : NutritionFoodSecurityForm
    , caringForm : NutritionCaringForm
    , activeTask : Maybe Pages.WellChild.Activity.Types.HomeVisitTask
    }


emptyHomeVisitData : HomeVisitData
emptyHomeVisitData =
    { feedingForm = emptyNutritionFeedingForm
    , hygieneForm = emptyNutritionHygieneForm
    , foodSecurityForm = emptyNutritionFoodSecurityForm
    , caringForm = emptyNutritionCaringForm
    , activeTask = Nothing
    }
