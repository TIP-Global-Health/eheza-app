module Pages.WellChild.Activity.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)
import Pages.WellChild.Activity.Types exposing (..)
import Pages.WellChild.Encounter.Model exposing (VaccinationProgressDict)


type Msg
    = SetActivePage Page
    | SetWarningPopupState (Maybe WarningPopupType)
    | NoOp
      -- PREGNANCY SUMMARY
    | SetExpectedDateConcluded Date
    | SetExpectedDateConcludedSelectorState (Maybe (DateSelectorConfig Msg))
    | SetDeliveryComplicationsPresent Bool
    | SetDeliveryComplication DeliveryComplication
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
    | SaveNextVisit PersonId (Maybe ( WellChildNextVisitId, WellChildNextVisit )) (Maybe NominalDate) (Maybe NominalDate) (Maybe Pages.WellChild.Activity.Types.NextStepsTask)
      -- PHOTO
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe WellChildPhotoId) PhotoUrl
      -- NCDA
    | SetNCDABoolInput (Bool -> NCDAForm -> NCDAForm) Bool
    | SaveNCDA PersonId (Maybe ( WellChildNCDAId, WellChildNCDA ))


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
    }


emptyPregnancySummaryForm : PregnancySummaryForm
emptyPregnancySummaryForm =
    { expectedDateConcluded = Nothing
    , dateSelectorPopupState = Nothing
    , deliveryComplicationsPresent = Nothing
    , deliveryComplications = Nothing
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


type alias MedicationAdministrationForm =
    { medicationAdministered : Maybe Bool
    , reasonForNonAdministration : Maybe AdministrationNote
    }


emptyMedicationAdministrationForm : MedicationAdministrationForm
emptyMedicationAdministrationForm =
    MedicationAdministrationForm Nothing Nothing


medicationTasks : List MedicationTask
medicationTasks =
    [ TaskAlbendazole, TaskMebendezole, TaskVitaminA ]


type alias NextStepsData =
    { contributingFactorsForm : ContributingFactorsForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    , followUpForm : FollowUpForm
    , nextVisitForm : NextVisitForm
    , activeTask : Maybe Pages.WellChild.Activity.Types.NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { contributingFactorsForm = emptyContributingFactorsForm
    , healthEducationForm = emptyHealthEducationForm
    , followUpForm = emptyFollowUpForm
    , sendToHCForm = emptySendToHCForm
    , nextVisitForm = emptyNextVisitForm
    , activeTask = Nothing
    }


type alias NextVisitForm =
    { immunisationDate : Maybe NominalDate
    , pediatricVisitDate : Maybe NominalDate
    }


emptyNextVisitForm : NextVisitForm
emptyNextVisitForm =
    NextVisitForm Nothing Nothing
