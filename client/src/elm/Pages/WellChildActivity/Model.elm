module Pages.WellChildActivity.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)
import Pages.WellChildEncounter.Model exposing (VaccinationProgressDict)


type Msg
    = SetActivePage Page
    | SetWarningPopupState (Maybe WarningPopupType)
    | NoOp
      -- PREGNANCY SUMMARY
    | SetExpectedDateConcluded Date
    | ToggleExpectedDateConcluded
    | SetDeliveryComplicationsPresent Bool
    | SetDeliveryComplication DeliveryComplication
    | SavePregnancySummary PersonId (Maybe ( WellChildPregnancySummaryId, WellChildPregnancySummary ))
      -- DANGER SIGNS
    | SetActiveDangerSignsTask DangerSignsTask
    | SetSymptom WellChildSymptom
    | SaveSymptomsReview PersonId (Maybe ( WellChildSymptomsReviewId, WellChildSymptomsReview )) (Maybe DangerSignsTask)
    | SetVitalsResporatoryRate String
    | SetVitalsBodyTemperature String
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
      -- VACCINATION HISTORY
    | SetCatchUpRequired Bool
    | SetVaccinationHistoryBoolInput VaccineType VaccineDose Bool
    | SetVaccinationHistoryDateInput VaccineType VaccineDose Date
    | ToggleVaccinationHistoryDateSelectorInput VaccineType VaccineDose
    | SaveVaccinationHistory PersonId (Dict VaccineType (EverySet VaccineDose)) (Maybe ( WellChildVaccinationHistoryId, WellChildVaccinationHistory ))
      -- IMMUNISATION
    | SetActiveImmunisationTask ImmunisationTask
    | SetVaccinationFormViewMode VaccineType VaccinationFormViewMode
    | SetAllowPreviousVaccinesUpdate VaccineType Bool
    | SetWillReceiveVaccineToday VaccineType VaccineDose Bool
    | SetAdministrationNote VaccineType AdministrationNote
    | ToggleDateSelectorInput VaccineType
    | SetVaccinationUpdateDate VaccineType NominalDate
    | SaveVaccinationUpdateDate VaccineType VaccineDose
    | DeleteVaccinationUpdateDate VaccineType VaccineDose NominalDate
    | SaveBCGImmunisation PersonId (Maybe ( WellChildBCGImmunisationId, WellChildBCGImmunisation )) (Maybe ImmunisationTask)
    | SaveDTPImmunisation PersonId (Maybe ( WellChildDTPImmunisationId, WellChildDTPImmunisation )) (Maybe ImmunisationTask)
    | SaveHPVImmunisation PersonId (Maybe ( WellChildHPVImmunisationId, WellChildHPVImmunisation )) (Maybe ImmunisationTask)
    | SaveIPVImmunisation PersonId (Maybe ( WellChildIPVImmunisationId, WellChildIPVImmunisation )) (Maybe ImmunisationTask)
    | SaveMRImmunisation PersonId (Maybe ( WellChildMRImmunisationId, WellChildMRImmunisation )) (Maybe ImmunisationTask)
    | SaveOPVImmunisation PersonId (Maybe ( WellChildOPVImmunisationId, WellChildOPVImmunisation )) (Maybe ImmunisationTask)
    | SavePCV13Immunisation PersonId (Maybe ( WellChildPCV13ImmunisationId, WellChildPCV13Immunisation )) (Maybe ImmunisationTask)
    | SaveRotarixImmunisation PersonId (Maybe ( WellChildRotarixImmunisationId, WellChildRotarixImmunisation )) (Maybe ImmunisationTask)
      -- @todo remove
    | SetImmunisationBoolInput (Bool -> ImmunisationForm -> ImmunisationForm) Bool
    | SetImmunisationAdministrationNoteInput (AdministrationNote -> ImmunisationForm -> ImmunisationForm) AdministrationNote
    | SetImmunisationDateInput (Date -> ImmunisationForm -> ImmunisationForm) Date
    | ToggleImmunisationDateSelectorInput (ImmunisationForm -> ImmunisationForm)
    | SaveImmunisation PersonId (Dict VaccineType VaccineDose) (Maybe ( WellChildImmunisationId, WellChildImmunisation ))
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
    | SetActiveNextStepsTask NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetEnrollToNutritionProgram Bool
    | SetReferToNutritionProgram Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SaveSendToHC PersonId (Maybe ( WellChildSendToHCId, WellChildSendToHC )) (Maybe NextStepsTask)
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SaveHealthEducation PersonId (Maybe ( WellChildHealthEducationId, WellChildHealthEducation )) (Maybe NextStepsTask)
    | SetContributingFactorsSign ContributingFactorsSign
    | SaveContributingFactors PersonId (Maybe ( WellChildContributingFactorsId, WellChildContributingFactors )) (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( WellChildFollowUpId, WellChildFollowUp )) (EverySet NutritionAssessment) (Maybe NextStepsTask)
    | SaveNextVisit PersonId (Maybe ( WellChildNextVisitId, WellChildNextVisit )) (Maybe NominalDate) (Maybe NominalDate) (Maybe NextStepsTask)
      -- PHOTO
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe WellChildPhotoId) PhotoUrl


type alias Model =
    { pregnancySummaryForm : PregnancySummaryForm
    , dangerSignsData : DangerSignsData
    , nutritionAssessmentData : NutritionAssessmentData
    , vaccinationHistoryForm : VaccinationHistoryForm
    , immunisationData : ImmunisationData

    -- @todo: remove
    , immunisationForm : ImmunisationForm
    , ecdForm : WellChildECDForm
    , medicationData : MedicationData
    , nextStepsData : NextStepsData
    , photoForm : PhotoForm
    , warningPopupState : Maybe WarningPopupType
    }


emptyModel : Model
emptyModel =
    { pregnancySummaryForm = emptyPregnancySummaryForm
    , dangerSignsData = emptyDangerSignsData
    , nutritionAssessmentData = emptyNutritionAssessmentData
    , vaccinationHistoryForm = emptyVaccinationHistoryForm
    , immunisationData = emptyImmunisationData

    -- @todo: remove
    , immunisationForm = emptyImmunisationForm
    , ecdForm = emptyWellChildECDForm
    , medicationData = emptyMedicationData
    , nextStepsData = emptyNextStepsData
    , photoForm = emptyPhotoForm
    , warningPopupState = Nothing
    }


type WarningPopupType
    = PopupNutritionAssessment (List NutritionAssessment)
    | PopupMacrocephaly PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | PopupMicrocephaly PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssessmentTask)
    | PopupVaccinationHistory VaccinationProgressDict


type VaccinationStatus
    = StatusBehind
    | StatusDone
    | StatusUpToDate


type alias PregnancySummaryForm =
    { expectedDateConcluded : Maybe Date
    , isExpectedDateConcludedSelectorOpen : Bool
    , deliveryComplicationsPresent : Maybe Bool
    , deliveryComplications : Maybe (List DeliveryComplication)
    }


emptyPregnancySummaryForm : PregnancySummaryForm
emptyPregnancySummaryForm =
    { expectedDateConcluded = Nothing
    , isExpectedDateConcludedSelectorOpen = False
    , deliveryComplicationsPresent = Nothing
    , deliveryComplications = Nothing
    }


type alias DangerSignsData =
    { symptomsReviewForm : SymptomsReviewForm
    , vitalsForm : BasicVitalsForm
    , activeTask : Maybe DangerSignsTask
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { symptomsReviewForm = emptySymptomsReviewForm
    , vitalsForm = emptyBasicVitalsForm
    , activeTask = Nothing
    }


type alias SymptomsReviewForm =
    { symptoms : Maybe (List WellChildSymptom)
    }


emptySymptomsReviewForm : SymptomsReviewForm
emptySymptomsReviewForm =
    SymptomsReviewForm Nothing


type DangerSignsTask
    = TaskSymptomsReview
    | TaskVitals


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


type NutritionAssessmentTask
    = TaskHeight
    | TaskHeadCircumference
    | TaskMuac
    | TaskNutrition
    | TaskWeight


type alias VaccinationHistoryForm =
    { catchUpRequired : Maybe Bool
    , suggestedVaccines : Dict VaccineType (EverySet VaccineDose)
    , administeredVaccines : Dict VaccineType (Dict VaccineDose (Maybe Bool))
    , administeredVaccinesDirty : Bool
    , vaccinationDates : Dict VaccineType (Dict VaccineDose (Maybe NominalDate))
    , vaccinationDatesDirty : Bool
    , dateSelectorsState : Dict ( VaccineType, VaccineDose ) Bool
    }


emptyVaccinationHistoryForm : VaccinationHistoryForm
emptyVaccinationHistoryForm =
    { catchUpRequired = Nothing
    , suggestedVaccines = Dict.empty
    , administeredVaccines = Dict.empty
    , administeredVaccinesDirty = False
    , vaccinationDates = Dict.empty
    , vaccinationDatesDirty = False
    , dateSelectorsState = Dict.empty
    }


type alias ImmunisationData =
    { bcgForm : VaccinationForm
    , dtpForm : VaccinationForm
    , hpvForm : VaccinationForm
    , ipvForm : VaccinationForm
    , mrForm : VaccinationForm
    , opvForm : VaccinationForm
    , pcv13Form : VaccinationForm
    , rotarixForm : VaccinationForm
    , activeTask : Maybe ImmunisationTask
    }


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


type alias VaccinationForm =
    { administeredDoses : Maybe (EverySet VaccineDose)
    , administrationDates : Maybe (EverySet NominalDate)

    -- This is the note for suggesed dose for encounter.
    -- There are situations where there will be no suggested dose,
    -- due to ability to uodate previous doses.
    -- In this case, we'll set 'AdministeredPreviously' value.
    , administrationNote : Maybe AdministrationNote

    -- Form inner functionality inputs
    , viewMode : VaccinationFormViewMode
    , allowPreviousVaccinesUpdate : Maybe Bool
    , willReceiveVaccineToday : Maybe Bool
    , vaccinationUpdateDate : Maybe NominalDate
    , dateSelectorOpen : Bool
    }


emptyVaccinationForm : VaccinationForm
emptyVaccinationForm =
    { administeredDoses = Nothing
    , administrationDates = Nothing
    , administrationNote = Nothing
    , viewMode = ViewModeInitial
    , allowPreviousVaccinesUpdate = Nothing
    , willReceiveVaccineToday = Nothing
    , vaccinationUpdateDate = Nothing
    , dateSelectorOpen = False
    }


type VaccinationFormViewMode
    = ViewModeInitial
    | ViewModeVaccinationUpdate VaccineDose


type ImmunisationTask
    = TaskBCG
    | TaskDTP
    | TaskHPV
    | TaskIPV
    | TaskMR
    | TaskOPV
    | TaskPCV13
    | TaskRotarix



-- @todo: remove


type alias ImmunisationForm =
    { suggestedVaccines : Dict VaccineType VaccineDose
    , bcgVaccinationAdministered : Maybe Bool
    , opvVaccinationAdministered : Maybe Bool
    , dtpVaccinationAdministered : Maybe Bool
    , pcv13VaccinationAdministered : Maybe Bool
    , rotarixVaccinationAdministered : Maybe Bool
    , ipvVaccinationAdministered : Maybe Bool
    , mrVaccinationAdministered : Maybe Bool
    , hpvVaccinationAdministered : Maybe Bool
    , bcgVaccinationNote : Maybe AdministrationNote
    , opvVaccinationNote : Maybe AdministrationNote
    , dtpVaccinationNote : Maybe AdministrationNote
    , pcv13VaccinationNote : Maybe AdministrationNote
    , rotarixVaccinationNote : Maybe AdministrationNote
    , ipvVaccinationNote : Maybe AdministrationNote
    , mrVaccinationNote : Maybe AdministrationNote
    , hpvVaccinationNote : Maybe AdministrationNote
    , bcgVaccinationDate : Maybe NominalDate
    , opvVaccinationDate : Maybe NominalDate
    , dtpVaccinationDate : Maybe NominalDate
    , pcv13VaccinationDate : Maybe NominalDate
    , rotarixVaccinationDate : Maybe NominalDate
    , ipvVaccinationDate : Maybe NominalDate
    , mrVaccinationDate : Maybe NominalDate
    , hpvVaccinationDate : Maybe NominalDate
    , bcgVaccinationDateSelectorOpen : Bool
    , opvVaccinationDateSelectorOpen : Bool
    , dtpVaccinationDateSelectorOpen : Bool
    , pcv13VaccinationDateSelectorOpen : Bool
    , rotarixVaccinationDateSelectorOpen : Bool
    , ipvVaccinationDateSelectorOpen : Bool
    , mrVaccinationDateSelectorOpen : Bool
    , hpvVaccinationDateSelectorOpen : Bool
    }


emptyImmunisationForm : ImmunisationForm
emptyImmunisationForm =
    { suggestedVaccines = Dict.empty
    , bcgVaccinationAdministered = Nothing
    , opvVaccinationAdministered = Nothing
    , dtpVaccinationAdministered = Nothing
    , pcv13VaccinationAdministered = Nothing
    , rotarixVaccinationAdministered = Nothing
    , ipvVaccinationAdministered = Nothing
    , mrVaccinationAdministered = Nothing
    , hpvVaccinationAdministered = Nothing
    , bcgVaccinationNote = Nothing
    , opvVaccinationNote = Nothing
    , dtpVaccinationNote = Nothing
    , pcv13VaccinationNote = Nothing
    , rotarixVaccinationNote = Nothing
    , ipvVaccinationNote = Nothing
    , mrVaccinationNote = Nothing
    , hpvVaccinationNote = Nothing
    , bcgVaccinationDate = Nothing
    , opvVaccinationDate = Nothing
    , dtpVaccinationDate = Nothing
    , pcv13VaccinationDate = Nothing
    , rotarixVaccinationDate = Nothing
    , ipvVaccinationDate = Nothing
    , mrVaccinationDate = Nothing
    , hpvVaccinationDate = Nothing
    , bcgVaccinationDateSelectorOpen = False
    , opvVaccinationDateSelectorOpen = False
    , dtpVaccinationDateSelectorOpen = False
    , pcv13VaccinationDateSelectorOpen = False
    , rotarixVaccinationDateSelectorOpen = False
    , ipvVaccinationDateSelectorOpen = False
    , mrVaccinationDateSelectorOpen = False
    , hpvVaccinationDateSelectorOpen = False
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


type MedicationTask
    = TaskAlbendazole
    | TaskMebendezole
    | TaskVitaminA


medicationTasks : List MedicationTask
medicationTasks =
    [ TaskAlbendazole, TaskMebendezole, TaskVitaminA ]


type alias NextStepsData =
    { contributingFactorsForm : ContributingFactorsForm
    , healthEducationForm : HealthEducationForm
    , sendToHCForm : SendToHCForm
    , followUpForm : FollowUpForm
    , nextVisitForm : NextVisitForm
    , activeTask : Maybe NextStepsTask
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


type NextStepsTask
    = TaskContributingFactors
    | TaskHealthEducation
    | TaskSendToHC
    | TaskFollowUp
    | TaskNextVisit
