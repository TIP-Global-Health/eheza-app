module Pages.WellChildActivity.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetWarningPopupState (List NutritionAssesment)
    | NoOp
      -- PREGNANCY SUMMARY
    | SetExpectedDateConcluded Date
    | ToggleExpectedDateConcluded
    | SetDateConcluded Date
    | ToggleDateConcluded
    | SetApgarsOneMinute String
    | SetApgarsFiveMinutes String
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
    | SetActiveNutritionAssesmentTask NutritionAssesmentTask
    | SetHeight String
    | SaveHeight PersonId (Maybe ( WellChildHeightId, WellChildHeight )) (Maybe NutritionAssesmentTask)
    | SetHeadCircumference String
    | ToggleHeadCircumferenceNotTaken
    | SaveHeadCircumference PersonId (Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference )) (Maybe NutritionAssesmentTask)
    | SetMuac String
    | SaveMuac PersonId (Maybe ( WellChildMuacId, WellChildMuac )) (Maybe NutritionAssesmentTask)
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( WellChildNutritionId, WellChildNutrition )) (Maybe NutritionAssesmentTask)
    | DropZoneComplete DropZoneFile
    | SavePhoto PersonId (Maybe WellChildPhotoId) PhotoUrl (Maybe NutritionAssesmentTask)
    | SetWeight String
    | SaveWeight PersonId (Maybe ( WellChildWeightId, WellChildWeight )) (Maybe NutritionAssesmentTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SaveSendToHC PersonId (Maybe ( WellChildSendToHCId, WellChildSendToHC )) (Maybe NutritionAssesmentTask)
    | SetProvidedEducationForDiagnosis Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SaveHealthEducation PersonId (Maybe ( WellChildHealthEducationId, WellChildHealthEducation )) (Maybe NutritionAssesmentTask)
    | SetContributingFactorsSign ContributingFactorsSign
    | SaveContributingFactors PersonId (Maybe ( WellChildContributingFactorsId, WellChildContributingFactors )) (Maybe NutritionAssesmentTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe ( WellChildFollowUpId, WellChildFollowUp )) (EverySet NutritionAssesment) (Maybe NutritionAssesmentTask)
      -- IMMUNISATION
    | SetImmunisationBoolInput (Bool -> ImmunisationForm -> ImmunisationForm) Bool
    | SetImmunisationAdministrationNoteInput (AdministrationNote -> ImmunisationForm -> ImmunisationForm) AdministrationNote
    | SetImmunisationDateInput (Date -> ImmunisationForm -> ImmunisationForm) Date
    | ToggleImmunisationDateSelectorInput (ImmunisationForm -> ImmunisationForm)
    | SaveImmunisation PersonId (Maybe ( WellChildImmunisationId, WellChildImmunisation ))
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


type alias Model =
    { pregnancySummaryForm : PregnancySummaryForm
    , dangerSignsData : DangerSignsData
    , nutritionAssessmentData : NutritionAssessmentData
    , immunisationForm : ImmunisationForm
    , ecdForm : WellChildECDForm
    , medicationData : MedicationData
    , warningPopupState : List NutritionAssesment
    }


emptyModel : Model
emptyModel =
    { pregnancySummaryForm = emptyPregnancySummaryForm
    , dangerSignsData = emptyDangerSignsData
    , nutritionAssessmentData = emptyNutritionAssessmentData
    , immunisationForm = emptyImmunisationForm
    , ecdForm = emptyWellChildECDForm
    , medicationData = emptyMedicationData
    , warningPopupState = []
    }


type alias PregnancySummaryForm =
    { expectedDateConcluded : Maybe Date
    , isExpectedDateConcludedSelectorOpen : Bool
    , dateConcluded : Maybe Date
    , isDateConcludedSelectorOpen : Bool
    , apgarsOneMinute : Maybe Int
    , apgarsFiveMinutes : Maybe Int
    , deliveryComplicationsPresent : Maybe Bool
    , deliveryComplications : Maybe (List DeliveryComplication)
    }


emptyPregnancySummaryForm : PregnancySummaryForm
emptyPregnancySummaryForm =
    { expectedDateConcluded = Nothing
    , isExpectedDateConcludedSelectorOpen = False
    , dateConcluded = Nothing
    , isDateConcludedSelectorOpen = False
    , apgarsOneMinute = Nothing
    , apgarsFiveMinutes = Nothing
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
    , photoForm : PhotoForm
    , weightForm : WeightForm
    , contributingFactorsForm : ContributingFactorsForm
    , healthEducationForm : HealthEducationForm
    , followUpForm : FollowUpForm
    , sendToHCForm : SendToHCForm
    , activeTask : Maybe NutritionAssesmentTask
    }


emptyNutritionAssessmentData : NutritionAssessmentData
emptyNutritionAssessmentData =
    { heightForm = emptyHeightForm
    , headCircumferenceForm = emptyHeadCircumferenceForm
    , muacForm = emptyMuacForm
    , nutritionForm = emptyNutritionForm
    , photoForm = emptyPhotoForm
    , weightForm = emptyWeightForm
    , contributingFactorsForm = emptyContributingFactorsForm
    , healthEducationForm = emptyHealthEducationForm
    , followUpForm = emptyFollowUpForm
    , sendToHCForm = emptySendToHCForm
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


type NutritionAssesmentTask
    = TaskHeight
    | TaskHeadCircumference
    | TaskMuac
    | TaskNutrition
    | TaskPhoto
    | TaskWeight
    | TaskContributingFactors
    | TaskHealthEducation
    | TaskFollowUp
    | TaskSendToHC


allNutritionAssesmentTasks : List NutritionAssesmentTask
allNutritionAssesmentTasks =
    [ TaskHeight
    , TaskHeadCircumference
    , TaskMuac
    , TaskNutrition
    , TaskPhoto
    , TaskWeight
    , TaskContributingFactors
    , TaskHealthEducation
    , TaskFollowUp
    , TaskSendToHC
    ]


type alias ImmunisationForm =
    { suggestedVaccines : Dict VaccineType VaccineDose
    , bcgVaccinationGiven : Maybe Bool
    , opvVaccinationGiven : Maybe Bool
    , dtpVaccinationGiven : Maybe Bool
    , pcv13VaccinationGiven : Maybe Bool
    , rotarixVaccinationGiven : Maybe Bool
    , ipvVaccinationGiven : Maybe Bool
    , mrVaccinationGiven : Maybe Bool
    , hpvVaccinationGiven : Maybe Bool
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
    , bcgVaccinationGiven = Nothing
    , opvVaccinationGiven = Nothing
    , dtpVaccinationGiven = Nothing
    , pcv13VaccinationGiven = Nothing
    , rotarixVaccinationGiven = Nothing
    , ipvVaccinationGiven = Nothing
    , mrVaccinationGiven = Nothing
    , hpvVaccinationGiven = Nothing
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
    { respondToSoundWithSound : Maybe Bool
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
    { respondToSoundWithSound = Nothing
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


allMedicationTasks : List MedicationTask
allMedicationTasks =
    [ TaskAlbendazole, TaskMebendezole, TaskVitaminA ]
