module Pages.WellChildActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetWarningPopupState (List NutritionAssesment)
    | NoOp
    | SetECDBoolInput (Bool -> WellChildECDForm -> WellChildECDForm) Bool
    | SaveECD PersonId (Maybe ( WellChildECDId, WellChildECD ))
      -- NUTRITION ASSESMENT
    | SetActiveNutritionAssesmentTask NutritionAssesmentTask
    | SetHeight String
    | SaveHeight PersonId (Maybe ( WellChildHeightId, WellChildHeight )) (Maybe NutritionAssesmentTask)
    | SetHeadCircumference String
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


type alias Model =
    { ecdForm : WellChildECDForm
    , nutritionAssessmentData : NutritionAssessmentData
    , warningPopupState : List NutritionAssesment
    }


emptyModel : Model
emptyModel =
    { ecdForm = emptyWellChildECDForm
    , nutritionAssessmentData = emptyNutritionAssessmentData
    , warningPopupState = []
    }


type alias WellChildECDForm =
    { respontToSoundWithSound : Maybe Bool
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
    { respontToSoundWithSound = Nothing
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
