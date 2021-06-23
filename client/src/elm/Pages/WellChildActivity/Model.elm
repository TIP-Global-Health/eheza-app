module Pages.WellChildActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Pages.NutritionActivity.Model
    exposing
        ( HeightForm
        , MuacForm
        , NutritionForm
        , PhotoForm
        , WeightForm
        , emptyHeightForm
        , emptyMuacForm
        , emptyNutritionForm
        , emptyPhotoForm
        , emptyWeightForm
        )
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetECDBoolInput (Bool -> WellChildECDForm -> WellChildECDForm) Bool
    | SaveECD PersonId (Maybe ( WellChildECDId, WellChildECD ))


type alias Model =
    { ecdForm : WellChildECDForm
    , nutritionAssesmentData : NutritionAssesmentData
    }


emptyModel : Model
emptyModel =
    { ecdForm = emptyWellChildECDForm
    , nutritionAssesmentData = emptyNutritionAssesmentData
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


type alias NutritionAssesmentData =
    { heightForm : HeightForm
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


emptyNutritionAssesmentData : NutritionAssesmentData
emptyNutritionAssesmentData =
    { heightForm = emptyHeightForm
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


type NutritionAssesmentTask
    = TaskHeight
    | TaskMuac
    | TaskNutrition
    | TaskPhoto
    | TaskWeight
    | TaskContributingFactors
    | TaskSendToHC
    | TaskHealthEducation
    | TaskFollowUp
