module Pages.HomeVisitActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetFeedingBoolInput (Bool -> NutritionFeedingForm -> NutritionFeedingForm) Bool
    | SetNutritionSupplementType NutritionSupplementType
    | SetSachetsPerDay String
    | SaveFeeding PersonId (Maybe ( NutritionFeedingId, NutritionFeeding ))
    | SetHygieneBoolInput (Bool -> NutritionHygieneForm -> NutritionHygieneForm) Bool
    | SetMainWaterSource MainWaterSource
    | SaveHygiene PersonId (Maybe ( NutritionHygieneId, NutritionHygiene ))
    | SetFoodSecurityBoolInput (Bool -> NutritionFoodSecurityForm -> NutritionFoodSecurityForm) Bool
    | SetMainIncomeSource MainIncomeSource
    | SetWaterPreparation WaterPreparation
    | SaveFoodSecurity PersonId (Maybe ( NutritionFoodSecurityId, NutritionFoodSecurity ))
    | SetParentsAliveAndHealthy Bool
    | SetChildClean Bool
    | SetNutritionCaringOption CaringOption
    | SaveNutritionCaring PersonId (Maybe ( NutritionCaringId, NutritionCaring ))


type alias Model =
    { feedingForm : NutritionFeedingForm
    , hygieneForm : NutritionHygieneForm
    , foodSecurityForm : NutritionFoodSecurityForm
    , caringForm : NutritionCaringForm
    }


emptyModel : Model
emptyModel =
    { feedingForm = emptyNutritionFeedingForm
    , hygieneForm = emptyNutritionHygieneForm
    , foodSecurityForm = emptyNutritionFoodSecurityForm
    , caringForm = emptyNutritionCaringForm
    }


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
    , waterPreparation : Maybe WaterPreparation
    }


emptyNutritionHygieneForm : NutritionHygieneForm
emptyNutritionHygieneForm =
    { soapInTheHouse = Nothing
    , washHandsBeforeFeeding = Nothing
    , foodCovered = Nothing
    , mainWaterSource = Nothing
    , waterPreparation = Nothing
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
