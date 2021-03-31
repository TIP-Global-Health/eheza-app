module Pages.HomeVisitActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetFeedingBoolInput (Bool -> NutritionFeedingForm -> NutritionFeedingForm) Bool
    | SetSachetsPerDay String


type alias Model =
    { feedingForm : NutritionFeedingForm
    }


emptyModel : Model
emptyModel =
    { feedingForm = emptyNutritionFeedingForm
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
