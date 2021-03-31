module Pages.HomeVisitActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetFeedingBoolInput (Bool -> FeedingForm -> FeedingForm) Bool
    | SetSachetsPerDay String


type alias Model =
    { feedingForm : FeedingForm
    }


emptyModel : Model
emptyModel =
    { feedingForm = emptyFeedingForm
    }


type alias FeedingForm =
    { receiveSupplement : Maybe Bool
    , rationPresentAtHome : Maybe Bool
    , enoughTillNextSession : Maybe Bool
    , supplementShared : Maybe Bool
    , encouragedToEat : Maybe Bool
    , refusingToEat : Maybe Bool
    , feedingSignBreastfeeding : Maybe Bool
    , cleanWaterAvailable : Maybe Bool
    , eatenWithWater : Maybe Bool
    , sachetsPerDay : Maybe Float
    }


emptyFeedingForm : FeedingForm
emptyFeedingForm =
    { receiveSupplement = Nothing
    , rationPresentAtHome = Nothing
    , enoughTillNextSession = Nothing
    , supplementShared = Nothing
    , encouragedToEat = Nothing
    , refusingToEat = Nothing
    , feedingSignBreastfeeding = Nothing
    , cleanWaterAvailable = Nothing
    , eatenWithWater = Nothing
    , sachetsPerDay = Nothing
    }
