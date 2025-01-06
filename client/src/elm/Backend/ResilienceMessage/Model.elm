module Backend.ResilienceMessage.Model exposing (..)

import Backend.Entities exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Time


type alias ResilienceMessage =
    { category : ResilienceCategory
    , order : ResilienceMessageOrder
    , displayDay : Int
    , timeRead : Maybe Time.Posix
    , nextReminder : Maybe Time.Posix
    , isFavorite : Bool
    }


type ResilienceCategory
    = ResilienceCategoryIntroduction
    | ResilienceCategoryGrowth
    | ResilienceCategoryStressManagement
    | ResilienceCategoryMindfulness
    | ResilienceCategoryConnecting
    | ResilienceCategorySelfCare
    | ResilienceCategoryEndOfPeriod


type ResilienceMessageOrder
    = ResilienceMessage1
    | ResilienceMessage2
    | ResilienceMessage3
    | ResilienceMessage4
    | ResilienceMessage5
    | ResilienceMessage6
    | ResilienceMessage7
    | ResilienceMessage8
    | ResilienceMessage9
    | ResilienceMessage10
    | ResilienceMessage11
    | ResilienceMessage12
    | ResilienceMessage13
    | ResilienceMessage14


type alias Model =
    { updateMessage : WebData () }


emptyModel : Model
emptyModel =
    { updateMessage = NotAsked }
