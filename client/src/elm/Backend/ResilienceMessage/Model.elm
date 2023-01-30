module Backend.ResilienceMessage.Model exposing (..)

import Backend.Entities exposing (..)


type alias ResilienceMessage =
    { nurse : NurseId
    , category : ResilienceCategory
    , order : ResilienceMessageOrder
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
