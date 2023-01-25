module Backend.ResilienceMessage.Model exposing (..)

import Backend.Entities exposing (..)
import Time


type alias ResilienceMessage =
    { nurse : NurseId
    , category : ResilienceCategory
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



-- @todo: remove ResilienceMessageType.


type ResilienceMessageType
    = ResilienceMessageIntroduction1
    | ResilienceMessageIntroduction2
    | ResilienceMessageIntroduction3
    | ResilienceMessageIntroduction4
    | ResilienceMessageIntroduction5
    | ResilienceMessageIntroduction6
    | ResilienceMessageIntroduction7
    | ResilienceMessageIntroduction8
    | ResilienceMessageGrowth1
    | ResilienceMessageGrowth2
    | ResilienceMessageGrowth3
    | ResilienceMessageGrowth4
    | ResilienceMessageStressManagement1
    | ResilienceMessageStressManagement2
    | ResilienceMessageStressManagement3
    | ResilienceMessageStressManagement4
    | ResilienceMessageStressManagement5
    | ResilienceMessageStressManagement6
    | ResilienceMessageStressManagement7
    | ResilienceMessageMindfulness1
    | ResilienceMessageMindfulness2
    | ResilienceMessageMindfulness3
    | ResilienceMessageMindfulness4
    | ResilienceMessageMindfulness5
    | ResilienceMessageMindfulness6
    | ResilienceMessageConnecting1
    | ResilienceMessageConnecting2
    | ResilienceMessageConnecting3
    | ResilienceMessageConnecting4
    | ResilienceMessageConnecting5
    | ResilienceMessageConnecting6
    | ResilienceMessageSelfCare1
    | ResilienceMessageSelfCare2
    | ResilienceMessageSelfCare3
    | ResilienceMessageEndOfFirstMonth
    | ResilienceMessageEndOfSecondMonth
