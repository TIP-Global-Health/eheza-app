module Backend.ResilienceMessage.Utils exposing (..)

import Backend.ResilienceMessage.Model exposing (..)


resilienceCategoryToString : ResilienceCategory -> String
resilienceCategoryToString value =
    case value of
        ResilienceCategoryIntroduction ->
            "introduction"

        ResilienceCategoryGrowth ->
            "growth"

        ResilienceCategoryStressManagement ->
            "stress-management"

        ResilienceCategoryMindfulness ->
            "mindfulness"

        ResilienceCategoryConnecting ->
            "connecting"

        ResilienceCategorySelfCare ->
            "self-care"

        ResilienceCategoryEndOfPeriod ->
            "end-of-period"


resilienceCategoryFromString : String -> Maybe ResilienceCategory
resilienceCategoryFromString value =
    case value of
        "introduction" ->
            Just ResilienceCategoryIntroduction

        "growth" ->
            Just ResilienceCategoryGrowth

        "stress-management" ->
            Just ResilienceCategoryStressManagement

        "mindfulness" ->
            Just ResilienceCategoryMindfulness

        "connecting" ->
            Just ResilienceCategoryConnecting

        "self-care" ->
            Just ResilienceCategorySelfCare

        "end-of-period" ->
            Just ResilienceCategoryEndOfPeriod

        _ ->
            Nothing


resilienceMessageOrderToString : ResilienceMessageOrder -> String
resilienceMessageOrderToString value =
    case value of
        ResilienceMessage1 ->
            "1"

        ResilienceMessage2 ->
            "2"

        ResilienceMessage3 ->
            "3"

        ResilienceMessage4 ->
            "4"

        ResilienceMessage5 ->
            "5"

        ResilienceMessage6 ->
            "6"

        ResilienceMessage7 ->
            "7"

        ResilienceMessage8 ->
            "8"


resilienceMessageOrderFromString : String -> Maybe ResilienceMessageOrder
resilienceMessageOrderFromString value =
    case value of
        "1" ->
            Just ResilienceMessage1

        "2" ->
            Just ResilienceMessage2

        "3" ->
            Just ResilienceMessage3

        "4" ->
            Just ResilienceMessage4

        "5" ->
            Just ResilienceMessage5

        "6" ->
            Just ResilienceMessage6

        "7" ->
            Just ResilienceMessage7

        "8" ->
            Just ResilienceMessage8

        _ ->
            Nothing
