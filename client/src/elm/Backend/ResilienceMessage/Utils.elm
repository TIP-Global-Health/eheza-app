module Backend.ResilienceMessage.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (NurseId, ResilienceMessageId)
import Backend.ResilienceMessage.Model exposing (..)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)


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

        ResilienceMessage9 ->
            "9"

        ResilienceMessage10 ->
            "10"

        ResilienceMessage11 ->
            "11"

        ResilienceMessage12 ->
            "12"

        ResilienceMessage13 ->
            "13"

        ResilienceMessage14 ->
            "14"

        ResilienceMessage15 ->
            "15"

        ResilienceMessage16 ->
            "16"

        ResilienceMessage17 ->
            "17"

        ResilienceMessage18 ->
            "18"

        ResilienceMessage19 ->
            "19"

        ResilienceMessage20 ->
            "20"

        ResilienceMessage21 ->
            "21"

        ResilienceMessage22 ->
            "22"

        ResilienceMessage23 ->
            "23"


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

        "9" ->
            Just ResilienceMessage9

        "10" ->
            Just ResilienceMessage10

        "11" ->
            Just ResilienceMessage11

        "12" ->
            Just ResilienceMessage12

        "13" ->
            Just ResilienceMessage13

        "14" ->
            Just ResilienceMessage14

        "15" ->
            Just ResilienceMessage15

        "16" ->
            Just ResilienceMessage16

        "17" ->
            Just ResilienceMessage17

        "18" ->
            Just ResilienceMessage18

        "19" ->
            Just ResilienceMessage19

        "20" ->
            Just ResilienceMessage20

        "21" ->
            Just ResilienceMessage21

        "22" ->
            Just ResilienceMessage22

        "23" ->
            Just ResilienceMessage23

        _ ->
            Nothing


generateEmptyMessagesByProgramStartDate : NominalDate -> NominalDate -> Dict ResilienceMessageId ResilienceMessage
generateEmptyMessagesByProgramStartDate currentDate programStartDate =
    Dict.filter
        (\_ message ->
            Date.compare (Date.add Days (message.displayDay - 1) programStartDate) currentDate == LT
        )
        emptyMessagesDict


emptyMessagesDict : Dict ResilienceMessageId ResilienceMessage
emptyMessagesDict =
    Dict.toList numberOfMessagesByCategory
        |> List.map
            (\( category, numberOfMessages ) ->
                List.range 1 numberOfMessages
                    |> List.filterMap
                        (String.fromInt
                            >> resilienceMessageOrderFromString
                            >> Maybe.andThen
                                (\order ->
                                    resolveDisplayDay category order
                                        |> Maybe.map
                                            (\displayDay ->
                                                ( generateResilienceMessageId category order
                                                , { category = category
                                                  , order = order
                                                  , displayDay = displayDay
                                                  , timeRead = Nothing
                                                  , nextReminder = Nothing
                                                  , isFavorite = False
                                                  }
                                                )
                                            )
                                )
                        )
            )
        |> List.concat
        |> Dict.fromList


generateResilienceMessageId : ResilienceCategory -> ResilienceMessageOrder -> ResilienceMessageId
generateResilienceMessageId category order =
    resilienceCategoryToString category ++ "-" ++ resilienceMessageOrderToString order


numberOfMessagesByCategory : Dict ResilienceCategory Int
numberOfMessagesByCategory =
    Dict.fromList
        [ ( ResilienceCategoryIntroduction, 8 )
        , ( ResilienceCategoryGrowth, 23 )
        , ( ResilienceCategoryStressManagement, 16 )
        , ( ResilienceCategoryMindfulness, 11 )
        , ( ResilienceCategoryConnecting, 18 )
        , ( ResilienceCategorySelfCare, 10 )
        , ( ResilienceCategoryEndOfPeriod, 7 )
        ]


resolveDisplayDay : ResilienceCategory -> ResilienceMessageOrder -> Maybe Int
resolveDisplayDay category order =
    case category of
        ResilienceCategoryIntroduction ->
            case order of
                ResilienceMessage1 ->
                    Just 0

                ResilienceMessage2 ->
                    Just 0

                ResilienceMessage3 ->
                    Just 0

                ResilienceMessage4 ->
                    Just 0

                ResilienceMessage5 ->
                    Just 2

                ResilienceMessage6 ->
                    Just 2

                ResilienceMessage7 ->
                    Just 2

                ResilienceMessage8 ->
                    Just 2

                _ ->
                    Nothing

        ResilienceCategoryGrowth ->
            case order of
                ResilienceMessage1 ->
                    Just 2

                ResilienceMessage2 ->
                    Just 28

                ResilienceMessage3 ->
                    Just 49

                ResilienceMessage4 ->
                    Just 61

                ResilienceMessage5 ->
                    Just 70

                ResilienceMessage6 ->
                    Just 96

                ResilienceMessage7 ->
                    Just 100

                ResilienceMessage8 ->
                    Just 105

                ResilienceMessage9 ->
                    Just 107

                ResilienceMessage10 ->
                    Just 123

                ResilienceMessage11 ->
                    Just 126

                ResilienceMessage12 ->
                    Just 140

                ResilienceMessage13 ->
                    Just 154

                ResilienceMessage14 ->
                    Just 156

                ResilienceMessage15 ->
                    Just 158

                ResilienceMessage16 ->
                    Just 161

                ResilienceMessage17 ->
                    Just 163

                ResilienceMessage18 ->
                    Just 170

                ResilienceMessage19 ->
                    Just 179

                ResilienceMessage20 ->
                    Just 184

                ResilienceMessage21 ->
                    Just 191

                ResilienceMessage22 ->
                    Just 196

                ResilienceMessage23 ->
                    Just 198

        ResilienceCategoryStressManagement ->
            case order of
                ResilienceMessage1 ->
                    Just 7

                ResilienceMessage2 ->
                    Just 9

                ResilienceMessage3 ->
                    Just 33

                ResilienceMessage4 ->
                    Just 44

                ResilienceMessage5 ->
                    Just 54

                ResilienceMessage6 ->
                    Just 63

                ResilienceMessage7 ->
                    Just 65

                ResilienceMessage8 ->
                    Just 79

                ResilienceMessage9 ->
                    Just 91

                ResilienceMessage10 ->
                    Just 93

                ResilienceMessage11 ->
                    Just 119

                ResilienceMessage12 ->
                    Just 121

                ResilienceMessage13 ->
                    Just 137

                ResilienceMessage14 ->
                    Just 144

                ResilienceMessage15 ->
                    Just 149

                ResilienceMessage16 ->
                    Just 186

                _ ->
                    Nothing

        ResilienceCategoryMindfulness ->
            case order of
                ResilienceMessage1 ->
                    Just 12

                ResilienceMessage2 ->
                    Just 14

                ResilienceMessage3 ->
                    Just 19

                ResilienceMessage4 ->
                    Just 35

                ResilienceMessage5 ->
                    Just 37

                ResilienceMessage6 ->
                    Just 47

                ResilienceMessage7 ->
                    Just 77

                ResilienceMessage8 ->
                    Just 84

                ResilienceMessage9 ->
                    Just 112

                ResilienceMessage10 ->
                    Just 133

                ResilienceMessage11 ->
                    Just 172

                _ ->
                    Nothing

        ResilienceCategoryConnecting ->
            case order of
                ResilienceMessage1 ->
                    Just 16

                ResilienceMessage2 ->
                    Just 23

                ResilienceMessage3 ->
                    Just 26

                ResilienceMessage4 ->
                    Just 40

                ResilienceMessage5 ->
                    Just 51

                ResilienceMessage6 ->
                    Just 68

                ResilienceMessage7 ->
                    Just 72

                ResilienceMessage8 ->
                    Just 75

                ResilienceMessage9 ->
                    Just 82

                ResilienceMessage10 ->
                    Just 89

                ResilienceMessage11 ->
                    Just 98

                ResilienceMessage12 ->
                    Just 110

                ResilienceMessage13 ->
                    Just 128

                ResilienceMessage14 ->
                    Just 130

                ResilienceMessage15 ->
                    Just 168

                ResilienceMessage16 ->
                    Just 175

                ResilienceMessage17 ->
                    Just 189

                ResilienceMessage18 ->
                    Just 193

                _ ->
                    Nothing

        ResilienceCategorySelfCare ->
            case order of
                ResilienceMessage1 ->
                    Just 21

                ResilienceMessage2 ->
                    Just 42

                ResilienceMessage3 ->
                    Just 58

                ResilienceMessage4 ->
                    Just 103

                ResilienceMessage5 ->
                    Just 114

                ResilienceMessage6 ->
                    Just 135

                ResilienceMessage7 ->
                    Just 142

                ResilienceMessage8 ->
                    Just 150

                ResilienceMessage9 ->
                    Just 165

                ResilienceMessage10 ->
                    Just 182

                _ ->
                    Nothing

        ResilienceCategoryEndOfPeriod ->
            case order of
                ResilienceMessage1 ->
                    Just 30

                ResilienceMessage2 ->
                    Just 56

                ResilienceMessage3 ->
                    Just 84

                ResilienceMessage4 ->
                    Just 117

                ResilienceMessage5 ->
                    Just 147

                ResilienceMessage6 ->
                    Just 177

                ResilienceMessage7 ->
                    Just 200

                _ ->
                    Nothing
