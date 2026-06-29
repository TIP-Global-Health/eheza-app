module Pages.Dashboard.Test exposing (all)

import AssocList as Dict
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, NutritionStatus(..), NutritionValue)
import Backend.Measurement.Model exposing (Gender(..))
import Date
import Expect
import Pages.Dashboard.Utils exposing (caseManagementMergeDuplicates)
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


emptyCaseNutrition : CaseNutrition
emptyCaseNutrition =
    { stunting = Dict.empty
    , underweight = Dict.empty
    , wasting = Dict.empty
    , muac = Dict.empty
    , nutritionSigns = Dict.empty
    }


nutritionValue : NutritionValue
nutritionValue =
    { class = Moderate, value = "1.0" }


{-| A case-management record for person `identifier`, holding a single stunting
measurement for the given `month`.
-}
caseWithStuntingMonth : Int -> Int -> CaseManagement
caseWithStuntingMonth identifier month =
    { identifier = identifier
    , birthDate = Date.fromCalendarDate 2018 Time.Jan 1
    , gender = Female
    , nutrition = { emptyCaseNutrition | stunting = Dict.singleton month nutritionValue }
    }


caseManagementMergeDuplicatesTest : Test
caseManagementMergeDuplicatesTest =
    describe "caseManagementMergeDuplicates"
        [ test "merging two records for the SAME person keeps BOTH persons' nutrition months" <|
            \_ ->
                -- The same child enrolled across programs appears as two records
                -- sharing one identifier, each carrying different months of
                -- nutrition data. The merge must combine them, not discard one.
                let
                    merged =
                        caseManagementMergeDuplicates
                            [ caseWithStuntingMonth 1 1
                            , caseWithStuntingMonth 1 2
                            ]
                in
                case merged of
                    [ person ] ->
                        Dict.keys person.nutrition.stunting
                            |> List.sort
                            |> Expect.equal [ 1, 2 ]

                    _ ->
                        Expect.fail "expected the two same-identifier records to merge into exactly one"
        , test "records with DIFFERENT identifiers are not merged" <|
            \_ ->
                caseManagementMergeDuplicates
                    [ caseWithStuntingMonth 1 1
                    , caseWithStuntingMonth 2 3
                    ]
                    |> List.length
                    |> Expect.equal 2
        ]


all : Test
all =
    describe "Pages.Dashboard.Utils"
        [ caseManagementMergeDuplicatesTest
        ]
