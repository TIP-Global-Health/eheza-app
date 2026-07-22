module Pages.Utils.Test exposing (all)

import Expect
import Pages.Utils exposing (dropLeadingMinus, percentageOfTotal)
import Test exposing (Test, describe, test)


percentageOfTotalTest : Test
percentageOfTotalTest =
    describe "Pages.Utils.percentageOfTotal"
        [ test "returns 0 when the total is 0, instead of NaN" <|
            \_ ->
                percentageOfTotal 0 0
                    |> Expect.equal 0
        , test "returns 0 for a positive part over a zero total" <|
            \_ ->
                percentageOfTotal 5 0
                    |> Expect.equal 0
        , test "computes a rounded proportion" <|
            \_ ->
                percentageOfTotal 3 4
                    |> Expect.equal 75
        , test "rounds to the nearest whole percent" <|
            \_ ->
                percentageOfTotal 1 3
                    |> Expect.equal 33
        , test "is 100 when part equals total" <|
            \_ ->
                percentageOfTotal 7 7
                    |> Expect.equal 100
        ]


dropLeadingMinusTest : Test
dropLeadingMinusTest =
    describe "Pages.Utils.dropLeadingMinus"
        [ test "drops a minus typed in front of a number" <|
            \_ ->
                dropLeadingMinus "-5"
                    |> Expect.equal "5"
        , test "keeps the digits, so the amount typed is not lost" <|
            \_ ->
                dropLeadingMinus "-37.5"
                    |> Expect.equal "37.5"
        , test "leaves a positive number alone" <|
            \_ ->
                dropLeadingMinus "37.5"
                    |> Expect.equal "37.5"
        , test "leaves an empty field alone, so it can still be cleared" <|
            \_ ->
                dropLeadingMinus ""
                    |> Expect.equal ""
        , test "leaves a lone minus as nothing, rather than as a value" <|
            \_ ->
                dropLeadingMinus "-"
                    |> Expect.equal ""
        , test "does not touch a minus that is part of the number, such as an exponent" <|
            -- A number field accepts 1e-5. Removing every minus would turn that
            -- into 1e5, which is a much larger number rather than a smaller one.
            \_ ->
                dropLeadingMinus "1e-5"
                    |> Expect.equal "1e-5"
        , test "drops only the leading minus of a negative exponent value" <|
            \_ ->
                dropLeadingMinus "-1e-5"
                    |> Expect.equal "1e-5"
        ]


all : Test
all =
    describe "Pages.Utils"
        [ percentageOfTotalTest
        , dropLeadingMinusTest
        ]
