module Pages.Utils.Test exposing (all)

import Expect
import Pages.Utils exposing (percentageOfTotal)
import Test exposing (Test, describe, test)


all : Test
all =
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
