module ZScore.Common exposing (ZScore(..), compareZScore, viewZScore)

{-| This module represents some common elements to determining
standard deviations for various measurements.
-}


{-| A ZScore. This isn't really a number -- e.g. you can't
meaningfully do any arithmetic with it. So, we just enumerate the
possibilities.

  - Use `compareZScore` to check ordering.

  - You can use Elm's `==` reliably with this type.

  - Use `viewZScore` to get the ZScore as a string, such that
    `ZSCore3Neg` displays as "-3", etc.

-}
type ZScore
    = ZScore3Neg
    | ZScore2Neg
    | ZScore1Neg
    | ZScore0
    | ZScore1
    | ZScore2
    | ZScore3


viewZScore : ZScore -> String
viewZScore =
    toString << zScoreToInt


{-| Is the first ZScore greater than, less than, or
equal to the second?
-}
compareZScore : ZScore -> ZScore -> Order
compareZScore z1 z2 =
    compare (zScoreToInt z1) (zScoreToInt z2)


zScoreToInt : ZScore -> Int
zScoreToInt z =
    -- Not exposed, since we don't want people to think of these as ints.
    -- But it's handy internally ...
    case z of
        ZScore3Neg ->
            -3

        ZScore2Neg ->
            -2

        ZScore1Neg ->
            -1

        ZScore0 ->
            0

        ZScore1 ->
            1

        ZScore2 ->
            2

        ZScore3 ->
            3
