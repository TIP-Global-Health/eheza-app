module ZScore.Model
    exposing
        ( AgeInDays(..)
        , Centimetres(..)
        , Kilograms(..)
        , ZScore(..)
        , compareZScore
        , viewZScore
        , zScoreForHeight
        , zScoreForWeight
        )

{-| This module determines a ZScore for various measurements, using an
algorithm described at <https://github.com/Gizra/ihangane/issues/303>.
-}

import Child.Model exposing (Gender(..))
import IntDict exposing (IntDict)
import ZScore.Internal.HeightBoys as HeightBoys
import ZScore.Internal.HeightGirls as HeightGirls
import ZScore.Internal.WeightBoys as WeightBoys
import ZScore.Internal.WeightGirls as WeightGirls


{- We might want to re-use some of these types generally. In fact, there would
   be ways to be more sophisticated about measurement units, but they might be
   overkill.

   The types are mainly to avoid any confusion about what the units are. It
   forces the caller to do things like:

       zScoreFromHeight (AgeInDays 27) Male (Centimetres 27)

   ... so that the caller has to think about what units are being provided.
-}


type AgeInDays
    = AgeInDays Int


type Centimetres
    = Centimetres Float


type Kilograms
    = Kilograms Float


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreForHeight : AgeInDays -> Gender -> Centimetres -> Maybe ZScore
zScoreForHeight (AgeInDays age) gender (Centimetres cm) =
    let
        data =
            case gender of
                Male ->
                    HeightBoys.data

                Female ->
                    HeightGirls.data
    in
        zScoreFromData age cm data


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreForWeight : AgeInDays -> Gender -> Kilograms -> Maybe ZScore
zScoreForWeight (AgeInDays age) gender (Kilograms kg) =
    let
        data =
            case gender of
                Male ->
                    WeightBoys.data

                Female ->
                    WeightGirls.data
    in
        zScoreFromData age kg data


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


{-| Convert the ZScore to a string for display purposes.

E.g.

    ZScore3Neg -> "-3"
    ZScore0 -> "0"
    ZScore1 -> "1"

-}
viewZScore : ZScore -> String
viewZScore =
    toString << zScoreToInt


{-| Is the first ZScore greater than, less than, or
equal to the second?
-}
compareZScore : ZScore -> ZScore -> Order
compareZScore z1 z2 =
    compare (zScoreToInt z1) (zScoreToInt z2)


{-| Not exposed, since we don't want people to think of these as ints.
-}
zScoreToInt : ZScore -> Int
zScoreToInt z =
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


zScoreFromInt : Int -> ZScore
zScoreFromInt z =
    if z <= -3 then
        ZScore3Neg
    else if z == -2 then
        ZScore2Neg
    else if z == -1 then
        ZScore1Neg
    else if z == 0 then
        ZScore0
    else if z == 1 then
        ZScore1
    else if z == 2 then
        ZScore2
    else
        ZScore3


{-| This one isn't exported ... it's a convenience for internal use.

We're using `IntDict` since our ages are ints, and it will be more
efficient internally for performance and memory than a regular `Dict`.
There would be ways of using even less memory, if that turns out to
be necessary, by writing some "native" code that uses a Javascript
ArrayBuffer.

We could use an Elm `Array` type, but that module has some bugs which
it would be best to avoid (at least until Elm 0.19).

-}
zScoreFromData : Int -> Float -> IntDict ( Float, Float ) -> Maybe ZScore
zScoreFromData key measurement data =
    IntDict.get key data
        |> Maybe.map
            (\( sd0, sd1 ) ->
                let
                    differenceFromMean =
                        measurement - sd0

                    oneStandardDeviation =
                        sd1 - sd0

                    zScoreFloat =
                        differenceFromMean / oneStandardDeviation

                    zScoreCeiling =
                        ceiling zScoreFloat
                in
                    zScoreFromInt zScoreCeiling
            )
