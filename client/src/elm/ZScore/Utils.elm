module ZScore.Utils
    exposing
        ( compareZScore
        , viewZScore
        , zScoreHeightForAge
        , zScoreWeightForAge
        , zScoreWeightForHeight
        )

{-| This module determines a ZScore for various measurements.
-}

import Backend.Child.Model exposing (Gender(..))
import IntDict exposing (IntDict)
import RemoteData
import Utils.NominalDate exposing (Days(..))
import ZScore.Model exposing (Centimetres(..), Kilograms(..), Model, ZScore(..), ZScoreEntry)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreHeightForAge : Model -> Days -> Gender -> Centimetres -> Maybe ZScore
zScoreHeightForAge model (Days age) gender (Centimetres cm) =
    let
        source =
            case gender of
                Male ->
                    .heightForAgeBoys

                Female ->
                    .heightForAgeGirls
    in
    source model
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreFromEntries age cm)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForAge : Model -> Days -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForAge model (Days age) gender (Kilograms kg) =
    let
        source =
            case gender of
                Male ->
                    .weightForAgeBoys

                Female ->
                    .weightForAgeGirls
    in
    source model
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreFromEntries age kg)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForHeight : Model -> Centimetres -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForHeight model (Centimetres cm) gender (Kilograms kg) =
    let
        -- This one is a little different, because the data is keyed by integer
        -- millimetres. So, we take the float cm, multiply by 10, and round to
        -- get to the closest key.
        integerMillimetres =
            round (cm * 10)

        source =
            case gender of
                Male ->
                    .weightForHeightBoys

                Female ->
                    .weightForHeightGirls
    in
    source model
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreFromEntries integerMillimetres kg)


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
    if z == -3 then
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


{-| Note that when we calculate a ZScore from a measurement, we apply a kind
of "ceiling" ... if a measurement is between two ZScore lines, we report
the higher one.
-}
zScoreFromEntries : Int -> Float -> IntDict ZScoreEntry -> Maybe ZScore
zScoreFromEntries key measurement entries =
    IntDict.get key entries
        |> Maybe.map
            (\entry ->
                if measurement <= entry.sd3neg then
                    ZScore3Neg
                else if measurement <= entry.sd2neg then
                    ZScore2Neg
                else if measurement <= entry.sd1neg then
                    ZScore1Neg
                else if measurement <= entry.sd0 then
                    ZScore0
                else if measurement <= entry.sd1 then
                    ZScore1
                else if measurement <= entry.sd2 then
                    ZScore2
                else
                    ZScore3
            )
