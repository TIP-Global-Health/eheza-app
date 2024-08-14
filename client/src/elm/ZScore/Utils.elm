module ZScore.Utils exposing
    ( diffDays
    , valueForZScore
    , viewZScore
    , zScoreBmiForAge
    , zScoreHeadCircumferenceForAge
    , zScoreLengthHeightForAge
    , zScoreWeightForAge
    , zScoreWeightForHeight
    , zScoreWeightForLength
    )

{-| This module determines a ZScore for various measurements.
-}

import Backend.Measurement.Model exposing (Gender(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (orElseLazy)
import RemoteData
import Round
import Utils.AllDict as AllDict exposing (AllDict)
import ZScore.Model exposing (..)


selectGender : Gender -> MaleAndFemale a -> a
selectGender gender =
    case gender of
        Male ->
            .male

        Female ->
            .female


{-| Should we clamp ZScores above +3 or below -3?
-}
type Clamp
    = Clamp
    | NoClamp


{-| Calculates the ZScore from the provided data.

It is assumed that the measurement represents "length" for children < 2 years
old, and "height" for chldren above 2 years old.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreLengthHeightForAge : Model -> Days -> Gender -> Centimetres -> Maybe ZScore
zScoreLengthHeightForAge model age gender cm =
    model.lengthHeightForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge NoClamp (\(Centimetres x) -> x) Centimetres age gender cm)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreBmiForAge : Model -> Days -> Gender -> BMI -> Maybe ZScore
zScoreBmiForAge model age gender bmi =
    model.bmiForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge Clamp (\(BMI x) -> x) BMI age gender bmi)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForAge : Model -> Days -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForAge model age gender kg =
    model.weightForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge Clamp (\(Kilograms x) -> x) Kilograms age gender kg)


zScoreHeadCircumferenceForAge : Model -> Days -> Gender -> Centimetres -> Maybe ZScore
zScoreHeadCircumferenceForAge model age gender cm =
    model.headCircumferenceForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (selectGender gender >> zScoreForDays NoClamp (\(Centimetres x) -> x) age cm)


zScoreForAge : Clamp -> (value -> Float) -> (Float -> value) -> Days -> Gender -> value -> MaleAndFemale (ByDaysAndMonths value) -> Maybe ZScore
zScoreForAge clamp unwrapValue wrapValue days gender value tables =
    let
        table =
            selectGender gender tables
    in
    zScoreForDays clamp unwrapValue days value table.byDay
        |> orElseLazy (\_ -> zScoreForMonths clamp unwrapValue wrapValue days value table.byMonth)


zScoreForDays : Clamp -> (value -> Float) -> Days -> value -> AllDict Days (ZScoreEntry value) Int -> Maybe ZScore
zScoreForDays clamp unwrapValue days value table =
    Maybe.map
        (calculateZScore clamp unwrapValue value)
        (AllDict.get days table)


zScoreForMonths : Clamp -> (value -> Float) -> (Float -> value) -> Days -> value -> AllDict Months (ZScoreEntry value) Int -> Maybe ZScore
zScoreForMonths clamp unwrapValue wrapValue (Days days) value table =
    let
        fractionalMonths =
            toFloat days * 12 / 365.25

        lowMonths =
            truncate fractionalMonths

        highMonths =
            ceiling fractionalMonths
    in
    Maybe.map2
        (\low high ->
            let
                -- We interpolate an entry from fractional months
                diffMonths =
                    fractionalMonths - toFloat lowMonths

                entry =
                    { l = low.l + (diffMonths * (high.l - low.l))
                    , m = wrapValue (unwrapValue low.m + diffMonths * (unwrapValue high.m - unwrapValue low.m))
                    , s = low.s + (diffMonths * (high.s - low.s))
                    }
            in
            calculateZScore clamp unwrapValue value entry
        )
        (AllDict.get (Months lowMonths) table)
        (AllDict.get (Months highMonths) table)


calculateZScore : Clamp -> (value -> Float) -> value -> ZScoreEntry value -> ZScore
calculateZScore clamp unwrapValue value entry =
    let
        result =
            (((unwrapValue value / unwrapValue entry.m) ^ entry.l) - 1) / (entry.s * entry.l)
    in
    case clamp of
        NoClamp ->
            result

        Clamp ->
            -- If we're clamping, we adjust things so that outside of +3 or
            -- -3, we have a constant distance between ZScores, where the
            -- constant difference is the distance between SD2 and SD3.
            if result > 3 then
                let
                    sd3pos =
                        valueForZScore unwrapValue 3 entry

                    sd2pos =
                        valueForZScore unwrapValue 2 entry

                    sd23pos =
                        sd3pos - sd2pos
                in
                3 + ((unwrapValue value - sd3pos) / sd23pos)

            else if result < -3 then
                let
                    sd3neg =
                        valueForZScore unwrapValue -3 entry

                    sd2neg =
                        valueForZScore unwrapValue -2 entry

                    sd23neg =
                        sd2neg - sd3neg
                in
                -3 + ((unwrapValue value - sd3neg) / sd23neg)

            else
                result


{-| Given a desired ZScore, what value will produce that ZScore?
-}
valueForZScore : (value -> Float) -> ZScore -> ZScoreEntry value -> Float
valueForZScore unwrapValue zscore entry =
    unwrapValue entry.m * ((1 + entry.l * entry.s * zscore) ^ (1 / entry.l))


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForHeight : Model -> Height -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForHeight model height gender kg =
    model.weightForHeight
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForCm height gender kg)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForLength : Model -> Length -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForLength model length gender kg =
    model.weightForLength
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForCm length gender kg)


zScoreForCm : key -> Gender -> Kilograms -> MaleAndFemale (AllDict key (ZScoreEntry Kilograms) Int) -> Maybe ZScore
zScoreForCm key gender kg tables =
    let
        table =
            selectGender gender tables
    in
    Maybe.map
        (calculateZScore Clamp (\(Kilograms x) -> x) kg)
        (AllDict.get key table)


{-| Convert the ZScore to a string for display purposes.

We'll show two decimal points.

-}
viewZScore : ZScore -> String
viewZScore =
    Round.round 2


{-| Difference in whole days between two dates.

The result is positive if the second parameter is after the first parameter.

-}
diffDays : NominalDate -> NominalDate -> Days
diffDays low high =
    -- delta gives us separate deltas for years, months and days ... so, for
    -- instance, for a difference of 2 years and 1 month, you'd get
    --
    -- { years : 2
    -- , months: 25
    -- , days: 760 -- roughly, depending on which months are involved
    -- }
    Gizra.NominalDate.diffDays low high
        |> Days
