module ZScore.Utils exposing
    ( compareZScore
    , viewZScore
    , zScoreBmiForAge
    , zScoreLengthHeightForAge
    , zScoreWeightForAge
    , zScoreWeightForHeight
    , zScoreWeightForLength
    )

{-| This module determines a ZScore for various measurements.
-}

import AllDict exposing (AllDict)
import Backend.Child.Model exposing (Gender(..))
import Maybe.Extra exposing (orElseLazy)
import RemoteData
import Utils.NominalDate exposing (Days(..), Months(..))
import ZScore.Model exposing (..)


selectGender : Gender -> MaleAndFemale a -> a
selectGender gender =
    case gender of
        Male ->
            .male

        Female ->
            .female


{-| Calculates the ZScore from the provided data.

It is assumed that the measurement represents "length" for children < 2 years
old, and "height" for chldren above 2 years old.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreLengthHeightForAge : Model -> Days -> Gender -> Centimetres -> Maybe ZScore
zScoreLengthHeightForAge model age gender cm =
    model.lengthHeightForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge (\(Centimetres x) -> x) age gender cm)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreBmiForAge : Model -> Days -> Gender -> BMI -> Maybe ZScore
zScoreBmiForAge model age gender bmi =
    model.bmiForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge (\(BMI x) -> x) age gender bmi)


{-| Calculates the ZScore from the provided data.

Returns a `Maybe` in case the age is out of the range of our data.

-}
zScoreWeightForAge : Model -> Days -> Gender -> Kilograms -> Maybe ZScore
zScoreWeightForAge model age gender kg =
    model.weightForAge
        |> RemoteData.toMaybe
        |> Maybe.andThen (zScoreForAge (\(Kilograms x) -> x) age gender kg)


zScoreForAge : (value -> Float) -> Days -> Gender -> value -> MaleAndFemale (ByDaysAndMonths value) -> Maybe ZScore
zScoreForAge unwrapValue days gender value tables =
    let
        table =
            selectGender gender tables
    in
    zScoreForDays unwrapValue days value table.byDay
        |> orElseLazy (\_ -> zScoreForMonths unwrapValue days value table.byMonth)


zScoreForDays : (value -> Float) -> Days -> value -> AllDict Days (ZScoreEntry value) Int -> Maybe ZScore
zScoreForDays unwrapValue days value table =
    Debug.crash "todo"


zScoreForMonths : (value -> Float) -> Days -> value -> AllDict Months (ZScoreEntry value) Int -> Maybe ZScore
zScoreForMonths unwrapValue days value table =
    Debug.crash "todo"


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
zScoreForCm key gender kg table =
    Debug.crash "todo"


{-| Convert the ZScore to a string for display purposes.
-}
viewZScore : ZScore -> String
viewZScore =
    toString


{-| Is the first ZScore greater than, less than, or equal to the second?
-}
compareZScore : ZScore -> ZScore -> Order
compareZScore =
    compare
