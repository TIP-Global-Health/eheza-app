module Pages.NutritionActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , HeightInCm(..)
        , MuacInCm(..)
        , MuacIndication(..)
        , NutritionMeasurement
        , NutritionMeasurements
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, or, unwrap)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, valueConsideringIsDirtyField)
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (Kilograms(..))
import ZScore.Utils exposing (zScoreWeightForAge)


expectActivity : NominalDate -> ZScore.Model.Model -> Person -> Bool -> NutritionMeasurements -> NutritionActivity -> Bool
expectActivity currentDate zscores child isChw measurements activity =
    case activity of
        -- Do not show for community health workers.
        Height ->
            not isChw

        -- Show for children that are at least 6 month old.
        Muac ->
            ageInMonths currentDate child
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        SendToHC ->
            if isChw && mandatoryActivitiesCompleted currentDate zscores child isChw measurements then
                let
                    abnormalMuac =
                        measurements.muac
                            |> Maybe.map
                                (Tuple.second
                                    >> .value
                                    >> muacAbnormal
                                )
                            |> Maybe.withDefault False

                    nutritionSigns =
                        measurements.nutrition
                            |> Maybe.map (Tuple.second >> .value >> EverySet.toList)
                            |> Maybe.withDefault []

                    abnormalNutrition =
                        ageInMonths currentDate child
                            |> Maybe.map
                                (\ageMonths ->
                                    if ageMonths > 5 then
                                        -- When over 6 months, abnormal only when Edema present.
                                        List.member Edema nutritionSigns

                                    else
                                        -- When under 6 months, abnormal when any nutrition sign present.
                                        case nutritionSigns of
                                            [] ->
                                                False

                                            [ NormalChildNutrition ] ->
                                                False

                                            _ ->
                                                True
                                )
                            |> Maybe.withDefault False

                    abnormalWeightForAgeZScore =
                        measurements.weight
                            |> Maybe.andThen
                                (Tuple.second
                                    >> .value
                                    >> (\(WeightInKg weight) -> calculateZScoreWeightForAge currentDate zscores child (Just weight))
                                )
                            |> Maybe.map (zScoreWeightForAgeAbnormal currentDate child)
                            |> Maybe.withDefault False
                in
                abnormalMuac || abnormalNutrition || abnormalWeightForAgeZScore

            else
                False

        HealthEducation ->
            expectActivity currentDate zscores child isChw measurements SendToHC

        _ ->
            True


activityCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> NutritionMeasurements -> NutritionActivity -> Bool
activityCompleted currentDate zscores child isChw measurements activity =
    case activity of
        Height ->
            (not <| expectActivity currentDate zscores child isChw measurements Height)
                || isJust measurements.height

        Muac ->
            (not <| expectActivity currentDate zscores child isChw measurements Muac)
                || isJust measurements.muac

        Nutrition ->
            isJust measurements.nutrition

        Photo ->
            isJust measurements.photo

        Weight ->
            isJust measurements.weight

        SendToHC ->
            (not <| expectActivity currentDate zscores child isChw measurements SendToHC)
                || isJust measurements.sendToHC

        HealthEducation ->
            (not <| expectActivity currentDate zscores child isChw measurements HealthEducation)
                || isJust measurements.healthEducation


mandatoryActivitiesCompleted : NominalDate -> ZScore.Model.Model -> Person -> Bool -> NutritionMeasurements -> Bool
mandatoryActivitiesCompleted currentDate zscores child isChw measurements =
    [ Height, Muac, Nutrition, Weight ]
        |> List.filter (not << activityCompleted currentDate zscores child isChw measurements)
        |> List.isEmpty


resolvePreviousIndividualValue : AssembledData -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a )) -> (a -> b) -> Maybe ( NominalDate, b )
resolvePreviousIndividualValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( date, ( _, measurements ) ) ->
                measurementFunc measurements
                    |> Maybe.map
                        (\measurement ->
                            ( date, Tuple.second measurement |> .value |> valueFunc )
                        )
            )
        |> List.reverse
        |> List.head


fromMuacValue : Maybe MuacInCm -> MuacForm
fromMuacValue saved =
    { muac = Maybe.map (\(MuacInCm cm) -> cm) saved
    , muacDirty = False
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = valueConsideringIsDirtyField form.muacDirty form.muac (value |> (\(MuacInCm cm) -> cm))
                , muacDirty = form.muacDirty
                }
            )


toMuacValueWithDefault : Maybe MuacInCm -> MuacForm -> Maybe MuacInCm
toMuacValueWithDefault saved form =
    muacFormWithDefault form saved
        |> toMuacValue


toMuacValue : MuacForm -> Maybe MuacInCm
toMuacValue form =
    Maybe.map MuacInCm form.muac


fromHeightValue : Maybe HeightInCm -> HeightForm
fromHeightValue saved =
    { height = Maybe.map (\(HeightInCm cm) -> cm) saved
    , heightDirty = False
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (value |> (\(HeightInCm cm) -> cm))
                , heightDirty = form.heightDirty
                }
            )


toHeightValueWithDefault : Maybe HeightInCm -> HeightForm -> Maybe HeightInCm
toHeightValueWithDefault saved form =
    heightFormWithDefault form saved
        |> toHeightValue


toHeightValue : HeightForm -> Maybe HeightInCm
toHeightValue form =
    Maybe.map HeightInCm form.height


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


fromWeightValue : Maybe WeightInKg -> WeightForm
fromWeightValue saved =
    { weight = Maybe.map (\(WeightInKg cm) -> cm) saved
    , weightDirty = False
    }


weightFormWithDefault : WeightForm -> Maybe WeightInKg -> WeightForm
weightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { weight = valueConsideringIsDirtyField form.weightDirty form.weight (value |> (\(WeightInKg cm) -> cm))
                , weightDirty = form.weightDirty
                }
            )


toWeightValueWithDefault : Maybe WeightInKg -> WeightForm -> Maybe WeightInKg
toWeightValueWithDefault saved form =
    weightFormWithDefault form saved
        |> toWeightValue


toWeightValue : WeightForm -> Maybe WeightInKg
toWeightValue form =
    Maybe.map WeightInKg form.weight


calculateZScoreWeightForAge : NominalDate -> ZScore.Model.Model -> Person -> Maybe Float -> Maybe Float
calculateZScoreWeightForAge currentDate zscores person maybeWeight =
    let
        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate
    in
    maybeWeight
        |> Maybe.andThen
            (\weight ->
                Maybe.andThen
                    (\ageInDays ->
                        zScoreWeightForAge zscores ageInDays person.gender (Kilograms weight)
                    )
                    maybeAgeInDays
            )


zScoreWeightForAgeAbnormal : NominalDate -> Person -> Float -> Bool
zScoreWeightForAgeAbnormal currentDate child zScore =
    -- Abnormal when we have severe value for any age.
    zScoreWeightForAgeSevere zScore
        || -- Abnormal when we have moderate value and child is 0-6 month old.
           zScoreWeightForAgeModerate currentDate child zScore


zScoreWeightForAgeSevere : Float -> Bool
zScoreWeightForAgeSevere zScore =
    zScore <= -3


zScoreWeightForAgeModerate : NominalDate -> Person -> Float -> Bool
zScoreWeightForAgeModerate currentDate child zScore =
    ageInMonths currentDate child
        |> Maybe.map
            (\ageMonths ->
                -- Abnormal when we have moderate value and child is 0-6 month old.
                ageMonths < 6 && (zScore > -3 && zScore <= -2)
            )
        |> Maybe.withDefault False


muacAbnormal : MuacInCm -> Bool
muacAbnormal muac =
    muacSevere muac || muacModerate muac


muacSevere : MuacInCm -> Bool
muacSevere muac =
    muacIndication muac == MuacRed


muacModerate : MuacInCm -> Bool
muacModerate muac =
    muacIndication muac == MuacYellow


malnutritionSignEdemaRecorded : NutritionMeasurements -> Bool
malnutritionSignEdemaRecorded measurements =
    measurements.nutrition
        |> Maybe.map
            (Tuple.second
                >> .value
                >> EverySet.member Edema
            )
        |> Maybe.withDefault False
