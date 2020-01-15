module Pages.PrenatalEncounter.Utils exposing (calculateEDDandEGADays, expectPrenatalActivity, generateEDDandEGA, generateEGAWeeksDaysLabel, generateGravida, generatePara, getLmpMeasurement)

import Backend.Measurement.Model exposing (..)
import Date.Extra as Date exposing (Interval(Day))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Maybe.Extra exposing (isJust, unwrap)
import PrenatalActivity.Model exposing (..)
import Translate exposing (Language, translate)


calculateEDDandEGADays : NominalDate -> NominalDate -> ( NominalDate, Int )
calculateEDDandEGADays currentDate lmpDate =
    ( toLocalDateTime lmpDate 12 0 0 0
        |> Date.add Day 280
        |> fromLocalDateTime
    , diffDays lmpDate currentDate
    )


generateEGAWeeksDaysLabel : Language -> Int -> String
generateEGAWeeksDaysLabel language diffInDays =
    let
        diffInWeeks =
            diffInDays // 7

        egaWeeks =
            translate language <| Translate.WeekSinglePlural diffInWeeks

        egaDays =
            translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)
    in
    egaWeeks ++ ", " ++ egaDays


generateEDDandEGA : Language -> NominalDate -> ( String, String ) -> Maybe NominalDate -> ( String, String )
generateEDDandEGA language currentDate defaults maybeLmpDate =
    unwrap
        defaults
        (\lmpDate ->
            let
                ( eddDate, diffInDays ) =
                    calculateEDDandEGADays currentDate lmpDate
            in
            ( formatMMDDYYYY eddDate, generateEGAWeeksDaysLabel language diffInDays )
        )
        maybeLmpDate


generateGravida : ObstetricHistoryValue -> String
generateGravida value =
    let
        total =
            (value.termPregnancy + value.preTermPregnancy + value.stillbirthsAtTerm + value.stillbirthsPreTerm + value.abortions)
                + (if value.currentlyPregnant then
                    1

                   else
                    0
                  )
    in
    if total < 10 then
        "0" ++ toString total

    else
        toString total


generatePara : ObstetricHistoryValue -> String
generatePara value =
    toString (value.termPregnancy + value.stillbirthsAtTerm)
        ++ toString (value.preTermPregnancy + value.stillbirthsPreTerm)
        ++ toString value.abortions
        ++ toString value.liveChildren


getLmpMeasurement : PrenatalMeasurements -> Maybe NominalDate
getLmpMeasurement measurements =
    measurements.lastMenstrualPeriod
        |> Maybe.map (Tuple.second >> .value >> .date)


expectPrenatalActivity : NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> PrenatalActivity -> Bool
expectPrenatalActivity currentDate previousMeasurementsWithDates activity =
    case activity of
        PrenatalPhoto ->
            expectPrenatalPhoto currentDate previousMeasurementsWithDates

        _ ->
            True


expectPrenatalPhoto : NominalDate -> List ( NominalDate, PrenatalMeasurements ) -> Bool
expectPrenatalPhoto currentDate previousMeasurementsWithDates =
    let
        maybeLmpDate =
            previousMeasurementsWithDates
                |> List.head
                |> Maybe.andThen (Tuple.second >> getLmpMeasurement)
    in
    maybeLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    rangeConditions =
                        [ [ (>) 13 ], [ (>) 30, (<=) 20 ], [ (<) 30 ] ]

                    currentWeek =
                        diffDays lmpDate currentDate // 7
                in
                rangeConditions
                    |> List.map
                        -- Range conditions.
                        (\conditions ->
                            -- If we're withing a range today.
                            if List.all (\condition -> condition currentWeek == True) conditions then
                                let
                                    measurementsWithingRangeWithPhoto =
                                        previousMeasurementsWithDates
                                            |> List.filterMap
                                                (\( encounterDate, measurements ) ->
                                                    let
                                                        encounterWeek =
                                                            diffDays lmpDate encounterDate // 7
                                                    in
                                                    -- Encounter is within range, and it's has a photo measurement.
                                                    if List.all (\condition -> condition encounterWeek == True) conditions && isJust measurements.prenatalPhoto then
                                                        Just True

                                                    else
                                                        Nothing
                                                )
                                in
                                -- None of enccounters are within range, got a photo measurement.
                                List.length measurementsWithingRangeWithPhoto == 0

                            else
                                -- Not withing a range today.
                                False
                        )
                    |> List.any ((==) True)
            )
        |> Maybe.withDefault True
