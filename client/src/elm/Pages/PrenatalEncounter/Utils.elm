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


expectPrenatalActivity : NominalDate -> PrenatalMeasurements -> List ( NominalDate, PrenatalMeasurements ) -> PrenatalActivity -> Bool
expectPrenatalActivity currentDate currentMeasurements previousMeasurementsWithDates activity =
    case activity of
        PrenatalPhoto ->
            expectPrenatalPhoto currentDate currentMeasurements previousMeasurementsWithDates

        _ ->
            True


expectPrenatalPhoto : NominalDate -> PrenatalMeasurements -> List ( NominalDate, PrenatalMeasurements ) -> Bool
expectPrenatalPhoto currentDate currentMeasurements previousMeasurementsWithDates =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 20 and week 29.
            --  3. Week 30, or more.
            [ [ (>) 13 ], [ (>) 30, (<=) 20 ], [ (<=) 30 ] ]

        maybeLmpDate =
            -- When there are no previous measurements, we try to resolve
            -- Lmp date from current encounter.
            if List.isEmpty previousMeasurementsWithDates then
                getLmpMeasurement currentMeasurements

            else
                -- When there are previous measurements, we know that Lmp date
                -- will be located at head of the list, becuase previous measurements
                -- are sorted by encounter date, and Lmp date is a mandatory measurement.
                previousMeasurementsWithDates
                    |> List.head
                    |> Maybe.andThen (Tuple.second >> getLmpMeasurement)
    in
    maybeLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    currentWeek =
                        diffDays lmpDate currentDate // 7

                    conditionsForCurrentWeek =
                        periods
                            |> List.filter
                                (\periodConditions ->
                                    List.all (\condition -> condition currentWeek == True) periodConditions
                                )
                            |> List.head
                in
                conditionsForCurrentWeek
                    |> Maybe.map
                        (\conditions ->
                            -- There should be no encounters that are  within dates range,
                            -- that got a photo measurement.
                            previousMeasurementsWithDates
                                |> List.filterMap
                                    (\( encounterDate, measurements ) ->
                                        let
                                            encounterWeek =
                                                diffDays lmpDate encounterDate // 7
                                        in
                                        -- Encounter is within dates range, and it's has a photo measurement.
                                        if
                                            List.all (\condition -> condition encounterWeek == True) conditions
                                                && isJust measurements.prenatalPhoto
                                        then
                                            Just encounterDate

                                        else
                                            Nothing
                                    )
                                |> List.isEmpty
                        )
                    -- There are no period conditions, meaning we're not within required dates
                    -- range. Therefore, we do not allow photo activity.
                    |> Maybe.withDefault False
            )
        -- We do not allow photo activity when Lmp date is not known.
        |> Maybe.withDefault False
