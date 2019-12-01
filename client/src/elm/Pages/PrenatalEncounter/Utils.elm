module Pages.PrenatalEncounter.Utils exposing (calculateEDDandEGADays, generateEDDandEGA, generateEGAWeeksDaysLabel, generateGravida, generatePara, getLmpMeasurement)

import Backend.Measurement.Model exposing (..)
import Date.Extra as Date exposing (Interval(Day))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Maybe.Extra exposing (unwrap)
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


generateGravida : Int -> Int -> Bool -> String
generateGravida termPregnancy preTermPregnancy currentlyPregnant =
    let
        total =
            (termPregnancy + preTermPregnancy)
                + (if currentlyPregnant then
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
