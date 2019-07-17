module Pages.PrenatalEncounter.Utils exposing (generateEDDandEGA, generateGravida, generatePara)

import Backend.Measurement.Model exposing (..)
import Date.Extra as Date exposing (Interval(Day))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Maybe.Extra exposing (unwrap)
import Translate exposing (Language, translate)


generateEDDandEGA : Language -> NominalDate -> Maybe NominalDate -> ( String, String )
generateEDDandEGA language currentDate maybeLmpDate =
    unwrap
        ( "", "" )
        (\lmpDate ->
            let
                eddDate =
                    toLocalDateTime lmpDate 12 0 0 0
                        |> Date.add Day 280
                        |> fromLocalDateTime

                diffInDays =
                    diffDays lmpDate currentDate

                diffInWeeks =
                    diffInDays // 7

                egaWeeks =
                    translate language <| Translate.WeekSinglePlural diffInWeeks

                egaDays =
                    translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)
            in
            ( formatMMDDYYYY eddDate, egaWeeks ++ ", " ++ egaDays )
        )
        maybeLmpDate


generateGravida : Int -> Int -> String
generateGravida termPregnancy preTermPregnancy =
    let
        total =
            termPregnancy + preTermPregnancy
    in
    if total < 10 then
        "0" ++ toString total

    else
        toString total


generatePara : Int -> Int -> Int -> Int -> String
generatePara termPregnancy preTermPregnancy abortions liveChildren =
    toString termPregnancy ++ toString preTermPregnancy ++ toString abortions ++ toString liveChildren
