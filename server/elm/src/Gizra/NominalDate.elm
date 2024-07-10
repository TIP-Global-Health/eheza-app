module Gizra.NominalDate exposing (..)

{-| Some utilities for dealing with "pure" dates that have no time or
time zone information.

@docs NominalDate
@docs decodeYYYYMMDD, encodeYYYYMMDD
@docs formatYYYYMMDD, formatMMDDYYYY
@docs fromLocalDateTime
@docs diffDays, diffCalendarMonthsAndDays


## Ranges

@docs NominalDateRange, decodeDrupalRange, encodeDrupalRange

-}

import Date exposing (..)
import Json.Decode exposing (Decoder, andThen, field, map2, string)
import Json.Decode.Extra exposing (fromResult)
import Json.Encode exposing (Value, object)
import Time exposing (Month(..))


{-| An alias for `Date.Date` from <https://github.com/justinmimbs/date>. Represents
a "pure" date without any time information or time zone information.
-}
type alias NominalDate =
    Date.Date


{-| A range of nominal dates, with a start and end.

Both the start and end date are included in the range.

-}
type alias NominalDateRange =
    { start : NominalDate
    , end : NominalDate
    }


{-| Convert a nominal date to formatted string.

    formatMMDDYYYY (date 2017 5 22) --> "22-05-2017"

-}
formatDDMMYYYY : NominalDate -> String
formatDDMMYYYY =
    customFormatDDMMYYYY "/"


customFormatDDMMYYYY : String -> NominalDate -> String
customFormatDDMMYYYY delimiter =
    Date.format <| "dd" ++ delimiter ++ "MM" ++ delimiter ++ "yyyy"


{-| Convert nominal date to a formatted string..

    formatYYYYMMDD (date 2017 5 2) --> "2017-05-02"

-}
formatYYYYMMDD : NominalDate -> String
formatYYYYMMDD date =
    Date.format "yyyy-MM-dd" date


{-| Convert nominal date to a formatted string..

    formatMMDDYYYY (date 2017 5 22) --> "22/05/17"

-}
formatDDMMYY : NominalDate -> String
formatDDMMYY date =
    Date.format "dd/MM/yy" date


{-| Converts an `elm-lang/core` `Date` to a `NominalDate`.

We pick up the date part according to whatever the local browser's time zone
is. Thus, results will be inconsistent from one locality to the next ... since
the same universal time might be considered one day in one time zone and a
different day in a different time zone.

-}
fromLocalDateTime : Time.Posix -> NominalDate
fromLocalDateTime =
    Date.fromPosix Time.utc


{-| Given a date, return date representing it's last month day.

    toLastDayOfMonth 2019-08-01" --> 2019-08-31"
    toLastDayOfMonth 2017-02-20" --> 2017-02-28"

-}
toLastDayOfMonth : NominalDate -> NominalDate
toLastDayOfMonth =
    Date.floor Month
        >> Date.add Months 1
        >> Date.add Days -1


{-| Decodes nominal date from string of the form "2017-02-20".

    import Json.Decode exposing (..)

    decodeString decodeYYYYMMDD """ "2017-02-20" """ --> Ok (date 2017 02 20)

-}
decodeYYYYMMDD : Decoder NominalDate
decodeYYYYMMDD =
    andThen (fromResult << Date.fromIsoString) string


{-| Encode nominal date to string of the form "2017-02-20".

    import Json.Encode exposing (encode)

    encodeYYYYMMDD (date 2017 2 20)
        |> encode 0 --> "\"2017-02-20\""

-}
encodeYYYYMMDD : NominalDate -> Value
encodeYYYYMMDD =
    Json.Encode.string << formatYYYYMMDD


{-| Given a decoder, Decodes a range as Drupal sends it, with a `value` and `value2`.

    """
        { "value": "2017-07-21"
        , "value2": "2017-07-23"
        }
    """
        |> decodeString (decodeDrupalRange decodeYYYYMMDD)
            --> Ok
            { start = date 2017 7 21
            , end = date 2017 7 23
            }

-}
decodeDrupalRange : Decoder NominalDate -> Decoder NominalDateRange
decodeDrupalRange decoder =
    map2
        (\start end ->
            { start = start
            , end = end
            }
        )
        (field "value" decoder)
        (field "value2" decoder)


{-| Given an encoder, encodes a range as Drupal expects it, with a `value` and `value2`.

    { start = date 2017 07 21
    , end = date 2017 07 23
    }
        |> encodeDrupalRange encodeYYYYMMDD
        |> encode 0
    --> """{"value":"2017-07-21","value2":"2017-07-23"}"""

-}
encodeDrupalRange : (NominalDate -> Value) -> NominalDateRange -> Value
encodeDrupalRange encoder range =
    object <|
        [ ( "value", encoder range.start )
        , ( "value2", encoder range.end )
        ]


{-| Difference in whole days between two dates.

The result is positive if the second parameter is after the first parameter.

    diffDays (date 2017 7 21) (date 2017 7 22) --> 1

    diffDays (date 2017 7 21) (date 2018 7 22) --> 366

-}
diffDays : NominalDate -> NominalDate -> Int
diffDays low high =
    Date.diff Date.Days low high


diffWeeks : NominalDate -> NominalDate -> Int
diffWeeks low high =
    Date.diff Date.Weeks low high


diffMonths : NominalDate -> NominalDate -> Int
diffMonths low high =
    Date.diff Date.Months low high


diffYears : NominalDate -> NominalDate -> Int
diffYears low high =
    Date.diff Date.Years low high


diffCalendarMonths : NominalDate -> NominalDate -> Int
diffCalendarMonths low high =
    diffCalendarMonthsAndDays low high |> .months


{-| Difference between two dates, in terms of months and days. This is based on
calendar months. So, if you're on the same day of the next month, you'd get {
months : 1, days: 0 }. Now, you can't tell from this how many actual deys
it is, because the month might have 28, 30 or 31 days. So, if you need the
actual days, use `diffDays` instead.

The result will be positive if the second parameter is after the first
parameter.

    diffCalendarMonthsAndDays
        (date 2017 07 21)
        (date 2017 07 22)
            --> { months = 0, days = 1 }

    diffCalendarMonthsAndDays
        (date 2017 07 21)
        (date 2017 08 22)
            --> { months = 1, days = 1 }

    diffCalendarMonthsAndDays
        (date 2017 07 21)
        (date 2018 08 23)
            --> { months = 13, days = 2 }

    diffCalendarMonthsAndDays
        (date 2017 07 21)
        (date 2017 09 20)
            --> { months = 1, days = 30 }

    diffCalendarMonthsAndDays
        (date 2017 06 21)
        (date 2017 09 20)
            --> { months = 2, days = 29 }

-}
diffCalendarMonthsAndDays : NominalDate -> NominalDate -> { months : Int, days : Int }
diffCalendarMonthsAndDays low high =
    let
        uncorrected =
            { days = Date.day high - Date.day low
            , months = (Date.year high * 12 + Date.monthNumber high) - (Date.year low * 12 + Date.monthNumber low)
            }
    in
    if uncorrected.days >= 0 then
        -- This is the easy case ... we're at the same day (or further
        -- along) in the target month than the original month, so we're
        -- done ... the answer is some number of full months (however
        -- long they were) and some number of additional days.
        uncorrected

    else
        -- This is the harder case. We're not as far along in our target
        -- month as we were in the original month. So, we need to subtract
        -- 1 from our months, and add something to the (negative) days.
        --
        -- Basically, we want to add however many days there were in the
        -- original month. We're "borrowing" that number of days, to use
        -- the language of subtraction-by-hand. And, it's the original
        -- month that is the "partial" month we're borrowing from ... all
        -- intervening months are full months, and the current month isn't
        -- finished, so it can't matter how many days it has.
        { months = uncorrected.months - 1
        , days = uncorrected.days + daysInMonth (Date.year low) (Date.month low)
        }


diffCalendarYearsAndMonths : NominalDate -> NominalDate -> { years : Int, months : Int }
diffCalendarYearsAndMonths low high =
    let
        months =
            diffCalendarMonthsAndDays low high |> .months

        fullYears =
            months // 12
    in
    { years = fullYears
    , months = months - 12 * fullYears
    }


{-| Indicate of diff of nominal is a Truth value.
-}
isDiffTruthy : NominalDate -> NominalDate -> ({ months : Int, days : Int } -> Bool) -> Bool
isDiffTruthy low high func =
    diffCalendarMonthsAndDays low high
        |> func


daysInMonth : Int -> Month -> Int
daysInMonth y m =
    case m of
        Jan ->
            31

        Feb ->
            if isLeapYear y then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear y =
    modBy 4 y == 0 && modBy 100 y /= 0 || modBy 400 y == 0


{-| A list of all the months, in the traditional order.
-}
allMonths : List Month
allMonths =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Oct
    , Nov
    , Dec
    ]


yearYYNumber : NominalDate -> Int
yearYYNumber date =
    modBy 100 (Date.year date)


sortByDateDesc : (a -> NominalDate) -> a -> a -> Order
sortByDateDesc getDateFunc entity1 entity2 =
    Date.compare (getDateFunc entity2) (getDateFunc entity1)
