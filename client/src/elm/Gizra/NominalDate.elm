module Gizra.NominalDate exposing
    ( NominalDate
    , decodeYYYYMMDD, encodeYYYYMMDD
    , formatYYYYMMDD, formatMMDDYYYY
    , fromLocalDateTime, toLocalDateTime
    , diffDays, diffCalendarMonthsAndDays
    , NominalDateRange, decodeDrupalRange, encodeDrupalRange
    )

{-| Some utilities for dealing with "pure" dates that have no time or
time zone information.

@docs NominalDate
@docs decodeYYYYMMDD, encodeYYYYMMDD
@docs formatYYYYMMDD, formatMMDDYYYY
@docs fromLocalDateTime, toLocalDateTime
@docs diffDays, diffCalendarMonthsAndDays


## Ranges

@docs NominalDateRange, decodeDrupalRange, encodeDrupalRange

-}

import Date
import Date.Extra exposing (Interval(..), diff, fromParts, monthToNumber, numberToMonth)
import Gizra.String exposing (addLeadingZero, addLeadingZeroes)
import Json.Decode exposing (Decoder, andThen, string, map2, field)
import Json.Decode.Extra exposing (fromResult)
import Json.Encode exposing (Value, object)
import Time.Date exposing (day, daysInMonth, delta, month, year)
import Time.Iso8601
import Time.Iso8601ErrorMsg exposing (renderText)


{-| An alias for `Time.Date.Date` from elm-community/elm-time. Represents
a "pure" date without any time information or time zone information.

This is basically to avoid confusion between `Time.Date.Date` and the
`Date.Date` in elm-lang/core.

-}
type alias NominalDate =
    Time.Date.Date


{-| A range of nominal dates, with a start and end.

Both the start and end date are included in the range.

-}
type alias NominalDateRange =
    { start : NominalDate
    , end : NominalDate
    }


{-| Convert a nominal date to formatted string.

    import Time.Date exposing (date)

    formatMMDDYYYY (date 2017 5 2) --> "05/02/2017"

-}
formatMMDDYYYY : NominalDate -> String
formatMMDDYYYY date =
    addLeadingZero (toString (month date)) ++ "/" ++ addLeadingZero (toString (day date)) ++ "/" ++ addLeadingZeroes 4 (toString (year date))


{-| Convert nominal date to a formatted string..

    formatYYYYMMDD (date 2017 5 2) --> "2017-05-02"

-}
formatYYYYMMDD : NominalDate -> String
formatYYYYMMDD date =
    addLeadingZeroes 4 (toString (year date)) ++ "-" ++ addLeadingZero (toString (month date)) ++ "-" ++ addLeadingZero (toString (day date))


{-| Converts an `elm-lang/core` `Date` to a `NominalDate`.

We pick up the date part according to whatever the local browser's time zone
is. Thus, results will be inconsistent from one locality to the next ... since
the same universal time might be considered one day in one time zone and a
different day in a different time zone.

-}
fromLocalDateTime : Date.Date -> NominalDate
fromLocalDateTime date =
    Time.Date.date
        (Date.year date)
        (monthToNumber (Date.month date))
        (Date.day date)


{-| Converts a `NominalDate` to an Elm-core `Date`, with the supplied values
for hour, minute, second and milliseconds (in that order).

The resulting `Date` will be at that time in the local time zone.

-}
toLocalDateTime : NominalDate -> Int -> Int -> Int -> Int -> Date.Date
toLocalDateTime nominal hour minutes seconds milliseconds =
    fromParts
        (year nominal)
        (numberToMonth <| month nominal)
        (day nominal)
        hour
        minutes
        seconds
        milliseconds


{-| Decodes nominal date from string of the form "2017-02-20".

    import Json.Decode exposing (..)

    decodeString decodeYYYYMMDD """ "2017-02-20" """ --> Ok (date 2017 02 20)

-}
decodeYYYYMMDD : Decoder NominalDate
decodeYYYYMMDD =
    andThen (fromResult << Result.mapError renderText << Time.Iso8601.toDate) string


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
    -- delta gives us separate deltas for years, months and days ... so, for
    -- instance, for a difference of 2 years and 1 month, you'd get
    --
    -- { years : 2
    -- , months: 25
    -- , days: 760 -- roughly, depending on which months are involved
    -- }
    delta high low
        |> .days


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
            { days = day high - day low
            , months = (year high * 12 + month high) - (year low * 12 + month low)
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
        , days = uncorrected.days + Maybe.withDefault 0 (daysInMonth (year low) (month low))
        }
