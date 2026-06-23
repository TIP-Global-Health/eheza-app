module Gizra.NominalDate.Test exposing (all)

import Date
import Expect
import Gizra.NominalDate exposing (fromLocalDateTime)
import Test exposing (Test, describe, test)
import Time


{-| 1970-01-01T23:30:00Z — 23h30m after the epoch, i.e. late evening in UTC.
-}
lateEveningUtc : Time.Posix
lateEveningUtc =
    Time.millisToPosix ((23 * 3600 + 30 * 60) * 1000)


all : Test
all =
    describe "Gizra.NominalDate.fromLocalDateTime"
        [ test "takes the calendar day as seen in UTC" <|
            \_ ->
                fromLocalDateTime Time.utc lateEveningUtc
                    |> Date.toIsoString
                    |> Expect.equal "1970-01-01"
        , test "a UTC+2 zone rolls a late-evening UTC instant into the next local day" <|
            -- The regression this fix addresses: in Rwanda/Burundi (UTC+2) a
            -- moment just before UTC midnight is already the next calendar day
            -- locally, so it must stamp that day -- not the UTC one.
            \_ ->
                fromLocalDateTime (Time.customZone 120 []) lateEveningUtc
                    |> Date.toIsoString
                    |> Expect.equal "1970-01-02"
        ]
