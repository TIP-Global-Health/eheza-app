module Backend.Scoreboard.Test exposing (all)

import Backend.Scoreboard.Decoder exposing (decodeSyncResponse)
import Backend.Scoreboard.Model exposing (CriterionBySeverities, PatientData)
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (decodeString, errorToString)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


{-| Decodes one patient, given the JSON fields that follow the required
dates.
-}
decodePatient : String -> Result String PatientData
decodePatient fields =
    ("{\"data\": {\"total_remaining\": 0, \"last\": 1, \"batch\": [{\"created\": \"2024-01-01\", \"birth_date\": \"2024-01-01\", \"edd_date\": \"2024-01-01\"" ++ fields ++ "}]}}")
        |> decodeString decodeSyncResponse
        |> Result.mapError errorToString
        |> Result.andThen (.records >> List.head >> Result.fromMaybe "no record")


{-| The stunting values kept from the given severity lists, each sorted.
-}
stunting : String -> Result String CriterionBySeverities
stunting severities =
    decodePatient (", \"nutrition\": {\"stunting\": {" ++ severities ++ "}, \"underweight\": {}, \"wasting\": {}, \"muac\": {}}")
        |> Result.map
            (\patient ->
                let
                    sorted =
                        List.sortBy Date.toRataDie
                in
                { severe = sorted patient.nutrition.stunting.severe
                , moderate = sorted patient.nutrition.stunting.moderate
                , normal = sorted patient.nutrition.stunting.normal
                }
            )


{-| The dates kept from the given NCDA monthly row.
-}
monthlyRow : String -> Result String (List NominalDate)
monthlyRow row =
    decodePatient (", \"ncda\": {\"pane2\": {\"row2\": " ++ row ++ "}}")
        |> Result.map (.ncda >> .universalIntervention >> .row2 >> List.sortBy Date.toRataDie)


day : Month -> Int -> NominalDate
day =
    Date.fromCalendarDate 2025


monthBucketingTest : Test
monthBucketingTest =
    describe "decodeSyncResponse keeps one value per calendar month"
        [ test "severe wins over moderate and normal in the same month" <|
            \_ ->
                stunting "\"severe\": [\"2025-07-05\"], \"moderate\": [\"2025-07-10\"], \"normal\": [\"2025-07-20\"]"
                    |> Expect.equal (Ok { severe = [ day Jul 5 ], moderate = [], normal = [] })
        , test "moderate wins over normal in the same month" <|
            \_ ->
                stunting "\"moderate\": [\"2025-07-10\"], \"normal\": [\"2025-07-20\"]"
                    |> Expect.equal (Ok { severe = [], moderate = [ day Jul 10 ], normal = [] })
        , test "a normal value in the next month stays" <|
            \_ ->
                stunting "\"severe\": [\"2025-07-05\"], \"normal\": [\"2025-08-03\"]"
                    |> Expect.equal (Ok { severe = [ day Jul 5 ], moderate = [], normal = [ day Aug 3 ] })
        , test "31 July and 1 August are two months" <|
            \_ ->
                stunting "\"severe\": [\"2025-07-31\"], \"normal\": [\"2025-08-01\"]"
                    |> Expect.equal (Ok { severe = [ day Jul 31 ], moderate = [], normal = [ day Aug 1 ] })
        , test "two values of one severity in a month keep the one listed last" <|
            \_ ->
                stunting "\"normal\": [\"2025-08-03\", \"2025-08-20\"]"
                    |> Expect.equal (Ok { severe = [], moderate = [], normal = [ day Aug 20 ] })
        , test "a missing severity is empty" <|
            \_ ->
                stunting ""
                    |> Expect.equal (Ok { severe = [], moderate = [], normal = [] })
        , test "an NCDA row keeps one date per month" <|
            \_ ->
                monthlyRow "[\"2025-07-01\", \"2025-07-15\", \"2025-08-01\"]"
                    |> Expect.equal (Ok [ day Jul 15, day Aug 1 ])
        , test "a missing NCDA row is empty" <|
            \_ ->
                decodePatient ""
                    |> Result.map (.ncda >> .universalIntervention >> .row2)
                    |> Expect.equal (Ok [])
        , test "a value that is not a date fails the batch" <|
            \_ ->
                monthlyRow "[false]"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


all : Test
all =
    describe "Backend.Scoreboard"
        [ monthBucketingTest ]
