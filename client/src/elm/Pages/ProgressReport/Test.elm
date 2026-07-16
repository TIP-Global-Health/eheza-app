module Pages.ProgressReport.Test exposing (all)

import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Date
import Expect
import Pages.ChildScoreboard.ProgressReport.Fetch
import Pages.Nutrition.ProgressReport.Fetch
import Pages.PatientRecord.Fetch
import Pages.ProgressReport.Fetch
import Pages.WellChild.ProgressReport.Fetch
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time


{-| Every progress report renders the Next Appointment pane, which resolves the
health center's name out of `db.healthCenters`. Those are dropped by
CheckDataWanted once they go unwanted for five minutes, so a report page that
never asks for them renders the Location line blank - silently, on a clinical
document that also gets shared over WhatsApp.

Only the WellChild variant used to fetch them. These pin that every page which
renders the report asks for them.

-}
all : Test
all =
    let
        db : ModelIndexedDb
        db =
            emptyModelIndexedDb

        currentDate =
            Date.fromCalendarDate 2026 Time.Jul 14

        asks msgs =
            List.member FetchHealthCenters msgs
    in
    describe "progress report pages fetch the health centers"
        [ test "WellChild (the one that always did)" <|
            \_ ->
                Pages.WellChild.ProgressReport.Fetch.fetch (toEntityUuid "encounter") db
                    |> asks
                    |> Expect.equal True
        , test "Nutrition" <|
            \_ ->
                Pages.Nutrition.ProgressReport.Fetch.fetch (toEntityUuid "encounter") db
                    |> asks
                    |> Expect.equal True
        , test "Child Scoreboard" <|
            \_ ->
                Pages.ChildScoreboard.ProgressReport.Fetch.fetch (toEntityUuid "encounter") db
                    |> asks
                    |> Expect.equal True
        , test "Group (Pages.ProgressReport)" <|
            \_ ->
                Pages.ProgressReport.Fetch.fetch (toEntityUuid "child") db
                    |> Tuple.first
                    |> asks
                    |> Expect.equal True
        , test "Patient Record" <|
            \_ ->
                Pages.PatientRecord.Fetch.fetch currentDate (toEntityUuid "person") db
                    |> asks
                    |> Expect.equal True
        ]
