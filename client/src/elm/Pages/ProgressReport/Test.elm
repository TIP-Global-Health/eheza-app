module Pages.ProgressReport.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model exposing (emptyChildMeasurementList, emptyNutritionValue)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Date
import Expect
import Pages.ChildScoreboard.ProgressReport.Fetch
import Pages.Nutrition.ProgressReport.Fetch
import Pages.PatientRecord.Fetch
import Pages.ProgressReport.Fetch
import Pages.WellChild.ProgressReport.Fetch
import Pages.WellChild.ProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChild.ProgressReport.View exposing (resolveDateOfLastNutritionAssessment)
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
healthCenterFetchTests : Test
healthCenterFetchTests =
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
                Pages.ProgressReport.Fetch.fetch (toEntityUuid "session") (toEntityUuid "child") db
                    |> Tuple.first
                    |> asks
                    |> Expect.equal True
        , test "Patient Record" <|
            \_ ->
                Pages.PatientRecord.Fetch.fetch currentDate (toEntityUuid "person") db
                    |> asks
                    |> Expect.equal True
        ]


{-| The group report reads whether today's activities are complete from the
session's measurements. After a reload or a sync download nothing else on the
page asks for them, and today's assessment shows as resolved.
-}
sessionMeasurementsFetchTest : Test
sessionMeasurementsFetchTest =
    test "the group report asks for the session's measurements" <|
        \_ ->
            let
                sessionId =
                    toEntityUuid "session"
            in
            Pages.ProgressReport.Fetch.fetch sessionId (toEntityUuid "child") emptyModelIndexedDb
                |> Tuple.second
                |> List.member (FetchEditableSessionMeasurements sessionId)
                |> Expect.equal True


{-| Which day's assessment is Active on the group report. "Taken" is whether
the mandatory activities of the session the report was opened from are complete.
-}
lastAssessmentDateTests : Test
lastAssessmentDateTests =
    let
        today =
            Date.fromCalendarDate 2026 Time.Nov 9

        lastMonth =
            Date.fromCalendarDate 2026 Time.Oct 10

        nutritionOn date =
            ( toEntityUuid (Date.toIsoString date)
            , { dateMeasured = date
              , nurse = Nothing
              , healthCenter = Nothing
              , participantId = toEntityUuid "child"
              , deleted = False
              , encounterId = Nothing
              , value = emptyNutritionValue
              }
            )

        lastAssessmentDate taken dates =
            resolveDateOfLastNutritionAssessment today
                (InitiatorNutritionGroup (toEntityUuid "session") (toEntityUuid "child"))
                taken
                []
                []
                { emptyChildMeasurementList | nutritions = Dict.fromList (List.map nutritionOn dates) }
    in
    describe "the assessment day that is Active on the group report"
        [ test "today, when today's activities are complete" <|
            \_ ->
                lastAssessmentDate True [ lastMonth, today ]
                    |> Expect.equal (Just today)
        , test "an earlier day, while today's activities are not complete" <|
            \_ ->
                lastAssessmentDate False [ lastMonth, today ]
                    |> Expect.equal (Just lastMonth)
        , test "the session's own day, when it was held on an earlier day" <|
            \_ ->
                lastAssessmentDate True [ lastMonth ]
                    |> Expect.equal (Just lastMonth)
        , test "today, when an earlier day's session was completed today" <|
            \_ ->
                lastAssessmentDate True [ today ]
                    |> Expect.equal (Just today)
        , test "nothing, for a child with no assessment" <|
            \_ ->
                lastAssessmentDate True []
                    |> Expect.equal Nothing
        ]


all : Test
all =
    describe "progress report pages"
        [ healthCenterFetchTests
        , lastAssessmentDateTests
        , sessionMeasurementsFetchTest
        ]
