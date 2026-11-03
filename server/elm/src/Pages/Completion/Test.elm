module Pages.Completion.Test exposing (all)

import Backend.Completion.Model exposing (..)
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Completion.Utils exposing (applyFilters, customApplyFilters, eliminateEmptyEncounters, spvTakenBy)
import Pages.Utils exposing (calculatePercentage)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


fromDate : NominalDate
fromDate =
    Date.fromCalendarDate 2026 Mar 1


toDate : NominalDate
toDate =
    Date.fromCalendarDate 2026 Mar 31


hivEncounter : NominalDate -> Maybe TakenBy -> List HIVActivity -> EncounterData HIVActivity
hivEncounter date takenBy completed =
    { startDate = date
    , takenBy = takenBy
    , completion = { expectedActivities = [ HIVDiagnostics, HIVFollowUp ], completedActivities = completed }
    }


wellChildEncounter : WellChildEncounterType -> WellChildEncounterData
wellChildEncounter encounterType =
    { startDate = fromDate
    , encounterType = encounterType
    , completion = { expectedActivities = [], completedActivities = [] }
    }


filtersTest : Test
filtersTest =
    describe "the Completion report filters"
        [ test "FROM and TO are both included" <|
            \_ ->
                [ Date.add Date.Days -1 fromDate, fromDate, toDate, Date.add Date.Days 1 toDate ]
                    |> List.map (\date -> hivEncounter date Nothing [])
                    |> applyFilters fromDate toDate Nothing
                    |> List.map .startDate
                    |> Expect.equal [ fromDate, toDate ]
        , test "a taken-by filter keeps only the encounters taken by that role" <|
            \_ ->
                [ Just TakenByNurse, Just TakenByCHW, Just TakenByUnknown, Nothing ]
                    |> List.map (\takenBy -> hivEncounter fromDate takenBy [])
                    |> applyFilters fromDate toDate (Just TakenByNurse)
                    |> List.map .takenBy
                    |> Expect.equal [ Just TakenByNurse ]
        , test "a standard pediatric visit is taken by a nurse, the CHW visit and the newborn exam by a CHW" <|
            \_ ->
                [ PediatricCare, PediatricCareChw, NewbornExam ]
                    |> List.map (wellChildEncounter >> spvTakenBy)
                    |> Expect.equal [ TakenByNurse, TakenByCHW, TakenByCHW ]
        , test "the CHW filter on standard pediatric visits leaves out those taken by a nurse" <|
            \_ ->
                [ PediatricCare, PediatricCareChw ]
                    |> List.map wellChildEncounter
                    |> customApplyFilters fromDate toDate spvTakenBy (Just TakenByCHW)
                    |> List.map .encounterType
                    |> Expect.equal [ PediatricCareChw ]
        , test "an encounter with nothing completed is left out" <|
            \_ ->
                [ hivEncounter fromDate Nothing [], hivEncounter toDate Nothing [ HIVDiagnostics ] ]
                    |> eliminateEmptyEncounters
                    |> List.map .startDate
                    |> Expect.equal [ toDate ]
        ]


percentageTest : Test
percentageTest =
    describe "the completed percentage"
        [ test "3 of 4 is 75.00%" <|
            \_ -> Expect.equal "75.00%" (calculatePercentage 3 4)
        , test "nothing expected is a bare 0" <|
            \_ -> Expect.equal "0" (calculatePercentage 0 0)
        ]


all : Test
all =
    describe "Pages.Completion"
        [ filtersTest
        , percentageTest
        ]
