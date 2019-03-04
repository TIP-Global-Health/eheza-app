module Activity.Test exposing (all)

import Activity.Model exposing (..)
import Activity.Utils exposing (..)
import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Counseling.Model exposing (..)
import Backend.Entities exposing (ChildId)
import Backend.Measurement.Model exposing (..)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import EverySet
import Expect
import Fixtures exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityId)
import Test exposing (Test, describe, test)
import Time.Date exposing (addDays, date)


all : Test
all =
    describe "Activity tests"
        [ testExpectCounselingActivity
        ]


testExpectCounselingActivity : Test
testExpectCounselingActivity =
    testCases
        |> List.map runTestCase
        |> describe "expectCounselingActivity"


{-| This is the guts of what we're testing ... the rest of the code sets up
the structures that `expectCounselingActivities` works with.
-}
testCases : List TestCase
testCases =
    [ { title = "Already editing, which overrides"
      , daysOld = 65
      , completed = []
      , editing = Just MidPoint
      , expected = Just MidPoint
      }
    , { title = "Not already editing, not completed entry"
      , daysOld = 65
      , completed = []
      , editing = Nothing
      , expected = Just Entry
      }
    , { title = "Completed entry"
      , daysOld = 280
      , completed = [ ( 32, Entry ) ]
      , editing = Nothing
      , expected = Nothing
      }
    , { title = "Reminder for midpoint"
      , daysOld = 300
      , completed = [ ( 30, Entry ) ]
      , editing = Nothing
      , expected = Just BeforeMidpoint
      }
    , -- The case where we've given a reminder, so we follow-up
      { title = "Early midpoint"
      , daysOld = 300
      , completed = [ ( 30, Entry ), ( 270, BeforeMidpoint ) ]
      , editing = Nothing
      , expected = Just MidPoint
      }
    , -- The case with no reminder
      { title = "No reminder midpoint"
      , daysOld = 330
      , completed = [ ( 30, Entry ) ]
      , editing = Nothing
      , expected = Just MidPoint
      }
    , { title = "Post midpoint"
      , daysOld = 650
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , editing = Nothing
      , expected = Nothing
      }
    , { title = "Reminder for exit"
      , daysOld = 660
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , editing = Nothing
      , expected = Just BeforeExit
      }
    , -- The case where we've given a reminder, so we follow-up
      { title = "Early exit"
      , daysOld = 660
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ), ( 630, BeforeExit ) ]
      , editing = Nothing
      , expected = Just Exit
      }
    , -- The case with no reminder
      { title = "No reminder exit"
      , daysOld = 690
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , editing = Nothing
      , expected = Just Exit
      }
    , { title = "Post exit"
      , daysOld = 720
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ), ( 630, BeforeExit ), ( 660, Exit ) ]
      , editing = Nothing
      , expected = Nothing
      }

    -- The next several cases model what we expect when the child enters the
    -- program after 1 year old. (Which is Entry first, then a month of nothing,
    -- then a reminder for midpoint, and then midpoint).
    , { title = "Late entry"
      , daysOld = 400
      , completed = []
      , editing = Nothing
      , expected = Just Entry
      }
    , { title = "Gap until midpoint reminder"
      , daysOld = 430
      , completed = [ ( 400, Entry ) ]
      , editing = Nothing
      , expected = Nothing
      }
    , { title = "Eventually present midpoint reminder"
      , daysOld = 480
      , completed = [ ( 400, Entry ) ]
      , editing = Nothing
      , expected = Just BeforeMidpoint
      }
    , { title = "Eventually present midpoint"
      , daysOld = 520
      , completed = [ ( 400, Entry ) ]
      , editing = Nothing
      , expected = Just MidPoint
      }
    ]


{-| Summarizes the things we want to vary from test to test,
and the expected result.

  - `editing` is `Just` if we've already saved an edit
  - `completed` is a list of which counseling sesssion are already done, and the
    age of the child when they were done.
  - `expected` is the correct answer

-}
type alias TestCase =
    { title : String
    , daysOld : Int
    , completed : List ( Int, CounselingTiming )
    , editing : Maybe CounselingTiming
    , expected : Maybe CounselingTiming
    }


runTestCase : TestCase -> Test
runTestCase testCase =
    test testCase.title <|
        \_ ->
            expectCounselingActivity (makeEditableSession testCase) childId
                |> Expect.equal testCase.expected


makeEditableSession : TestCase -> EditableSession
makeEditableSession test =
    { offlineSession = makeOfflineSession test
    , edits = makeEdits test
    , update = NotAsked
    , childForms = EveryDict.empty -- not relevant
    , motherForms = EveryDict.empty -- not relevant
    }


makeEdits : TestCase -> MeasurementEdits
makeEdits test =
    let
        counseling =
            case test.editing of
                Nothing ->
                    Unedited

                Just timing ->
                    Created (makeCounselingSession sessionDate timing)

        childEdits =
            { emptyChildEdits | counseling = counseling }
    in
    { explicitlyClosed = False
    , mothers = EveryDict.empty
    , children = EveryDict.fromList [ ( childId, childEdits ) ]
    }


makeCounselingSession : NominalDate -> CounselingTiming -> CounselingSession
makeCounselingSession when timing =
    { participantId = childId
    , sessionId = Nothing -- not needed
    , dateMeasured = when
    , value = ( timing, EverySet.empty )
    }


makeOfflineSession : TestCase -> OfflineSession
makeOfflineSession test =
    { session = session sessionDate
    , allSessions = EveryDictList.empty -- not relevant
    , allParticipantForms = EveryDictList.empty -- not relevant
    , everyCounselingSchedule = EveryDict.empty -- not relevant
    , clinics = EveryDictList.empty -- not relevant
    , mothers = EveryDictList.empty -- not relevant
    , children = makeChildren test
    , historicalMeasurements = makeHistoricalMeasurements test
    , currentMeasurements = emptyMeasurements -- not needed
    , previousMeasurements = emptyMeasurements -- not relevant
    }


makeHistoricalMeasurements : TestCase -> HistoricalMeasurements
makeHistoricalMeasurements test =
    { mothers = EveryDict.empty
    , children = EveryDict.fromList [ ( childId, makeChildMeasurementList test ) ]
    }


makeChildMeasurementList : TestCase -> ChildMeasurementList
makeChildMeasurementList test =
    let
        counselingSessions =
            List.map makeCounselingSessionWithId test.completed

        makeCounselingSessionWithId ( daysOld, timing ) =
            ( toEntityId 1
              -- the id doesn't matter
            , makeCounselingSession (addDays (daysOld - test.daysOld) sessionDate) timing
            )
    in
    { emptyChildMeasurementList | counselingSessions = counselingSessions }


{-| We keep the date of the session constant, and vary the other things!
-}
sessionDate : NominalDate
sessionDate =
    date 2018 8 1


session : NominalDate -> Session
session start =
    { scheduledDate =
        { start = start
        , end = start
        }
    , clinicId = toEntityId 1 -- not relevant
    , closed = False
    , training = False
    }


{-| We just need one child ...
-}
childId : ChildId
childId =
    toEntityId 1


makeChildren : TestCase -> EveryDictList ChildId Child
makeChildren test =
    EveryDictList.fromList
        [ ( childId, makeChild test )
        ]


makeChild : TestCase -> Child
makeChild test =
    { name = "Test Child"
    , avatarUrl = Nothing
    , motherId = Nothing -- not relevant
    , birthDate = addDays -test.daysOld sessionDate
    , gender = Male
    }
