module Activity.Test exposing (all)

import Activity.Model exposing (..)
import Activity.Utils exposing (..)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (..)
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import EverySet
import Expect
import Fixtures exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Time.Date exposing (addDays, date)


{-| These tests are disabled for now -- they relied on an exposed way of making
and editable session, which is not exposed any longer.
-}
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
    [ { title = "Not completed entry"
      , daysOld = 65
      , completed = []
      , expected = Just Entry
      }
    , { title = "Completed entry"
      , daysOld = 280
      , completed = [ ( 32, Entry ) ]
      , expected = Nothing
      }
    , { title = "Reminder for midpoint"
      , daysOld = 300
      , completed = [ ( 30, Entry ) ]
      , expected = Just BeforeMidpoint
      }
    , -- The case where we've given a reminder, so we follow-up
      { title = "Early midpoint"
      , daysOld = 300
      , completed = [ ( 30, Entry ), ( 270, BeforeMidpoint ) ]
      , expected = Just MidPoint
      }
    , -- The case with no reminder
      { title = "No reminder midpoint"
      , daysOld = 330
      , completed = [ ( 30, Entry ) ]
      , expected = Just MidPoint
      }
    , { title = "Post midpoint"
      , daysOld = 650
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , expected = Nothing
      }
    , { title = "Reminder for exit"
      , daysOld = 660
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , expected = Just BeforeExit
      }
    , -- The case where we've given a reminder, so we follow-up
      { title = "Early exit"
      , daysOld = 660
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ), ( 630, BeforeExit ) ]
      , expected = Just Exit
      }
    , -- The case with no reminder
      { title = "No reminder exit"
      , daysOld = 690
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ) ]
      , expected = Just Exit
      }
    , { title = "Post exit"
      , daysOld = 720
      , completed = [ ( 30, Entry ), ( 300, BeforeMidpoint ), ( 330, MidPoint ), ( 630, BeforeExit ), ( 660, Exit ) ]
      , expected = Nothing
      }

    -- The next several cases model what we expect when the child enters the
    -- program after 1 year old. (Which is Entry first, then a month of nothing,
    -- then a reminder for midpoint, and then midpoint).
    , { title = "Late entry"
      , daysOld = 400
      , completed = []
      , expected = Just Entry
      }
    , { title = "Gap until midpoint reminder"
      , daysOld = 430
      , completed = [ ( 400, Entry ) ]
      , expected = Nothing
      }
    , { title = "Eventually present midpoint reminder"
      , daysOld = 480
      , completed = [ ( 400, Entry ) ]
      , expected = Just BeforeMidpoint
      }
    , { title = "Eventually present midpoint"
      , daysOld = 520
      , completed = [ ( 400, Entry ) ]
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
    , expected : Maybe CounselingTiming
    }


runTestCase : TestCase -> Test
runTestCase testCase =
    test testCase.title <|
        \_ ->
            Expect.pass



--   expectCounselingActivity (makeEditableSession testCase) childId
--       |> Expect.equal testCase.expected
{-
   makeEditableSession : TestCase -> EditableSession
   makeEditableSession test =
       { offlineSession = makeOfflineSession test
       , update = NotAsked
       }
-}


makeCounselingSession : NominalDate -> CounselingTiming -> CounselingSession
makeCounselingSession when timing =
    { participantId = childId
    , encounterId = Nothing -- not needed
    , dateMeasured = when
    , value = ( timing, EverySet.empty )
    , nurse = Nothing
    , healthCenter = Nothing
    }



{-
   makeOfflineSession : TestCase -> OfflineSession
   makeOfflineSession test =
       { session = session sessionDate
       , allParticipantForms = Dict.empty -- not relevant
       , everyCounselingSchedule = Dict.empty -- not relevant
       , participants =
           { byId = Dict.empty
           , byChildId = Dict.empty
           , byMotherId = Dict.empty
           }
       , mothers = Dict.empty -- not relevant
       , children = makeChildren test
       , historicalMeasurements = makeHistoricalMeasurements test
       , currentMeasurements = emptyMeasurements -- not needed
       , previousMeasurements = emptyMeasurements -- not relevant
       }
-}


makeHistoricalMeasurements : TestCase -> HistoricalMeasurements
makeHistoricalMeasurements test =
    { mothers = Dict.empty
    , children = Dict.fromList [ ( childId, makeChildMeasurementList test ) ]
    }


makeChildMeasurementList : TestCase -> ChildMeasurementList
makeChildMeasurementList test =
    let
        counselingSessions =
            List.map makeCounselingSessionWithId test.completed
                |> Dict.fromList

        makeCounselingSessionWithId ( daysOld, timing ) =
            -- We need a locally unique ID, but it doesn't need to be real.
            ( toEntityUuid (Debug.toString ( daysOld, timing ))
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
    { startDate = start
    , endDate = Nothing
    , clinicId = toEntityUuid "1" -- not relevant
    , clinicType = Pmtct
    }


{-| We just need one child ...
-}
childId : PersonId
childId =
    toEntityUuid "1"


makeChildren : TestCase -> Dict PersonId Person
makeChildren test =
    Dict.fromList
        [ ( childId, makeChild test )
        ]


makeChild : TestCase -> Person
makeChild test =
    { name = "Test Child"
    , avatarUrl = Nothing
    , birthDate = Just <| addDays -test.daysOld sessionDate
    , gender = Male
    , cell = Nothing
    , district = Nothing
    , firstName = ""
    , hivStatus = Nothing
    , modeOfDelivery = Nothing
    , numberOfChildren = Nothing
    , educationLevel = Nothing
    , maritalStatus = Nothing
    , isDateOfBirthEstimated = False
    , nationalIdNumber = Nothing
    , province = Nothing
    , secondName = ""
    , sector = Nothing
    , telephoneNumber = Nothing
    , ubudehe = Nothing
    , village = Nothing
    , healthCenterId = Nothing
    , hmisNumber = Nothing
    }
