module Backend.Fetch.Test exposing (all)

{-| What forgetting stale fetch data does to an open group session.

The app calls `Backend.Fetch.forget` for every fetch message nobody has asked
for in five minutes. An open group session rebuilds its participants list,
attendance list, activity lists, To Do / Completed counts and measurements from
`db.expectedParticipants` together with `db.people`, `db.childMeasurements`
and `db.motherMeasurements`. These tests check that forgetting the messages of
a child's, an activity row's or a mother's page leaves the data that rebuild
reads for the session's families in memory.

-}

import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Fetch
import Backend.Measurement.Model
    exposing
        ( Attendance
        , ChildMeasurementList
        , FamilyPlanning
        , FamilyPlanningSign(..)
        , GroupMeasurement
        , MotherMeasurementList
        , Weight
        , WeightInKg(..)
        , emptyChildMeasurementList
        , emptyMotherMeasurementList
        )
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), emptyModelIndexedDb)
import Backend.NutritionEncounter.Fetch
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..), PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Session.Model exposing (EditableSession, ExpectedParticipants, Session)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import LocalData exposing (LocalData(..))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testChild, testPerson)
import Time



-- SCENARIO DATA
--
-- One open group session (started today, no end date) with three families:
--
--   * Family A: mother A and child A. Both are checked in, child A has a
--     weight saved in this session and one from an earlier session, mother A
--     has attendance and family planning saved in this session. This is the
--     family whose pages the nurse opened more than five minutes ago.
--   * Family B: mother B and child B, checked in, child B has a weight saved
--     in this session. This is the family the nurse is working on now.
--   * Family C: mother C and child C, expected but nothing recorded yet. Both
--     measurement lists are loaded and empty.
--
-- A stranger, who is not expected at any session, also has data in memory;
-- that data is still forgotten as before.


today : NominalDate
today =
    Date.fromCalendarDate 2026 Time.Nov 15


earlierSessionDate : NominalDate
earlierSessionDate =
    Date.fromCalendarDate 2026 Time.Oct 15


sessionId : SessionId
sessionId =
    toEntityUuid "session-open"


earlierSessionId : SessionId
earlierSessionId =
    toEntityUuid "session-earlier"


clinicId : ClinicId
clinicId =
    toEntityUuid "clinic"


motherAId : PersonId
motherAId =
    toEntityUuid "mother-a"


childAId : PersonId
childAId =
    toEntityUuid "child-a"


motherBId : PersonId
motherBId =
    toEntityUuid "mother-b"


childBId : PersonId
childBId =
    toEntityUuid "child-b"


motherCId : PersonId
motherCId =
    toEntityUuid "mother-c"


childCId : PersonId
childCId =
    toEntityUuid "child-c"


strangerId : PersonId
strangerId =
    toEntityUuid "stranger"


childIds : List PersonId
childIds =
    [ childAId, childBId, childCId ]


motherIds : List PersonId
motherIds =
    [ motherAId, motherBId, motherCId ]


session : Session
session =
    { startDate = today
    , endDate = Nothing
    , clinicId = clinicId
    , clinicType = Pmtct
    , deleted = False
    }


mother : String -> Person
mother name =
    { testPerson | name = name, firstName = name }


child : String -> Person
child name =
    let
        base =
            testChild (Date.fromCalendarDate 2025 Time.Jun 1)
    in
    { base | name = name, firstName = name }


peopleById : Dict PersonId Person
peopleById =
    Dict.fromList
        [ ( motherAId, mother "Mother A" )
        , ( childAId, child "Child A" )
        , ( motherBId, mother "Mother B" )
        , ( childBId, child "Child B" )
        , ( motherCId, mother "Mother C" )
        , ( childCId, child "Child C" )
        , ( strangerId, child "Stranger" )
        ]


participant : PersonId -> PersonId -> PmtctParticipant
participant motherId childId =
    { child = childId
    , adult = motherId
    , adultActivities = MotherActivities
    , start = Date.fromCalendarDate 2025 Time.Jun 1
    , end = Nothing
    , clinic = clinicId
    , deleted = False
    }


expectedParticipants : ExpectedParticipants
expectedParticipants =
    let
        entries =
            [ ( toEntityUuid "pmtct-a", participant motherAId childAId )
            , ( toEntityUuid "pmtct-b", participant motherBId childBId )
            , ( toEntityUuid "pmtct-c", participant motherCId childCId )
            ]
    in
    { byId = Dict.fromList entries
    , byChildId = Dict.fromList (List.map (\( _, p ) -> ( p.child, [ p ] )) entries)
    , byMotherId = Dict.fromList (List.map (\( _, p ) -> ( p.adult, [ p ] )) entries)
    }


groupMeasurement : PersonId -> SessionId -> NominalDate -> value -> GroupMeasurement value
groupMeasurement personId inSession date value =
    { dateMeasured = date
    , nurse = Nothing
    , healthCenter = Nothing
    , participantId = personId
    , deleted = False
    , encounterId = Just inSession
    , value = value
    }


{-| The weight saved for child A in this session, the one a correction must
update rather than duplicate.
-}
weightAToday : WeightId
weightAToday =
    toEntityUuid "weight-a-today"


weightAEarlier : WeightId
weightAEarlier =
    toEntityUuid "weight-a-earlier"


childAWeights : Dict WeightId Weight
childAWeights =
    Dict.fromList
        [ ( weightAToday, groupMeasurement childAId sessionId today (WeightInKg 9.4) )
        , ( weightAEarlier, groupMeasurement childAId earlierSessionId earlierSessionDate (WeightInKg 8.9) )
        ]


childMeasurementsById : Dict PersonId ChildMeasurementList
childMeasurementsById =
    Dict.fromList
        [ ( childAId, { emptyChildMeasurementList | weights = childAWeights } )
        , ( childBId
          , { emptyChildMeasurementList
                | weights = Dict.fromList [ ( toEntityUuid "weight-b-today", groupMeasurement childBId sessionId today (WeightInKg 10.1) ) ]
            }
          )
        , ( childCId, emptyChildMeasurementList )
        , ( strangerId, emptyChildMeasurementList )
        ]


attendance : PersonId -> Dict AttendanceId Attendance
attendance motherId =
    Dict.fromList [ ( toEntityUuid ("attendance-" ++ Restful.Endpoint.fromEntityUuid motherId), groupMeasurement motherId sessionId today True ) ]


motherAFamilyPlannings : Dict FamilyPlanningId FamilyPlanning
motherAFamilyPlannings =
    Dict.fromList [ ( toEntityUuid "family-planning-a", groupMeasurement motherAId sessionId today (EverySet.fromList [ Condoms ]) ) ]


motherMeasurementsById : Dict PersonId MotherMeasurementList
motherMeasurementsById =
    Dict.fromList
        [ ( motherAId, { emptyMotherMeasurementList | attendances = attendance motherAId, familyPlannings = motherAFamilyPlannings } )
        , ( motherBId, { emptyMotherMeasurementList | attendances = attendance motherBId } )
        , ( motherCId, emptyMotherMeasurementList )
        ]


editableSession : EditableSession
editableSession =
    { offlineSession =
        { session = session
        , allParticipantForms = Dict.empty
        , everyCounselingSchedule = Dict.empty
        , participants = expectedParticipants
        , mothers = Dict.filter (\id _ -> List.member id motherIds) peopleById
        , children = Dict.filter (\id _ -> List.member id childIds) peopleById
        , measurements = NotNeeded
        }
    , saveRequests = Backend.Session.Model.emptyModel
    , checkedIn = NotNeeded
    , summaryByParticipant = NotNeeded
    , summaryByActivity = NotNeeded
    }


{-| Everything in memory while the session is open on screen and every page
of the session has been shown at least once.
-}
db : ModelIndexedDb
db =
    { emptyModelIndexedDb
        | sessions = Dict.fromList [ ( sessionId, Success session ) ]
        , editableSessions = Dict.fromList [ ( sessionId, Success editableSession ) ]
        , expectedParticipants = Dict.fromList [ ( sessionId, Success expectedParticipants ) ]
        , people = Dict.map (\_ person -> Success person) peopleById
        , childMeasurements = Dict.map (\_ list -> Success list) childMeasurementsById
        , motherMeasurements = Dict.map (\_ list -> Success list) motherMeasurementsById
        , relationshipsByPerson =
            Dict.fromList
                [ ( childAId, Success (Dict.fromList [ ( toEntityUuid "relationship-a", { relatedTo = motherAId, relatedBy = MyParent } ) ]) )
                , ( childCId, Success (Dict.fromList [ ( toEntityUuid "relationship-c", { relatedTo = motherCId, relatedBy = MyParent } ) ]) )
                ]
    }



-- HELPERS


forgetAll : List MsgIndexedDb -> ModelIndexedDb -> ModelIndexedDb
forgetAll msgs model =
    List.foldl Backend.Fetch.forget model msgs


{-| The data messages a child's page asks for, which is also what an activity
page asks for about the child whose row it shows. With the child's parent known,
this includes `FetchPerson` for the mother.
-}
childPageMsgs : PersonId -> List MsgIndexedDb
childPageMsgs childId =
    Backend.NutritionEncounter.Fetch.fetch childId db


{-| The messages a mother's page or her Patient Record ask for.
-}
motherPageMsgs : PersonId -> List MsgIndexedDb
motherPageMsgs motherId =
    [ FetchPerson motherId, FetchMotherMeasurements motherId ]


{-| Five minutes after the nurse left child A's page: every message only that
page asked for is forgotten.
-}
afterChildAPageForgotten : ModelIndexedDb
afterChildAPageForgotten =
    forgetAll (childPageMsgs childAId) db


expectedPerson : PersonId -> Maybe (WebData Person)
expectedPerson id =
    Dict.get id peopleById |> Maybe.map Success


expectedChildMeasurements : PersonId -> Maybe (WebData ChildMeasurementList)
expectedChildMeasurements id =
    Dict.get id childMeasurementsById |> Maybe.map Success


expectedMotherMeasurements : PersonId -> Maybe (WebData MotherMeasurementList)
expectedMotherMeasurements id =
    Dict.get id motherMeasurementsById |> Maybe.map Success


{-| The `Person` records an open session's rebuild reads for every child and
mother it expects, keyed by id.
-}
sessionPeople : ModelIndexedDb -> List ( PersonId, Maybe (WebData Person) )
sessionPeople model =
    List.map (\id -> ( id, Dict.get id model.people )) (childIds ++ motherIds)


expectedSessionPeople : List ( PersonId, Maybe (WebData Person) )
expectedSessionPeople =
    List.map (\id -> ( id, expectedPerson id )) (childIds ++ motherIds)


{-| This session's weights for a child, as the child's page reads them.
-}
weightsInSession : PersonId -> ModelIndexedDb -> List WeightId
weightsInSession childId model =
    Dict.get childId model.childMeasurements
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map (.weights >> Dict.filter (\_ weight -> weight.encounterId == Just sessionId) >> Dict.keys)
        |> Maybe.withDefault []



-- CRITERIA


childStaysListedTest : Test
childStaysListedTest =
    test "After saving a group measurement for another child, the first child and their mother are still listed on the participants page, the attendance page and every activity list" <|
        \_ ->
            afterChildAPageForgotten
                |> Expect.all
                    [ \model -> Dict.get childAId model.people |> Expect.equal (expectedPerson childAId)
                    , \model -> Dict.get motherAId model.people |> Expect.equal (expectedPerson motherAId)
                    ]


downloadRebuildTest : Test
downloadRebuildTest =
    -- A sync download that brings a group measurement rebuilds the session
    -- from the same memory as a local save does. The seam cannot tell the two
    -- apart, so this checks what that rebuild reads for every family.
    test "The same holds when the group measurement comes from another device in a sync download, instead of a local save" <|
        \_ ->
            sessionPeople afterChildAPageForgotten
                |> Expect.equal expectedSessionPeople


activityRowTest : Test
activityRowTest =
    -- Working activity by activity, the activity page asks for the data of
    -- the child whose row it shows, the same messages as the child's page.
    test "The same holds for a child whose row was shown on an activity page (working activity by activity)" <|
        \_ ->
            forgetAll (childPageMsgs childAId) db
                |> Expect.all
                    [ \model -> Dict.get childAId model.people |> Expect.equal (expectedPerson childAId)
                    , \model -> Dict.get motherAId model.people |> Expect.equal (expectedPerson motherAId)
                    , \model -> Dict.get childAId model.childMeasurements |> Expect.equal (expectedChildMeasurements childAId)
                    ]


reopenShowsSavedTest : Test
reopenShowsSavedTest =
    test "Reopening that child more than five minutes later shows the measurements already saved for them in this session as recorded" <|
        \_ ->
            Dict.get childAId afterChildAPageForgotten.childMeasurements
                |> Expect.equal (expectedChildMeasurements childAId)


correctionUpdatesSavedTest : Test
correctionUpdatesSavedTest =
    -- A correction edits the measurement the page knows about. If the saved
    -- weight is no longer known, Save either posts a second weight or does
    -- nothing.
    test "Correcting one of those measurements updates the saved one: Save responds, and the child has no second measurement of that type in this session" <|
        \_ ->
            weightsInSession childAId afterChildAPageForgotten
                |> Expect.equal [ weightAToday ]


motherStaysListedTest : Test
motherStaysListedTest =
    test "A mother whose page was opened more than five minutes earlier stays listed too, with her saved measurements shown" <|
        \_ ->
            forgetAll (motherPageMsgs motherAId) db
                |> Expect.all
                    [ \model -> Dict.get motherAId model.people |> Expect.equal (expectedPerson motherAId)
                    , \model -> Dict.get motherAId model.motherMeasurements |> Expect.equal (expectedMotherMeasurements motherAId)
                    ]


countsKeepFamiliesTest : Test
countsKeepFamiliesTest =
    -- The nurse has opened every page of family A (mother page and child
    -- page) and moved on. The counts are built from people and measurements
    -- of every expected participant, so all of it must still be there.
    test "The To Do and Completed counts keep including those families" <|
        \_ ->
            let
                model =
                    forgetAll (motherPageMsgs motherAId ++ childPageMsgs childAId) db
            in
            ( sessionPeople model
            , List.map (\id -> ( id, Dict.get id model.childMeasurements )) childIds
            , List.map (\id -> ( id, Dict.get id model.motherMeasurements )) motherIds
            )
                |> Expect.equal
                    ( expectedSessionPeople
                    , List.map (\id -> ( id, expectedChildMeasurements id )) childIds
                    , List.map (\id -> ( id, expectedMotherMeasurements id )) motherIds
                    )



-- BOUNDARIES


forgottenAgainTest : Test
forgottenAgainTest =
    -- The nurse reopens child A, leaves again and another five minutes pass.
    test "boundary: forgetting the same child's page a second time still keeps the family" <|
        \_ ->
            forgetAll (childPageMsgs childAId) afterChildAPageForgotten
                |> Expect.all
                    [ \model -> Dict.get childAId model.people |> Expect.equal (expectedPerson childAId)
                    , \model -> Dict.get motherAId model.people |> Expect.equal (expectedPerson motherAId)
                    , \model -> Dict.get childAId model.childMeasurements |> Expect.equal (expectedChildMeasurements childAId)
                    ]


nothingRecordedYetTest : Test
nothingRecordedYetTest =
    -- Family C is expected but has nothing recorded. Their loaded, empty
    -- measurement lists are what the first save for them is added to.
    test "boundary: a family with nothing recorded yet keeps its people and its empty measurement lists" <|
        \_ ->
            forgetAll (childPageMsgs childCId ++ motherPageMsgs motherCId) db
                |> Expect.all
                    [ \model -> Dict.get childCId model.people |> Expect.equal (expectedPerson childCId)
                    , \model -> Dict.get motherCId model.people |> Expect.equal (expectedPerson motherCId)
                    , \model -> Dict.get childCId model.childMeasurements |> Expect.equal (Just (Success emptyChildMeasurementList))
                    , \model -> Dict.get motherCId model.motherMeasurements |> Expect.equal (Just (Success emptyMotherMeasurementList))
                    ]


earlierSessionWeightKeptTest : Test
earlierSessionWeightKeptTest =
    -- Child A's earlier-session weight is what this session's weight is
    -- compared with; it lives in the same list and must stay with it.
    test "boundary: the child's measurements from earlier sessions stay alongside this session's" <|
        \_ ->
            Dict.get childAId afterChildAPageForgotten.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map (.weights >> Dict.keys >> List.sortBy Restful.Endpoint.fromEntityUuid)
                |> Expect.equal (Just [ weightAEarlier, weightAToday ])


otherFamiliesUntouchedTest : Test
otherFamiliesUntouchedTest =
    test "guard: forgetting family A's pages leaves family B's data as it was" <|
        \_ ->
            afterChildAPageForgotten
                |> Expect.all
                    [ \model -> Dict.get childBId model.people |> Expect.equal (expectedPerson childBId)
                    , \model -> Dict.get motherBId model.people |> Expect.equal (expectedPerson motherBId)
                    , \model -> Dict.get childBId model.childMeasurements |> Expect.equal (expectedChildMeasurements childBId)
                    , \model -> Dict.get motherBId model.motherMeasurements |> Expect.equal (expectedMotherMeasurements motherBId)
                    ]


strangerStillForgottenTest : Test
strangerStillForgottenTest =
    -- Someone expected at no session in memory: their data goes as before,
    -- so the app does not keep everything it ever loaded.
    test "guard: a person not expected at any session in memory is still forgotten" <|
        \_ ->
            forgetAll [ FetchPerson strangerId, FetchChildMeasurements strangerId ] db
                |> Expect.all
                    [ \model -> Dict.get strangerId model.people |> Expect.equal Nothing
                    , \model -> Dict.get strangerId model.childMeasurements |> Expect.equal Nothing
                    ]


all : Test
all =
    describe "Backend.Fetch.forget in an open group session"
        [ describe "criteria"
            [ childStaysListedTest
            , downloadRebuildTest
            , activityRowTest
            , reopenShowsSavedTest
            , correctionUpdatesSavedTest
            , motherStaysListedTest
            , countsKeepFamiliesTest
            ]
        , describe "boundaries"
            [ forgottenAgainTest
            , nothingRecordedYetTest
            , earlierSessionWeightKeptTest
            , otherFamiliesUntouchedTest
            , strangerStillForgottenTest
            ]
        ]
