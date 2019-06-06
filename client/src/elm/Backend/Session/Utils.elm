module Backend.Session.Utils exposing (emptyMotherMeasurementData, getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMyMother, isAuthorized, isClosed, makeEditableSession)

import AllDict
import AllDictList
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)


{-| Given a mother's id, get all her children from the offline session.
-}
getChildren : PersonId -> OfflineSession -> List ( PersonId, Person )
getChildren motherId session =
    AllDict.get motherId session.participants.byMotherId
        |> Maybe.withDefault []
        |> List.filterMap
            (\participant ->
                AllDictList.get participant.child session.children
                    |> Maybe.map (\child -> ( participant.child, child ))
            )


getChild : PersonId -> OfflineSession -> Maybe Person
getChild childId session =
    AllDictList.get childId session.children


getMother : PersonId -> OfflineSession -> Maybe Person
getMother motherId session =
    AllDictList.get motherId session.mothers


getMyMother : PersonId -> OfflineSession -> Maybe ( PersonId, Person )
getMyMother childId session =
    AllDict.get childId session.participants.byChildId
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.andThen
            (\participant ->
                AllDictList.get participant.adult session.mothers
                    |> Maybe.map (\person -> ( participant.adult, person ))
            )


getChildHistoricalMeasurements : PersonId -> OfflineSession -> ChildMeasurementList
getChildHistoricalMeasurements childId session =
    AllDict.get childId session.historicalMeasurements.children
        |> Maybe.withDefault emptyChildMeasurementList


getMotherHistoricalMeasurements : PersonId -> OfflineSession -> MotherMeasurementList
getMotherHistoricalMeasurements motherId session =
    AllDict.get motherId session.historicalMeasurements.mothers
        |> Maybe.withDefault emptyMotherMeasurementList


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : PersonId -> EditableSession -> MeasurementData ChildMeasurements
getChildMeasurementData childId session =
    { current =
        AllDict.get childId session.offlineSession.currentMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , previous =
        AllDict.get childId session.offlineSession.previousMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , update = session.update
    }


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getMotherMeasurementData : PersonId -> EditableSession -> MeasurementData MotherMeasurements
getMotherMeasurementData motherId session =
    { current =
        AllDict.get motherId session.offlineSession.currentMeasurements.mothers
            |> Maybe.withDefault emptyMotherMeasurements
    , previous =
        AllDict.get motherId session.offlineSession.previousMeasurements.mothers
            |> Maybe.withDefault emptyMotherMeasurements
    , update = session.update
    }


emptyMotherMeasurementData : EditableSession -> MeasurementData MotherMeasurements
emptyMotherMeasurementData session =
    { current = emptyMotherMeasurements
    , previous = emptyMotherMeasurements
    , update = session.update
    }


{-| Construct an EditableSession from our data, if we have all the needed data.

This is a convenience, because so many functions work in terms of an
EditableSession. In future, we might refactor that, or it might prove to
continue to be convenient. (It's probably not efficient to calculate all of
this on the fly every time, but it's much easier for now to work within
existing types).

-}
makeEditableSession : SessionId -> ModelIndexedDb -> WebData EditableSession
makeEditableSession sessionId db =
    let
        sessionData =
            AllDict.get sessionId db.sessions
                |> Maybe.withDefault NotAsked

        allParticipantFormsData =
            db.participantForms

        everyCounselingScheduleData =
            db.everyCounselingSchedule

        participantsData =
            AllDict.get sessionId db.expectedParticipants
                |> Maybe.withDefault NotAsked

        mothersData =
            RemoteData.andThen
                (\participants ->
                    AllDict.keys participants.byMotherId
                        |> List.map
                            (\id ->
                                AllDict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map (EntityUuidDictList.fromList >> AllDictList.sortBy .name)
                )
                participantsData

        childrenData =
            RemoteData.andThen
                (\participants ->
                    AllDict.keys participants.byChildId
                        |> List.map
                            (\id ->
                                AllDict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EntityUuidDictList.fromList
                )
                participantsData

        childMeasurementListData =
            RemoteData.andThen
                (\children ->
                    AllDictList.keys children
                        |> List.map
                            (\childId ->
                                AllDict.get childId db.childMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( childId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EntityUuidDict.fromList
                )
                childrenData

        adultMeasurementListData =
            RemoteData.andThen
                (\mothers ->
                    AllDictList.keys mothers
                        |> List.map
                            (\motherId ->
                                AllDict.get motherId db.motherMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( motherId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EntityUuidDict.fromList
                )
                mothersData

        childMeasurementsSplitData =
            RemoteData.map (splitChildMeasurements sessionId) childMeasurementListData

        adultMeasurementsSplitData =
            RemoteData.map (splitMotherMeasurements sessionId) adultMeasurementListData

        historicalMeasurementData =
            RemoteData.map2 HistoricalMeasurements adultMeasurementListData childMeasurementListData

        currentAndPrevious =
            RemoteData.map2
                (\childData motherData ->
                    { current =
                        { mothers = AllDict.map (always .current) motherData
                        , children = AllDict.map (always .current) childData
                        }
                    , previous =
                        { mothers = AllDict.map (always .previous) motherData
                        , children = AllDict.map (always .previous) childData
                        }
                    }
                )
                childMeasurementsSplitData
                adultMeasurementsSplitData

        currentMeasurementData =
            RemoteData.map .current currentAndPrevious

        previousMeasurementData =
            RemoteData.map .previous currentAndPrevious

        offlineSession =
            RemoteData.map OfflineSession sessionData
                |> RemoteData.andMap allParticipantFormsData
                |> RemoteData.andMap everyCounselingScheduleData
                |> RemoteData.andMap participantsData
                |> RemoteData.andMap mothersData
                |> RemoteData.andMap childrenData
                |> RemoteData.andMap historicalMeasurementData
                |> RemoteData.andMap currentMeasurementData
                |> RemoteData.andMap previousMeasurementData
    in
    RemoteData.map
        (\offline ->
            { offlineSession = offline
            , update = NotAsked
            }
        )
        offlineSession


{-| Tracks the various ways in which the session ought to be considered closed:

  - Was explicitly closed.
  - Has passed its closed date.

-}
isClosed : NominalDate -> Session -> Bool
isClosed currentDate session =
    let
        pastEnd =
            Time.Date.compare currentDate session.scheduledDate.end == GT
    in
    session.closed || pastEnd


isAuthorized : Nurse -> Session -> Bool
isAuthorized nurse session =
    List.member session.clinicId nurse.clinics
