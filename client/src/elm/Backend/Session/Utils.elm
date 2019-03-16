module Backend.Session.Utils exposing (emptyMotherMeasurementData, getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMyMother, isAuthorized, isClosed, makeEditableSession)

import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Mother.Model exposing (Mother)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Session.Model exposing (..)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date


{-| Given a mother's id, get all her children from the offline session.
-}
getChildren : MotherId -> OfflineSession -> List ( ChildId, Child )
getChildren motherId session =
    session.children
        |> EveryDictList.filter (\_ child -> child.motherId == Just motherId)
        |> EveryDictList.toList


getChild : ChildId -> OfflineSession -> Maybe Child
getChild childId session =
    EveryDictList.get childId session.children


getMother : MotherId -> OfflineSession -> Maybe Mother
getMother motherId session =
    EveryDictList.get motherId session.mothers


getMyMother : ChildId -> OfflineSession -> Maybe ( MotherId, Mother )
getMyMother childId session =
    getChild childId session
        |> Maybe.andThen .motherId
        |> Maybe.andThen
            (\motherId ->
                getMother motherId session
                    |> Maybe.map (\mother -> ( motherId, mother ))
            )


getChildHistoricalMeasurements : ChildId -> OfflineSession -> ChildMeasurementList
getChildHistoricalMeasurements childId session =
    EveryDict.get childId session.historicalMeasurements.children
        |> Maybe.withDefault emptyChildMeasurementList


getMotherHistoricalMeasurements : MotherId -> OfflineSession -> MotherMeasurementList
getMotherHistoricalMeasurements motherId session =
    EveryDict.get motherId session.historicalMeasurements.mothers
        |> Maybe.withDefault emptyMotherMeasurementList


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : ChildId -> EditableSession -> MeasurementData ChildMeasurements
getChildMeasurementData childId session =
    { current =
        EveryDict.get childId session.offlineSession.currentMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , previous =
        EveryDict.get childId session.offlineSession.previousMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , update = session.update
    }


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getMotherMeasurementData : MotherId -> EditableSession -> MeasurementData MotherMeasurements
getMotherMeasurementData motherId session =
    { current =
        EveryDict.get motherId session.offlineSession.currentMeasurements.mothers
            |> Maybe.withDefault emptyMotherMeasurements
    , previous =
        EveryDict.get motherId session.offlineSession.previousMeasurements.mothers
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
            EveryDict.get sessionId db.sessions
                |> Maybe.withDefault NotAsked

        allParticipantFormsData =
            db.participantForms

        everyCounselingScheduleData =
            db.everyCounselingSchedule

        participantData =
            EveryDict.get sessionId db.expectedParticipants
                |> Maybe.withDefault NotAsked

        mothersData =
            RemoteData.map .mothers participantData

        childrenData =
            RemoteData.map .children participantData

        childMeasurementListData =
            RemoteData.andThen
                (\children ->
                    EveryDictList.keys children
                        |> List.map
                            (\childId ->
                                EveryDict.get childId db.childMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( childId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EveryDict.fromList
                )
                childrenData

        motherMeasurementListData =
            RemoteData.andThen
                (\mothers ->
                    EveryDictList.keys mothers
                        |> List.map
                            (\motherId ->
                                EveryDict.get motherId db.motherMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( motherId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EveryDict.fromList
                )
                mothersData

        childMeasurementsSplitData =
            RemoteData.map (splitChildMeasurements sessionId) childMeasurementListData

        motherMeasurementsSplitData =
            RemoteData.map (splitMotherMeasurements sessionId) motherMeasurementListData

        historicalMeasurementData =
            RemoteData.map2 HistoricalMeasurements motherMeasurementListData childMeasurementListData

        currentAndPrevious =
            RemoteData.map2
                (\childData motherData ->
                    { current =
                        { mothers = EveryDict.map (always .current) motherData
                        , children = EveryDict.map (always .current) childData
                        }
                    , previous =
                        { mothers = EveryDict.map (always .previous) motherData
                        , children = EveryDict.map (always .previous) childData
                        }
                    }
                )
                childMeasurementsSplitData
                motherMeasurementsSplitData

        currentMeasurementData =
            RemoteData.map .current currentAndPrevious

        previousMeasurementData =
            RemoteData.map .previous currentAndPrevious

        offlineSession =
            RemoteData.map OfflineSession sessionData
                |> RemoteData.andMap allParticipantFormsData
                |> RemoteData.andMap everyCounselingScheduleData
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
