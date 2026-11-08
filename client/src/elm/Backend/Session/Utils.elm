module Backend.Session.Utils exposing (childActivitySaveRequest, emptyMotherMeasurementData, getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildMeasurementData2, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2, getMyMother, getSession, isClosed, isSaveAllowed, motherActivitySaveRequest)

import Activity.Model exposing (ChildActivity, MotherActivity)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession, Model, OfflineSession, Session)
import Date
import Gizra.NominalDate exposing (NominalDate)
import LocalData exposing (LocalData)
import RemoteData exposing (RemoteData(..), WebData)


{-| Given a mother's id, get all her children from the offline session.
-}
getChildren : PersonId -> OfflineSession -> List ( PersonId, Person )
getChildren motherId session =
    Dict.get motherId session.participants.byMotherId
        |> Maybe.withDefault []
        |> List.filterMap
            (\participant ->
                Dict.get participant.child session.children
                    |> Maybe.map (\child -> ( participant.child, child ))
            )


getChild : PersonId -> OfflineSession -> Maybe Person
getChild childId session =
    Dict.get childId session.children


getMother : PersonId -> OfflineSession -> Maybe Person
getMother motherId session =
    Dict.get motherId session.mothers


getMyMother : PersonId -> OfflineSession -> Maybe ( PersonId, Person )
getMyMother childId session =
    Dict.get childId session.participants.byChildId
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.andThen
            (\participant ->
                Dict.get participant.adult session.mothers
                    |> Maybe.map (\person -> ( participant.adult, person ))
            )


getChildHistoricalMeasurements : PersonId -> OfflineSession -> LocalData ChildMeasurementList
getChildHistoricalMeasurements childId session =
    LocalData.map
        (.historical >> .children >> Dict.get childId >> Maybe.withDefault emptyChildMeasurementList)
        session.measurements


getMotherHistoricalMeasurements : PersonId -> OfflineSession -> LocalData MotherMeasurementList
getMotherHistoricalMeasurements motherId session =
    LocalData.map
        (.historical >> .mothers >> Dict.get motherId >> Maybe.withDefault emptyMotherMeasurementList)
        session.measurements


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : PersonId -> EditableSession -> LocalData (MeasurementData ChildMeasurements)
getChildMeasurementData childId session =
    LocalData.map
        (\measurements ->
            { current =
                Dict.get childId measurements.current.children
                    |> Maybe.withDefault emptyChildMeasurements
            , previous =
                Dict.get childId measurements.previous.children
                    |> Maybe.withDefault emptyChildMeasurements
            , update = NotAsked
            }
        )
        session.offlineSession.measurements


getChildMeasurementData2 : PersonId -> OfflineSession -> LocalData (MeasurementData ChildMeasurements)
getChildMeasurementData2 childId session =
    LocalData.map
        (\measurements ->
            { current =
                Dict.get childId measurements.current.children
                    |> Maybe.withDefault emptyChildMeasurements
            , previous =
                Dict.get childId measurements.previous.children
                    |> Maybe.withDefault emptyChildMeasurements
            , update = NotAsked
            }
        )
        session.measurements


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getMotherMeasurementData : PersonId -> EditableSession -> LocalData (MeasurementData MotherMeasurements)
getMotherMeasurementData motherId session =
    LocalData.map
        (\measurements ->
            { current =
                Dict.get motherId measurements.current.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , previous =
                Dict.get motherId measurements.previous.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , update = NotAsked
            }
        )
        session.offlineSession.measurements


getMotherMeasurementData2 : PersonId -> OfflineSession -> LocalData (MeasurementData MotherMeasurements)
getMotherMeasurementData2 motherId session =
    LocalData.map
        (\measurements ->
            { current =
                Dict.get motherId measurements.current.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , previous =
                Dict.get motherId measurements.previous.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , update = NotAsked
            }
        )
        session.measurements


emptyMotherMeasurementData : EditableSession -> MeasurementData MotherMeasurements
emptyMotherMeasurementData session =
    { current = emptyMotherMeasurements
    , previous = emptyMotherMeasurements
    , update = NotAsked
    }


isClosed : NominalDate -> Session -> Bool
isClosed currentDate session =
    let
        pastEnd =
            session.endDate
                |> Maybe.map
                    (\endDate ->
                        Date.compare currentDate endDate == GT
                    )
                |> Maybe.withDefault False

        beforeBeginning =
            Date.compare currentDate session.startDate == LT
    in
    pastEnd || beforeBeginning


getSession : SessionId -> ModelIndexedDb -> Maybe Session
getSession sessionId db =
    Dict.get sessionId db.sessions
        |> Maybe.withDefault NotAsked
        |> RemoteData.toMaybe


{-| The state of the latest save of a child's activity.
-}
childActivitySaveRequest : ChildActivity -> PersonId -> Model -> WebData ()
childActivitySaveRequest _ _ _ =
    NotAsked


{-| The state of the latest save of a mother's activity.
-}
motherActivitySaveRequest : MotherActivity -> PersonId -> Model -> WebData ()
motherActivitySaveRequest _ _ _ =
    NotAsked


{-| Whether a Save may go ahead, given the ID of the saved record (if the
device has one) and the state of the latest save.
-}
isSaveAllowed : Maybe valueId -> WebData () -> Bool
isSaveAllowed _ _ =
    True
