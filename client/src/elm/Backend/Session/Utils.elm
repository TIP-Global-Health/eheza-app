module Backend.Session.Utils exposing (emptyMotherMeasurementData, getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildMeasurementData2, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2, getMyMother, isAuthorized, isClosed)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Lazy exposing (Lazy, lazy)
import RemoteData exposing (RemoteData(..))
import Time.Date


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


getChildHistoricalMeasurements : PersonId -> OfflineSession -> Lazy ChildMeasurementList
getChildHistoricalMeasurements childId session =
    Lazy.map
        (.historical >> .children >> Dict.get childId >> Maybe.withDefault emptyChildMeasurementList)
        session.measurements


getMotherHistoricalMeasurements : PersonId -> OfflineSession -> Lazy MotherMeasurementList
getMotherHistoricalMeasurements motherId session =
    Lazy.map
        (.historical >> .mothers >> Dict.get motherId >> Maybe.withDefault emptyMotherMeasurementList)
        session.measurements


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : PersonId -> EditableSession -> Lazy (MeasurementData ChildMeasurements)
getChildMeasurementData childId session =
    Lazy.map
        (\measurements ->
            { current =
                Dict.get childId measurements.current.children
                    |> Maybe.withDefault emptyChildMeasurements
            , previous =
                Dict.get childId measurements.previous.children
                    |> Maybe.withDefault emptyChildMeasurements
            , update = session.update
            }
        )
        session.offlineSession.measurements


getChildMeasurementData2 : PersonId -> OfflineSession -> Lazy (MeasurementData ChildMeasurements)
getChildMeasurementData2 childId session =
    Lazy.map
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
getMotherMeasurementData : PersonId -> EditableSession -> Lazy (MeasurementData MotherMeasurements)
getMotherMeasurementData motherId session =
    Lazy.map
        (\measurements ->
            { current =
                Dict.get motherId measurements.current.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , previous =
                Dict.get motherId measurements.previous.mothers
                    |> Maybe.withDefault emptyMotherMeasurements
            , update = session.update
            }
        )
        session.offlineSession.measurements


getMotherMeasurementData2 : PersonId -> OfflineSession -> Lazy (MeasurementData MotherMeasurements)
getMotherMeasurementData2 motherId session =
    Lazy.map
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
    , update = session.update
    }


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
