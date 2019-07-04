module Backend.Session.Utils exposing (emptyMotherMeasurementData, getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildMeasurementData2, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2, getMyMother, isAuthorized, isClosed)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))


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


getChildHistoricalMeasurements : PersonId -> OfflineSession -> ChildMeasurementList
getChildHistoricalMeasurements childId session =
    session.measurements
        |> (.historical >> .children >> Dict.get childId >> Maybe.withDefault emptyChildMeasurementList)


getMotherHistoricalMeasurements : PersonId -> OfflineSession -> MotherMeasurementList
getMotherHistoricalMeasurements motherId session =
    session.measurements
        |> (.historical >> .mothers >> Dict.get motherId >> Maybe.withDefault emptyMotherMeasurementList)


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : PersonId -> EditableSession -> MeasurementData ChildMeasurements
getChildMeasurementData childId session =
    session.offlineSession.measurements
        |> (\measurements ->
                { current =
                    Dict.get childId measurements.current.children
                        |> Maybe.withDefault emptyChildMeasurements
                , previous =
                    Dict.get childId measurements.previous.children
                        |> Maybe.withDefault emptyChildMeasurements
                , update = session.update
                }
           )


getChildMeasurementData2 : PersonId -> OfflineSession -> MeasurementData ChildMeasurements
getChildMeasurementData2 childId session =
    session.measurements
        |> (\measurements ->
                { current =
                    Dict.get childId measurements.current.children
                        |> Maybe.withDefault emptyChildMeasurements
                , previous =
                    Dict.get childId measurements.previous.children
                        |> Maybe.withDefault emptyChildMeasurements
                , update = NotAsked
                }
           )


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getMotherMeasurementData : PersonId -> EditableSession -> MeasurementData MotherMeasurements
getMotherMeasurementData motherId session =
    session.offlineSession.measurements
        |> (\measurements ->
                { current =
                    Dict.get motherId measurements.current.mothers
                        |> Maybe.withDefault emptyMotherMeasurements
                , previous =
                    Dict.get motherId measurements.previous.mothers
                        |> Maybe.withDefault emptyMotherMeasurements
                , update = session.update
                }
           )


getMotherMeasurementData2 : PersonId -> OfflineSession -> MeasurementData MotherMeasurements
getMotherMeasurementData2 motherId session =
    session.measurements
        |> (\measurements ->
                { current =
                    Dict.get motherId measurements.current.mothers
                        |> Maybe.withDefault emptyMotherMeasurements
                , previous =
                    Dict.get motherId measurements.previous.mothers
                        |> Maybe.withDefault emptyMotherMeasurements
                , update = NotAsked
                }
           )


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
            Gizra.NominalDate.compare currentDate session.scheduledDate.end == GT
    in
    session.closed || pastEnd


isAuthorized : Nurse -> Session -> Bool
isAuthorized nurse session =
    List.member session.clinicId nurse.clinics
