module Backend.Session.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import EveryDict
import EveryDictList


{-| Given a mother's id, get all her children from the offline session.

  - If we can't find the mother in the OfflineSession, we just return an
    empty list. So, it's the caller's job to know whether we really have
    that mother or not.

  - If the mother's data indicates that she has a child, but the child's
    data isn't in the OfflineSession, we don't include the child in the
    results. So, it's someone else's job to make sure that the OfflineSession
    is internally consistent. (The backend sends it that way).

-}
getChildren : MotherId -> OfflineSession -> List ( ChildId, Child )
getChildren motherId session =
    getMother motherId session
        |> Maybe.map
            (\mother ->
                mother.children
                    |> List.filterMap
                        (\childId ->
                            EveryDict.get childId session.children
                                |> Maybe.map (\child -> ( childId, child ))
                        )
            )
        |> Maybe.withDefault []


getMother : MotherId -> OfflineSession -> Maybe Mother
getMother motherId session =
    EveryDictList.get motherId session.mothers


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getChildMeasurementData : ChildId -> EditableSession -> MeasurementData ChildMeasurements ChildEdits
getChildMeasurementData childId session =
    { current =
        EveryDict.get childId session.offlineSession.currentMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , previous =
        EveryDict.get childId session.offlineSession.previousMeasurements.children
            |> Maybe.withDefault emptyChildMeasurements
    , edits =
        EveryDict.get childId session.edits.children
            |> Maybe.withDefault emptyChildEdits
    , status = session.editStatus
    }


{-| Gets the data in the form that `Measurement.View` (and others) will want.
-}
getMotherMeasurementData : MotherId -> EditableSession -> MeasurementData MotherMeasurements MotherEdits
getMotherMeasurementData motherId session =
    { current =
        EveryDict.get motherId session.offlineSession.currentMeasurements.mothers
            |> Maybe.withDefault emptyMotherMeasurements
    , previous =
        EveryDict.get motherId session.offlineSession.previousMeasurements.mothers
            |> Maybe.withDefault emptyMotherMeasurements
    , edits =
        EveryDict.get motherId session.edits.mothers
            |> Maybe.withDefault emptyMotherEdits
    , status = session.editStatus
    }
