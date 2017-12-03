module Backend.Session.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import EveryDict
import EveryDictList
import RemoteData exposing (RemoteData(..))


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


getChild : ChildId -> OfflineSession -> Maybe Child
getChild childId session =
    EveryDict.get childId session.children


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
    , update = session.update
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
    , update = session.update
    }


{-| Given an OfflineSession for which we don't have edits, fill in default
values as a starting point for an EditableSession.
-}
makeEditableSession : OfflineSession -> EditableSession
makeEditableSession offlineSession =
    { offlineSession = offlineSession
    , edits = emptyMeasurementEdits
    , update = NotAsked
    , childForms = EveryDict.empty
    , motherForms = EveryDict.empty
    }


{-| Given a function that changes ChildEdits, apply that to the motherId.
-}
mapChildEdits : (ChildEdits -> ChildEdits) -> ChildId -> EditableSession -> EditableSession
mapChildEdits func childId session =
    let
        edits =
            session.edits
    in
        EveryDict.get childId edits.children
            |> Maybe.withDefault emptyChildEdits
            |> (\childEdits ->
                    { session
                        | edits =
                            { edits
                                | children =
                                    EveryDict.insert childId (func childEdits) edits.children
                            }
                    }
               )


{-| Given a function that changes MotherEdits, apply that to the motherId.
-}
mapMotherEdits : (MotherEdits -> MotherEdits) -> MotherId -> EditableSession -> EditableSession
mapMotherEdits func motherId session =
    let
        edits =
            session.edits
    in
        EveryDict.get motherId edits.mothers
            |> Maybe.withDefault emptyMotherEdits
            |> (\motherEdits ->
                    { session
                        | edits =
                            { edits
                                | mothers =
                                    EveryDict.insert motherId (func motherEdits) edits.mothers
                            }
                    }
               )


{-| Return a list of all the photo URLs we ought to cache to work with this offline.
-}
getPhotoUrls : OfflineSession -> List String
getPhotoUrls session =
    let
        fromMothers =
            session.mothers
                |> EveryDictList.values
                |> List.filterMap .avatarUrl

        fromChildren =
            session.children
                |> EveryDict.values
                |> List.filterMap .avatarUrl

        fromMeasurements =
            session.historicalMeasurements.children
                |> EveryDict.values
                |> List.map
                    (\measurements ->
                        measurements.photos
                            |> List.map (Tuple.second >> .value >> .url)
                    )
                |> List.concat
    in
        fromMothers ++ fromChildren ++ fromMeasurements
