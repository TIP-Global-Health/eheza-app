module Backend.Measurement.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDict exposing (EveryDict)


{-| Picks out a particular mother's `MotherEdits`. If not found, makes
an empty one.
-}
getMotherEdits : MotherId -> MeasurementEdits -> MotherEdits
getMotherEdits motherId measurements =
    EveryDict.get motherId measurements.mothers
        |> Maybe.withDefault emptyMotherEdits


{-| Picks out a particular child's `ChildEdits`. If not found, makes
an empty one.
-}
getChildEdits : ChildId -> MeasurementEdits -> ChildEdits
getChildEdits childId measurements =
    EveryDict.get childId measurements.children
        |> Maybe.withDefault emptyChildEdits


{-| Given a MUAC in cm, classify according to the measurement tool shown
at <https://github.com/Gizra/ihangane/issues/282>
-}
muacIndication : MuacInCm -> MuacIndication
muacIndication (MuacInCm value) =
    if value <= 11.5 then
        MuacRed
    else if value <= 12.5 then
        MuacYellow
    else
        MuacGreen


{-| Apply an edit to an underlying value.
-}
applyEdit : Edit id value -> Maybe value -> Maybe value
applyEdit edit value =
    case edit of
        Unedited ->
            value

        Created new ->
            Just new

        Edited { edited } ->
            Just edited

        Deleted _ _ ->
            Nothing


{-| Like `applyEdit`, but for cases where we have multiple entities and edits
in the same session. (E.g. for participantConsent, but not weight).
-}
applyEdits : List (Edit id value) -> EveryDict id value -> List ( Maybe id, value )
applyEdits edits base =
    let
        go edit ( existing, new ) =
            case edit of
                Unedited ->
                    ( existing, new )

                Created created ->
                    ( existing, created :: new )

                Edited edited ->
                    ( EveryDict.insert edited.id edited.edited existing
                    , new
                    )

                Deleted id _ ->
                    ( EveryDict.remove id existing
                    , new
                    )

        ( remaining, created ) =
            List.foldl go ( base, [] ) edits
    in
    List.concat
        [ List.map (Tuple.mapFirst Just) (EveryDict.toList remaining)
        , List.map (\n -> ( Nothing, n )) created
        ]


{-| Given the data, do we have a current value? May be the value
stored in the backend, or an edited value.
-}
currentValue : MeasurementData (Maybe ( id, value )) (Edit id value) -> Maybe value
currentValue data =
    applyEdit data.edits (Maybe.map Tuple.second data.current)


{-| Like `currentValue`, but also supplies the ID if we have one
(i.e. if we're editing a value saved on the backend).
-}
currentValueWithId : MeasurementData (Maybe ( id, value )) (Edit id value) -> Maybe ( Maybe id, value )
currentValueWithId data =
    currentValue data
        |> Maybe.map (\value -> ( Maybe.map Tuple.first data.current, value ))


{-| Like `currentValue`, but for cases where we have a list of values.
-}
currentValues : MeasurementData (EveryDict id value) (List (Edit id value)) -> List ( Maybe id, value )
currentValues data =
    applyEdits data.edits data.current


{-| Like `currentValue`, but just considers the backend.
-}
backendValue : MeasurementData (Maybe ( id, value )) (Edit id value) -> Maybe ( id, value )
backendValue data =
    data.current


mapMeasurementData : (d1 -> d2) -> (e1 -> e2) -> MeasurementData d1 e1 -> MeasurementData d2 e2
mapMeasurementData dataFunc editFunc measurements =
    { previous = dataFunc measurements.previous
    , current = dataFunc measurements.current
    , edits = editFunc measurements.edits
    , update = measurements.update
    }


{-| Get a list of all the photo edits (some will be `Unedited`).
-}
getPhotoEdits : MeasurementEdits -> List (Edit PhotoId Photo)
getPhotoEdits edits =
    edits.children
        |> EveryDict.values
        |> List.map .photo


{-| Given the photo edits, get all the photos for which we don't know the
file ID ... that is, those which we haven't uploaded to the backend yet.
-}
getPhotosToUpload : MeasurementEdits -> List Photo
getPhotosToUpload =
    getPhotoEdits
        >> List.filterMap
            (\edit ->
                case edit of
                    Unedited ->
                        Nothing

                    Created photo ->
                        if photo.value.fid == Nothing then
                            Just photo
                        else
                            Nothing

                    Edited { edited } ->
                        if edited.value.fid == Nothing then
                            Just edited
                        else
                            Nothing

                    Deleted _ _ ->
                        Nothing
            )
