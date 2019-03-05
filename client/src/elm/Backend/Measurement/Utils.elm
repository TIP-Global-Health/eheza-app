module Backend.Measurement.Utils exposing (currentValue, currentValueWithId, currentValues, getCurrentAndPrevious, mapMeasurementData, muacIndication, splitChildMeasurements, splitMotherMeasurements)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Time.Date


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


{-| Given the data, do we have a current value? May be the value
stored in the backend, or an edited value.
-}
currentValue : MeasurementData (Maybe ( id, value )) -> Maybe value
currentValue data =
    Maybe.map Tuple.second data.current


{-| Like `currentValue`, but also supplies the ID if we have one
(i.e. if we're editing a value saved on the backend).
-}
currentValueWithId : MeasurementData (Maybe ( id, value )) -> Maybe ( Maybe id, value )
currentValueWithId data =
    currentValue data
        |> Maybe.map (\value -> ( Maybe.map Tuple.first data.current, value ))


{-| Like `currentValue`, but for cases where we have a list of values.
-}
currentValues : MeasurementData (EveryDictList id value) -> List ( Maybe id, value )
currentValues data =
    data.current
        |> EveryDictList.map (\k v -> ( Just k, v ))
        |> EveryDictList.values


mapMeasurementData : (a -> b) -> MeasurementData a -> MeasurementData b
mapMeasurementData dataFunc measurements =
    { previous = dataFunc measurements.previous
    , current = dataFunc measurements.current
    , update = measurements.update
    }



-- TODO: Reimplement, possibly in service worker.
{- Get a list of all the photo edits (some will be `Unedited`). -}
{-
   getPhotoEdits : MeasurementEdits -> List (Edit PhotoId Photo)
   getPhotoEdits edits =
       edits.children
           |> EveryDict.values
           |> List.map .photo
-}
{- Given the photo edits, get all the photos for which we don't know the
   file ID ... that is, those which we haven't uploaded to the backend yet.
-}
{-
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
-}


splitMotherMeasurements : SessionId -> EveryDict MotherId MotherMeasurementList -> EveryDict MotherId { current : MotherMeasurements, previous : MotherMeasurements }
splitMotherMeasurements sessionId =
    EveryDict.map
        (\_ list ->
            let
                attendance =
                    getCurrentAndPrevious sessionId list.attendances

                familyPlanning =
                    getCurrentAndPrevious sessionId list.familyPlannings

                consent =
                    getCurrentAndPrevious sessionId list.consents
                        |> .current
            in
            { current =
                { attendance = EveryDictList.head attendance.current
                , familyPlanning = EveryDictList.head familyPlanning.current
                , consent = consent
                }
            , previous =
                -- We don't "compare" consents, so previous doesn't mean
                -- anything for it.
                { attendance = attendance.previous
                , familyPlanning = familyPlanning.previous
                , consent = EveryDictList.empty
                }
            }
        )


splitChildMeasurements : SessionId -> EveryDict ChildId ChildMeasurementList -> EveryDict ChildId { current : ChildMeasurements, previous : ChildMeasurements }
splitChildMeasurements sessionId =
    EveryDict.map
        (\_ list ->
            let
                height =
                    getCurrentAndPrevious sessionId list.heights

                weight =
                    getCurrentAndPrevious sessionId list.weights

                muac =
                    getCurrentAndPrevious sessionId list.muacs

                nutrition =
                    getCurrentAndPrevious sessionId list.nutritions

                photo =
                    getCurrentAndPrevious sessionId list.photos

                counselingSession =
                    getCurrentAndPrevious sessionId list.counselingSessions
            in
            { current =
                -- We can only have one per session ... we enforce that here.
                { height = EveryDictList.head height.current
                , weight = EveryDictList.head weight.current
                , muac = EveryDictList.head muac.current
                , nutrition = EveryDictList.head nutrition.current
                , photo = EveryDictList.head photo.current
                , counselingSession = EveryDictList.head counselingSession.current
                }
            , previous =
                { height = height.previous
                , weight = weight.previous
                , muac = muac.previous
                , nutrition = nutrition.previous
                , photo = photo.previous
                , counselingSession = counselingSession.previous
                }
            }
        )


{-| Picks out current and previous values from a list of measurements.
-}
getCurrentAndPrevious : SessionId -> EveryDictList id (Measurement a b) -> { current : EveryDictList id (Measurement a b), previous : Maybe ( id, Measurement a b ) }
getCurrentAndPrevious sessionId =
    let
        -- This is designed to iterate through each list only once, to get both
        -- the current and previous value
        go id value acc =
            if value.sessionId == Just sessionId then
                -- If it's got our session ID, then it's current
                { acc | current = EveryDictList.cons id value acc.current }

            else
                case acc.previous of
                    -- Otherwise, it might be previous
                    Nothing ->
                        { acc | previous = Just ( id, value ) }

                    Just ( _, previousValue ) ->
                        if Time.Date.compare value.dateMeasured previousValue.dateMeasured == GT then
                            { acc | previous = Just ( id, value ) }

                        else
                            acc
    in
    EveryDictList.foldl go
        { current = EveryDictList.empty
        , previous = Nothing
        }
