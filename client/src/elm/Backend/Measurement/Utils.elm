module Backend.Measurement.Utils exposing (currentValue, currentValueWithId, currentValues, getCurrentAndPrevious, mapMeasurementData, muacIndication, splitChildMeasurements, splitMotherMeasurements)

import AllDict
import AllDictList
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Restful.Endpoint exposing (EntityUuid)
import Time.Date
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)


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
currentValues : MeasurementData (EntityUuidDictList id value) -> List ( Maybe id, value )
currentValues data =
    data.current
        |> AllDictList.map (\k v -> ( Just k, v ))
        |> AllDictList.values


mapMeasurementData : (a -> b) -> MeasurementData a -> MeasurementData b
mapMeasurementData dataFunc measurements =
    { previous = dataFunc measurements.previous
    , current = dataFunc measurements.current
    , update = measurements.update
    }


splitMotherMeasurements : SessionId -> EntityUuidDict PersonId MotherMeasurementList -> EntityUuidDict PersonId { current : MotherMeasurements, previous : MotherMeasurements }
splitMotherMeasurements sessionId =
    AllDict.map
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
                { attendance = AllDictList.head attendance.current
                , familyPlanning = AllDictList.head familyPlanning.current
                , consent = consent
                }
            , previous =
                -- We don't "compare" consents, so previous doesn't mean
                -- anything for it.
                { attendance = attendance.previous
                , familyPlanning = familyPlanning.previous
                , consent = EntityUuidDictList.empty
                }
            }
        )


splitChildMeasurements : SessionId -> EntityUuidDict PersonId ChildMeasurementList -> EntityUuidDict PersonId { current : ChildMeasurements, previous : ChildMeasurements }
splitChildMeasurements sessionId =
    AllDict.map
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
                { height = AllDictList.head height.current
                , weight = AllDictList.head weight.current
                , muac = AllDictList.head muac.current
                , nutrition = AllDictList.head nutrition.current
                , photo = AllDictList.head photo.current
                , counselingSession = AllDictList.head counselingSession.current
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
getCurrentAndPrevious : SessionId -> EntityUuidDictList (EntityUuid id) (Measurement a b) -> { current : EntityUuidDictList (EntityUuid id) (Measurement a b), previous : Maybe ( EntityUuid id, Measurement a b ) }
getCurrentAndPrevious sessionId =
    let
        -- This is designed to iterate through each list only once, to get both
        -- the current and previous value
        go id value acc =
            if value.sessionId == Just sessionId then
                -- If it's got our session ID, then it's current
                { acc | current = AllDictList.cons id value acc.current }

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
    AllDictList.foldl go
        { current = EntityUuidDictList.empty
        , previous = Nothing
        }
