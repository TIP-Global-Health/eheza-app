module Backend.Measurement.Utils exposing (currentValue, currentValueWithId, currentValues, getCurrentAndPrevious, mapMeasurementData, muacIndication, socialHistoryHivTestingResultFromString, splitChildMeasurements, splitMotherMeasurements)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Restful.Endpoint exposing (EntityUuid)
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


splitMotherMeasurements : SessionId -> EveryDict PersonId MotherMeasurementList -> EveryDict PersonId { current : MotherMeasurements, previous : MotherMeasurements }
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


splitChildMeasurements : SessionId -> EveryDict PersonId ChildMeasurementList -> EveryDict PersonId { current : ChildMeasurements, previous : ChildMeasurements }
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
getCurrentAndPrevious : SessionId -> EveryDictList (EntityUuid id) (GroupMeasurement b) -> { current : EveryDictList (EntityUuid id) (GroupMeasurement b), previous : Maybe ( EntityUuid id, GroupMeasurement b ) }
getCurrentAndPrevious sessionId =
    let
        -- This is designed to iterate through each list only once, to get both
        -- the current and previous value
        go id value acc =
            if value.encounterId == Just sessionId then
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


socialHistoryHivTestingResultFromString : String -> Maybe SocialHistoryHivTestingResult
socialHistoryHivTestingResultFromString result =
    case result of
        "positive" ->
            Just ResultHivPositive

        "negative" ->
            Just ResultHivNegative

        "indeterminate" ->
            Just ResultHivIndeterminate

        "none" ->
            Just NoHivTesting

        _ ->
            Nothing
