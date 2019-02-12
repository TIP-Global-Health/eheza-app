module Backend.Session.Decoder exposing (decodeChildren, decodeMothers, decodeSession, decodeTrainingSessionAction, decodeTrainingSessionRequest, getCurrentAndPrevious, splitChildMeasurements, splitMotherMeasurements)

import Backend.Child.Decoder exposing (decodeChild)
import Backend.Child.Model exposing (Child)
import Backend.Clinic.Decoder exposing (decodeClinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (decodeHistoricalMeasurements)
import Backend.Measurement.Model exposing (ChildMeasurementList, ChildMeasurements, Measurement, MotherMeasurementList, MotherMeasurements, emptyMeasurements)
import Backend.Model exposing (TrainingSessionAction(..), TrainingSessionRequest)
import Backend.Mother.Decoder exposing (decodeMother)
import Backend.Mother.Model exposing (Mother)
import Backend.ParticipantConsent.Decoder exposing (decodeParticipantForm)
import Backend.Session.Model exposing (..)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (decodeDrupalRange, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Time.Date


decodeTrainingSessionRequest : Decoder TrainingSessionRequest
decodeTrainingSessionRequest =
    succeed TrainingSessionRequest
        |> required "action" decodeTrainingSessionAction


decodeTrainingSessionAction : Decoder TrainingSessionAction
decodeTrainingSessionAction =
    string
        |> andThen
            (\action ->
                case action of
                    "create_all" ->
                        succeed CreateAll

                    "delete_all" ->
                        succeed DeleteAll

                    _ ->
                        fail <| action ++ " is not a recognized TrainingSessionAction"
            )


{-| Decodes the JSON sent by /api/sessions
-}
decodeSession : Decoder Session
decodeSession =
    decode Session
        |> required "scheduled_date" (decodeDrupalRange decodeYYYYMMDD)
        |> custom
            (oneOf
                -- Work with "full_view" true or false, or with the
                -- structure we encode for the cache.
                [ field "clinic" decodeEntityUuid
                , field "clinic_id" decodeEntityUuid
                , at [ "clinic", "id" ] decodeEntityUuid
                ]
            )
        |> optional "closed" bool False
        |> optional "training" bool False


splitMotherMeasurements : SessionId -> EveryDict MotherId MotherMeasurementList -> EveryDict MotherId { current : MotherMeasurements, previous : MotherMeasurements }
splitMotherMeasurements sessionId =
    EveryDict.map
        (\_ list ->
            let
                familyPlanning =
                    getCurrentAndPrevious sessionId list.familyPlannings

                consent =
                    getCurrentAndPrevious sessionId list.consents
                        |> .current
                        |> EveryDict.fromList
            in
            { current =
                { familyPlanning = List.head familyPlanning.current
                , consent = consent
                }
            , previous =
                -- We don't "compare" consents, so previous doesn't mean
                -- anything for it.
                { familyPlanning = familyPlanning.previous
                , consent = EveryDict.empty
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
                { height = List.head height.current
                , weight = List.head weight.current
                , muac = List.head muac.current
                , nutrition = List.head nutrition.current
                , photo = List.head photo.current
                , counselingSession = List.head counselingSession.current
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
getCurrentAndPrevious : SessionId -> List ( id, Measurement a b ) -> { current : List ( id, Measurement a b ), previous : Maybe ( id, Measurement a b ) }
getCurrentAndPrevious sessionId =
    let
        -- This is designed to iterate through each list only once, to get both
        -- the current and previous value
        go measurement acc =
            if .sessionId (Tuple.second measurement) == Just sessionId then
                -- If it's got our session ID, then it's current
                { acc | current = measurement :: acc.current }

            else
                case acc.previous of
                    -- Otherwise, it might be previous
                    Nothing ->
                        { acc | previous = Just measurement }

                    Just ( _, previousValue ) ->
                        if Time.Date.compare (.dateMeasured (Tuple.second measurement)) previousValue.dateMeasured == GT then
                            { acc | previous = Just measurement }

                        else
                            acc
    in
    List.foldl go
        { current = []
        , previous = Nothing
        }


decodeMothers : Decoder (EveryDictList MotherId Mother)
decodeMothers =
    EveryDictList.decodeArray2 (field "id" decodeEntityUuid) decodeMother
        |> map (EveryDictList.sortBy .name)


decodeChildren : Decoder (EveryDictList ChildId Child)
decodeChildren =
    EveryDictList.decodeArray2 (field "id" decodeEntityUuid) decodeChild
        |> map (EveryDictList.sortBy .name)
