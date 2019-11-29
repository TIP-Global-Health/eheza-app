module Backend.PrenatalParticipant.Decoder exposing (decodePrenatalParticipant)

import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodePrenatalParticipant : Decoder PrenatalParticipant
decodePrenatalParticipant =
    succeed PrenatalParticipant
        |> required "person" decodeEntityUuid
        |> required "encounter_type" decodeEncounterType
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing


decodeEncounterType : Decoder EncounterType
decodeEncounterType =
    string
        |> andThen
            (\s ->
                case s of
                    "antenatal" ->
                        succeed AntenatalEncounter

                    "inmmunization" ->
                        succeed InmmunizationEncounter

                    "nutrition" ->
                        succeed NutritionEncounter

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized EncounterType"
            )
