module Backend.Scoreboard.Decoder exposing (decodeScoreboardData)

import Backend.Scoreboard.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, fail, float, int, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeScoreboardData : Decoder ScoreboardData
decodeScoreboardData =
    succeed ScoreboardData
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "district" ->
                        succeed EntityDistrict

                    "sector" ->
                        succeed EntitySector

                    "cell" ->
                        succeed EntityCell

                    "village" ->
                        succeed EntityVillage

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )
