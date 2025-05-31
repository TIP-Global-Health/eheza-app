module Backend.Components.Decoder exposing (..)

import Backend.Components.Model exposing (HealthCenterData, MenuScope(..))
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import Json.Decode.Pipeline exposing (required)


decodeHealthCenterData : Decoder HealthCenterData
decodeHealthCenterData =
    succeed HealthCenterData
        |> required "id" decodeInt
        |> required "name" string


decodeMenuScope : Decoder MenuScope
decodeMenuScope =
    string
        |> andThen
            (\scope ->
                case scope of
                    "full" ->
                        succeed ScopeFull

                    "health_centers" ->
                        succeed ScopeHealthCenters

                    _ ->
                        fail <| scope ++ " is unknown MenuScope type"
            )
