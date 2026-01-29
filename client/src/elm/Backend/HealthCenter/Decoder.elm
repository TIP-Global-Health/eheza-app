module Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)

import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Json.Decode exposing (Decoder, bool, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeHealthCenter : Decoder HealthCenter
decodeHealthCenter =
    succeed HealthCenter
        |> required "catchment_area" decodeEntityUuid
        |> required "label" string
        |> required "deleted" bool


decodeCatchmentArea : Decoder CatchmentArea
decodeCatchmentArea =
    succeed CatchmentArea
        |> required "label" string
