module Backend.HealthCenter.Decoder exposing (decodeCatchmentArea, decodeHealthCenter)

import Backend.HealthCenter.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeHealthCenter : Decoder HealthCenter
decodeHealthCenter =
    decode HealthCenter
        |> required "catchment_area" decodeEntityUuid
        |> required "label" string


decodeCatchmentArea : Decoder CatchmentArea
decodeCatchmentArea =
    decode CatchmentArea
        |> required "label" string
