module Backend.Village.Decoder exposing (decodeVillage)

import Backend.Person.Decoder exposing (decodeGeoField)
import Backend.Village.Model exposing (Village)
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeVillage : Decoder Village
decodeVillage =
    succeed Village
        |> required "health_center" decodeEntityUuid
        |> required "label" string
        |> required "province" decodeGeoField
        |> required "district" decodeGeoField
        |> required "sector" decodeGeoField
        |> required "cell" decodeGeoField
        |> required "village" decodeGeoField
