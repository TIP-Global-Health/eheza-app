module Child.Decoder
    exposing
        ( decodeChild
        )

import Activity.Decoder exposing (decodeChildActivityDates)
import Child.Model exposing (..)
import EveryDictList
import Examination.Model exposing (ExaminationStorage(New))
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import RemoteData exposing (RemoteData(NotAsked))
import Utils.Json exposing (decodeInt)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string "https://placehold.it/200x200"
        |> required "mother" (nullable decodeInt)
        -- @todo We would issue a call to /api/examinations to populate this
        |> hardcoded EveryDictList.empty
        -- We assume measurementes are for a new examination. In the future they
        -- could be updates of existing ones.
        |> hardcoded (Just <| New NotAsked)
        |> custom decodeChildActivityDates
