module Backend.Components.Encoder exposing (..)

import Backend.Components.Model exposing (ReportParams)
import Json.Encode exposing (Value, int, object, string)
import Maybe.Extra


encodeReportParams : ReportParams -> List ( String, Value )
encodeReportParams params =
    [ Maybe.map (\value -> ( "province", string value )) params.province
    , Maybe.map (\value -> ( "district", string value )) params.district
    , Maybe.map (\value -> ( "sector", string value )) params.sector
    , Maybe.map (\value -> ( "cell", string value )) params.cell
    , Maybe.map (\value -> ( "village", string value )) params.village
    , Maybe.map (\value -> ( "health_center", int value )) params.healthCenter
    ]
        |> Maybe.Extra.values
