module Backend.Clinic.Encoder exposing (encodeClinic, encodeClinicType)

import Backend.Clinic.Model exposing (..)
import Json.Encode exposing (..)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeClinic : Clinic -> List ( String, Value )
encodeClinic clinic =
    [ ( "label", string clinic.name )
    , ( "health_center", encodeEntityUuid clinic.healthCenterId )
    , ( "group_type", encodeClinicType clinic.clinicType )
    , ( "type", string "clinic" )
    ]
        ++ (clinic.villageId
                |> Maybe.map (\uuid -> [ ( "village", encodeEntityUuid uuid ) ])
                |> Maybe.withDefault []
           )


encodeClinicType : ClinicType -> Value
encodeClinicType clinicType =
    let
        clinicTypeAsString =
            case clinicType of
                Chw ->
                    "chw"

                Pmtct ->
                    "pmtct"

                Fbf ->
                    "fbf"

                Sorwathe ->
                    "sorwathe"
    in
    string clinicTypeAsString
