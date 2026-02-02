module Backend.Clinic.Encoder exposing (encodeClinic, encodeClinicType)

import Backend.Clinic.Model exposing (Clinic, ClinicType(..))
import Json.Encode exposing (Value, bool, string)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeClinic : Clinic -> List ( String, Value )
encodeClinic clinic =
    [ ( "label", string clinic.name )
    , ( "health_center", encodeEntityUuid clinic.healthCenterId )
    , ( "group_type", encodeClinicType clinic.clinicType )
    , ( "deleted", bool False )
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
                Achi ->
                    "achi"

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
