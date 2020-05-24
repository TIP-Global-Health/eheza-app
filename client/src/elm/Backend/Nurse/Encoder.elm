module Backend.Nurse.Encoder exposing (encodeNurse)

import Backend.Nurse.Model exposing (Nurse, Role(..))
import EverySet
import Json.Encode exposing (..)
import Json.Encode.Extra
import Restful.Endpoint exposing (encodeEntityUuid)


encodeNurse : Nurse -> List ( String, Value )
encodeNurse nurse =
    [ ( "label", string nurse.name )
    , ( "health_centers", list encodeEntityUuid (EverySet.toList nurse.healthCenters) )
    , ( "villages", list encodeEntityUuid (EverySet.toList nurse.villages) )
    , ( "role", list encodeRole (EverySet.toList nurse.roles) )
    , ( "email", Json.Encode.Extra.maybe string nurse.email )
    , ( "pin_code", string nurse.pinCode )
    , ( "type", string "nurse" )
    ]


encodeRole : Role -> Value
encodeRole role =
    case role of
        RoleAdministrator ->
            string "admin"

        RoleCHW ->
            string "chw"

        RoleNurse ->
            string "nurse"
