module Backend.Nurse.Encoder exposing (encodeNurse)

import Backend.Nurse.Model exposing (..)
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeGender, encodeMaritalStatus, encodeUbudehe)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


encodeNurse : Nurse -> List ( String, Value )
encodeNurse nurse =
    let
        email =
            Maybe.map (\nurseEmail -> [ ( "email", string nurseEmail ) ]) nurse.email
                |> Maybe.withDefault []
    in
    [ ( "label", string nurse.name )
    , ( "health_centers", list encodeEntityUuid (EverySet.toList nurse.healthCenters) )
    , ( "villages", list encodeEntityUuid (EverySet.toList nurse.villages) )
    , ( "role", list encodeRole (EverySet.toList nurse.roles) )
    , ( "pin_code", string nurse.pinCode )
    , ( "resilience_program", bool nurse.resilienceProgramEnabled )
    , ( "resilience_start_date", maybe encodeYYYYMMDD nurse.resilienceProgramStartDate )
    , ( "resilience_role", maybe encodeResilienceRole nurse.resilienceRole )
    , ( "birth_date", maybe encodeYYYYMMDD nurse.resilienceBirthDate )
    , ( "gender", maybe encodeGender nurse.resilienceGender )
    , ( "education_level", maybe encodeEducationLevel nurse.resilienceEducationLevel )
    , ( "ubudehe", maybe encodeUbudehe nurse.resilienceUbudehe )
    , ( "marital_status", maybe encodeMaritalStatus nurse.resilienceMaritalStatus )
    , ( "deleted", bool False )
    , ( "type", string "nurse" )
    ]
        ++ email


encodeRole : Role -> Value
encodeRole role =
    case role of
        RoleAdministrator ->
            string "admin"

        RoleCHW ->
            string "chw"

        RoleNurse ->
            string "nurse"


encodeResilienceRole : ResilienceRole -> Value
encodeResilienceRole =
    resilienceRoleToString >> string
