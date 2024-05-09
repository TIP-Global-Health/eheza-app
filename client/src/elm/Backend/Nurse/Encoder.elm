module Backend.Nurse.Encoder exposing (encodeNurse)

import Backend.Nurse.Model exposing (..)
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeGender, encodeMaritalStatus, encodeUbudehe)
import Backend.ResilienceMessage.Encoder exposing (encodeResilienceMessage)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Gizra.TimePosix exposing (encodePosixAsSeconds)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet, encodeNullable)


encodeNurse : Nurse -> List ( String, Value )
encodeNurse nurse =
    let
        resilienceMessages =
            if List.isEmpty nurse.resilienceMessages then
                []

            else
                [ ( "resilience_messages", list (encodeResilienceMessage >> Json.Encode.object) nurse.resilienceMessages ) ]
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
        ++ encodeIfSet "email" nurse.email string
        ++ encodeIfSet "next_reminder" nurse.resilienceNextReminder encodePosixAsSeconds
        ++ resilienceMessages


encodeRole : Role -> Value
encodeRole role =
    case role of
        RoleAdministrator ->
            string "admin"

        RoleCHW ->
            string "chw"

        RoleNurse ->
            string "nurse"

        RoleLabTech ->
            string "lab-tech"


encodeResilienceRole : ResilienceRole -> Value
encodeResilienceRole =
    resilienceRoleToString >> string
