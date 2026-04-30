module Backend.Nurse.Encoder exposing (encodeNurse)

import AssocList as Dict
import Backend.Nurse.Model exposing (Nurse, ResilienceRole, Role(..))
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.Person.Encoder exposing (encodeEducationLevel, encodeGender, encodeMaritalStatus, encodeUbudehe)
import Backend.ResilienceMessage.Encoder exposing (encodeResilienceMessage)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Gizra.TimePosix exposing (encodePosixAsSeconds)
import Json.Encode exposing (Value, bool, list, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


encodeNurse : Nurse -> List ( String, Value )
encodeNurse nurse =
    let
        resilienceMessages =
            let
                messages =
                    Dict.values nurse.resilienceMessages
            in
            if List.isEmpty messages then
                []

            else
                [ ( "resilience_messages", list (encodeResilienceMessage >> Json.Encode.object) messages ) ]
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
