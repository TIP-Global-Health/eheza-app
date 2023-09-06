module Backend.Nurse.Utils exposing (..)

import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (..)
import EverySet


assignedToHealthCenter : HealthCenterId -> Nurse -> Bool
assignedToHealthCenter healthCenterId nurse =
    EverySet.member healthCenterId nurse.healthCenters


assignedToVillage : VillageId -> Nurse -> Bool
assignedToVillage villageId nurse =
    EverySet.member villageId nurse.villages


isCommunityHealthWorker : Nurse -> Bool
isCommunityHealthWorker nurse =
    if EverySet.member RoleNurse nurse.roles then
        False

    else
        EverySet.member RoleCHW nurse.roles


isAuthorithedNurse : Clinic -> Nurse -> Bool
isAuthorithedNurse clinic nurse =
    if isCommunityHealthWorker nurse then
        clinic.villageId
            |> Maybe.map (\id -> assignedToVillage id nurse)
            |> Maybe.withDefault False

    else
        assignedToHealthCenter clinic.healthCenterId nurse


resilienceRoleFromString : String -> Maybe ResilienceRole
resilienceRoleFromString role =
    case role of
        "nurse" ->
            Just ResilienceRoleNurse

        "chw" ->
            Just ResilienceRoleCHW

        "line-manager" ->
            Just ResilienceRoleLineManager

        "supervisor" ->
            Just ResilienceRoleSupervisor

        "director" ->
            Just ResilienceRoleDirector

        _ ->
            Nothing


resilienceRoleToString : ResilienceRole -> String
resilienceRoleToString role =
    case role of
        ResilienceRoleNurse ->
            "nurse"

        ResilienceRoleCHW ->
            "chw"

        ResilienceRoleLineManager ->
            "line-manager"

        ResilienceRoleSupervisor ->
            "supervisor"

        ResilienceRoleDirector ->
            "director"
