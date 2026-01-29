module Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isAuthorithedNurse, isCommunityHealthWorker, isLabTechnician, resilienceRoleFromString, resilienceRoleToString, resolveMainRole)

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


resolveMainRole : Nurse -> Maybe Role
resolveMainRole nurse =
    if EverySet.member RoleNurse nurse.roles then
        Just RoleNurse

    else if EverySet.member RoleCHW nurse.roles then
        Just RoleCHW

    else if EverySet.member RoleLabTech nurse.roles then
        Just RoleLabTech

    else
        Nothing


isCommunityHealthWorker : Nurse -> Bool
isCommunityHealthWorker =
    resolveMainRole >> (==) (Just RoleCHW)


isLabTechnician : Nurse -> Bool
isLabTechnician =
    resolveMainRole >> (==) (Just RoleLabTech)


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
