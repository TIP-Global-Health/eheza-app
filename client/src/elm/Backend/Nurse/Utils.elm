module Backend.Nurse.Utils exposing (assignedToHealthCenter, assignedToVillage, isCommunityHealthWorker)

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
