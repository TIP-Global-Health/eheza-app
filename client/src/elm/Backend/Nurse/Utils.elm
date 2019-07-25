module Backend.Nurse.Utils exposing (assignedToHealthCenter)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (..)
import EverySet


assignedToHealthCenter : HealthCenterId -> Nurse -> Bool
assignedToHealthCenter healthCenterId nurse =
    EverySet.member healthCenterId nurse.healthCenters
