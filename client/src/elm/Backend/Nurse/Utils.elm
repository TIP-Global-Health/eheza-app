module Backend.Nurse.Utils exposing (assignedToClinic)

import Backend.Entities exposing (..)
import Backend.Nurse.Model exposing (..)


assignedToClinic : ClinicId -> Nurse -> Bool
assignedToClinic clinicId nurse =
    List.any ((==) clinicId) nurse.clinics
