module User.Utils exposing (assignedToClinic)

import Backend.Entities exposing (..)
import User.Model exposing (..)


assignedToClinic : ClinicId -> User -> Bool
assignedToClinic clinicId user =
    List.any ((==) clinicId) user.clinics
