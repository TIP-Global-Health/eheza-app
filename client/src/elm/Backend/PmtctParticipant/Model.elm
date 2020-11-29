module Backend.PmtctParticipant.Model exposing (AdultActivities(..), PmtctParticipant)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias PmtctParticipant =
    { child : PersonId
    , adult : PersonId
    , adultActivities : AdultActivities
    , start : NominalDate
    , end : Maybe NominalDate
    , clinic : ClinicId
    , deleted : Bool
    }


type AdultActivities
    = CaregiverActivities
    | MotherActivities
