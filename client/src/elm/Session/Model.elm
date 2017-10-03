module Session.Model exposing (..)

{-| Represents an occasion on which measurements may be taken,
including the time and the place.
-}

import Clinic.Model exposing (ClinicId)
import Gizra.NominalDate exposing (NominalDateRange)


type SessionId
    = SessionId Int


type alias Session =
    { name : String
    , scheduledDate : NominalDateRange
    , clnicId : ClinicId
    }
