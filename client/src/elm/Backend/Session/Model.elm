module Backend.Session.Model exposing (..)

{-| Represents an occasion on which measurements may be taken,
including the time and the place.
-}

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import Dict exposing (Dict)
import EveryDictList exposing (EveryDictList)
import EveryDict exposing (EveryDict)
import Gizra.NominalDate exposing (NominalDateRange)


{-| This is the basic `Session` data that we get when we're
online.
-}
type alias Session =
    { scheduledDate : NominalDateRange
    , clinicId : ClinicId
    }


{-| This adds the additional information we get when we take
a Session offline for data-entry. It includes everything we need for
data-entry.
-}
type alias OfflineSession =
    { session : Session
    , clinic : Clinic

    -- We'll sort by mother's name
    , mothers : EveryDictList MotherId Mother
    , children : EveryDict ChildId Child
    , motherMeasurements : EveryDict MotherId MotherMeasurements
    , childMeasurements : EveryDict ChildId ChildMeasurements
    }


{-| Represents the different kind of measurements we can have for a mother.
-}
type alias MotherMeasurements =
    { familyPlannings : EveryDictList FamilyPlanningId FamilyPlanning
    }


{-| We'll sort these by the date measured, with the most recent first, since
we're particularly interested in the most recent one, and it is faster to
access if it is first.
-}
type alias ChildMeasurements =
    { heights : EveryDictList HeightId Height
    , muacs : EveryDictList MuacId Muac
    , nutritions : EveryDictList ChildNutritionId ChildNutrition
    , photos : EveryDictList PhotoId Photo
    , weights : EveryDictList WeightId Weight
    }
