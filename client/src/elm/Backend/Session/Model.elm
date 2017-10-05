module Backend.Session.Model exposing (..)

{-| Represents an occasion on which measurements may be taken,
including the time and the place.
-}

import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Mother.Model exposing (Mother)
import Dict exposing (Dict)
import DictList exposing (DictList)
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
    , mothers : DictList MotherId Mother
    , children : DictList ChildId Child
    , motherMeasurements : Dict MotherId MotherMeasurements
    , childMeasurements : Dict ChildId ChildMeasurements
    }


{-| Represents the different kind of measurements we can have for a mother.
-}
type alias MotherMeasurements =
    { familyPlannings : DictList FamilyPlanningId FamilyPlanning
    }


type alias ChildMeasurements =
    { heights : DictList HeightId Height
    , muacs : DictList MuacId Muac
    , nutritions : DictList ChildNutritionId ChildNutrition
    , photos : DictList PhotoId Photo
    , weights : DictList WeightId Weight
    }
