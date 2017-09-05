module Measurement.Model exposing (..)

import Activity.Model exposing (ActivityType, ChildNutritionSign, FamilyPlanningSign)
import EverySet exposing (EverySet)
import Http
import RemoteData exposing (RemoteData(..), WebData)


{-| Indicate which Activity was completed, and to which Activity to redirect to.
This can be used as a return value in an `update` function upon form save.
-}
type alias CompletedAndRedirectToActivityTuple =
    ( ActivityType, ActivityType )


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FloatInput =
    Maybe Float


type alias FileId =
    Int


type alias PhotoId =
    Int


type alias Photo =
    { url : String }


type Msg
    = FamilyPlanningSignsSave
    | FamilyPlanningSignsToggle FamilyPlanningSign
    | HandleDropzoneUploadedFile Int
    | HandleFamilyPlanningSave (Result Http.Error ())
    | HandleHeightSave (Result Http.Error ())
    | HandleNutritionSignsSave (Result Http.Error ())
    | HandleMuacSave (Result Http.Error ())
    | HandlePhotoSave (Result Http.Error ( PhotoId, Photo ))
    | HandleWeightSave (Result Http.Error ())
    | HeightSave
    | HeightUpdate Float
    | MuacUpdate Float
    | MuacSave
    | NutritionSignsToggle ChildNutritionSign
    | NutritionSignsSave
    | PhotoSave
    | ResetDropZone
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput
    , nutritionSigns : EverySet ChildNutritionSign
    , familyPlanningSigns : EverySet FamilyPlanningSign
    , photo : ( Maybe FileId, Maybe ( PhotoId, Photo ) )
    , weight : FloatInput
    }


{-| An interpretation of a MUAC, according to the measurement
tool referenced at <https://github.com/Gizra/ihangane/issues/282>
-}
type MuacIndication
    = MuacGreen
    | MuacRed
    | MuacYellow


type FloatMeasurements
    = HeightFloat
    | MuacFloat
    | WeightFloat


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { minVal = 0.5
    , maxVal = 100
    }


getInputConstraintsMuac : FloatInputConstraints
getInputConstraintsMuac =
    { minVal = 0.5
    , maxVal = 40
    }


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { minVal = 0.5
    , maxVal = 60
    }


emptyModel : Model
emptyModel =
    { status = NotAsked
    , height = Nothing
    , muac = Nothing
    , nutritionSigns = EverySet.empty
    , familyPlanningSigns = EverySet.empty
    , photo = ( Nothing, Nothing )
    , weight = Nothing
    }
