module Measurement.Model exposing (..)

import Activity.Model exposing (ActivityType, ChildNutritionSign, FamilyPlanningSign)
import EveryDict exposing (EveryDict)
import Http
import RemoteData exposing (RemoteData(..), WebData)


{-| Indicate which Activity was completed, and to which Activity to redirect to.
This can be used as a return value in an `update` function upon form save.
-}
type alias CompletedAndRedirectToActivityTuple =
    ( ActivityType, ActivityType )


type alias EveryDictChildNutritionSign =
    EveryDict ChildNutritionSign ()


type alias EveryDictFamilyPlanningSigns =
    EveryDict FamilyPlanningSign ()


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FloatInput =
    Maybe String


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
    | HeightUpdate String
    | MuacUpdate String
    | MuacSave
    | NutritionSignsToggle ChildNutritionSign
    | NutritionSignsSave
    | PhotoSave
    | ResetDropZone
    | WeightSave
    | WeightUpdate String


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput

    -- We use EveryDict instead of Set, as we want the key to be a typed value
    -- and not have to cast it to string.
    , nutritionSigns : EveryDictChildNutritionSign
    , familyPlanningSigns : EveryDictFamilyPlanningSigns
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
    , nutritionSigns = EveryDict.empty
    , familyPlanningSigns = EveryDict.empty
    , photo = ( Nothing, Nothing )
    , weight = Nothing
    }


saveMeasurementMessage : Msg -> Bool
saveMeasurementMessage msg =
    case msg of
        FamilyPlanningSignsSave ->
            True

        HeightSave ->
            True

        MuacSave ->
            True

        NutritionSignsSave ->
            True

        PhotoSave ->
            True

        WeightSave ->
            True

        _ ->
            False


getFloatInputValue : String -> Float
getFloatInputValue input =
    let
        normilizedInput =
            if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
                String.dropRight 1 input
            else
                input
    in
        case String.toFloat normilizedInput of
            Ok value ->
                value

            Err error ->
                0.0


normalizeFloatFormInput : String -> String
normalizeFloatFormInput input =
    let
        normilizedInput =
            if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
                String.dropRight 1 input
            else
                input
    in
        case String.toFloat normilizedInput of
            Ok value ->
                input

            Err error ->
                if input == "." then
                    "0."
                else
                    String.dropRight 1 input


normalizeFloatInput : FloatInput -> FloatInput
normalizeFloatInput floatInput =
    let
        input =
            floatInput |> Maybe.withDefault "0.0"
    in
        if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
            Just <| String.dropRight 1 input
        else
            Just input
