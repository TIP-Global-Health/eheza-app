module Measurement.Model exposing (..)

{-| These modules manage the UI for the various measurements that collectively
form an examination for a participant.
-}

import Activity.Model exposing (ActivityType, ChildNutritionSign, FamilyPlanningSign)
import EverySet exposing (EverySet)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import StorageKey exposing (StorageKey(..))


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
    Maybe String


type alias FileId =
    Int


type alias PhotoId =
    Int


type HeightId
    = HeightId Int


type MuacId
    = MuacId Int


type WeightId
    = WeightId Int


type NutritionId
    = NutritionId Int


type FamilyPlanningId
    = FamilyPlanningId Int


type alias Photo =
    { url : String }


{-| For the `Handle` msgs, we should probably actually decode the
response from the backend. For the moment, we're just threading
through the value we supplied, and assuming that if the backend
indicated success, it accepted the value unchanged.
-}
type Msg
    = FamilyPlanningSignsSave
    | FamilyPlanningSignsToggle FamilyPlanningSign
    | HandleDropzoneUploadedFile Int
    | HandleFamilyPlanningSave (Result Http.Error ( StorageKey FamilyPlanningId, EverySet FamilyPlanningSign ))
    | HandleHeightSave (Result Http.Error ( StorageKey HeightId, Float ))
    | HandleNutritionSignsSave (Result Http.Error ( StorageKey NutritionId, EverySet ChildNutritionSign ))
    | HandleMuacSave (Result Http.Error ( StorageKey MuacId, Float ))
    | HandlePhotoSave (Result Http.Error ( PhotoId, Photo ))
    | HandleWeightSave (Result Http.Error ( StorageKey WeightId, Float ))
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


{-| The strategy here, at least for now, is this:

  - The values in the `Model` here reflect what is entered in the UI. So, they
    are updated on every key-press etc.

  - The `update` function takes an `examination` parameter. It updates that
    parameter only when the value has actually been saved to the backend.

An alternative would be to operate on the examination directly, so we wouldn't
have these values in the `Model` at all. In that case, the values in the
`ExaminationChild` would need to be more complex types -- e.g. something
like `EditableWebData`. Which is probably ultimately a good idea -- for the
moment, just trying to get this to work at all.

-}
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
