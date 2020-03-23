module Pages.NutritionActivity.Utils exposing (fromNutritionValue, nutritionFormWithDefault, toNutritionValue, toNutritionValueWithDefault)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (ChildNutritionSign(..))
import EverySet exposing (EverySet)
import Maybe.Extra exposing (or, unwrap)
import Pages.NutritionActivity.Model exposing (..)


ifEmpty : a -> EverySet a -> EverySet a
ifEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEmpty NormalChildNutrition) form.signs
