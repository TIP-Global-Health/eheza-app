module Pages.AcuteIllnessActivity.Utils exposing (acuteIllnessFormWithDefault, fromAcuteIllnessValue, toAcuteIllnessValue, toAcuteIllnessValueWithDefault)

import AssocList as Dict exposing (Dict)
import EverySet exposing (EverySet)
import Maybe.Extra exposing (or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)


ifEmpty : a -> EverySet a -> EverySet a
ifEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


fromAcuteIllnessValue : AcuteIllnessValue -> AcuteIllnessForm
fromAcuteIllnessValue saved =
    {}


acuteIllnessFormWithDefault : AcuteIllnessForm -> AcuteIllnessValue -> AcuteIllnessForm
acuteIllnessFormWithDefault form saved =
    form


toAcuteIllnessValueWithDefault : AcuteIllnessValue -> AcuteIllnessForm -> AcuteIllnessValue
toAcuteIllnessValueWithDefault saved form =
    acuteIllnessFormWithDefault form saved
        |> toAcuteIllnessValue


toAcuteIllnessValue : AcuteIllnessForm -> AcuteIllnessValue
toAcuteIllnessValue form =
    {}
