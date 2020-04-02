module Pages.AcuteIllnessActivity.Utils exposing (fromSymptomsGIValue, fromSymptomsGeneralValue, fromSymptomsRespiratoryValue, ifEmpty, symptomsGIFormWithDefault, symptomsGeneralFormWithDefault, symptomsRespiratoryFormWithDefault, symptomsTasksCompletedFromTotal, toSymptomsGIValue, toSymptomsGIValueWithDefault, toSymptomsGeneralValue, toSymptomsGeneralValueWithDefault, toSymptomsRespiratoryValue, toSymptomsRespiratoryValueWithDefault)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements, SymptomsGISign(..), SymptomsGeneralSign(..), SymptomsRespiratorySign(..))
import EverySet exposing (EverySet)
import Maybe.Extra exposing (or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.Utils exposing (taskCompleted)


symptomsTasksCompletedFromTotal : AcuteIllnessMeasurements -> SymptomsData -> SymptomsTask -> ( Int, Int )
symptomsTasksCompletedFromTotal measurements data task =
    case task of
        SymptomsGeneral ->
            let
                form =
                    measurements.symptomsGeneral
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
            in
            ( taskCompleted form.signs
            , 1
            )

        SymptomsRespiratory ->
            let
                form =
                    measurements.symptomsRespiratory
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
            in
            ( taskCompleted form.signs
            , 1
            )

        SymptomsGI ->
            let
                form =
                    measurements.symptomsGI
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGIFormWithDefault data.symptomsGIForm
            in
            ( taskCompleted form.signs
            , 1
            )


ifEmpty : a -> EverySet a -> EverySet a
ifEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


fromSymptomsGeneralValue : Maybe (EverySet SymptomsGeneralSign) -> SymptomsGeneralForm
fromSymptomsGeneralValue saved =
    { signs = Maybe.map EverySet.toList saved }


symptomsGeneralFormWithDefault : SymptomsGeneralForm -> Maybe (EverySet SymptomsGeneralSign) -> SymptomsGeneralForm
symptomsGeneralFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toSymptomsGeneralValueWithDefault : Maybe (EverySet SymptomsGeneralSign) -> SymptomsGeneralForm -> Maybe (EverySet SymptomsGeneralSign)
toSymptomsGeneralValueWithDefault saved form =
    symptomsGeneralFormWithDefault form saved
        |> toSymptomsGeneralValue


toSymptomsGeneralValue : SymptomsGeneralForm -> Maybe (EverySet SymptomsGeneralSign)
toSymptomsGeneralValue form =
    Maybe.map (EverySet.fromList >> ifEmpty NoSymptomsGeneral) form.signs


fromSymptomsRespiratoryValue : Maybe (EverySet SymptomsRespiratorySign) -> SymptomsRespiratoryForm
fromSymptomsRespiratoryValue saved =
    { signs = Maybe.map EverySet.toList saved }


symptomsRespiratoryFormWithDefault : SymptomsRespiratoryForm -> Maybe (EverySet SymptomsRespiratorySign) -> SymptomsRespiratoryForm
symptomsRespiratoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toSymptomsRespiratoryValueWithDefault : Maybe (EverySet SymptomsRespiratorySign) -> SymptomsRespiratoryForm -> Maybe (EverySet SymptomsRespiratorySign)
toSymptomsRespiratoryValueWithDefault saved form =
    symptomsRespiratoryFormWithDefault form saved
        |> toSymptomsRespiratoryValue


toSymptomsRespiratoryValue : SymptomsRespiratoryForm -> Maybe (EverySet SymptomsRespiratorySign)
toSymptomsRespiratoryValue form =
    Maybe.map (EverySet.fromList >> ifEmpty NoSymptomsRespiratory) form.signs


fromSymptomsGIValue : Maybe (EverySet SymptomsGISign) -> SymptomsGIForm
fromSymptomsGIValue saved =
    { signs = Maybe.map EverySet.toList saved }


symptomsGIFormWithDefault : SymptomsGIForm -> Maybe (EverySet SymptomsGISign) -> SymptomsGIForm
symptomsGIFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toSymptomsGIValueWithDefault : Maybe (EverySet SymptomsGISign) -> SymptomsGIForm -> Maybe (EverySet SymptomsGISign)
toSymptomsGIValueWithDefault saved form =
    symptomsGIFormWithDefault form saved
        |> toSymptomsGIValue


toSymptomsGIValue : SymptomsGIForm -> Maybe (EverySet SymptomsGISign)
toSymptomsGIValue form =
    Maybe.map (EverySet.fromList >> ifEmpty NoSymptomsGI) form.signs
