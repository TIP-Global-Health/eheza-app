module Pages.AcuteIllnessActivity.Utils exposing (allSymptomsGISigns, allSymptomsGeneralSigns, allSymptomsRespiratorySigns, symptomsGIFormWithDefault, symptomsGeneralFormWithDefault, symptomsRespiratoryFormWithDefault, symptomsTasksCompletedFromTotal, taskCompleted, toggleSymptomsSign)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (AcuteIllnessMeasurements, SymptomsGISign(..), SymptomsGeneralSign(..), SymptomsRespiratorySign(..))
import Maybe.Extra exposing (or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)



-- import Pages.Utils exposing (taskCompleted)


taskCompleted : Bool -> Int
taskCompleted notCompleted =
    if notCompleted then
        0

    else
        1


allSymptomsGeneralSigns : ( List SymptomsGeneralSign, SymptomsGeneralSign )
allSymptomsGeneralSigns =
    ( [ SymptomGeneralFever
      , Chills
      , NightSweats
      , BodyAches
      , Headache
      ]
    , NoSymptomsGeneral
    )


allSymptomsRespiratorySigns : ( List SymptomsRespiratorySign, SymptomsRespiratorySign )
allSymptomsRespiratorySigns =
    ( [ Cough
      , ShortnessOfBreath
      , NasalCongestion
      , BloodInSputum
      , SoreThroat
      ]
    , NoSymptomsRespiratory
    )


allSymptomsGISigns : ( List SymptomsGISign, SymptomsGISign )
allSymptomsGISigns =
    ( [ BloodyDiarrhea
      , NonBloodyDiarrhea
      , Nausea
      , Vomiting
      , SymptomGIAbdominalPain
      ]
    , NoSymptomsGI
    )


toggleSymptomsSign : SymptomsTask -> a -> a -> { signs : Dict a Int } -> { signs : Dict a Int }
toggleSymptomsSign task sign noneSign form =
    let
        signs =
            form.signs

        updatedSigns =
            if sign == noneSign then
                Dict.singleton sign 1

            else
                let
                    signs_ =
                        Dict.remove noneSign signs
                in
                if Dict.member sign signs_ then
                    Dict.remove sign signs_

                else
                    Dict.insert sign 1 signs_
    in
    { form | signs = updatedSigns }


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
            ( taskCompleted (Dict.isEmpty form.signs)
            , 1
            )

        SymptomsRespiratory ->
            let
                form =
                    measurements.symptomsRespiratory
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
            in
            ( taskCompleted (Dict.isEmpty form.signs)
            , 1
            )

        SymptomsGI ->
            let
                form =
                    measurements.symptomsGI
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGIFormWithDefault data.symptomsGIForm
            in
            ( taskCompleted (Dict.isEmpty form.signs)
            , 1
            )



-- fromSymptomsGeneralValue : Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm
-- fromSymptomsGeneralValue saved =
--     { signs = saved }
--
--


symptomsGeneralFormWithDefault : SymptomsGeneralForm -> Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm
symptomsGeneralFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsGeneralForm value

                else
                    form
            )



--
--
-- toSymptomsGeneralValueWithDefault : Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm -> Maybe (Dict SymptomsGeneralSign Int)
-- toSymptomsGeneralValueWithDefault saved form =
--     symptomsGeneralFormWithDefault form saved
--         |> toSymptomsGeneralValue
--
--
-- toSymptomsGeneralValue : SymptomsGeneralForm -> Maybe (Dict SymptomsGeneralSign Int)
-- toSymptomsGeneralValue form =
--     form.signs
--
--
-- fromSymptomsRespiratoryValue : Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm
-- fromSymptomsRespiratoryValue saved =
--     { signs = saved }
--
--


symptomsRespiratoryFormWithDefault : SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm
symptomsRespiratoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsRespiratoryForm value

                else
                    form
            )



--
--
-- toSymptomsRespiratoryValueWithDefault : Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int)
-- toSymptomsRespiratoryValueWithDefault saved form =
--     symptomsRespiratoryFormWithDefault form saved
--         |> toSymptomsRespiratoryValue
--
--
-- toSymptomsRespiratoryValue : SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int)
-- toSymptomsRespiratoryValue form =
--     form.signs
--
--
-- fromSymptomsGIValue : Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm
-- fromSymptomsGIValue saved =
--     { signs = saved }
--
--


symptomsGIFormWithDefault : SymptomsGIForm -> Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm
symptomsGIFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsGIForm value

                else
                    form
            )



--
--
-- toSymptomsGIValueWithDefault : Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm -> Maybe (Dict SymptomsGISign Int)
-- toSymptomsGIValueWithDefault saved form =
--     symptomsGIFormWithDefault form saved
--         |> toSymptomsGIValue
--
--
-- toSymptomsGIValue : SymptomsGIForm -> Maybe (Dict SymptomsGISign Int)
-- toSymptomsGIValue form =
--     form.signs
