module Pages.AcuteIllnessEncounter.Utils exposing (suspectedCovid19Case)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecomendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaTestingSign(..)
        , ReasonForNotIsolating(..)
        , ResponsePeriod(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        )
import EverySet exposing (EverySet)


suspectedCovid19Case : AcuteIllnessMeasurements -> Bool
suspectedCovid19Case measurements =
    let
        calculateSymptoms measurement_ default =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                Dict.size set
                        in
                        case setSize of
                            1 ->
                                if Dict.member default set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        calculateSigns measurement_ default =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                EverySet.size set
                        in
                        case setSize of
                            1 ->
                                if EverySet.member default set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        totalSymptoms =
            calculateSymptoms measurements.symptomsGeneral NoSymptomsGeneral
                + calculateSymptoms measurements.symptomsRespiratory NoSymptomsRespiratory
                + calculateSymptoms measurements.symptomsGI NoSymptomsGI

        totalSigns =
            calculateSigns measurements.travelHistory NoTravelHistorySigns
                + calculateSigns measurements.exposure NoExposureSigns

        fever =
            measurements.vitals
                |> Maybe.map
                    (\measurement ->
                        let
                            bodyTemperature =
                                Tuple.second measurement |> .value |> .bodyTemperature
                        in
                        bodyTemperature > 37.5
                    )
                |> Maybe.withDefault False
    in
    totalSigns > 0 && (fever || totalSymptoms > 1)
