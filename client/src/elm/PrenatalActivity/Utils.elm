module PrenatalActivity.Utils exposing
    ( decodeActivityFromString
    , defaultActivity
    , encodeActivityAsString
    , generateHighRiskAlertData
    , generateHighSeverityAlertData
    , generateRiskFactorAlertData
    , getActivityIcon
    , getAllActivities
    )

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.Measurement.Model exposing (PrenatalMeasurements, PreviousDeliverySign(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate, diffDays)
import PrenatalActivity.Model exposing (..)
import Translate exposing (Language, TranslationId, translate)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
encodeActivityAsString : PrenatalActivity -> String
encodeActivityAsString activity =
    case activity of
        DangerSigns ->
            "danger-signs"

        Examination ->
            "examination"

        FamilyPlanning ->
            "planning"

        History ->
            "history"

        PatientProvisions ->
            "patient-provisions"

        PregnancyDating ->
            "pregnancy-dating"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe PrenatalActivity
decodeActivityFromString s =
    case s of
        "danger-signs" ->
            Just DangerSigns

        "examination" ->
            Just Examination

        "planning" ->
            Just FamilyPlanning

        "history" ->
            Just History

        "patient-provisions" ->
            Just PatientProvisions

        "pregnancy-dating" ->
            Just PregnancyDating

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : PrenatalActivity
defaultActivity =
    PregnancyDating


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : PrenatalActivity -> String
getActivityIcon activity =
    encodeActivityAsString activity


getAllActivities : List PrenatalActivity
getAllActivities =
    [ PregnancyDating, History, Examination, FamilyPlanning, PatientProvisions, DangerSigns ]


generateHighRiskAlertData : Language -> PrenatalMeasurements -> HighRiskFactor -> Maybe String
generateHighRiskAlertData language measurements factor =
    let
        transAlert factor =
            translate language (Translate.HighRiskFactor factor)
    in
    case factor of
        PrenatalActivity.Model.ConvulsionsAndUnconsciousPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.ConvulsionsAndUnconsciousPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        PrenatalActivity.Model.ConvulsionsPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.ConvulsionsPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )


generateHighSeverityAlertData : Language -> NominalDate -> PrenatalMeasurements -> HighSeverityAlert -> Maybe ( String, String )
generateHighSeverityAlertData language currentDate measurements alert =
    let
        trans =
            translate language

        transAlert alert =
            trans (Translate.HighSeverityAlert alert)
    in
    case alert of
        BodyTemperature ->
            measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .bodyTemperature
                        in
                        if value >= 38.5 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , toString value ++ "°C"
                                )

                        else if value < 35 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , toString value ++ "°C"
                                )

                        else
                            Nothing
                    )

        BloodPressure ->
            measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            sys =
                                Tuple.second measurement |> .value |> .sys

                            dia =
                                Tuple.second measurement |> .value |> .dia
                        in
                        if sys > 180 || dia > 100 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , toString sys ++ "/" ++ toString dia ++ trans Translate.MMHGUnit
                                )

                        else
                            Nothing
                    )

        FetalHeartRate ->
            measurements.lastMenstrualPeriod
                |> Maybe.andThen
                    (\lastMenstrualPeriod ->
                        let
                            lmpDate =
                                Tuple.second lastMenstrualPeriod |> .value |> .date

                            egaInWeeks =
                                diffDays lmpDate currentDate // 7
                        in
                        if egaInWeeks > 19 then
                            measurements.obstetricalExam
                                |> Maybe.andThen
                                    (\measurement ->
                                        let
                                            value =
                                                Tuple.second measurement |> .value |> .fetalHeartRate
                                        in
                                        if value == 0 then
                                            Just ( transAlert alert, "" )

                                        else
                                            Nothing
                                    )

                        else
                            Nothing
                    )

        FetalMovement ->
            measurements.lastMenstrualPeriod
                |> Maybe.andThen
                    (\lastMenstrualPeriod ->
                        let
                            lmpDate =
                                Tuple.second lastMenstrualPeriod |> .value |> .date

                            egaInWeeks =
                                diffDays lmpDate currentDate // 7
                        in
                        if egaInWeeks > 19 then
                            measurements.obstetricalExam
                                |> Maybe.andThen
                                    (\measurement ->
                                        let
                                            value =
                                                Tuple.second measurement |> .value |> .fetalMovement
                                        in
                                        if value == False then
                                            Just ( transAlert alert, "" )

                                        else
                                            Nothing
                                    )

                        else
                            Nothing
                    )

        HeartRate ->
            measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .heartRate
                        in
                        if value >= 120 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , toString value ++ trans Translate.BpmUnit
                                )

                        else if value < 40 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , toString value ++ trans Translate.BpmUnit
                                )

                        else
                            Nothing
                    )

        RespiratoryRate ->
            measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .respiratoryRate
                        in
                        if value > 30 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , toString value ++ trans Translate.BpmUnit
                                )

                        else if value < 12 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , toString value ++ trans Translate.BpmUnit
                                )

                        else
                            Nothing
                    )


generateRiskFactorAlertData : Language -> NominalDate -> PrenatalMeasurements -> RiskFactor -> Maybe String
generateRiskFactorAlertData language currentDate measurements factor =
    let
        trans =
            translate language

        transAlert alert =
            trans (Translate.RiskFactorAlert alert)
    in
    case factor of
        FactorNumberOfCSections dummy ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .cSections
                        in
                        if value > 0 then
                            Just (transAlert (FactorNumberOfCSections value))

                        else
                            Nothing
                    )

        FactorCSectionInPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.CSectionInPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorCSectionReason ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                -- There must be only one value.
                                Tuple.second measurement
                                    |> .value
                                    |> .cSectionReason
                                    |> EverySet.toList
                                    |> List.head
                                    |> Maybe.withDefault Backend.Measurement.Model.None
                        in
                        if value /= Backend.Measurement.Model.None then
                            Just (transAlert factor ++ " " ++ trans (Translate.CSectionReasons value))

                        else
                            Nothing
                    )

        FactorPreviousDeliveryPeriod ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                -- There must be only one value.
                                Tuple.second measurement
                                    |> .value
                                    |> .previousDeliveryPeriod
                                    |> EverySet.toList
                                    |> List.head
                                    |> Maybe.withDefault Backend.Measurement.Model.Neither
                        in
                        if value /= Backend.Measurement.Model.Neither then
                            Just (transAlert factor ++ " " ++ trans (Translate.PreviousDeliveryPeriods value))

                        else
                            Nothing
                    )

        FactorSuccessiveAbortions ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.SuccessiveAbortions signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorSuccessivePrematureDeliveries ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.SuccessivePrematureDeliveries signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorStillbornPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.StillbornPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorBabyDiedOnDayOfBirthPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.BabyDiedOnDayOfBirthPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorPartialPlacentaPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.PartialPlacentaPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorSevereHemorrhagingPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.SevereHemorrhagingPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorPreeclampsiaPreviousPregnancy ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.PreeclampsiaPreviousPregnancy signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorConvulsionsPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.ConvulsionsPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorConvulsionsAndUnconsciousPreviousDelivery ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .previousDelivery
                        in
                        if EverySet.member Backend.Measurement.Model.ConvulsionsAndUnconsciousPreviousDelivery signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorIncompleteCervixPreviousPregnancy ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.IncompleteCervixPreviousPregnancy signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )


generateMedicalDiagnosisAlertData : Language -> NominalDate -> PrenatalMeasurements -> MedicalDiagnosis -> Maybe String
generateMedicalDiagnosisAlertData language currentDate measurements diagnosis =
    let
        transAlert diagnosis =
            translate language (Translate.MedicalDiagnosisAlert diagnosis)
    in
    case diagnosis of
        DiagnosisUterineMyoma ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.UterineMyoma value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisDiabetes ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.Diabetes value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisCardiacDisease ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.CardiacDisease value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisRenalDisease ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.RenalDisease value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisHypertensionBeforePregnancy ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.HypertensionBeforePregnancy value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisTuberculosis ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if
                            EverySet.member Backend.Measurement.Model.TuberculosisPast value
                                || EverySet.member Backend.Measurement.Model.TuberculosisPresent value
                        then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisAsthma ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.Asthma value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisBowedLegs ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.BowedLegs value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisHIV ->
            measurements.medicalHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.HIV value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosMentalHealthHistory ->
            measurements.socialHistory
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value
                        in
                        if EverySet.member Backend.Measurement.Model.MentalHealthHistory value then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )


generateObstetricDiagnosisAlertData : Language -> NominalDate -> PrenatalMeasurements -> ObstetricDiagnosis -> Maybe String
generateObstetricDiagnosisAlertData language currentDate measurements diagnosis =
    let
        transAlert diagnosis =
            translate language (Translate.ObstetricDiagnosisAlert diagnosis)
    in
    case diagnosis of
        DiagnosisGestationalDiabetesPreviousPregnancy ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.GestationalDiabetesPreviousPregnancy signs then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )

        DiagnosisRhNegative ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.GestationalDiabetesPreviousPregnancy signs then
                            Just (transAlert diagnosis)

                        else
                            Nothing
                    )
