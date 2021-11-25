module Backend.PrenatalActivity.Utils exposing
    ( decodeActivityFromString
    , encodeActivityAsString
    , generateHighRiskAlertData
    , generateHighSeverityAlertData
    , generateMedicalDiagnosisAlertData
    , generateObstetricalDiagnosisAlertData
    , generateRecurringHighSeverityAlertData
    , generateRiskFactorAlertData
    , getActivityIcon
    , getEncounterTrimesterData
    )

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.Measurement.Model exposing (HeightInCm(..), MuacInCm(..), PrenatalMeasurements, PreviousDeliverySign(..), WeightInKg(..))
import Backend.Measurement.Utils exposing (heightValueFunc, muacValueFunc, weightValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMyyyy)
import Maybe.Extra exposing (isJust)
import Pages.PrenatalActivity.Utils exposing (calculateBmi)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (getLastEncounterMeasurements, getLastEncounterMeasurementsWithDate)
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

        PrenatalPhoto ->
            "photo"

        Laboratory ->
            "laboratory"

        HealthEducation ->
            "health-education"

        BirthPlan ->
            "birth-plan"

        NextSteps ->
            "next-steps"

        PregnancyOutcome ->
            "pregnancy-outcome"


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

        "photo" ->
            Just PrenatalPhoto

        "laboratory" ->
            Just Laboratory

        "health-education" ->
            Just HealthEducation

        "birth-plan" ->
            Just BirthPlan

        "next-steps" ->
            Just NextSteps

        "pregnancy-outcome" ->
            Just PregnancyOutcome

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : PrenatalActivity -> String
getActivityIcon activity =
    encodeActivityAsString activity


generateHighRiskAlertData : Language -> PrenatalMeasurements -> HighRiskFactor -> Maybe String
generateHighRiskAlertData language measurements factor =
    let
        transAlert factor_ =
            translate language (Translate.HighRiskFactor factor_)
    in
    case factor of
        Backend.PrenatalActivity.Model.ConvulsionsAndUnconsciousPreviousDelivery ->
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

        Backend.PrenatalActivity.Model.ConvulsionsPreviousDelivery ->
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


generateHighSeverityAlertData : Language -> NominalDate -> Bool -> AssembledData -> HighSeverityAlert -> Maybe ( String, String )
generateHighSeverityAlertData language currentDate isChw data alert =
    let
        trans =
            translate language

        transAlert alert_ =
            trans (Translate.HighSeverityAlert alert_)

        lastEncounterMeasurementsWithDate =
            getLastEncounterMeasurementsWithDate currentDate isChw data
    in
    case alert of
        BodyTemperature ->
            data.measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .bodyTemperature
                        in
                        if value >= 38.5 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , String.fromFloat value ++ "°C"
                                )

                        else if value < 35 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , String.fromFloat value ++ "°C"
                                )

                        else
                            Nothing
                    )

        FetalHeartRate ->
            let
                resolveAlert ( date, measurements ) =
                    data.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        diffDays lmpDate date // 7
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
            in
            -- If obstetricalExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.obstetricalExam then
                resolveAlert ( currentDate, data.measurements )

            else
                resolveAlert lastEncounterMeasurementsWithDate

        FetalMovement ->
            let
                resolveAlert ( date, measurements ) =
                    data.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        diffDays lmpDate date // 7
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
            in
            -- If obstetricalExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.obstetricalExam then
                resolveAlert ( currentDate, data.measurements )

            else
                resolveAlert lastEncounterMeasurementsWithDate

        HeartRate ->
            data.measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .heartRate
                        in
                        if value >= 120 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , trans <| Translate.BpmUnit value
                                )

                        else if value < 40 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , trans <| Translate.BpmUnit value
                                )

                        else
                            Nothing
                    )

        RespiratoryRate ->
            data.measurements.vitals
                |> Maybe.andThen
                    (\measurement ->
                        let
                            value =
                                Tuple.second measurement |> .value |> .respiratoryRate
                        in
                        if value > 30 then
                            Just
                                ( trans Translate.High ++ " " ++ transAlert alert
                                , trans <| Translate.BpmUnit value
                                )

                        else if value < 12 then
                            Just
                                ( trans Translate.Low ++ " " ++ transAlert alert
                                , trans <| Translate.BpmUnit value
                                )

                        else
                            Nothing
                    )


generateRecurringHighSeverityAlertData : Language -> NominalDate -> Bool -> AssembledData -> RecurringHighSeverityAlert -> List ( String, String, String )
generateRecurringHighSeverityAlertData language currentDate isChw data alert =
    let
        trans =
            translate language

        transAlert alert_ =
            trans (Translate.RecurringHighSeverityAlert alert_)
    in
    case alert of
        BloodPressure ->
            let
                resolveAlert ( date, measurements ) =
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
                                        , String.fromFloat sys ++ "/" ++ String.fromFloat dia ++ trans Translate.MMHGUnit
                                        , formatDDMMyyyy date
                                        )

                                else
                                    Nothing
                            )
            in
            getAllNurseMeasurements currentDate isChw data
                |> List.filterMap resolveAlert


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

        FactorVerticalCSectionScar ->
            measurements.obstetricalExam
                |> Maybe.andThen
                    (\measurement ->
                        let
                            scar =
                                Tuple.second measurement |> .value |> .cSectionScar
                        in
                        if scar == Backend.Measurement.Model.Vertical then
                            Just (transAlert factor)

                        else
                            Nothing
                    )

        FactorGestationalDiabetesPreviousPregnancy ->
            measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\measurement ->
                        let
                            signs =
                                Tuple.second measurement |> .value |> .obstetricHistory
                        in
                        if EverySet.member Backend.Measurement.Model.GestationalDiabetesPreviousPregnancy signs then
                            Just (transAlert factor)

                        else
                            Nothing
                    )


generateMedicalDiagnosisAlertData : Language -> NominalDate -> PrenatalMeasurements -> MedicalDiagnosis -> Maybe String
generateMedicalDiagnosisAlertData language currentDate measurements diagnosis =
    let
        transAlert diagnosis_ =
            translate language (Translate.MedicalDiagnosisAlert diagnosis_)
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

        DiagnosisMentalHealthHistory ->
            measurements.medicalHistory
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


generateObstetricalDiagnosisAlertData : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> ObstetricalDiagnosis -> Maybe String
generateObstetricalDiagnosisAlertData language currentDate isChw firstEncounterMeasurements data diagnosis =
    let
        transAlert diagnosis_ =
            translate language (Translate.ObstetricalDiagnosisAlert diagnosis_)

        lastEncounterMeasurements =
            getLastEncounterMeasurements currentDate isChw data
    in
    case diagnosis of
        DiagnosisRhNegative ->
            firstEncounterMeasurements.obstetricHistoryStep2
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

        DiagnosisModerateUnderweight ->
            let
                resolveAlert measurements =
                    measurements.nutrition
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    muac =
                                        Tuple.second measurement
                                            |> .value
                                            |> .muac
                                            |> muacValueFunc
                                in
                                if muac >= 18.5 && muac < 22 then
                                    Just (transAlert diagnosis)

                                else
                                    Nothing
                            )
            in
            -- If nutrition measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.nutrition then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisSevereUnderweight ->
            let
                resolveAlert measurements =
                    measurements.nutrition
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    height =
                                        Tuple.second measurement
                                            |> .value
                                            |> .height
                                            |> heightValueFunc

                                    weight =
                                        Tuple.second measurement
                                            |> .value
                                            |> .weight
                                            |> weightValueFunc

                                    muac =
                                        Tuple.second measurement
                                            |> .value
                                            |> .muac
                                            |> muacValueFunc
                                in
                                if muac < 18.5 then
                                    Just (transAlert diagnosis)

                                else
                                    calculateBmi (Just height) (Just weight)
                                        |> Maybe.andThen
                                            (\bmi_ ->
                                                if bmi_ < 18.5 then
                                                    Just (transAlert diagnosis)

                                                else
                                                    Nothing
                                            )
                            )
            in
            -- If nutrition measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.nutrition then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisOverweight ->
            let
                resolveAlert measurements =
                    measurements.nutrition
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    height =
                                        Tuple.second measurement
                                            |> .value
                                            |> .height
                                            |> heightValueFunc

                                    weight =
                                        Tuple.second measurement
                                            |> .value
                                            |> .weight
                                            |> weightValueFunc
                                in
                                calculateBmi (Just height) (Just weight)
                                    |> Maybe.andThen
                                        (\bmi_ ->
                                            if bmi_ >= 25 && bmi_ <= 30 then
                                                Just (transAlert diagnosis)

                                            else
                                                Nothing
                                        )
                            )
            in
            -- If nutrition measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.nutrition then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisObese ->
            let
                resolveAlert measurements =
                    measurements.nutrition
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    height =
                                        Tuple.second measurement
                                            |> .value
                                            |> .height
                                            |> heightValueFunc

                                    weight =
                                        Tuple.second measurement
                                            |> .value
                                            |> .weight
                                            |> weightValueFunc
                                in
                                calculateBmi (Just height) (Just weight)
                                    |> Maybe.andThen
                                        (\bmi_ ->
                                            if bmi_ > 30 then
                                                Just (transAlert diagnosis)

                                            else
                                                Nothing
                                        )
                            )
            in
            -- If nutrition measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.nutrition then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DisgnosisPeripheralEdema ->
            let
                resolveAlert measurements =
                    measurements.corePhysicalExam
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    hands =
                                        Tuple.second measurement |> .value |> .hands

                                    legs =
                                        Tuple.second measurement |> .value |> .legs
                                in
                                if
                                    EverySet.member Backend.Measurement.Model.EdemaHands hands
                                        || EverySet.member Backend.Measurement.Model.EdemaLegs legs
                                then
                                    Just (transAlert diagnosis)

                                else
                                    Nothing
                            )
            in
            -- If corePhysicalExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.corePhysicalExam then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisFetusBreech ->
            let
                resolveAlert measurements =
                    data.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        diffDays lmpDate currentDate // 7 |> toFloat
                                in
                                if egaInWeeks < 32 then
                                    Nothing

                                else
                                    measurements.obstetricalExam
                                        |> Maybe.andThen
                                            (\measurement ->
                                                let
                                                    value =
                                                        Tuple.second measurement |> .value |> .fetalPresentation
                                                in
                                                if value == Backend.Measurement.Model.FetalBreech then
                                                    Just (transAlert diagnosis)

                                                else
                                                    Nothing
                                            )
                            )
            in
            -- If obstetricalExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.obstetricalExam then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisFetusTransverse ->
            let
                resolveAlert measurements =
                    data.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        diffDays lmpDate currentDate // 7 |> toFloat
                                in
                                if egaInWeeks < 32 then
                                    Nothing

                                else
                                    measurements.obstetricalExam
                                        |> Maybe.andThen
                                            (\measurement ->
                                                let
                                                    value =
                                                        Tuple.second measurement |> .value |> .fetalPresentation
                                                in
                                                if value == Backend.Measurement.Model.Transverse then
                                                    Just (transAlert diagnosis)

                                                else
                                                    Nothing
                                            )
                            )
            in
            -- If obstetricalExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.obstetricalExam then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisBreastExamination ->
            let
                resolveAlert measurements =
                    measurements.breastExam
                        |> Maybe.andThen
                            (\measurement ->
                                let
                                    signs =
                                        Tuple.second measurement |> .value |> .exam
                                in
                                if
                                    EverySet.isEmpty signs
                                        || EverySet.member Backend.Measurement.Model.NormalBreast signs
                                then
                                    Nothing

                                else
                                    let
                                        transSigns =
                                            EverySet.toList signs
                                                |> List.map (\sign -> translate language (Translate.BreastExamSign sign))
                                                |> List.intersperse ", "
                                                |> String.concat
                                    in
                                    Just (transAlert diagnosis ++ " " ++ transSigns)
                            )
            in
            -- If breastExam measurements were taken at current encounter,
            -- we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.breastExam then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisHypotension ->
            let
                lowBloodPressureOccasions =
                    getAllNurseMeasurements currentDate isChw data
                        |> List.filterMap
                            (\( _, measurements ) ->
                                measurements.vitals
                                    |> Maybe.andThen
                                        (\measurement ->
                                            let
                                                sys =
                                                    Tuple.second measurement |> .value |> .sys

                                                dia =
                                                    Tuple.second measurement |> .value |> .dia
                                            in
                                            if sys < 110 || dia < 70 then
                                                Just True

                                            else
                                                Nothing
                                        )
                            )
                        |> List.length
            in
            if lowBloodPressureOccasions > 1 then
                Just (transAlert diagnosis)

            else
                Nothing

        DiagnosisPregnancyInducedHypertension ->
            if isJust (generateMedicalDiagnosisAlertData language currentDate firstEncounterMeasurements DiagnosisHypertensionBeforePregnancy) then
                Nothing

            else
                let
                    highBloodPressureOccasions =
                        getAllNurseMeasurements currentDate isChw data
                            |> List.filterMap
                                (\( _, measurements ) ->
                                    measurements.vitals
                                        |> Maybe.andThen
                                            (\measurement ->
                                                let
                                                    sys =
                                                        Tuple.second measurement |> .value |> .sys

                                                    dia =
                                                        Tuple.second measurement |> .value |> .dia
                                                in
                                                if sys > 140 || dia > 90 then
                                                    Just True

                                                else
                                                    Nothing
                                            )
                                )
                            |> List.length
                in
                if highBloodPressureOccasions > 1 then
                    Just (transAlert diagnosis)

                else
                    Nothing

        DiagnosisPreeclampsiaHighRisk ->
            let
                resolveAlert measurements =
                    measurements.vitals
                        |> Maybe.andThen
                            (\vitals ->
                                let
                                    sys =
                                        Tuple.second vitals |> .value |> .sys

                                    dia =
                                        Tuple.second vitals |> .value |> .dia
                                in
                                if sys > 140 || dia > 90 then
                                    measurements.corePhysicalExam
                                        |> Maybe.andThen
                                            (\corePhysicalExam ->
                                                let
                                                    hands =
                                                        Tuple.second corePhysicalExam |> .value |> .hands

                                                    legs =
                                                        Tuple.second corePhysicalExam |> .value |> .legs
                                                in
                                                if
                                                    EverySet.member Backend.Measurement.Model.EdemaHands hands
                                                        || EverySet.member Backend.Measurement.Model.EdemaLegs legs
                                                then
                                                    Just (transAlert diagnosis)

                                                else
                                                    Nothing
                                            )

                                else
                                    Nothing
                            )
            in
            -- If vitals and corePhysicalExam measurements were taken
            -- at current encounter, we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust data.measurements.vitals && isJust data.measurements.corePhysicalExam then
                resolveAlert data.measurements

            else
                resolveAlert lastEncounterMeasurements


getEncounterTrimesterData : NominalDate -> Maybe NominalDate -> Maybe PregnancyTrimester
getEncounterTrimesterData encounterDate maybeLmpDate =
    maybeLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    diffInWeeks =
                        diffDays lmpDate encounterDate // 7
                in
                if diffInWeeks < 13 then
                    FirstTrimester

                else if diffInWeeks < 28 then
                    SecondTrimester

                else
                    ThirdTrimester
            )


getAllNurseMeasurements : NominalDate -> Bool -> AssembledData -> List ( NominalDate, PrenatalMeasurements )
getAllNurseMeasurements currentDate isChw data =
    let
        currentEncounterData =
            if isChw then
                []

            else
                [ ( currentDate, data.measurements ) ]
    in
    currentEncounterData
        ++ data.nursePreviousMeasurementsWithDates
