module Backend.PrenatalActivity.Utils exposing (..)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.Measurement.Model
    exposing
        ( PrenatalMeasurements
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust)
import Pages.Prenatal.Encounter.Utils exposing (getLastEncounterMeasurementsWithDate)
import Pages.Prenatal.Model exposing (AssembledData)
import Translate exposing (Language, translate)


{-| Used for URL etc., not for display in the normal UI (since we'd translate
for that).
-}
activityToString : PrenatalActivity -> String
activityToString activity =
    case activity of
        DangerSigns ->
            "danger-signs"

        Examination ->
            "examination"

        FamilyPlanning ->
            "planning"

        History ->
            "history"

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

        MalariaPrevention ->
            "malaria"

        Medication ->
            "medication"

        SymptomReview ->
            "symptoms"

        PrenatalTreatmentReview ->
            "prior-treatment"

        MaternalMentalHealth ->
            "mental-health"

        PrenatalImmunisation ->
            "immunisation"

        Breastfeeding ->
            "breastfeeding"

        SpecialityCare ->
            "speciality-care"

        PostpartumTreatmentReview ->
            "postpartum-treatment-review"


{-| The inverse of encodeActivityTypeAsString
-}
activityFromString : String -> Maybe PrenatalActivity
activityFromString s =
    case s of
        "danger-signs" ->
            Just DangerSigns

        "examination" ->
            Just Examination

        "planning" ->
            Just FamilyPlanning

        "history" ->
            Just History

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

        "malaria" ->
            Just MalariaPrevention

        "medication" ->
            Just Medication

        "symptoms" ->
            Just SymptomReview

        "prior-treatment" ->
            Just PrenatalTreatmentReview

        "mental-health" ->
            Just MaternalMentalHealth

        "immunisation" ->
            Just PrenatalImmunisation

        "breastfeeding" ->
            Just Breastfeeding

        "speciality-care" ->
            Just SpecialityCare

        "postpartum-treatment-review" ->
            Just PostpartumTreatmentReview

        _ ->
            Nothing


recurrentActivityToString : PrenatalRecurrentActivity -> String
recurrentActivityToString activity =
    case activity of
        LabResults ->
            "laboratory"

        RecurrentNextSteps ->
            "next-steps"

        RecurrentExamination ->
            "examination"

        RecurrentMalariaPrevention ->
            "malaria"

        LabsResultsFollowUps ->
            "laboratory-follow-ups"


recurrentActivityFromString : String -> Maybe PrenatalRecurrentActivity
recurrentActivityFromString s =
    case s of
        "laboratory" ->
            Just LabResults

        "next-steps" ->
            Just RecurrentNextSteps

        "examination" ->
            Just RecurrentExamination

        "malaria" ->
            Just RecurrentMalariaPrevention

        "laboratory-follow-ups" ->
            Just LabsResultsFollowUps

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : PrenatalActivity -> String
getActivityIcon activity =
    activityToString activity


getRecurrentActivityIcon : PrenatalRecurrentActivity -> String
getRecurrentActivityIcon activity =
    recurrentActivityToString activity


generateHighRiskAlertData : Language -> PrenatalMeasurements -> HighRiskFactor -> Maybe String
generateHighRiskAlertData language measurements factor =
    let
        transAlert factor_ =
            translate language (Translate.HighRiskFactor factor_)
    in
    case factor of
        HighRiskConvulsionsAndUnconsciousPreviousDelivery ->
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

        HighRiskConvulsionsPreviousDelivery ->
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
            getMeasurementValueFunc data.measurements.vitals
                |> Maybe.andThen
                    (\value ->
                        Maybe.andThen
                            (\heartRate ->
                                if heartRate >= 120 then
                                    Just
                                        ( trans Translate.High ++ " " ++ transAlert alert
                                        , trans <| Translate.BpmUnit heartRate
                                        )

                                else if heartRate < 40 then
                                    Just
                                        ( trans Translate.Low ++ " " ++ transAlert alert
                                        , trans <| Translate.BpmUnit heartRate
                                        )

                                else
                                    Nothing
                            )
                            value.heartRate
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


generateRiskFactorAlertData : Language -> NominalDate -> PrenatalMeasurements -> RiskFactor -> Maybe String
generateRiskFactorAlertData language currentDate measurements factor =
    let
        trans =
            translate language

        transAlert alert =
            trans (Translate.RiskFactorAlert alert)
    in
    case factor of
        FactorNumberOfCSections _ ->
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
            getMeasurementValueFunc measurements.obstetricHistoryStep2
                |> Maybe.andThen
                    (\value ->
                        let
                            reason =
                                -- There must be only one value.
                                Maybe.andThen (EverySet.toList >> List.head) value.cSectionReason
                                    |> Maybe.withDefault Backend.Measurement.Model.None
                        in
                        if reason /= Backend.Measurement.Model.None then
                            Just (transAlert factor ++ " " ++ trans (Translate.CSectionReasons reason))

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
