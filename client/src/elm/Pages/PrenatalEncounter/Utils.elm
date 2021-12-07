module Pages.PrenatalEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuples)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, orElse, unwrap)
import Pages.PrenatalActivity.Model exposing (NextStepsTask(..))
import Pages.PrenatalActivity.Utils exposing (isFirstEncounter)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


getAllActivities : AssembledData -> List PrenatalActivity
getAllActivities data =
    case data.encounter.encounterType of
        NurseEncounter ->
            if isFirstEncounter data then
                [ PregnancyDating
                , History
                , Examination
                , FamilyPlanning
                , Medication
                , Backend.PrenatalActivity.Model.MalariaPrevention
                , DangerSigns
                , Laboratory
                , PrenatalPhoto
                ]

            else
                [ DangerSigns
                , PregnancyDating
                , History
                , Examination
                , FamilyPlanning
                , Medication
                , Backend.PrenatalActivity.Model.MalariaPrevention
                , Laboratory
                , PrenatalPhoto
                ]

        ChwFirstEncounter ->
            [ PregnancyDating, Laboratory, DangerSigns, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwSecondEncounter ->
            [ DangerSigns, BirthPlan, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwThirdPlusEncounter ->
            [ DangerSigns, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwPostpartumEncounter ->
            [ PregnancyOutcome, DangerSigns, NextSteps ]


getSubsequentEncounterType : PrenatalEncounterType -> Maybe PrenatalEncounterType
getSubsequentEncounterType currentEncounterType =
    case currentEncounterType of
        NurseEncounter ->
            Nothing

        ChwFirstEncounter ->
            Just ChwSecondEncounter

        ChwSecondEncounter ->
            Just ChwThirdPlusEncounter

        ChwThirdPlusEncounter ->
            Just ChwThirdPlusEncounter

        ChwPostpartumEncounter ->
            Nothing


generatePostCreateDestination : PrenatalEncounterType -> Bool -> PrenatalEncounterPostCreateDestination
generatePostCreateDestination encounterType hasNurseEncounter =
    case encounterType of
        ChwFirstEncounter ->
            if hasNurseEncounter then
                DestinationClinicalProgressReportPage

            else
                DestinationEncounterPage

        ChwSecondEncounter ->
            if hasNurseEncounter then
                DestinationClinicalProgressReportPage

            else
                DestinationEncounterPageWithWarningPopup

        ChwThirdPlusEncounter ->
            if hasNurseEncounter then
                DestinationClinicalProgressReportPage

            else
                DestinationEncounterPageWithWarningPopup

        ChwPostpartumEncounter ->
            DestinationEncounterPage

        -- We should never get here.
        NurseEncounter ->
            DestinationEncounterPage


calculateEDD : NominalDate -> NominalDate
calculateEDD lmpDate =
    Date.add Days 280 lmpDate


calculateEDDandEGADays : NominalDate -> NominalDate -> ( NominalDate, Int )
calculateEDDandEGADays currentDate lmpDate =
    ( calculateEDD lmpDate
    , diffDays lmpDate currentDate
    )


generateEGAWeeksDaysLabel : Language -> Int -> String
generateEGAWeeksDaysLabel language diffInDays =
    let
        diffInWeeks =
            diffInDays // 7

        egaWeeks =
            translate language <| Translate.WeekSinglePlural diffInWeeks

        egaDays =
            translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)
    in
    egaWeeks ++ ", " ++ egaDays


generateEDDandEGA : Language -> NominalDate -> ( String, String ) -> Maybe NominalDate -> ( String, String )
generateEDDandEGA language currentDate defaults maybeLmpDate =
    unwrap
        defaults
        (\lmpDate ->
            let
                ( eddDate, diffInDays ) =
                    calculateEDDandEGADays currentDate lmpDate
            in
            ( formatDDMMYYYY eddDate, generateEGAWeeksDaysLabel language diffInDays )
        )
        maybeLmpDate


generateGravida : ObstetricHistoryValue -> String
generateGravida value =
    let
        total =
            (value.termPregnancy + value.preTermPregnancy + value.stillbirthsAtTerm + value.stillbirthsPreTerm + value.abortions)
                + (if value.currentlyPregnant then
                    1

                   else
                    0
                  )
    in
    if total < 10 then
        "0" ++ String.fromInt total

    else
        String.fromInt total


generatePara : ObstetricHistoryValue -> String
generatePara value =
    String.fromInt (value.termPregnancy + value.stillbirthsAtTerm)
        ++ String.fromInt (value.preTermPregnancy + value.stillbirthsPreTerm)
        ++ String.fromInt value.abortions
        ++ String.fromInt value.liveChildren


getLastMenstrualPeriodValue : PrenatalMeasurements -> Maybe LastMenstrualPeriodValue
getLastMenstrualPeriodValue measurements =
    measurements.lastMenstrualPeriod
        |> Maybe.map (Tuple.second >> .value)


getLmpDate : PrenatalMeasurements -> Maybe NominalDate
getLmpDate measurements =
    getLastMenstrualPeriodValue measurements
        |> Maybe.map .date


getObstetricHistory : PrenatalMeasurements -> Maybe ObstetricHistoryValue
getObstetricHistory measurements =
    measurements.obstetricHistory
        |> getMeasurementValueFunc


resolveGlobalLmpDate : List PrenatalMeasurements -> List PrenatalMeasurements -> PrenatalMeasurements -> Maybe NominalDate
resolveGlobalLmpDate nursePreviousMeasurements chwPreviousMeasurements measurements =
    let
        -- When measurements list is not empty, we know that Lmp date
        -- will be located at head of the list, becuase previous measurements
        -- are sorted ASC by encounter date, and Lmp date is a mandatory
        -- measurement at first encounter.
        getLmpDateFromList measurementsList =
            List.head measurementsList
                |> Maybe.andThen getLmpDate
    in
    getLmpDateFromList nursePreviousMeasurements
        |> orElse (getLmpDate measurements)
        |> orElse (getLmpDateFromList chwPreviousMeasurements)


resolveGlobalObstetricHistory : List PrenatalMeasurements -> PrenatalMeasurements -> Maybe ObstetricHistoryValue
resolveGlobalObstetricHistory nursePreviousMeasurements measurements =
    -- When there are no previous measurements, we try to resolve
    -- from current encounter.
    if List.isEmpty nursePreviousMeasurements then
        getObstetricHistory measurements

    else
        -- When there are previous measurements, we know that Lmp Obstetric history
        -- will be located at head of the list, becuase previous measurements
        -- are sorted by encounter date, and Obstetric history date is a mandatory measurement.
        nursePreviousMeasurements
            |> List.head
            |> Maybe.andThen getObstetricHistory


generatePreviousMeasurements :
    PrenatalEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> ( List ( NominalDate, PrenatalMeasurements ), List ( NominalDate, PrenatalEncounterType, PrenatalMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    Dict.get participantId db.prenatalEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (Dict.toList
                >> List.filter
                    (\( id, _ ) ->
                        -- We do not want to get data of current encounter.
                        id /= currentEncounterId
                    )
                >> List.sortWith sortEncounterTuples
                >> (\previousEncounters ->
                        let
                            ( nurseEncounters, chwEncounters ) =
                                List.partition (Tuple.second >> .encounterType >> (==) NurseEncounter) previousEncounters

                            getEncounterMeasurements ( encounterId, encounter ) =
                                case Dict.get encounterId db.prenatalMeasurements of
                                    Just (Success measurements) ->
                                        Just ( encounter.startDate, encounter.encounterType, measurements )

                                    _ ->
                                        Nothing
                        in
                        ( List.filterMap getEncounterMeasurements nurseEncounters
                            |> List.map (\( date, _, measurements ) -> ( date, measurements ))
                        , List.filterMap getEncounterMeasurements chwEncounters
                        )
                   )
            )
        |> Maybe.withDefault ( [], [] )


generateAssembledData : PrenatalEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.prenatalMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        ( nursePreviousMeasurementsWithDates, chwPreviousMeasurementsWithDates ) =
            encounter
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> Maybe.withDefault ( [], [] )

        nursePreviousMeasurements =
            List.map Tuple.second nursePreviousMeasurementsWithDates

        chwPreviousMeasurements =
            List.map (\( _, _, previousMeasurements ) -> previousMeasurements) chwPreviousMeasurementsWithDates

        globalLmpDate =
            measurements
                |> RemoteData.map (resolveGlobalLmpDate nursePreviousMeasurements chwPreviousMeasurements)
                |> RemoteData.withDefault Nothing

        globalObstetricHistory =
            measurements
                |> RemoteData.map (resolveGlobalObstetricHistory nursePreviousMeasurements)
                |> RemoteData.withDefault Nothing
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success nursePreviousMeasurementsWithDates)
        |> RemoteData.andMap (Success chwPreviousMeasurementsWithDates)
        |> RemoteData.andMap (Success globalLmpDate)
        |> RemoteData.andMap (Success globalObstetricHistory)


getFirstEncounterMeasurements : Bool -> AssembledData -> PrenatalMeasurements
getFirstEncounterMeasurements isChw data =
    case data.nursePreviousMeasurementsWithDates of
        [] ->
            if isChw then
                emptyPrenatalMeasurements

            else
                data.measurements

        first :: others ->
            Tuple.second first


getLastEncounterMeasurementsWithDate : NominalDate -> Bool -> AssembledData -> ( NominalDate, PrenatalMeasurements )
getLastEncounterMeasurementsWithDate currentDate isChw data =
    case List.reverse data.nursePreviousMeasurementsWithDates of
        [] ->
            if isChw then
                ( currentDate, emptyPrenatalMeasurements )

            else
                ( currentDate, data.measurements )

        first :: others ->
            first


getLastEncounterMeasurements : NominalDate -> Bool -> AssembledData -> PrenatalMeasurements
getLastEncounterMeasurements currentDate isChw data =
    getLastEncounterMeasurementsWithDate currentDate isChw data |> Tuple.second


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
                                        , formatDDMMYYYY date
                                        )

                                else
                                    Nothing
                            )
            in
            getAllNurseMeasurements currentDate isChw data
                |> List.filterMap resolveAlert


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


calculateBmi : Maybe Float -> Maybe Float -> Maybe Float
calculateBmi maybeHeight maybeWeight =
    Maybe.map2 (\height weight -> weight / ((height / 100) ^ 2)) maybeHeight maybeWeight
