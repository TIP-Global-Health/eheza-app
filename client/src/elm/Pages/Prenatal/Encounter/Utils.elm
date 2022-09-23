module Pages.Prenatal.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuples, sortEncounterTuplesDesc)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, orElse, unwrap)
import Pages.Prenatal.Activity.Types exposing (NextStepsTask(..))
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Utils exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


isFirstEncounter : AssembledData -> Bool
isFirstEncounter assembled =
    List.isEmpty assembled.nursePreviousMeasurementsWithDates


getAllActivities : AssembledData -> List PrenatalActivity
getAllActivities assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            if isFirstEncounter assembled then
                [ PregnancyDating
                , History
                , Examination
                , FamilyPlanning
                , Medication
                , Backend.PrenatalActivity.Model.MalariaPrevention
                , DangerSigns
                , Laboratory
                , PrenatalPhoto
                , NextSteps
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
                , NextSteps
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


calculateEDDandEGADays : NominalDate -> NominalDate -> ( NominalDate, Int )
calculateEDDandEGADays currentDate lmpDate =
    ( lmpToEDDDate lmpDate
    , calculateEGADays currentDate lmpDate
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


getPrenatalEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( PrenatalEncounterId, PrenatalEncounter )
getPrenatalEncountersForParticipant db participantId =
    Dict.get participantId db.prenatalEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
        |> List.sortWith sortEncounterTuplesDesc


generatePreviousMeasurements :
    PrenatalEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    ->
        ( List ( NominalDate, EverySet PrenatalDiagnosis, PrenatalMeasurements )
        , List ( NominalDate, PrenatalEncounterType, PrenatalMeasurements )
        )
generatePreviousMeasurements currentEncounterId participantId db =
    getPrenatalEncountersForParticipant db participantId
        |> List.filter
            (\( id, _ ) ->
                -- We do not want to get data of current encounter.
                id /= currentEncounterId
            )
        |> List.sortWith sortEncounterTuples
        |> (\previousEncounters ->
                let
                    ( nurseEncounters, chwEncounters ) =
                        List.partition (Tuple.second >> .encounterType >> (==) NurseEncounter) previousEncounters

                    getEncounterDataForNurse ( encounterId, encounter ) =
                        case Dict.get encounterId db.prenatalMeasurements of
                            Just (Success measurements) ->
                                Just ( encounter.startDate, encounter.diagnoses, measurements )

                            _ ->
                                Nothing

                    getEncounterDataForChw ( encounterId, encounter ) =
                        case Dict.get encounterId db.prenatalMeasurements of
                            Just (Success measurements) ->
                                Just ( encounter.startDate, encounter.encounterType, measurements )

                            _ ->
                                Nothing
                in
                ( List.filterMap getEncounterDataForNurse nurseEncounters
                , List.filterMap getEncounterDataForChw chwEncounters
                )
           )


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
            List.map (\( _, _, previousMeasurements ) -> previousMeasurements) nursePreviousMeasurementsWithDates

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
getFirstEncounterMeasurements isChw assembled =
    case assembled.nursePreviousMeasurementsWithDates of
        [] ->
            if isChw then
                emptyPrenatalMeasurements

            else
                assembled.measurements

        ( _, _, measurements ) :: others ->
            measurements


getLastEncounterMeasurementsWithDate : NominalDate -> Bool -> AssembledData -> ( NominalDate, PrenatalMeasurements )
getLastEncounterMeasurementsWithDate currentDate isChw assembled =
    case List.reverse assembled.nursePreviousMeasurementsWithDates of
        [] ->
            if isChw then
                ( currentDate, emptyPrenatalMeasurements )

            else
                ( currentDate, assembled.measurements )

        ( date, _, measurements ) :: others ->
            ( date, measurements )


getLastEncounterMeasurements : NominalDate -> Bool -> AssembledData -> PrenatalMeasurements
getLastEncounterMeasurements currentDate isChw assembled =
    getLastEncounterMeasurementsWithDate currentDate isChw assembled |> Tuple.second


getAllNurseMeasurements : NominalDate -> Bool -> AssembledData -> List ( NominalDate, EverySet PrenatalDiagnosis, PrenatalMeasurements )
getAllNurseMeasurements currentDate isChw assembled =
    let
        currentEncounterData =
            if isChw then
                []

            else
                [ ( currentDate, assembled.encounter.diagnoses, assembled.measurements ) ]
    in
    currentEncounterData
        ++ assembled.nursePreviousMeasurementsWithDates


generateRecurringHighSeverityAlertData : Language -> NominalDate -> Bool -> AssembledData -> RecurringHighSeverityAlert -> List ( String, String, String )
generateRecurringHighSeverityAlertData language currentDate isChw assembled alert =
    let
        trans =
            translate language

        transAlert alert_ =
            trans (Translate.RecurringHighSeverityAlert alert_)
    in
    case alert of
        BloodPressure ->
            let
                resolveAlert ( date, _, measurements ) =
                    getMeasurementValueFunc measurements.vitals
                        |> Maybe.andThen
                            (\value ->
                                case ( value.sys, value.dia ) of
                                    ( Just sys, Just dia ) ->
                                        if sys > 180 || dia > 100 then
                                            Just
                                                ( trans Translate.High ++ " " ++ transAlert alert
                                                , String.fromFloat sys ++ "/" ++ String.fromFloat dia ++ trans Translate.MMHGUnit
                                                , formatDDMMYYYY date
                                                )

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
            in
            getAllNurseMeasurements currentDate isChw assembled
                |> List.filterMap resolveAlert


generateObstetricalDiagnosisAlertData : Language -> NominalDate -> Bool -> PrenatalMeasurements -> AssembledData -> ObstetricalDiagnosis -> Maybe String
generateObstetricalDiagnosisAlertData language currentDate isChw firstEncounterMeasurements assembled diagnosis =
    let
        transAlert diagnosis_ =
            translate language (Translate.ObstetricalDiagnosisAlert diagnosis_)

        lastEncounterMeasurements =
            getLastEncounterMeasurements currentDate isChw assembled
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
            if isJust assembled.measurements.nutrition then
                resolveAlert assembled.measurements

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
            if isJust assembled.measurements.nutrition then
                resolveAlert assembled.measurements

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
            if isJust assembled.measurements.nutrition then
                resolveAlert assembled.measurements

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
            if isJust assembled.measurements.nutrition then
                resolveAlert assembled.measurements

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
            if isJust assembled.measurements.corePhysicalExam then
                resolveAlert assembled.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisFetusBreech ->
            let
                resolveAlert measurements =
                    assembled.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        calculateEGAWeeks currentDate lmpDate
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
            if isJust assembled.measurements.obstetricalExam then
                resolveAlert assembled.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisFetusTransverse ->
            let
                resolveAlert measurements =
                    assembled.globalLmpDate
                        |> Maybe.andThen
                            (\lmpDate ->
                                let
                                    egaInWeeks =
                                        calculateEGAWeeks currentDate lmpDate
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
            if isJust assembled.measurements.obstetricalExam then
                resolveAlert assembled.measurements

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
            if isJust assembled.measurements.breastExam then
                resolveAlert assembled.measurements

            else
                resolveAlert lastEncounterMeasurements

        DiagnosisHypotension ->
            let
                lowBloodPressureOccasions =
                    getAllNurseMeasurements currentDate isChw assembled
                        |> List.filterMap
                            (\( _, _, measurements ) ->
                                getMeasurementValueFunc measurements.vitals
                                    |> Maybe.andThen
                                        (\value ->
                                            case ( value.sys, value.dia ) of
                                                ( Just sys, Just dia ) ->
                                                    if sys < 110 || dia < 70 then
                                                        Just True

                                                    else
                                                        Nothing

                                                _ ->
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
                        getAllNurseMeasurements currentDate isChw assembled
                            |> List.filterMap
                                (\( _, _, measurements ) ->
                                    getMeasurementValueFunc measurements.vitals
                                        |> Maybe.andThen
                                            (\value ->
                                                case ( value.sys, value.dia ) of
                                                    ( Just sys, Just dia ) ->
                                                        if sys > 140 || dia > 90 then
                                                            Just True

                                                        else
                                                            Nothing

                                                    _ ->
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
                    getMeasurementValueFunc measurements.vitals
                        |> Maybe.andThen
                            (\value ->
                                case ( value.sys, value.dia ) of
                                    ( Just sys, Just dia ) ->
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

                                    _ ->
                                        Nothing
                            )
            in
            -- If vitals and corePhysicalExam measurements were taken
            -- at current encounter, we issue the alarm according to those values.
            -- Otherwise, we use values of last encounter.
            if isJust assembled.measurements.vitals && isJust assembled.measurements.corePhysicalExam then
                resolveAlert assembled.measurements

            else
                resolveAlert lastEncounterMeasurements


generateMedicalDiagnosisAlertData : Language -> NominalDate -> PrenatalMeasurements -> MedicalDiagnosis -> Maybe String
generateMedicalDiagnosisAlertData language currentDate measurements diagnosis =
    let
        generateAlertForDiagnosis triggeringSigns =
            getMeasurementValueFunc measurements.medicalHistory
                |> Maybe.andThen
                    (\value ->
                        if
                            List.any (\sign -> EverySet.member sign value)
                                triggeringSigns
                        then
                            Just <| translate language (Translate.MedicalDiagnosisAlert diagnosis)

                        else
                            Nothing
                    )
    in
    case diagnosis of
        DiagnosisUterineMyoma ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.UterineMyoma ]

        DiagnosisDiabetes ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.Diabetes ]

        DiagnosisCardiacDisease ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.CardiacDisease ]

        DiagnosisRenalDisease ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.RenalDisease ]

        DiagnosisHypertensionBeforePregnancy ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.HypertensionBeforePregnancy ]

        DiagnosisTuberculosis ->
            generateAlertForDiagnosis
                [ Backend.Measurement.Model.TuberculosisPast
                , Backend.Measurement.Model.TuberculosisPresent
                ]

        DiagnosisAsthma ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.Asthma ]

        DiagnosisBowedLegs ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.BowedLegs ]

        DiagnosisKnownHIV ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.HIV ]

        DiagnosisMentalHealthHistory ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.MentalHealthHistory ]


calculateBmi : Maybe Float -> Maybe Float -> Maybe Float
calculateBmi maybeHeight maybeWeight =
    Maybe.map2 (\height weight -> weight / ((height / 100) ^ 2)) maybeHeight maybeWeight


secondPhaseRequired : AssembledData -> Bool
secondPhaseRequired assembled =
    (not <| emergencyReferalRequired assembled)
        && (getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.map
                    (\value ->
                        EverySet.size value.completedTests < EverySet.size value.performedTests
                    )
                |> Maybe.withDefault False
           )


emergencyReferalRequired : AssembledData -> Bool
emergencyReferalRequired assembled =
    EverySet.toList assembled.encounter.diagnoses
        |> List.filter diagnosisRequiresEmergencyReferal
        |> List.isEmpty
        |> not


diagnosisRequiresEmergencyReferal : PrenatalDiagnosis -> Bool
diagnosisRequiresEmergencyReferal diagnosis =
    List.member diagnosis emergencyReferralDiagnosesInitial
