module Pages.Prenatal.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getHeightValue, getMeasurementValueFunc, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import Backend.PrenatalEncounter.Utils exposing (isNurseEncounter, lmpToEDDDate)
import EverySet
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, orElse, unwrap)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.Utils exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.NominalDate exposing (sortEncounterTuples, sortEncounterTuplesDesc)


getAllActivities : AssembledData -> List PrenatalActivity
getAllActivities assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            if nurseEncounterNotPerformed assembled then
                [ PregnancyDating
                , History
                , Examination
                , FamilyPlanning
                , Medication
                , Backend.PrenatalActivity.Model.MalariaPrevention
                , DangerSigns
                , SymptomReview
                , PrenatalImmunisation
                , Laboratory
                , MaternalMentalHealth
                , PrenatalPhoto
                , NextSteps
                ]

            else
                [ DangerSigns
                , SymptomReview
                , History
                , Examination
                , FamilyPlanning
                , PrenatalTreatmentReview
                , Backend.PrenatalActivity.Model.MalariaPrevention
                , PrenatalImmunisation
                , Laboratory
                , MaternalMentalHealth
                , PrenatalPhoto
                , NextSteps
                ]

        NursePostpartumEncounter ->
            [ PregnancyOutcome
            , SymptomReview
            , MaternalMentalHealth
            , Backend.PrenatalActivity.Model.Breastfeeding
            , Examination
            , FamilyPlanning
            , PostpartumTreatmentReview
            , SpecialityCare
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

        NursePostpartumEncounter ->
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

        -- We should never get here.
        NursePostpartumEncounter ->
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


getLmpValue : PrenatalMeasurements -> Maybe LastMenstrualPeriodValue
getLmpValue =
    .lastMenstrualPeriod >> getMeasurementValueFunc


getObstetricHistory : PrenatalMeasurements -> Maybe ObstetricHistoryValue
getObstetricHistory measurements =
    measurements.obstetricHistory
        |> getMeasurementValueFunc


resolveGlobalLmpValue : List PrenatalMeasurements -> List PrenatalMeasurements -> PrenatalMeasurements -> Maybe LastMenstrualPeriodValue
resolveGlobalLmpValue nursePreviousMeasurements chwPreviousMeasurements measurements =
    let
        -- When measurements list is not empty, we know that Lmp date
        -- will be located at head of the list, becuase previous measurements
        -- are sorted ASC by encounter date, and Lmp date is a mandatory
        -- measurement at first encounter.
        getLmpValueFromList measurementsList =
            List.head measurementsList
                |> Maybe.andThen getLmpValue
    in
    getLmpValueFromList nursePreviousMeasurements
        |> orElse (getLmpValue measurements)
        |> orElse (getLmpValueFromList chwPreviousMeasurements)


resolveGlobalLmpDate : List PrenatalMeasurements -> List PrenatalMeasurements -> PrenatalMeasurements -> Maybe NominalDate
resolveGlobalLmpDate nursePreviousMeasurements chwPreviousMeasurements measurements =
    resolveGlobalLmpValue nursePreviousMeasurements chwPreviousMeasurements measurements
        |> Maybe.map .date


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
    Backend.NutritionEncounter.Utils.getPrenatalEncountersForParticipant db participantId
        |> List.sortWith sortEncounterTuplesDesc


generatePreviousMeasurements :
    PrenatalEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    ->
        ( List PreviousEncounterData
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
                        List.partition (Tuple.second >> .encounterType >> isNurseEncounter) previousEncounters

                    getEncounterDataForNurse ( encounterId, encounter ) =
                        case Dict.get encounterId db.prenatalMeasurements of
                            Just (Success measurements) ->
                                Just
                                    { startDate = encounter.startDate
                                    , diagnoses = encounter.diagnoses
                                    , pastDiagnoses = encounter.pastDiagnoses
                                    , measurements = measurements
                                    }

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

        ( nursePreviousEncountersData, chwPreviousMeasurementsWithDates ) =
            encounter
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> Maybe.withDefault ( [], [] )

        nursePreviousMeasurements =
            List.map .measurements nursePreviousEncountersData

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

        ( vaccinationHistory, vaccinationProgress ) =
            ( generateVaccinationProgress nursePreviousMeasurements
            , RemoteData.toMaybe measurements
                |> Maybe.map (\measurements_ -> measurements_ :: nursePreviousMeasurements)
                |> Maybe.withDefault nursePreviousMeasurements
                |> generateVaccinationProgress
            )
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success nursePreviousEncountersData)
        |> RemoteData.andMap (Success chwPreviousMeasurementsWithDates)
        |> RemoteData.andMap (Success globalLmpDate)
        |> RemoteData.andMap (Success globalObstetricHistory)
        |> RemoteData.andMap (Success vaccinationHistory)
        |> RemoteData.andMap (Success vaccinationProgress)


getFirstEncounterMeasurements : Bool -> AssembledData -> PrenatalMeasurements
getFirstEncounterMeasurements isChw assembled =
    case assembled.nursePreviousEncountersData of
        [] ->
            if isChw then
                emptyPrenatalMeasurements

            else
                assembled.measurements

        encounterData :: _ ->
            encounterData.measurements


getLastEncounterMeasurementsWithDate : NominalDate -> Bool -> AssembledData -> ( NominalDate, PrenatalMeasurements )
getLastEncounterMeasurementsWithDate currentDate isChw assembled =
    case List.reverse assembled.nursePreviousEncountersData of
        [] ->
            if isChw then
                ( currentDate, emptyPrenatalMeasurements )

            else
                ( assembled.encounter.startDate, assembled.measurements )

        encounterData :: _ ->
            ( encounterData.startDate, encounterData.measurements )


getLastEncounterMeasurements : NominalDate -> Bool -> AssembledData -> PrenatalMeasurements
getLastEncounterMeasurements currentDate isChw assembled =
    getLastEncounterMeasurementsWithDate currentDate isChw assembled |> Tuple.second


getAllNurseMeasurements : NominalDate -> Bool -> AssembledData -> List PreviousEncounterData
getAllNurseMeasurements currentDate isChw assembled =
    let
        currentEncounterData =
            if isChw then
                []

            else
                [ { startDate = assembled.encounter.startDate
                  , diagnoses = assembled.encounter.diagnoses
                  , pastDiagnoses = assembled.encounter.pastDiagnoses
                  , measurements = assembled.measurements
                  }
                ]
    in
    currentEncounterData
        ++ assembled.nursePreviousEncountersData


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
                resolveAlert data =
                    getMeasurementValueFunc data.measurements.vitals
                        |> Maybe.andThen
                            (\value ->
                                case ( value.sys, value.dia ) of
                                    ( Just sys, Just dia ) ->
                                        if sys > 180 || dia > 100 then
                                            Just
                                                ( trans Translate.High ++ " " ++ transAlert alert
                                                , String.fromFloat sys ++ "/" ++ String.fromFloat dia ++ trans Translate.MMHGUnit
                                                , formatDDMMYYYY data.startDate
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
                severeUnderweight =
                    isJust <| generateObstetricalDiagnosisAlertData language currentDate isChw firstEncounterMeasurements assembled DiagnosisSevereUnderweight
            in
            if severeUnderweight then
                Nothing

            else
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
                                    muac =
                                        Tuple.second measurement
                                            |> .value
                                            |> .muac
                                            |> muacValueFunc
                                in
                                if muac < 18.5 then
                                    Just (transAlert diagnosis)

                                else
                                    let
                                        height =
                                            Tuple.second measurement
                                                |> .value
                                                |> .height
                                                |> getHeightValue

                                        weight =
                                            Tuple.second measurement
                                                |> .value
                                                |> .weight
                                                |> weightValueFunc
                                    in
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
                                            |> getHeightValue

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
                                            |> getHeightValue

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
                            (.measurements
                                >> .vitals
                                >> getMeasurementValueFunc
                                >> Maybe.andThen
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
                                (.measurements
                                    >> .vitals
                                    >> getMeasurementValueFunc
                                    >> Maybe.andThen
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

        Backend.PrenatalActivity.Model.DiagnosisDiabetes ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.Diabetes ]

        DiagnosisCardiacDisease ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.CardiacDisease ]

        DiagnosisRenalDisease ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.RenalDisease ]

        DiagnosisHypertensionBeforePregnancy ->
            generateAlertForDiagnosis [ Backend.Measurement.Model.HypertensionBeforePregnancy ]

        Backend.PrenatalActivity.Model.DiagnosisTuberculosis ->
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
