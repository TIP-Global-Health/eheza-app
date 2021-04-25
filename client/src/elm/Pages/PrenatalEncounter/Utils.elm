module Pages.PrenatalEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


getAllActivities : AssembledData -> List PrenatalActivity
getAllActivities data =
    case data.encounter.encounterType of
        NurseEncounter ->
            if List.isEmpty data.previousMeasurementsWithDates then
                [ PregnancyDating, History, Examination, FamilyPlanning, PatientProvisions, DangerSigns, PrenatalPhoto ]

            else
                [ DangerSigns, PregnancyDating, History, Examination, FamilyPlanning, PatientProvisions, PrenatalPhoto ]

        ChwFirstEncounter ->
            [ PregnancyDating, Laboratory, DangerSigns, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwSecondEncounter ->
            [ DangerSigns, BirthPlan, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwThirdEncounter ->
            [ DangerSigns, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]

        ChwPostpartumEncounter ->
            [ PregnancyOutcome, DangerSigns, NextSteps ]


expectActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate data activity =
    case data.encounter.encounterType of
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    -- Only show on first encounter
                    List.isEmpty data.previousMeasurementsWithDates

                History ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                PatientProvisions ->
                    True

                DangerSigns ->
                    True

                PrenatalPhoto ->
                    expectPrenatalPhoto currentDate data

                -- Unique Chw activities.
                _ ->
                    False

        ChwFirstEncounter ->
            case activity of
                PregnancyDating ->
                    -- Do not show, if patient already visited health center.
                    not <| List.isEmpty data.previousMeasurementsWithDates

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    not <| List.isEmpty data.previousMeasurementsWithDates

                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    -- @todo: show if no danger signs.
                    True

                NextSteps ->
                    -- @todo: show after mandatory activities are completed.
                    True

                -- Unique nurse activities.
                _ ->
                    False

        -- [ PregnancyDating, Laboratory, DangerSigns, Backend.PrenatalActivity.Model.HealthEducation, NextSteps ]
        ChwSecondEncounter ->
            case activity of
                DangerSigns ->
                    True

                BirthPlan ->
                    -- @todo: show if no danger signs.
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    -- @todo: show if no danger signs.
                    True

                NextSteps ->
                    -- @todo: show after mandatory activities are completed.
                    True

                -- Unique nurse activities.
                _ ->
                    False

        ChwThirdEncounter ->
            case activity of
                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    -- @todo: show if no danger signs.
                    True

                NextSteps ->
                    -- @todo: show after mandatory activities are completed.
                    True

                -- Unique nurse activities.
                _ ->
                    False

        ChwPostpartumEncounter ->
            case activity of
                PregnancyOutcome ->
                    True

                DangerSigns ->
                    True

                NextSteps ->
                    -- @todo: show after mandatory activities are completed.
                    True

                -- Unique nurse activities.
                _ ->
                    False


activityCompleted : NominalDate -> AssembledData -> PrenatalActivity -> Bool
activityCompleted currentDate data activity =
    case activity of
        PregnancyDating ->
            isJust data.measurements.lastMenstrualPeriod

        History ->
            if List.isEmpty data.previousMeasurementsWithDates then
                -- First antenatal encounter - all tasks should be completed
                isJust data.measurements.obstetricHistory
                    && isJust data.measurements.obstetricHistoryStep2
                    && isJust data.measurements.medicalHistory
                    && isJust data.measurements.socialHistory

            else
                -- Subsequent antenatal encounter - only Social history task
                -- needs to be completed.
                isJust data.measurements.socialHistory

        Examination ->
            isJust data.measurements.vitals
                && isJust data.measurements.nutrition
                && isJust data.measurements.corePhysicalExam
                && isJust data.measurements.obstetricalExam
                && isJust data.measurements.breastExam

        FamilyPlanning ->
            isJust data.measurements.familyPlanning

        PatientProvisions ->
            if shouldShowPatientProvisionsResourcesTask data then
                isJust data.measurements.medication && isJust data.measurements.resource

            else
                isJust data.measurements.medication

        DangerSigns ->
            isJust data.measurements.dangerSigns

        PrenatalPhoto ->
            isJust data.measurements.prenatalPhoto

        Laboratory ->
            -- @todo
            False

        Backend.PrenatalActivity.Model.HealthEducation ->
            -- @todo
            False

        BirthPlan ->
            -- @todo
            False

        NextSteps ->
            -- @todo
            False

        PregnancyOutcome ->
            -- @todo
            False


calculateEDD : NominalDate -> NominalDate
calculateEDD lmpDate =
    lmpDate
        |> Date.add Days 280


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


getLmpMeasurement : PrenatalMeasurements -> Maybe NominalDate
getLmpMeasurement measurements =
    measurements.lastMenstrualPeriod
        |> Maybe.map (Tuple.second >> .value >> .date)


getObstetricHistory : PrenatalMeasurements -> Maybe ObstetricHistoryValue
getObstetricHistory measurements =
    measurements.obstetricHistory
        |> Maybe.map (Tuple.second >> .value)


getMotherHeightMeasurement : PrenatalMeasurements -> Maybe HeightInCm
getMotherHeightMeasurement measurements =
    measurements.nutrition
        |> Maybe.map (Tuple.second >> .value >> .height)


resolveGlobalLmpDate : PrenatalMeasurements -> List PrenatalMeasurements -> Maybe NominalDate
resolveGlobalLmpDate measurements previousMeasurements =
    -- When there are no previous measurements, we try to resolve
    -- from current encounter.
    if List.isEmpty previousMeasurements then
        getLmpMeasurement measurements

    else
        -- When there are previous measurements, we know that Lmp date
        -- will be located at head of the list, becuase previous measurements
        -- are sorted by encounter date, and Lmp date is a mandatory measurement.
        previousMeasurements
            |> List.head
            |> Maybe.andThen getLmpMeasurement


resolveGlobalObstetricHistory : PrenatalMeasurements -> List PrenatalMeasurements -> Maybe ObstetricHistoryValue
resolveGlobalObstetricHistory measurements previousMeasurements =
    -- When there are no previous measurements, we try to resolve
    -- from current encounter.
    if List.isEmpty previousMeasurements then
        getObstetricHistory measurements

    else
        -- When there are previous measurements, we know that Lmp Obstetric history
        -- will be located at head of the list, becuase previous measurements
        -- are sorted by encounter date, and Obstetric history date is a mandatory measurement.
        previousMeasurements
            |> List.head
            |> Maybe.andThen getObstetricHistory


generatePreviousMeasurements :
    PrenatalEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> ( List ( NominalDate, PrenatalMeasurements ), List ( NominalDate, PrenatalMeasurements ) )
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
                >> (\previousEncounters ->
                        let
                            ( nurseEncounters, chwEncounters ) =
                                List.partition (Tuple.second >> .encounterType >> (==) NurseEncounter) previousEncounters

                            getEncounterMeasurements ( encounterId, encounter ) =
                                case Dict.get encounterId db.prenatalMeasurements of
                                    Just (Success measurements) ->
                                        Just ( encounter.startDate, measurements )

                                    _ ->
                                        Nothing

                            sortAsc =
                                \( date1, _ ) ( date2, _ ) -> Gizra.NominalDate.compare date1 date2
                        in
                        ( List.filterMap getEncounterMeasurements nurseEncounters
                            |> List.sortWith sortAsc
                        , List.filterMap getEncounterMeasurements chwEncounters
                            |> List.sortWith sortAsc
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

        ( previousMeasurementsWithDates, chwPreviousMeasurementsWithDates ) =
            encounter
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\encounter_ ->
                        generatePreviousMeasurements id encounter_.participant db
                    )
                |> Maybe.withDefault ( [], [] )

        previousMeasurements =
            List.map Tuple.second previousMeasurementsWithDates

        globalLmpDate =
            measurements
                |> RemoteData.map (\measurements_ -> resolveGlobalLmpDate measurements_ previousMeasurements)
                |> RemoteData.withDefault Nothing

        globalObstetricHistory =
            measurements
                |> RemoteData.map (\measurements_ -> resolveGlobalObstetricHistory measurements_ previousMeasurements)
                |> RemoteData.withDefault Nothing
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)
        |> RemoteData.andMap (Success chwPreviousMeasurementsWithDates)
        |> RemoteData.andMap (Success globalLmpDate)
        |> RemoteData.andMap (Success globalObstetricHistory)


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate data =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 13 and week 27.
            --  3. Week 28, or more.
            [ [ (>) 13 ], [ (>) 28, (<=) 13 ], [ (<=) 28 ] ]

        previousMeasurements =
            List.map Tuple.second data.previousMeasurementsWithDates
    in
    data.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    currentWeek =
                        diffDays lmpDate currentDate // 7

                    conditionsForCurrentWeek =
                        periods
                            |> List.filter
                                (\periodConditions ->
                                    List.all (\condition -> condition currentWeek == True) periodConditions
                                )
                            |> List.head
                in
                conditionsForCurrentWeek
                    |> Maybe.map
                        (\conditions ->
                            -- There should be no encounters that are  within dates range,
                            -- that got a photo measurement.
                            data.previousMeasurementsWithDates
                                |> List.filterMap
                                    (\( encounterDate, measurements ) ->
                                        let
                                            encounterWeek =
                                                diffDays lmpDate encounterDate // 7
                                        in
                                        -- Encounter is within dates range, and it's has a photo measurement.
                                        if
                                            List.all (\condition -> condition encounterWeek == True) conditions
                                                && isJust measurements.prenatalPhoto
                                        then
                                            Just encounterDate

                                        else
                                            Nothing
                                    )
                                |> List.isEmpty
                        )
                    -- There are no period conditions, meaning we're not within required dates
                    -- range. Therefore, we do not allow photo activity.
                    |> Maybe.withDefault False
            )
        -- We do not allow photo activity when Lmp date is not known.
        |> Maybe.withDefault False


shouldShowPatientProvisionsResourcesTask : AssembledData -> Bool
shouldShowPatientProvisionsResourcesTask assembled =
    assembled.previousMeasurementsWithDates
        |> List.filter
            (\( _, measurements ) ->
                measurements.resource
                    |> Maybe.map (Tuple.second >> .value >> EverySet.member MosquitoNet)
                    |> Maybe.withDefault False
            )
        |> List.isEmpty


isFirstPrenatalEncounter : AssembledData -> Bool
isFirstPrenatalEncounter assembled =
    List.isEmpty assembled.previousMeasurementsWithDates


getFirstEncounterMeasurements : AssembledData -> PrenatalMeasurements
getFirstEncounterMeasurements data =
    case data.previousMeasurementsWithDates of
        [] ->
            data.measurements

        first :: others ->
            Tuple.second first


getLastEncounterMeasurementsWithDate : NominalDate -> AssembledData -> ( NominalDate, PrenatalMeasurements )
getLastEncounterMeasurementsWithDate currentDate data =
    case List.reverse data.previousMeasurementsWithDates of
        [] ->
            ( currentDate, data.measurements )

        first :: others ->
            first


getLastEncounterMeasurements : NominalDate -> AssembledData -> PrenatalMeasurements
getLastEncounterMeasurements currentDate data =
    getLastEncounterMeasurementsWithDate currentDate data |> Tuple.second
