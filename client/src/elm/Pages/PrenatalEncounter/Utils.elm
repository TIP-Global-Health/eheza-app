module Pages.PrenatalEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuples)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Maybe.Extra exposing (isJust, orElse, unwrap)
import Pages.PrenatalActivity.Model exposing (NextStepsTask(..))
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


expectActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate assembled activity =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    -- Only show on first encounter
                    isFirstEncounter assembled

                History ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, measurements ) ->
                                measurements.malariaPrevention
                                    |> Maybe.map (Tuple.second >> .value >> EverySet.member MosquitoNet)
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                Medication ->
                    True

                DangerSigns ->
                    True

                Laboratory ->
                    -- @todo
                    True

                PrenatalPhoto ->
                    expectPrenatalPhoto currentDate assembled

                -- Unique Chw activities.
                _ ->
                    False

        ChwFirstEncounter ->
            case activity of
                PregnancyDating ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False

        ChwSecondEncounter ->
            case activity of
                DangerSigns ->
                    True

                BirthPlan ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False

        ChwThirdPlusEncounter ->
            case activity of
                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

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
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate data =
    case data.encounter.encounterType of
        NurseEncounter ->
            -- There're no mandatory activities for nurse encounters.
            True

        ChwFirstEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    ((not <| expectActivity currentDate data PregnancyDating) || activityCompleted currentDate data PregnancyDating)
                        && ((not <| expectActivity currentDate data Laboratory) || activityCompleted currentDate data Laboratory)
                        && activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwSecondEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data BirthPlan
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwThirdPlusEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwPostpartumEncounter ->
            activityCompleted currentDate data PregnancyOutcome
                && activityCompleted currentDate data DangerSigns


noDangerSigns : AssembledData -> Bool
noDangerSigns data =
    let
        getDangerSignsType getFunc =
            data.measurements.dangerSigns
                |> Maybe.map (Tuple.second >> .value >> getFunc >> EverySet.toList)
                |> Maybe.withDefault []

        dangerSignsEmpty emptySign signsList =
            List.isEmpty signsList || signsList == [ emptySign ]
    in
    case data.encounter.encounterType of
        ChwPostpartumEncounter ->
            let
                motherSignsEmpty =
                    getDangerSignsType .postpartumMother
                        |> dangerSignsEmpty NoPostpartumMotherDangerSigns

                childSignsEmpty =
                    getDangerSignsType .postpartumChild
                        |> dangerSignsEmpty NoPostpartumChildDangerSigns
            in
            motherSignsEmpty && childSignsEmpty

        _ ->
            getDangerSignsType .signs
                |> dangerSignsEmpty NoDangerSign


dangerSignsPresent : AssembledData -> Bool
dangerSignsPresent data =
    isJust data.measurements.dangerSigns && not (noDangerSigns data)


generateDangerSignsList : Language -> AssembledData -> List String
generateDangerSignsList language data =
    let
        getDangerSignsListForType getFunc translateFunc noSignsValue =
            data.measurements.dangerSigns
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> getFunc
                        >> EverySet.toList
                        >> List.filter ((/=) noSignsValue)
                        >> List.map (translateFunc >> translate language)
                    )
                |> Maybe.withDefault []
    in
    case data.encounter.encounterType of
        ChwPostpartumEncounter ->
            let
                motherSigns =
                    getDangerSignsListForType .postpartumMother Translate.PostpartumMotherDangerSign NoPostpartumMotherDangerSigns

                childSigns =
                    getDangerSignsListForType .postpartumChild Translate.PostpartumChildDangerSign NoPostpartumChildDangerSigns
            in
            motherSigns ++ childSigns

        _ ->
            getDangerSignsListForType .signs Translate.DangerSign NoDangerSign


activityCompleted : NominalDate -> AssembledData -> PrenatalActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        PregnancyDating ->
            isJust assembled.measurements.lastMenstrualPeriod

        History ->
            if isFirstEncounter assembled then
                -- First antenatal encounter - all tasks should be completed
                isJust assembled.measurements.obstetricHistory
                    && isJust assembled.measurements.obstetricHistoryStep2
                    && isJust assembled.measurements.medicalHistory
                    && isJust assembled.measurements.socialHistory

            else
                -- Subsequent antenatal encounter - only Social history task
                -- needs to be completed.
                isJust assembled.measurements.socialHistory

        Examination ->
            isJust assembled.measurements.vitals
                && isJust assembled.measurements.nutrition
                && isJust assembled.measurements.corePhysicalExam
                && isJust assembled.measurements.obstetricalExam
                && isJust assembled.measurements.breastExam

        FamilyPlanning ->
            isJust assembled.measurements.familyPlanning

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            isJust assembled.measurements.malariaPrevention

        Medication ->
            isJust assembled.measurements.medication

        DangerSigns ->
            isJust assembled.measurements.dangerSigns

        PrenatalPhoto ->
            isJust assembled.measurements.prenatalPhoto

        Laboratory ->
            if assembled.encounter.encounterType == NurseEncounter then
                -- @todo
                False

            else
                -- CHW only got one lab on first encounter
                isJust assembled.measurements.pregnancyTest

        Backend.PrenatalActivity.Model.HealthEducation ->
            isJust assembled.measurements.healthEducation

        BirthPlan ->
            isJust assembled.measurements.birthPlan

        NextSteps ->
            let
                nextStepsTasks =
                    resolveNextStepsTasks currentDate assembled
            in
            case nextStepsTasks of
                [ NextStepsAppointmentConfirmation, NextStepsFollowUp ] ->
                    isJust assembled.measurements.appointmentConfirmation && isJust assembled.measurements.followUp

                [ NextStepsSendToHC, NextStepsFollowUp ] ->
                    isJust assembled.measurements.sendToHC && isJust assembled.measurements.followUp

                [ NextStepsHealthEducation, NextStepsNewbornEnrolment ] ->
                    isJust assembled.measurements.healthEducation
                        && isJust assembled.participant.newborn

                [ NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNewbornEnrolment ] ->
                    isJust assembled.measurements.sendToHC
                        && isJust assembled.measurements.followUp
                        && isJust assembled.measurements.healthEducation
                        && isJust assembled.participant.newborn

                _ ->
                    False

        PregnancyOutcome ->
            isJust assembled.participant.dateConcluded


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate data =
    case data.encounter.encounterType of
        -- We should never get here, as Next Steps
        -- displayed only for CHW.
        NurseEncounter ->
            []

        _ ->
            -- The order is important. Do not change.
            [ NextStepsAppointmentConfirmation, NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNewbornEnrolment ]
                |> List.filter (expectNextStepsTasks currentDate data)


expectNextStepsTasks : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTasks currentDate data task =
    let
        dangerSigns =
            dangerSignsPresent data
    in
    case task of
        NextStepsAppointmentConfirmation ->
            not dangerSigns && data.encounter.encounterType /= ChwPostpartumEncounter

        NextStepsFollowUp ->
            case data.encounter.encounterType of
                ChwPostpartumEncounter ->
                    dangerSigns

                _ ->
                    True

        NextStepsSendToHC ->
            dangerSigns

        NextStepsHealthEducation ->
            data.encounter.encounterType == ChwPostpartumEncounter

        NextStepsNewbornEnrolment ->
            data.encounter.encounterType == ChwPostpartumEncounter


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


getMotherHeightMeasurement : PrenatalMeasurements -> Maybe HeightInCm
getMotherHeightMeasurement measurements =
    measurements.nutrition
        |> Maybe.map (Tuple.second >> .value >> .height)


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


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate data =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 13 and week 27.
            --  3. Week 28, or more.
            [ [ (>) 13 ], [ (>) 28, (<=) 13 ], [ (<=) 28 ] ]

        nursePreviousMeasurements =
            List.map Tuple.second data.nursePreviousMeasurementsWithDates
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
                            data.nursePreviousMeasurementsWithDates
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


isFirstEncounter : AssembledData -> Bool
isFirstEncounter assembled =
    List.isEmpty assembled.nursePreviousMeasurementsWithDates


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
