module Pages.PrenatalEncounter.Utils exposing (calculateEDDandEGADays, expectPrenatalActivity, generateAssembledData, generateEDDandEGA, generateEGAWeeksDaysLabel, generateGravida, generatePara, generatePreviousMeasurements, getLmpMeasurement, resolveGlobalLmpDate, resolveGlobalObstetricHistory)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Date.Extra as Date exposing (Interval(Day))
import EveryDict
import EveryDictList
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import PrenatalActivity.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)
import Utils.NominalDate exposing (compareDates)


calculateEDDandEGADays : NominalDate -> NominalDate -> ( NominalDate, Int )
calculateEDDandEGADays currentDate lmpDate =
    ( toLocalDateTime lmpDate 12 0 0 0
        |> Date.add Day 280
        |> fromLocalDateTime
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
            ( formatMMDDYYYY eddDate, generateEGAWeeksDaysLabel language diffInDays )
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
        "0" ++ toString total

    else
        toString total


generatePara : ObstetricHistoryValue -> String
generatePara value =
    toString (value.termPregnancy + value.stillbirthsAtTerm)
        ++ toString (value.preTermPregnancy + value.stillbirthsPreTerm)
        ++ toString value.abortions
        ++ toString value.liveChildren


getLmpMeasurement : PrenatalMeasurements -> Maybe NominalDate
getLmpMeasurement measurements =
    measurements.lastMenstrualPeriod
        |> Maybe.map (Tuple.second >> .value >> .date)


getObstetricHistory : PrenatalMeasurements -> Maybe ObstetricHistoryValue
getObstetricHistory measurements =
    measurements.obstetricHistory
        |> Maybe.map (Tuple.second >> .value)


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


generatePreviousMeasurements : PrenatalEncounterId -> PrenatalParticipantId -> ModelIndexedDb -> WebData (List ( NominalDate, PrenatalMeasurements ))
generatePreviousMeasurements currentEncounterId participantId db =
    EveryDict.get participantId db.prenatalEncountersByParticipant
        |> Maybe.withDefault NotAsked
        |> RemoteData.map
            (EveryDictList.toList
                >> List.filterMap
                    (\( encounterId, encounter ) ->
                        -- We do not want to get data of current encounter.
                        if encounterId == currentEncounterId then
                            Nothing

                        else
                            case EveryDict.get encounterId db.prenatalMeasurements of
                                Just (Success data) ->
                                    Just ( encounter.startDate, data )

                                _ ->
                                    Nothing
                    )
                >> List.sortWith
                    (\( date1, _ ) ( date2, _ ) -> compareDates date1 date2)
            )


generateAssembledData : PrenatalEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            EveryDict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            EveryDict.get id db.prenatalMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        EveryDict.get encounter.participant db.prenatalParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant ->
                        EveryDict.get participant.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        generatePreviousMeasurements id encounter.participant db
                    )
                |> RemoteData.withDefault []

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
        |> RemoteData.andMap (Success globalLmpDate)
        |> RemoteData.andMap (Success globalObstetricHistory)


expectPrenatalActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectPrenatalActivity currentDate data activity =
    let
        isFirstEncounter =
            List.isEmpty data.previousMeasurementsWithDates
    in
    case activity of
        PregnancyDating ->
            isFirstEncounter

        History ->
            isFirstEncounter

        PrenatalPhoto ->
            expectPrenatalPhoto currentDate data

        _ ->
            True


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate data =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 20 and week 29.
            --  3. Week 30, or more.
            [ [ (>) 13 ], [ (>) 30, (<=) 20 ], [ (<=) 30 ] ]

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
