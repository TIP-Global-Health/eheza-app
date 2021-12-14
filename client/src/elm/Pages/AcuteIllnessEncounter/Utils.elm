module Pages.AcuteIllnessEncounter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter, AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.AcuteIllnessActivity.Types exposing (ExposureTask(..), LaboratoryTask(..), NextStepsTask(..), PhysicalExamTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (resolveAcuteIllnessDiagnosis)
import Pages.AcuteIllnessEncounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


generateAssembledData : NominalDate -> AcuteIllnessEncounterId -> Bool -> ModelIndexedDb -> WebData AssembledData
generateAssembledData currentDate id isChw db =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.acuteIllnessMeasurements
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

        previousEncountersData =
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements id encounter_.participant db)
                |> Maybe.withDefault []

        initialEncounter =
            -- Intial encounter is the one where all measurements are taken and
            -- initial diagnosis is made.
            -- For CHW, it's only the first encounter at illness.
            -- For nurse, it's always an initial encounter, since
            -- nurse can do only single encouter throughout the illness.
            not isChw || List.isEmpty previousEncountersData

        assembled =
            RemoteData.map AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousEncountersData)
                |> RemoteData.andMap (Success [])
                |> RemoteData.andMap (Success [])
                |> RemoteData.andMap (Success initialEncounter)
                |> RemoteData.andMap (Success Nothing)

        currentDiagnosis =
            if initialEncounter || isJust diagnosisByCurrentEncounterMeasurements then
                diagnosisByCurrentEncounterMeasurements

            else
                diagnosisByPreviousEncounters

        diagnosisByCurrentEncounterMeasurements =
            RemoteData.toMaybe assembled
                |> Maybe.andThen (resolveAcuteIllnessDiagnosis currentDate isChw)
                |> Maybe.map (\diagnosis -> ( currentDate, diagnosis ))

        diagnosisByPreviousEncounters =
            RemoteData.toMaybe encounter
                |> Maybe.map
                    (.participant
                        >> getAcuteIllnessDiagnosisByPreviousEncounters id db
                    )
                |> Maybe.withDefault Nothing
    in
    RemoteData.map
        (\data ->
            let
                currentEncounterData =
                    AcuteIllnessEncounterData id
                        data.encounter.encounterType
                        data.encounter.startDate
                        data.encounter.sequenceNumber
                        data.encounter.diagnosis
                        data.measurements

                allEncountersData =
                    data.previousEncountersData ++ [ currentEncounterData ]

                ( firstInitialWithSubsequent, secondInitialWithSubsequent ) =
                    let
                        nurseEncounterIndex =
                            List.indexedMap (\index encounterData -> ( index, encounterData.encounterType ))
                                allEncountersData
                                |> List.filter (Tuple.second >> (==) AcuteIllnessEncounterNurse)
                                |> List.reverse
                                |> List.head
                                |> Maybe.map Tuple.first
                    in
                    Maybe.map
                        (\nurseIndex ->
                            if nurseIndex == 0 then
                                ( allEncountersData, [] )

                            else
                                ( List.take nurseIndex allEncountersData
                                , List.drop nurseIndex allEncountersData
                                )
                        )
                        nurseEncounterIndex
                        |> Maybe.withDefault ( allEncountersData, [] )
            in
            { data
                | diagnosis = currentDiagnosis
                , firstInitialWithSubsequent = firstInitialWithSubsequent
                , secondInitialWithSubsequent = secondInitialWithSubsequent
            }
        )
        assembled


generatePreviousMeasurements :
    AcuteIllnessEncounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List AcuteIllnessEncounterData
generatePreviousMeasurements currentEncounterId participantId db =
    getAcuteIllnessEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter.
                if encounterId == currentEncounterId then
                    Nothing

                else
                    case Dict.get encounterId db.acuteIllnessMeasurements of
                        Just (Success measurements) ->
                            Just <|
                                AcuteIllnessEncounterData encounterId
                                    encounter.encounterType
                                    encounter.startDate
                                    encounter.sequenceNumber
                                    encounter.diagnosis
                                    measurements

                        _ ->
                            Nothing
            )
        >> List.sortWith compareAcuteIllnessEncounterDataAsc


getAcuteIllnessEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
getAcuteIllnessEncountersForParticipant db participantId =
    Dict.get participantId db.acuteIllnessEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
        |> List.sortWith (\( _, e1 ) ( _, e2 ) -> compareAcuteIllnessEncounterDataDesc e1 e2)


compareAcuteIllnessEncounterDataDesc :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncounterDataDesc data1 data2 =
    compareAcuteIllnessEncounterDataAsc data2 data1


compareAcuteIllnessEncounterDataAsc :
    { a | startDate : NominalDate, sequenceNumber : Int }
    -> { a | startDate : NominalDate, sequenceNumber : Int }
    -> Order
compareAcuteIllnessEncounterDataAsc data1 data2 =
    case Date.compare data1.startDate data2.startDate of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare data1.sequenceNumber data2.sequenceNumber


getAcuteIllnessDiagnosisByPreviousEncounters :
    AcuteIllnessEncounterId
    -> ModelIndexedDb
    -> IndividualEncounterParticipantId
    -> Maybe ( NominalDate, AcuteIllnessDiagnosis )
getAcuteIllnessDiagnosisByPreviousEncounters currentEncounterId db participantId =
    getAcuteIllnessEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter,
                -- and those that do not have diagnosis set.
                if encounterId == currentEncounterId || encounter.diagnosis == NoAcuteIllnessDiagnosis then
                    Nothing

                else
                    Just ( encounter.startDate, encounter.diagnosis )
            )
        |> List.head


{-| Since there can be multiple encounters, resolved diagnosis is the one
that was set in most recent encounter.
-}
getAcuteIllnessDiagnosisForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe ( NominalDate, AcuteIllnessDiagnosis )
getAcuteIllnessDiagnosisForParticipant db participantId =
    getAcuteIllnessEncountersForParticipant db participantId
        |> getAcuteIllnessDiagnosisForEncounters


getAcuteIllnessDiagnosisForEncounters : List ( AcuteIllnessEncounterId, AcuteIllnessEncounter ) -> Maybe ( NominalDate, AcuteIllnessDiagnosis )
getAcuteIllnessDiagnosisForEncounters encounters =
    List.filterMap
        (\( _, encounter ) ->
            if encounter.diagnosis /= NoAcuteIllnessDiagnosis then
                Just ( encounter.startDate, encounter.diagnosis )

            else
                Nothing
        )
        encounters
        -- We know that encounters are sorted DESC, so the one at
        -- head is the most recent.
        |> List.head
