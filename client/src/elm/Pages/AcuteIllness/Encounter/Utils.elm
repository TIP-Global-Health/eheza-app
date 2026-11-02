module Pages.AcuteIllness.Encounter.Utils exposing (generateAllEncountersData, generateAssembledData, getAcuteIllnessDiagnosisForParticipant, splitByInitialNurseEncounter)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis, AcuteIllnessEncounterType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.AcuteIllness.Activity.Utils exposing (resolveAcuteIllnessDiagnosis)
import Pages.AcuteIllness.Encounter.Model exposing (AcuteIllnessEncounterData, AssembledData)
import Pages.Report.Utils exposing (compareAcuteIllnessEncounters, getAcuteIllnessDiagnosisForEncounters, getAcuteIllnessEncountersForParticipant)
import RemoteData exposing (RemoteData(..), WebData)
import SyncManager.Model exposing (SiteFeature)


generateAssembledData : NominalDate -> EverySet SiteFeature -> AcuteIllnessEncounterId -> Bool -> ModelIndexedDb -> WebData AssembledData
generateAssembledData currentDate features id isChw db =
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

        -- Encounters before this one, by date and then sequence number,
        -- most recent first. Later encounters are not its history.
        previousEncounters =
            RemoteData.toMaybe encounter
                |> Maybe.map
                    (\encounter_ ->
                        getAcuteIllnessEncountersForParticipant db encounter_.participant
                            |> List.filter (\( _, other ) -> compareAcuteIllnessEncounters other encounter_ == LT)
                    )
                |> Maybe.withDefault []

        previousEncountersData =
            generatePreviousMeasurements previousEncounters db

        assembled =
            RemoteData.map AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousEncountersData)
                |> RemoteData.andMap (Success [])
                |> RemoteData.andMap (Success [])
                |> RemoteData.andMap (Success True)
                |> RemoteData.andMap (Success Nothing)
    in
    RemoteData.map
        (\data ->
            let
                -- Intial encounter is the one where all measurements are taken and
                -- initial diagnosis is made.
                initialEncounter =
                    case data.encounter.encounterType of
                        AcuteIllnessEncounterNurse ->
                            -- This type of encounter is assigned to first encounter
                            -- performed by nurse. By definition, it's considered
                            -- initital.
                            True

                        AcuteIllnessEncounterNurseSubsequent ->
                            -- This type of encounter is assigned when first nurse
                            -- encounter was performed already. By definition,
                            -- it's considered not an initital.
                            False

                        AcuteIllnessEncounterCHW ->
                            -- For CHW, it's only the first encounter at illness.
                            List.isEmpty previousEncountersData

                currentDiagnosis =
                    if initialEncounter || isJust diagnosisByCurrentEncounterMeasurements then
                        diagnosisByCurrentEncounterMeasurements

                    else
                        getAcuteIllnessDiagnosisForEncounters previousEncounters

                diagnosisByCurrentEncounterMeasurements =
                    resolveAcuteIllnessDiagnosis currentDate features isChw data
                        |> Maybe.map (\diagnosis -> ( currentDate, diagnosis ))

                ( previousFirstInitialWithSubsequent, previousSecondInitialWithSubsequent ) =
                    splitByInitialNurseEncounter data.previousEncountersData
            in
            { data
                | initialEncounter = initialEncounter
                , diagnosis = currentDiagnosis
                , previousFirstInitialWithSubsequent = previousFirstInitialWithSubsequent
                , previousSecondInitialWithSubsequent = previousSecondInitialWithSubsequent
            }
        )
        assembled


{-| The encounters of the illness up to the one being viewed, ordered by
date. The one being viewed comes last.
-}
generateAllEncountersData : AssembledData -> List AcuteIllnessEncounterData
generateAllEncountersData assembled =
    assembled.previousEncountersData
        ++ [ AcuteIllnessEncounterData assembled.id
                assembled.encounter.encounterType
                assembled.encounter.startDate
                assembled.encounter.sequenceNumber
                assembled.encounter.diagnosis
                assembled.measurements
           ]


{-| Splits encounters into the sequence that starts at the initial CHW
encounter, and the one that starts at the encounter where nurse took over.
When nurse ran the first encounter, there is no CHW sequence, so all
encounters belong to the first one.
-}
splitByInitialNurseEncounter :
    List AcuteIllnessEncounterData
    -> ( List AcuteIllnessEncounterData, List AcuteIllnessEncounterData )
splitByInitialNurseEncounter encountersData =
    let
        nurseEncounterIndex =
            List.indexedMap (\index encounterData -> ( index, encounterData.encounterType ))
                encountersData
                |> List.filter (Tuple.second >> (==) AcuteIllnessEncounterNurse)
                |> List.reverse
                |> List.head
                |> Maybe.map Tuple.first
    in
    Maybe.map
        (\nurseIndex ->
            if nurseIndex == 0 then
                ( encountersData, [] )

            else
                ( List.take nurseIndex encountersData
                , List.drop nurseIndex encountersData
                )
        )
        nurseEncounterIndex
        |> Maybe.withDefault ( encountersData, [] )


{-| Data of the given encounters (sorted most recent first) whose
measurements are loaded, oldest first.
-}
generatePreviousMeasurements :
    List ( AcuteIllnessEncounterId, AcuteIllnessEncounter )
    -> ModelIndexedDb
    -> List AcuteIllnessEncounterData
generatePreviousMeasurements encounters db =
    encounters
        |> List.filterMap
            (\( encounterId, encounter ) ->
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
        |> List.reverse


{-| Since there can be multiple encounters, resolved diagnosis is the one
that was set in most recent encounter.
-}
getAcuteIllnessDiagnosisForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> Maybe ( NominalDate, AcuteIllnessDiagnosis )
getAcuteIllnessDiagnosisForParticipant db participantId =
    getAcuteIllnessEncountersForParticipant db participantId
        |> getAcuteIllnessDiagnosisForEncounters
