module Pages.GlobalCaseManagement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements, FollowUpOption(..), FollowUpValue)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Village.Utils exposing (personLivesInVillage)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.GlobalCaseManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


allEncounterTypes : List IndividualEncounterType
allEncounterTypes =
    [ AcuteIllnessEncounter, NutritionEncounter ]


generateNutritionFollowUps : ModelIndexedDb -> FollowUpMeasurements -> Dict PersonId NutritionFollowUpItem
generateNutritionFollowUps db followUps =
    let
        nutritionIndividual =
            Dict.values followUps.nutritionIndividual

        nutritionGroup =
            Dict.values followUps.nutritionGroup

        generateFollowUpItems itemsList accumDict =
            itemsList
                |> List.foldl
                    (\item accum ->
                        let
                            newItem =
                                NutritionFollowUpItem item.dateMeasured "" item.value
                        in
                        Dict.get item.participantId accum
                            |> Maybe.map
                                (\member ->
                                    if Date.compare item.dateMeasured member.dateMeasured == GT then
                                        Dict.insert item.participantId newItem accum

                                    else
                                        accum
                                )
                            |> Maybe.withDefault (Dict.insert item.participantId newItem accum)
                    )
                    accumDict
    in
    generateFollowUpItems nutritionIndividual Dict.empty
        |> generateFollowUpItems nutritionGroup


generateAcuteIllnessFollowUps : ModelIndexedDb -> FollowUpMeasurements -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
generateAcuteIllnessFollowUps db followUps =
    let
        encountersData =
            generateAcuteIllnessEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.acuteIllnessEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, ( encounter.participant, encounter.sequenceNumber ) ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.acuteIllness
        |> List.foldl
            (\item accum ->
                let
                    encounterData =
                        item.encounterId
                            |> Maybe.andThen
                                (\encounterId -> Dict.get encounterId encountersData)
                in
                encounterData
                    |> Maybe.map
                        (\( participantId, encounterSequenceNumber ) ->
                            let
                                personId =
                                    item.participantId

                                newItem =
                                    AcuteIllnessFollowUpItem item.dateMeasured "" item.encounterId encounterSequenceNumber item.value
                            in
                            Dict.get ( participantId, personId ) accum
                                |> Maybe.map
                                    (\member ->
                                        if compareAcuteIllnessFollowUpItems newItem member == GT then
                                            Dict.insert ( participantId, personId ) newItem accum

                                        else
                                            accum
                                    )
                                |> Maybe.withDefault
                                    (Dict.insert ( participantId, personId ) newItem accum)
                        )
                    |> Maybe.withDefault accum
            )
            Dict.empty


generatePrenatalFollowUps : ModelIndexedDb -> FollowUpMeasurements -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
generatePrenatalFollowUps db followUps =
    let
        encountersData =
            generatePrenatalEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.prenatalEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, encounter.participant ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.prenatal
        |> List.foldl
            (\item accum ->
                let
                    encounterData =
                        item.encounterId
                            |> Maybe.andThen
                                (\encounterId -> Dict.get encounterId encountersData)
                in
                encounterData
                    |> Maybe.map
                        (\participantId ->
                            let
                                personId =
                                    item.participantId

                                newItem =
                                    PrenatalFollowUpItem item.dateMeasured "" item.encounterId item.value
                            in
                            Dict.get ( participantId, personId ) accum
                                |> Maybe.map
                                    (\member ->
                                        if Date.compare newItem.dateMeasured member.dateMeasured == GT then
                                            Dict.insert ( participantId, personId ) newItem accum

                                        else
                                            accum
                                    )
                                |> Maybe.withDefault
                                    (Dict.insert ( participantId, personId ) newItem accum)
                        )
                    |> Maybe.withDefault accum
            )
            Dict.empty


filterVillageResidents : VillageId -> (k -> PersonId) -> ModelIndexedDb -> Dict k { v | personName : String } -> Dict k { v | personName : String }
filterVillageResidents villageId keyToPersonIdFunc db dict =
    Dict.toList dict
        |> List.filterMap
            (\( k, v ) ->
                Dict.get (keyToPersonIdFunc k) db.people
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.andThen
                        (\person ->
                            if personLivesInVillage person db villageId then
                                Just ( k, { v | personName = person.name } )

                            else
                                Nothing
                        )
            )
        |> Dict.fromList


generateAcuteIllnessEncounters : FollowUpMeasurements -> EverySet AcuteIllnessEncounterId
generateAcuteIllnessEncounters followUps =
    generateEncountersIdsFromMeasurements .acuteIllness followUps


generatePrenatalEncounters : FollowUpMeasurements -> EverySet PrenatalEncounterId
generatePrenatalEncounters followUps =
    generateEncountersIdsFromMeasurements .prenatal followUps


generateEncountersIdsFromMeasurements :
    (FollowUpMeasurements -> Dict measurementId { a | encounterId : Maybe encounterId })
    -> FollowUpMeasurements
    -> EverySet encounterId
generateEncountersIdsFromMeasurements getMeasurementsFunc followUps =
    getMeasurementsFunc followUps
        |> Dict.values
        |> List.filterMap .encounterId
        |> EverySet.fromList


generateAcuteIllnessParticipants : EverySet AcuteIllnessEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generateAcuteIllnessParticipants encounters db =
    generateParticipantsIdsByEncounters .acuteIllnessEncounters encounters db


generatePrenatalParticipants : EverySet PrenatalEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generatePrenatalParticipants encounters db =
    generateParticipantsIdsByEncounters .prenatalEncounters encounters db


generateParticipantsIdsByEncounters :
    (ModelIndexedDb -> Dict encounterId (WebData { a | participant : IndividualEncounterParticipantId }))
    -> EverySet encounterId
    -> ModelIndexedDb
    -> EverySet IndividualEncounterParticipantId
generateParticipantsIdsByEncounters getEncountersFunc encounters db =
    encounters
        |> EverySet.toList
        |> List.filterMap
            (\encounterId ->
                getEncountersFunc db
                    |> Dict.get encounterId
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map .participant
            )
        |> EverySet.fromList


followUpDueOptionByDate : NominalDate -> NominalDate -> EverySet FollowUpOption -> FollowUpDueOption
followUpDueOptionByDate currentDate dateMeasured options =
    let
        dueIn =
            EverySet.toList options
                |> List.head
                |> Maybe.map
                    (\option ->
                        case option of
                            OneDay ->
                                1

                            ThreeDays ->
                                3

                            OneWeek ->
                                7

                            TwoWeeks ->
                                14
                    )
                |> Maybe.withDefault 0

        diff =
            diffDays currentDate dateMeasured + dueIn
    in
    if diff < 0 then
        OverDue

    else if diff == 0 then
        DueToday

    else if diff < 7 then
        DueThisWeek

    else
        DueThisMonth


compareAcuteIllnessFollowUpItems : AcuteIllnessFollowUpItem -> AcuteIllnessFollowUpItem -> Order
compareAcuteIllnessFollowUpItems item1 item2 =
    let
        byDate =
            Date.compare item1.dateMeasured item2.dateMeasured
    in
    if byDate == EQ then
        compare item1.encounterSequenceNumber item2.encounterSequenceNumber

    else
        byDate
