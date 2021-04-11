module Pages.GlobalCaseManagement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements, FollowUpOption(..), FollowUpValue)
import Backend.Model exposing (ModelIndexedDb)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.GlobalCaseManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..))


allEncounterTypes : List IndividualEncounterType
allEncounterTypes =
    [ AcuteIllnessEncounter, NutritionEncounter ]


generateNutritionFollowUps : NominalDate -> FollowUpMeasurements -> Dict PersonId FollowUpItem
generateNutritionFollowUps currentDate followUps =
    let
        nutritionIndividual =
            Dict.values followUps.nutritionIndividual

        nutritionGroup =
            Dict.values followUps.nutritionGroup

        generateFollowUpItems itemsList accumDict =
            itemsList
                |> List.foldl
                    (\item accum ->
                        Dict.get item.participantId accum
                            |> Maybe.map
                                (\member ->
                                    if Date.compare item.dateMeasured member.dateMeasured == GT then
                                        Dict.insert item.participantId
                                            (FollowUpItem item.dateMeasured item.value)
                                            accum

                                    else
                                        accum
                                )
                            |> Maybe.withDefault
                                (Dict.insert item.participantId
                                    (FollowUpItem item.dateMeasured item.value)
                                    accum
                                )
                    )
                    accumDict
    in
    generateFollowUpItems nutritionIndividual Dict.empty
        |> generateFollowUpItems nutritionGroup


generateAcuteIllnessFollowUps : ModelIndexedDb -> FollowUpMeasurements -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
generateAcuteIllnessFollowUps db followUps =
    let
        encountersToParticipantsDict =
            generateAcuteIllnessEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.acuteIllnessEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, encounter.participant ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.acuteIllness
        |> List.foldl
            (\item accum ->
                let
                    maybeIndividualParticipantId =
                        item.encounterId
                            |> Maybe.andThen
                                (\encounterId ->
                                    Dict.get encounterId encountersToParticipantsDict
                                )
                in
                maybeIndividualParticipantId
                    |> Maybe.map
                        (\participantId ->
                            let
                                personId =
                                    item.participantId
                            in
                            Dict.get ( participantId, personId ) accum
                                |> Maybe.map
                                    (\member ->
                                        if Date.compare item.dateMeasured member.dateMeasured == GT then
                                            Dict.insert ( participantId, personId )
                                                (AcuteIllnessFollowUpItem item.dateMeasured item.value)
                                                accum

                                        else
                                            accum
                                    )
                                |> Maybe.withDefault
                                    (Dict.insert ( participantId, personId )
                                        (AcuteIllnessFollowUpItem item.dateMeasured item.value)
                                        accum
                                    )
                        )
                    |> Maybe.withDefault accum
            )
            Dict.empty


generateAcuteIllnessEncounters : FollowUpMeasurements -> EverySet AcuteIllnessEncounterId
generateAcuteIllnessEncounters followUps =
    Dict.values followUps.acuteIllness
        |> List.filterMap .encounterId
        |> EverySet.fromList


generateAcuteIllnessParticipants : EverySet AcuteIllnessEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generateAcuteIllnessParticipants encounters db =
    encounters
        |> EverySet.toList
        |> List.filterMap
            (\encounterId ->
                Dict.get encounterId db.acuteIllnessEncounters
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map .participant
            )
        |> EverySet.fromList


followUpDueOptionByDate : NominalDate -> NominalDate -> FollowUpValue -> FollowUpDueOption
followUpDueOptionByDate currentDate dateMeasured value =
    let
        dueIn =
            EverySet.toList value.options
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
            diffDays dateMeasured currentDate + dueIn
    in
    if diff < 0 then
        OverDue

    else if diff == 0 then
        DueToday

    else if diff < 7 then
        DueThisWeek

    else
        DueThisMonth
