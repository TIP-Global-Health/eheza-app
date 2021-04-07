module Pages.GlobalCaseManagement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpOption(..), FollowUpValue)
import Backend.Model exposing (ModelIndexedDb)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.GlobalCaseManagement.Model exposing (..)
import RemoteData exposing (RemoteData(..))


allEncounterTypes : List IndividualEncounterType
allEncounterTypes =
    [ AcuteIllnessEncounter, NutritionEncounter ]


generateNutritionFollowUps : NominalDate -> HealthCenterId -> ModelIndexedDb -> Dict PersonId FollowUpItem
generateNutritionFollowUps currentDate healthCenterId db =
    let
        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        nutritionIndividual =
            followUps
                |> Maybe.map (.nutritionIndividual >> Dict.values)
                |> Maybe.withDefault []

        _ =
            Debug.log "nutritionIndividual" (List.length nutritionIndividual)

        nutritionGroup =
            followUps
                |> Maybe.map (.nutritionGroup >> Dict.values)
                |> Maybe.withDefault []

        _ =
            Debug.log "nutritionGroup" (List.length nutritionGroup)

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
                    Dict.empty
    in
    generateFollowUpItems nutritionIndividual Dict.empty
        |> generateFollowUpItems nutritionGroup


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
    if diff < 1 then
        OverDue

    else if diff == 1 then
        DueToday

    else if diff < 8 then
        DueThisWeek

    else
        DueThisMonth
