module Pages.Nutrition.ProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.AcuteIllness.Participant.Fetch
import Pages.Nutrition.Encounter.Fetch
import RemoteData


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.nutritionEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        fetchAcuteIllnessDataMsgs =
            Maybe.map
                (\personId ->
                    Pages.AcuteIllness.Participant.Fetch.fetch personId db
                )
                maybePersonId
                |> Maybe.withDefault []
    in
    Pages.Nutrition.Encounter.Fetch.fetch id db ++ fetchAcuteIllnessDataMsgs
