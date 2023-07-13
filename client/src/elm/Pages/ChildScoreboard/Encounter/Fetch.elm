module Pages.ChildScoreboard.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : ChildScoreboardEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants) participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        -- Trying to resolve the pregnancy tracked on E-Heza, at which
        -- CHW Postpartum encounter the child was registered on E-Heza.
        maybePregnancyId =
            Maybe.andThen (\personId -> Dict.get personId db.pregnancyByNewborn) maybePersonId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.Extra.join
                |> Maybe.map Tuple.first
    in
    Maybe.Extra.values
        [ Just <| FetchChildScoreboardEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        , Maybe.map FetchPregnancyByNewborn maybePersonId

        -- Loading data for pregnancy participant and encounters.
        , Maybe.map FetchIndividualEncounterParticipant maybePregnancyId
        , Maybe.map FetchPrenatalEncountersForParticipant maybePregnancyId
        ]
        ++ (Maybe.map (\personId -> Backend.NutritionEncounter.Fetch.fetch personId db) maybePersonId
                |> Maybe.withDefault []
           )
