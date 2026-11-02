module Pages.HomeVisit.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.NutritionEncounter.Utils exposing (getHomeVisitEncountersForParticipant)
import Backend.Utils exposing (resolveIndividualParticipantWithDuplicates)
import Maybe.Extra
import RemoteData


fetch : HomeVisitEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.homeVisitEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant

        maybePersonId =
            Maybe.andThen (\id_ -> Dict.get id_ db.individualParticipants)
                participantId
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        participantsIds =
            Maybe.map (\id_ -> resolveIndividualParticipantWithDuplicates id_ db) participantId
                |> Maybe.withDefault []

        encountersIds =
            List.concatMap (getHomeVisitEncountersForParticipant db >> List.map Tuple.first) participantsIds

        -- We fetch measurements of all encounters.
        fetchMeasurements =
            List.map FetchHomeVisitMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Just <| FetchHomeVisitEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        , Just <| FetchHomeVisitEncountersForParticipants participantsIds
        ]
        ++ (Maybe.map
                (\personId ->
                    FetchPerson personId
                        :: Backend.NutritionEncounter.Fetch.fetch personId db
                )
                maybePersonId
                |> Maybe.withDefault []
           )
        ++ fetchMeasurements
