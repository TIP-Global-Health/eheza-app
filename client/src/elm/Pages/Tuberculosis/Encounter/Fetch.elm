module Pages.Tuberculosis.Encounter.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Maybe.Extra
import RemoteData


fetch : TuberculosisEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        participantId =
            Dict.get id db.tuberculosisEncounters
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .participant
    in
    Maybe.Extra.values
        [ Just <| FetchTuberculosisEncounter id
        , Maybe.map FetchIndividualEncounterParticipant participantId
        ]
