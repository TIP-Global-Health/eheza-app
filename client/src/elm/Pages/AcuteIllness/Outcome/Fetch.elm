module Pages.AcuteIllness.Outcome.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import Pages.Report.Utils exposing (getAcuteIllnessEncountersForParticipant)
import RemoteData


fetch : IndividualEncounterParticipantId -> ModelIndexedDb -> List MsgIndexedDb
fetch participantId db =
    let
        personId =
            Dict.get participantId db.individualParticipants
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            getAcuteIllnessEncountersForParticipant db participantId
                |> List.map Tuple.first

        -- The page is assembled on the latest visit.
        latestEncounterId =
            List.head encountersIds

        -- We fetch measurements for all encounters, to be
        -- able to apply `expectedAcuteIllnessActivity` logic.
        fetchMeasurements =
            List.map FetchAcuteIllnessMeasurements encountersIds
    in
    Maybe.Extra.values
        [ Just (FetchIndividualEncounterParticipant participantId)
        , Maybe.map FetchPerson personId
        , Just (FetchAcuteIllnessEncountersForParticipant participantId)
        , Maybe.map FetchAcuteIllnessEncounter latestEncounterId
        ]
        ++ fetchMeasurements
