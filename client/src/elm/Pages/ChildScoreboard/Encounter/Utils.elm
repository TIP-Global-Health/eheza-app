module Pages.ChildScoreboard.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


generateAssembledData : ChildScoreboardEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.childScoreboardMeasurements
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

        ancEncounters =
            RemoteData.toMaybe participant
                |> Maybe.andThen (.person >> countANCEncounters db)
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success ancEncounters)


countANCEncounters : ModelIndexedDb -> PersonId -> Maybe Int
countANCEncounters db childId =
    Dict.get childId db.pregnancyByNewborn
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.andThen
            (\( participantId, _ ) ->
                Dict.get participantId db.prenatalEncountersByParticipant
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map
                        (Dict.filter
                            (\_ encounter ->
                                not <| List.member encounter.encounterType [ NursePostpartumEncounter, ChwPostpartumEncounter ]
                            )
                            >> Dict.size
                        )
            )
