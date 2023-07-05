module Pages.ChildScoreboard.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
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
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
