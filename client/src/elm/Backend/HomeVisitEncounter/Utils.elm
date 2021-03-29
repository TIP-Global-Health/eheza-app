module Backend.HomeVisitEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (isNothing)
import Pages.HomeVisitEncounter.Model exposing (HomeVisitAssesment(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)


resolveHomeVisitParticipantForChild : PersonId -> ModelIndexedDb -> Maybe IndividualEncounterParticipantId
resolveHomeVisitParticipantForChild id db =
    Dict.get id db.individualParticipantsByPerson
        |> Maybe.withDefault NotAsked
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (Dict.toList
                >> List.filter
                    (\( _, participant ) ->
                        participant.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
                    )
                >> List.head
                >> Maybe.map Tuple.first
            )
