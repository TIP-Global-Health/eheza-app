module Backend.NCDEncounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)


getNCDEncountersForParticipant : ModelIndexedDb -> IndividualEncounterParticipantId -> List ( NCDEncounterId, NCDEncounter )
getNCDEncountersForParticipant db participantId =
    Dict.get participantId db.ncdEncountersByParticipant
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
