module Pages.People.Fetch exposing (fetch)

import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import Maybe.Extra
import Pages.ParticipantRegistration.Model exposing (..)
import Participant.Model exposing (ParticipantId(..), ParticipantType(..))
import RemoteData exposing (RemoteData(..))


fetch : Maybe String -> List MsgIndexedDb
fetch search =
    let
        trimmed =
            search
                |> Maybe.withDefault ""
                |> String.trim
    in
    if String.isEmpty trimmed then
        []

    else
        [ FetchPeopleByName trimmed
        ]
