module Pages.PrenatalParticipant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))


fetch : PersonId -> List MsgIndexedDb
fetch id =
    [ FetchPerson id
    , FetchPrenatalParticipantsForPerson id
    ]
