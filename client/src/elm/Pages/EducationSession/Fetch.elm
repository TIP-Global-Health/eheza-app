module Pages.EducationSession.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))


fetch : EducationSessionId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    [-- @todo
     -- FetchEducationSession id
    ]
