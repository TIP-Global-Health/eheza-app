module Pages.Dashboard.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import Pages.People.Model exposing (..)


fetch : Maybe PersonId -> Model -> List MsgIndexedDb
fetch relation model =
    []
