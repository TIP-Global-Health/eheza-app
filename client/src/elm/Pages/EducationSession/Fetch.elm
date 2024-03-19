module Pages.EducationSession.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.EducationSession.Model exposing (Model)


fetch : EducationSessionId -> Model -> List MsgIndexedDb
fetch id model =
    let
        trimmed =
            Maybe.withDefault "" model.search
                |> String.trim

        fetchPeopleByName =
            if String.isEmpty trimmed then
                []

            else
                [ FetchPeopleByName trimmed ]
    in
    [ FetchVillages, FetchEducationSession id ] ++ fetchPeopleByName
