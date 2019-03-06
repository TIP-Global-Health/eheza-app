module Pages.PatientRegistration.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))
import Pages.PatientRegistration.Model exposing (..)


fetch : Model -> List MsgIndexedDb
fetch model =
    let
        trimmed =
            model.submittedSearch
                |> Maybe.withDefault ""
                |> String.trim
    in
    if trimmed == "" then
        []

    else
        [ FetchParticipantsByName trimmed ]
