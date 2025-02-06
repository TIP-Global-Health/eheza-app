module Components.PatientsSearchForm.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Person.Model exposing (Initiator)
import Components.PatientsSearchForm.Model exposing (..)
import Maybe.Extra


fetch : Model -> List MsgIndexedDb
fetch model =
    let
        trimmed =
            Maybe.withDefault "" model.search
    in
    if String.isEmpty trimmed then
        []

    else
        let
            fetchMsg =
                case model.mode of
                    ModeSearchByName ->
                        FetchPeopleByName

                    ModeSearchByNationalId ->
                        FetchPeopleByNationalId
        in
        [ fetchMsg trimmed ]
