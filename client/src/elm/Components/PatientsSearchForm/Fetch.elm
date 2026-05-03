module Components.PatientsSearchForm.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))
import Components.PatientsSearchForm.Model exposing (Model, PatientsSearchFormMode(..))


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
