module Pages.Completion.Fetch exposing (fetch)

import Backend.Model
import Pages.Completion.Model exposing (Model)


fetch : Backend.Model.ModelBackend -> Model -> List Backend.Model.Msg
fetch modelBackend model =
    []
