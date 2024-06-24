module Pages.Reports.Fetch exposing (fetch)

import Backend.Model
import Pages.Reports.Model exposing (Model)


fetch : Backend.Model.ModelBackend -> Model -> List Backend.Model.Msg
fetch modelBackend model =
    []
