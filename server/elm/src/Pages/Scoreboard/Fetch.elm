module Pages.Scoreboard.Fetch exposing (fetch)

import Backend.Model
import Pages.Scoreboard.Model exposing (Model)


fetch : Backend.Model.ModelBackend -> Model -> List Backend.Model.Msg
fetch modelBackend model =
    []
