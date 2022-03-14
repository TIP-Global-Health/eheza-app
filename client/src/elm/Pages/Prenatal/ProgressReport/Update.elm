module  Pages.Prenatal.ProgressReport.Update exposing (update)

import App.Model
import  Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetLabResultsMode mode ->
            ( { model | labResultsMode = mode }, Cmd.none, [] )
