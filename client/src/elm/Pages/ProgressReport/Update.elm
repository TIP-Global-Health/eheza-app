module Pages.ProgressReport.Update exposing (update)

import Backend.Model
import Pages.Page exposing (Page(..))
import Pages.ProgressReport.Model exposing (..)
import Pages.Session.Model


update : Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model, Cmd.none, [ Pages.Session.Model.SetActivePage page ] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )
