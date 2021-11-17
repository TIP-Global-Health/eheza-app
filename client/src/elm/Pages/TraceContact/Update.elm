module Pages.TraceContact.Update exposing (update)

import App.Model
import Backend.Model
import Pages.TraceContact.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
