module Pages.Dashboard.Update exposing (update)

import App.Model
import Pages.Dashboard.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )
