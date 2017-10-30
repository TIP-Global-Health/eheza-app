module Pages.Update exposing (..)

import Pages.Model exposing (..)
import Pages.Activity.Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgActivity subMsg ->
            let
                ( subModel, subCmd ) =
                    Pages.Activity.Update.update subMsg model.activityPage
            in
                ( { model | activityPage = subModel }
                , Cmd.map MsgActivity subCmd
                )

        SetUserAttention val ->
            ( { model | userAttention = val }
            , Cmd.none
            )
