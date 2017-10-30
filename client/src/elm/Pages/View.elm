module Pages.View exposing (..)

import Backend.Session.Model exposing (EditableSession)
import Date exposing (Date)
import Html exposing (..)
import Pages.Model exposing (..)
import Pages.Activity.View
import Translate exposing (Language)


view : Language -> Date -> EditableSession -> Model -> Html Msg
view language currentDate session model =
    case model.userAttention of
        ActivityPage ->
            Pages.Activity.View.view language currentDate session model.activityPage
                |> Html.map MsgActivity
