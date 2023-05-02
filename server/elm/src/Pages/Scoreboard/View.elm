module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , classList
        , href
        , id
        , src
        , style
        )
import Html.Events exposing (onClick)
import Icons
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (Model, Msg(..))
import Pages.Scoreboard.Utils exposing (..)


view : Language -> Model -> Html Msg
view language model =
    text "This is ELM App!"
