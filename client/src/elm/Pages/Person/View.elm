module Pages.Person.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Html exposing (..)
import Translate exposing (Language)


view : Language -> PersonId -> ModelIndexedDb -> Html Msg
view language id db =
    div []
        [ text "TODO" ]
