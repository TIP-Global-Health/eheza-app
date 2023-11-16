module Main exposing (main)

import App.Fetch
import App.Model exposing (Flags, Model, Msg)
import App.Update
import App.View
import Browser
import Update.Fetch


main : Program Flags Model Msg
main =
    Browser.element
        { init = App.Update.init
        , update = Update.Fetch.andThenFetch App.Fetch.fetch App.Update.update
        , view = App.View.view
        , subscriptions = App.Update.subscriptions
        }
