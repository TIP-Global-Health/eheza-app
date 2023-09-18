module Main exposing (main)

import App.Model
import App.Update
import App.View
import Browser


main =
    Browser.application
        { init = App.Update.init
        , update = App.Update.updateAndThenFetch
        , view = App.View.view
        , subscriptions = App.Update.subscriptions
        , onUrlRequest = App.Model.UrlRequested
        , onUrlChange = App.Model.UrlChanged
        }
