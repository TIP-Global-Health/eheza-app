module App.Router exposing (delta2url, location2messages)

import App.Model exposing (..)
import Maybe.Extra
import Navigation exposing (Location)
import Pages.Router
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing (Parser, map, parseHash, s, oneOf, (</>), int, string, top)


{-| For now, we just pass the current `activePage` and previous `activePage`
down to `Pages.Router` ... if we wanted to route based on additional
information, we could pass that as well.
-}
delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    Pages.Router.delta2url previous.activePage current.activePage


{-| For now, Pages.Router.parseUrl just returns an `ActivePage`, so we
map that to a message. We could return other things from `parseUrl` if
necessary (such as messages themselves).
-}
location2messages : Location -> List Msg
location2messages location =
    UrlParser.parseHash Pages.Router.parseUrl location
        |> Maybe.map SetActivePage
        |> Maybe.Extra.toList
