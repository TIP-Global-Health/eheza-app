module App.Router exposing (delta2url, location2messages)

import App.Model exposing (..)
import App.PageType exposing (..)
import Navigation exposing (Location)
import Restful.Endpoint exposing (toEntityId, fromEntityId)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing (Parser, map, parseHash, s, oneOf, (</>), int, string, top)


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    case current.activePage of
        AccessDenied ->
            Nothing

        Activities ->
            Just <| UrlChange NewEntry "#activities"

        Activity _ ->
            Just <| UrlChange NewEntry "#activity"

        Login ->
            Just <| UrlChange NewEntry "#login"

        MyAccount ->
            Just <| UrlChange NewEntry "#my-account"

        OfflineSession ->
            Just <| UrlChange NewEntry "#offline-session"

        OpenSessions ->
            Just <| UrlChange NewEntry "#open-sessions"

        PageNotFound ->
            Just <| UrlChange NewEntry "#404"

        PageChild id ->
            Just <| UrlChange NewEntry ("#child/" ++ toString (fromEntityId id))

        PageMother id ->
            Just <| UrlChange NewEntry ("#mother/" ++ toString (fromEntityId id))

        Dashboard _ ->
            Just <| UrlChange NewEntry "#dashboard"


location2messages : Location -> List Msg
location2messages location =
    case UrlParser.parseHash parseUrl location of
        Just msgs ->
            [ msgs ]

        Nothing ->
            []


parseUrl : Parser (Msg -> c) c
parseUrl =
    oneOf
        [ map (SetActivePage <| Dashboard []) (s "dashboard")
        , map (SetActivePage Activities) (s "activities")
        , map (SetActivePage <| Activity Nothing) (s "activity")
        , map (\id -> SetActivePage <| PageChild (toEntityId id)) (s "child" </> int)
        , map (\id -> SetActivePage <| PageMother (toEntityId id)) (s "mother" </> int)
        , map (SetActivePage Login) (s "login")
        , map (SetActivePage OfflineSession) (s "offline-session")
        , map (SetActivePage OpenSessions) (s "open-sessions")
        , map (SetActivePage MyAccount) (s "my-account")
        , map (SetActivePage OpenSessions) top
        ]
