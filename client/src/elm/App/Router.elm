module App.Router exposing (delta2url, location2messages)

import App.Model exposing (..)
import App.PageType exposing (..)
import Navigation exposing (Location)
import PatientManager.Update
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing (Parser, map, parseHash, s, oneOf, (</>), int, string)


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    case current.activePage of
        AccessDenied ->
            Nothing

        Activities ->
            Just <| UrlChange NewEntry "#activities"

        Login ->
            Just <| UrlChange NewEntry "#login"

        MyAccount ->
            Just <| UrlChange NewEntry "#my-account"

        PageNotFound ->
            Just <| UrlChange NewEntry "#404"

        Patient id ->
            Just <| UrlChange NewEntry ("#patient/" ++ id)

        Dashboard _ ->
            let
                fragment =
                    PatientManager.Update.dashboardUrlFragment current.pagePatient

                url =
                    if fragment == "" then
                        -- Hack to allow dashboard to change the URL.
                        "# "
                    else
                        "#/" ++ fragment
            in
                Just <| UrlChange NewEntry url


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
        [ map (SetActivePage <| Dashboard []) (s "")
        , map (SetActivePage Activities) (s "activities")
        , map (\id -> SetActivePage <| Patient (toString id)) (s "patient" </> int)
        , map (SetActivePage Login) (s "login")
        , map (SetActivePage MyAccount) (s "my-account")
        ]
