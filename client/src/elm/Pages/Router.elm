module Pages.Router exposing (delta2url, parseUrl)

import Activity.Model exposing (Activity)
import Activity.Utils exposing (decodeActivityFromString, defaultActivity, encodeActivityAsString)
import Http exposing (encodeUri)
import Pages.Page exposing (..)
import Restful.Endpoint exposing (EntityUuid, fromEntityUuid, toEntityUuid)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing ((</>), Parser, custom, int, map, oneOf, parseHash, s, string, top)


{-| For now, we're given just the previous and current page ...if
we need any additional information for routing at some point, the
caller could provide it.
-}
delta2url : Page -> Page -> Maybe UrlChange
delta2url previous current =
    case current of
        DevicePage ->
            Just <| UrlChange NewEntry "#device"

        PinCodePage ->
            Just <| UrlChange NewEntry "#pincode"

        PageNotFound url ->
            -- If we couldn't interpret the URL, we don't try to change it.
            Nothing

        ServiceWorkerPage ->
            Just <| UrlChange NewEntry "#deployment"

        -- These are pages that required a logged-in user
        UserPage userPage ->
            case userPage of
                AdminPage ->
                    Just <| UrlChange NewEntry "#admin"

                ClinicsPage clinicId ->
                    let
                        clinic =
                            clinicId
                                |> Maybe.map (\id -> "/" ++ fromEntityUuid id)
                                |> Maybe.withDefault ""
                    in
                    Just <| UrlChange NewEntry ("#clinics" ++ clinic)

                MyAccountPage ->
                    Just <| UrlChange NewEntry "#my-account"

                ParticipantRegistrationPage ->
                    Just <| UrlChange NewEntry "#participant-registration"

                CreatePersonPage relationId ->
                    let
                        relation =
                            relationId
                                |> Maybe.map (\id -> "/" ++ fromEntityUuid id)
                                |> Maybe.withDefault ""
                    in
                    Just <| UrlChange NewEntry ("#person/new" ++ relation)

                PersonPage id ->
                    Just <| UrlChange NewEntry <| "#person/" ++ fromEntityUuid id

                PersonsPage search related ->
                    let
                        change =
                            -- If we're typing the search string, we just
                            -- modify the entry.
                            case previous of
                                UserPage (PersonsPage _ previousRelated) ->
                                    if related == previousRelated then
                                        ModifyEntry

                                    else
                                        NewEntry

                                _ ->
                                    NewEntry

                        encodedSearch =
                            search
                                |> Maybe.map (\s -> "/" ++ encodeUri s)
                                |> Maybe.withDefault ""

                        url =
                            case related of
                                Nothing ->
                                    "#persons" ++ encodedSearch

                                Just relatedId ->
                                    "#relations/" ++ fromEntityUuid relatedId ++ encodedSearch
                    in
                    Just <| UrlChange change url

                RelationshipPage id1 id2 ->
                    Just <|
                        UrlChange NewEntry <|
                            "#relationship/"
                                ++ fromEntityUuid id1
                                ++ "/"
                                ++ fromEntityUuid id2

                SessionPage sessionId sessionPage ->
                    let
                        subUrl =
                            case sessionPage of
                                ActivitiesPage ->
                                    "/activities"

                                ActivityPage activity ->
                                    "/activity/" ++ encodeActivityAsString activity

                                AttendancePage ->
                                    ""

                                ChildPage id ->
                                    "/child/" ++ fromEntityUuid id

                                MotherPage id ->
                                    "/mother/" ++ fromEntityUuid id

                                ParticipantsPage ->
                                    "/participants"

                                ProgressReportPage id ->
                                    "/progress/" ++ fromEntityUuid id

                        url =
                            "#session/" ++ fromEntityUuid sessionId ++ subUrl
                    in
                    Just <| UrlChange NewEntry url


{-| For now, the only messages we're generating from the URL are messages
to set the active page. So, we just return a `Page`, and the caller can
map it to a msg. If we eventually needed to send different kinds of messages,
we could change that here.
-}
parseUrl : Parser (Page -> c) c
parseUrl =
    oneOf
        [ map (UserPage << ClinicsPage << Just) (s "clinics" </> parseUuid)
        , map (UserPage (ClinicsPage Nothing)) (s "clinics")
        , map (UserPage AdminPage) (s "admin")
        , map DevicePage (s "device")
        , map PinCodePage (s "pincode")
        , map ServiceWorkerPage (s "deployment")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (UserPage ParticipantRegistrationPage) (s "participant-registration")
        , map (\id page -> UserPage <| SessionPage id page) (s "session" </> parseUuid </> parseSessionPage)
        , map (UserPage <| PersonsPage Nothing Nothing) (s "persons")
        , map (\search -> UserPage <| PersonsPage (Just search) Nothing) (s "persons" </> string)
        , map (\id -> UserPage <| PersonsPage Nothing (Just id)) (s "relations" </> parseUuid)
        , map (\id search -> UserPage <| PersonsPage (Just search) (Just id)) (s "relations" </> parseUuid </> string)
        , map (\id -> UserPage <| CreatePersonPage (Just id)) (s "person" </> s "new" </> parseUuid)
        , map (UserPage <| CreatePersonPage Nothing) (s "person" </> s "new")
        , map (\id -> UserPage <| PersonPage id) (s "person" </> parseUuid)
        , map (\id1 id2 -> UserPage <| RelationshipPage id1 id2) (s "relationship" </> parseUuid </> parseUuid)

        -- `top` represents the page without any segements ... i.e. the
        -- root page.
        , map PinCodePage top
        ]


parseSessionPage : Parser (SessionPage -> c) c
parseSessionPage =
    oneOf
        [ map ActivitiesPage (s "activities")
        , map ActivityPage (s "activity" </> parseActivity)
        , map ChildPage (s "child" </> parseUuid)
        , map ProgressReportPage (s "progress" </> parseUuid)
        , map MotherPage (s "mother" </> parseUuid)
        , map ParticipantsPage (s "participants")
        , map AttendancePage top
        ]


parseUuid : Parser (EntityUuid a -> c) c
parseUuid =
    map toEntityUuid string


parseActivity : Parser (Activity -> c) c
parseActivity =
    custom "Activity" <|
        \part ->
            case decodeActivityFromString part of
                Just activity ->
                    Ok activity

                Nothing ->
                    Err <| part ++ " is not an Activity"
