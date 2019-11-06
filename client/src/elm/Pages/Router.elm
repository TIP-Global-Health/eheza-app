module Pages.Router exposing (delta2url, parseUrl)

import Activity.Model exposing (Activity)
import Activity.Utils
import Pages.Page exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)
import PrenatalActivity.Utils
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
                ClinicalPage ->
                    Just <| UrlChange NewEntry "#clinical"

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

                PersonsPage related ->
                    let
                        url =
                            case related of
                                Nothing ->
                                    "#persons"

                                Just relatedId ->
                                    "#relations/" ++ fromEntityUuid relatedId
                    in
                    Just <| UrlChange NewEntry url

                PrenatalParticipantPage id ->
                    Just <| UrlChange NewEntry <| "#prenatal-participant/" ++ fromEntityUuid id

                PrenatalParticipantsPage ->
                    Just <| UrlChange NewEntry "#prenatal-participants"

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
                                    "/activity/" ++ Activity.Utils.encodeActivityAsString activity

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

                PrenatalEncounterPage id ->
                    Just <| UrlChange NewEntry <| "#prenatal-encounter/" ++ fromEntityUuid id

                PrenatalActivityPage id activity ->
                    Just <| UrlChange NewEntry <| "#prenatal-activity/" ++ fromEntityUuid id ++ "/" ++ PrenatalActivity.Utils.encodeActivityAsString activity


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
        , map DevicePage (s "device")
        , map PinCodePage (s "pincode")
        , map ServiceWorkerPage (s "deployment")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (UserPage ClinicalPage) (s "clinical")
        , map (\id page -> UserPage <| SessionPage id page) (s "session" </> parseUuid </> parseSessionPage)
        , map (UserPage <| PersonsPage Nothing) (s "persons")
        , map (\id -> UserPage <| PersonsPage (Just id)) (s "relations" </> parseUuid)
        , map (\id -> UserPage <| CreatePersonPage (Just id)) (s "person" </> s "new" </> parseUuid)
        , map (UserPage <| CreatePersonPage Nothing) (s "person" </> s "new")
        , map (\id -> UserPage <| PersonPage id) (s "person" </> parseUuid)
        , map (UserPage PrenatalParticipantsPage) (s "prenatal-participants")
        , map (\id -> UserPage <| PrenatalParticipantPage id) (s "prenatal-participant" </> parseUuid)
        , map (\id1 id2 -> UserPage <| RelationshipPage id1 id2) (s "relationship" </> parseUuid </> parseUuid)
        , map (\id -> UserPage <| PrenatalEncounterPage id) (s "prenatal-encounter" </> parseUuid)
        , map (\id activity -> UserPage <| PrenatalActivityPage id activity) (s "prenatal-activity" </> parseUuid </> parsePrenatalActivity)

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
            case Activity.Utils.decodeActivityFromString part of
                Just activity ->
                    Ok activity

                Nothing ->
                    Err <| part ++ " is not an Activity"


parsePrenatalActivity : Parser (PrenatalActivity -> c) c
parsePrenatalActivity =
    custom "PrenatalActivity" <|
        \part ->
            case PrenatalActivity.Utils.decodeActivityFromString part of
                Just activity ->
                    Ok activity

                Nothing ->
                    Err <| part ++ " is not an PrenatalActivity"
