module Pages.Router exposing (delta2url, parseUrl)

import Activity.Utils exposing (encodeActivityTypeAsString, decodeActivityTypeFromString, defaultActivityType)
import Pages.Page exposing (..)
import Restful.Endpoint exposing (toEntityId, fromEntityId)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing (Parser, map, parseHash, s, oneOf, (</>), int, string, top)


{-| For now, we're given just the previous and current page ...if
we need any additional information for routing at some point, the
caller could provide it.
-}
delta2url : Page -> Page -> Maybe UrlChange
delta2url previous current =
    case current of
        LoginPage ->
            Just <| UrlChange NewEntry "#login"

        PageNotFound url ->
            -- If we couldn't interpret the URL, we don't try to change it.
            Nothing

        -- These are pages which depend on having a downloaded session
        SessionPage sessionPage ->
            case sessionPage of
                ActivitiesPage ->
                    Just <| UrlChange NewEntry "#activities"

                ActivityPage activityType ->
                    Just <| UrlChange NewEntry ("#activity/" ++ encodeActivityTypeAsString activityType)

                AttendancePage ->
                    Just <| UrlChange NewEntry "#attendance"

                ChildPage id ->
                    Just <| UrlChange NewEntry ("#child/" ++ toString (fromEntityId id))

                MotherPage id ->
                    Just <| UrlChange NewEntry ("#mother/" ++ toString (fromEntityId id))

                ParticipantsPage ->
                    Just <| UrlChange NewEntry "#participants"

                ProgressReportPage id ->
                    Just <| UrlChange NewEntry ("#progress/" ++ toString (fromEntityId id))

        -- These are pages that required a logged-in user
        UserPage userPage ->
            case userPage of
                AdminPage ->
                    Just <| UrlChange NewEntry "#admin"

                ClinicsPage clinicId ->
                    let
                        clinic =
                            clinicId
                                |> Maybe.map (\id -> "/" ++ toString (fromEntityId (id)))
                                |> Maybe.withDefault ""
                    in
                        Just <| UrlChange NewEntry ("#clinics" ++ clinic)

                MyAccountPage ->
                    Just <| UrlChange NewEntry "#my-account"


{-| For now, the only messages we're generating from the URL are messages
to set the active page. So, we just return a `Page`, and the caller can
map it to a msg. If we eventually needed to send different kinds of messages,
we could change that here.
-}
parseUrl : Parser (Page -> c) c
parseUrl =
    oneOf
        [ map (SessionPage ActivitiesPage) (s "activities")

        -- TODO: Should probably fail with an unrecongized activity type,
        -- rather than use the default
        , map
            (SessionPage << ActivityPage << Maybe.withDefault defaultActivityType << decodeActivityTypeFromString)
            (s "activity" </> string)
        , map (SessionPage AttendancePage) (s "attendance")
        , map (SessionPage << ChildPage << toEntityId) (s "child" </> int)
        , map (SessionPage << ProgressReportPage << toEntityId) (s "progress" </> int)
        , map (UserPage << ClinicsPage << Just << toEntityId) (s "clinics" </> int)
        , map (UserPage (ClinicsPage Nothing)) (s "clinics")
        , map (UserPage AdminPage) (s "admin")
        , map LoginPage (s "login")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (SessionPage << MotherPage << toEntityId) (s "mother" </> int)
        , map (SessionPage ParticipantsPage) (s "participants")

        -- TODO: `top` represents the page without any segements ... i.e. the
        -- root page.  Should figure out how to handle this best. Possibly a
        -- special Page called `Root`?  Or, we could eventually redirect to an
        -- appropriate page. But it might be simpler to record the user's
        -- intention to be at the "root" page.
        , map LoginPage top
        ]
