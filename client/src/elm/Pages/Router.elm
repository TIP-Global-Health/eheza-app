module Pages.Router exposing (delta2url, parseUrl)

import Activity.Utils exposing (decodeActivityTypeFromString, defaultActivityType, encodeActivityTypeAsString)
import Pages.Page exposing (..)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import UrlParser exposing ((</>), Parser, int, map, oneOf, parseHash, s, string, top)


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

                SessionPage sessionPage ->
                    case sessionPage of
                        ActivitiesPage ->
                            Just <| UrlChange NewEntry "#activities"

                        ActivityPage activityType ->
                            Just <| UrlChange NewEntry ("#activity/" ++ encodeActivityTypeAsString activityType)

                        AttendancePage ->
                            Just <| UrlChange NewEntry "#attendance"

                        ChildPage id ->
                            Just <| UrlChange NewEntry ("#child/" ++ fromEntityUuid id)

                        MotherPage id ->
                            Just <| UrlChange NewEntry ("#mother/" ++ fromEntityUuid id)

                        ParticipantsPage ->
                            Just <| UrlChange NewEntry "#participants"

                        ProgressReportPage id ->
                            Just <| UrlChange NewEntry ("#progress/" ++ fromEntityUuid id)


{-| For now, the only messages we're generating from the URL are messages
to set the active page. So, we just return a `Page`, and the caller can
map it to a msg. If we eventually needed to send different kinds of messages,
we could change that here.
-}
parseUrl : Parser (Page -> c) c
parseUrl =
    oneOf
        [ map (UserPage <| SessionPage ActivitiesPage) (s "activities")

        -- TODO: Should probably fail with an unrecongized activity type,
        -- rather than use the default
        , map
            (UserPage << SessionPage << ActivityPage << Maybe.withDefault defaultActivityType << decodeActivityTypeFromString)
            (s "activity" </> string)
        , map (UserPage <| SessionPage AttendancePage) (s "attendance")
        , map (UserPage << SessionPage << ChildPage << toEntityUuid) (s "child" </> string)
        , map (UserPage << SessionPage << ProgressReportPage << toEntityUuid) (s "progress" </> string)
        , map (UserPage << ClinicsPage << Just << toEntityUuid) (s "clinics" </> string)
        , map (UserPage (ClinicsPage Nothing)) (s "clinics")
        , map (UserPage AdminPage) (s "admin")
        , map DevicePage (s "device")
        , map PinCodePage (s "pincode")
        , map ServiceWorkerPage (s "deployment")
        , map (UserPage MyAccountPage) (s "my-account")
        , map (UserPage << SessionPage << MotherPage << toEntityUuid) (s "mother" </> string)
        , map (UserPage <| SessionPage ParticipantsPage) (s "participants")

        -- `top` represents the page without any segements ... i.e. the
        -- root page.
        , map PinCodePage top
        ]
