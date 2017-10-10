module App.PageType exposing (DashboardPage(..), Page(..))

import Activity.Model exposing (ActivityType)


{-| Prevent circular dependency.
-}
type alias ParticipantId =
    Int


type Page
    = AccessDenied
    | Activities
    | Activity (Maybe ActivityType)
    | Dashboard (List ActivityType)
    | Login
    | MyAccount
    | OfflineSession
    | OpenSessions
    | PageNotFound
    | Participant ParticipantId


type DashboardPage
    = ActivitiesDashboard
    | ParticipantsDashboard
