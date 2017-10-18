module App.PageType exposing (DashboardPage(..), Page(..))

import Activity.Model exposing (ActivityType)
import Backend.Entities exposing (..)


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
    | PageChild ChildId
    | PageMother MotherId


type DashboardPage
    = ActivitiesDashboard
    | ParticipantsDashboard
