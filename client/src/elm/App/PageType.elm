module App.PageType exposing (Page(..))

import Activity.Model exposing (ActivityType)


{-| Prevent circular dependency.
-}
type alias PatientId =
    String


type Page
    = AccessDenied
    | Activities
    | Dashboard (List ActivityType)
    | Patient PatientId
    | Login
    | MyAccount
    | PageNotFound
