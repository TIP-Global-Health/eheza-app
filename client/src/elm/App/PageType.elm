module App.PageType exposing (Page(..))

{-| Prevent circular dependency.
-}


type alias PatientId =
    String


type Page
    = AccessDenied
    | Activities
    | Dashboard
    | Patient PatientId
    | Login
    | MyAccount
    | PageNotFound
