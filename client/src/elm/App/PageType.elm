module App.PageType exposing (Page(..), authenticatedPages)

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


authenticatedPages : List Page
authenticatedPages =
    [ Activities
    , Dashboard
    , MyAccount
    ]
