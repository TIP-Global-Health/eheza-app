module App.PageType exposing (Page(..), authenticatedPages)

{-| Prevent circular dependency.
-}


type alias ItemId =
    String


type Page
    = AccessDenied
    | Activities
    | Dashboard
    | Item ItemId
    | Login
    | MyAccount
    | PageNotFound


authenticatedPages : List Page
authenticatedPages =
    [ Activities
    , Dashboard
    , MyAccount
    ]
