module App.Types exposing
    ( Language(..)
    , Page(..)
    , Site(..)
    )


type Page
    = Menu
    | Scoreboard
    | NotFound


type Language
    = English
    | Kinyarwanda
    | Kirundi


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteUnknown
