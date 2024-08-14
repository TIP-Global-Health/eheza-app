module App.Types exposing
    ( Language(..)
    , Page(..)
    , Site(..)
    )


type Page
    = ScoreboardMenu
    | Scoreboard
    | ReportsMenu
    | Reports
    | NotFound


type Language
    = English
    | Kinyarwanda
    | Kirundi


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteUnknown
