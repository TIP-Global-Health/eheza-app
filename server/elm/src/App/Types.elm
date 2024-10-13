module App.Types exposing
    ( Language(..)
    , Page(..)
    , Site(..)
    )


type Page
    = CompletionMenu
    | Completion
    | ReportsMenu
    | Reports
    | ScoreboardMenu
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
