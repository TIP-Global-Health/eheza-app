module App.Types exposing
    ( Language(..)
    , Page(..)
    , Site(..)
    )


type Page
    = ScoreboardMenu
    | Scoreboard
    | ReportsMenu
      -- @todo
      -- | Reports
    | NotFound


type Language
    = English
    | Kinyarwanda
    | Kirundi


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteUnknown
