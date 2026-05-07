module App.Types exposing
    ( Language(..)
    , Page(..)
    , Site(..)
    , SiteFeature(..)
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
    | Somali


type Site
    = SiteRwanda
    | SiteBurundi
    | SiteSomalia
    | SiteUnknown


{-| Toggleable site features. Mirrors `SiteFeature` in
client/src/elm/SyncManager/Model.elm and the canonical list in
hedley\_admin\_get\_available\_features (PHP). Update all together.
-}
type SiteFeature
    = FeatureFamilyNutrition
    | FeatureGPSCoordinates
    | FeatureGroupEducation
    | FeatureHealthyStart
    | FeatureHIVManagement
    | FeatureNCDA
    | FeatureReportToWhatsApp
    | FeatureStockManagementHC
    | FeatureStockManagementVillage
    | FeatureTuberculosisManagement
