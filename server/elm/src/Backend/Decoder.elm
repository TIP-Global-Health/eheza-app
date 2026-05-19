module Backend.Decoder exposing (decodeSite, decodeSiteFeatures, decodeWithFallback)

import App.Types exposing (Site(..), SiteFeature(..))
import EverySet exposing (EverySet)
import Json.Decode exposing (Decoder, oneOf, string, succeed)
import Maybe.Extra


decodeSite : Decoder Site
decodeSite =
    string
        |> Json.Decode.map siteFromString


siteFromString : String -> Site
siteFromString str =
    case String.toLower str of
        "rwanda" ->
            SiteRwanda

        "burundi" ->
            SiteBurundi

        "somalia" ->
            SiteSomalia

        _ ->
            SiteUnknown


{-| Decodes the space-delimited `features` string emitted by
hedley\_admin\_get\_enabled\_features\_string into the set of currently
enabled features. Unknown identifiers are silently dropped so adding a new
flag on the server never breaks an older admin app build.
-}
decodeSiteFeatures : Decoder (EverySet SiteFeature)
decodeSiteFeatures =
    string
        |> Json.Decode.map siteFeaturesFromString


siteFeaturesFromString : String -> EverySet SiteFeature
siteFeaturesFromString str =
    String.words str
        |> List.map siteFeatureFromString
        |> Maybe.Extra.values
        |> EverySet.fromList


siteFeatureFromString : String -> Maybe SiteFeature
siteFeatureFromString str =
    case String.toLower str of
        "family_nutrition" ->
            Just FeatureFamilyNutrition

        "gps_coordinates" ->
            Just FeatureGPSCoordinates

        "group_education" ->
            Just FeatureGroupEducation

        "healthy_start" ->
            Just FeatureHealthyStart

        "hiv_management" ->
            Just FeatureHIVManagement

        "ncda" ->
            Just FeatureNCDA

        "report_to_whatsapp" ->
            Just FeatureReportToWhatsApp

        "stock_management_hc" ->
            Just FeatureStockManagementHC

        "stock_management_village" ->
            Just FeatureStockManagementVillage

        "tuberculosis_management" ->
            Just FeatureTuberculosisManagement

        _ ->
            Nothing


decodeWithFallback : a -> Decoder a -> Decoder a
decodeWithFallback fallback decoder =
    oneOf [ decoder, succeed fallback ]
