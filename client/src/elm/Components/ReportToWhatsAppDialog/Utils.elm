module Components.ReportToWhatsAppDialog.Utils exposing (..)

import Components.ReportToWhatsAppDialog.Model exposing (..)
import EverySet exposing (EverySet)
import SyncManager.Model exposing (Site(..))


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportAcuteIllness ->
            "acute-illness"

        ReportAntenatal ->
            "antenatal"

        ReportNCD ->
            "ncd"

        ReportTuberculosis ->
            "tuberculosis"

        ReportWellChild ->
            "well-child"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "acute-illness" ->
            Just ReportAcuteIllness

        "antenatal" ->
            Just ReportAntenatal

        "ncd" ->
            Just ReportNCD

        "tuberculosis" ->
            Just ReportTuberculosis

        "well-child" ->
            Just ReportWellChild

        _ ->
            Nothing


countryCodeFromString : String -> Maybe CountryCode
countryCodeFromString code =
    case code of
        "250" ->
            Just CountryCodeRwanda

        "256" ->
            Just CountryCodeUganda

        "243" ->
            Just CountryCodeCongo

        "254" ->
            Just CountryCodeKenya

        "255" ->
            Just CountryCodeTanzania

        "257" ->
            Just CountryCodeBurundi

        "1" ->
            Just CountryCodeUSACanada

        "972" ->
            Just CountryCodeIsrael

        "252" ->
            Just CountryCodeSomalia

        _ ->
            Nothing


countryCodeToString : CountryCode -> String
countryCodeToString code =
    case code of
        CountryCodeRwanda ->
            "250"

        CountryCodeUganda ->
            "256"

        CountryCodeCongo ->
            "243"

        CountryCodeKenya ->
            "254"

        CountryCodeTanzania ->
            "255"

        CountryCodeBurundi ->
            "257"

        CountryCodeUSACanada ->
            "1"

        CountryCodeIsrael ->
            "972"

        CountryCodeSomalia ->
            "252"


siteToCountryCode : Site -> CountryCode
siteToCountryCode site =
    case site of
        SiteRwanda ->
            CountryCodeRwanda

        SiteBurundi ->
            CountryCodeBurundi

        SiteSomalia ->
            CountryCodeSomalia

        -- We should never get here, as the should always
        -- be info of the site at which device operate.
        SiteUnknown ->
            CountryCodeRwanda


trimLeadingZeros : String -> String
trimLeadingZeros number =
    if String.startsWith "0" number then
        String.dropLeft 1 number
            |> trimLeadingZeros

    else
        number


showComponent : Maybe (EverySet c) -> c -> Bool
showComponent components component =
    -- Show component if it was selected to be shared via WhatsApp,
    -- or, if viewing not for sharing via WhatsApp.
    Maybe.map (EverySet.member component) components
        |> Maybe.withDefault True


allCountryCodes : List CountryCode
allCountryCodes =
    [ CountryCodeRwanda
    , CountryCodeUganda
    , CountryCodeCongo
    , CountryCodeKenya
    , CountryCodeTanzania
    , CountryCodeBurundi
    , CountryCodeUSACanada
    , CountryCodeSomalia
    ]


minimalNumberLength : Int
minimalNumberLength =
    6
