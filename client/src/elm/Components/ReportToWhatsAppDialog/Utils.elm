module Components.ReportToWhatsAppDialog.Utils exposing (allCountryCodes, countryCodeFromString, countryCodeToString, emptyPhoneData, reportTypeFromString, reportTypeToString, showComponent, siteToCountryCode, toInternationalNumber, toLocalPhoneNumber, toWhatsAppNumber)

import Components.ReportToWhatsAppDialog.Model exposing (CountryCode(..), PhoneData, ReportType(..))
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


{-| A new number starts with the country code of the site.
-}
emptyPhoneData : Site -> PhoneData
emptyPhoneData site =
    { countryCode = siteToCountryCode site
    , phone = ""
    }


{-| A number of the site's own country, written the way that site's profiles
store it: Rwanda with a leading 0 (0788123456), Burundi and Somalia without.
-}
toLocalPhoneNumber : Site -> String -> String
toLocalPhoneNumber site phone =
    case site of
        SiteBurundi ->
            trimLeadingZeros phone

        SiteRwanda ->
            "0" ++ trimLeadingZeros phone

        SiteSomalia ->
            trimLeadingZeros phone

        -- An unknown site uses the Rwanda country code.
        SiteUnknown ->
            "0" ++ trimLeadingZeros phone


{-| A number as WhatsApp needs it, or Nothing when it is not valid. A number
without "+" belongs to the site's country: at Rwanda, 0788123456 gives +250788123456.
-}
toWhatsAppNumber : Site -> String -> Maybe String
toWhatsAppNumber site number =
    if String.startsWith "+" number then
        let
            digits =
                String.dropLeft 1 number

            knownCountry =
                List.range 1 3
                    |> List.filterMap
                        (\prefixLength ->
                            countryCodeFromString (String.left prefixLength digits)
                                |> Maybe.map (\code -> ( code, String.dropLeft prefixLength digits ))
                        )
                    |> List.head
        in
        case knownCountry of
            Just ( code, national ) ->
                toInternationalNumber code national

            Nothing ->
                -- A country with no known number length needs at least 6 digits.
                if String.all Char.isDigit digits && String.length (trimLeadingZeros digits) >= minimalNumberLength then
                    Just number

                else
                    Nothing

    else
        toInternationalNumber (siteToCountryCode site) number


{-| The number with its country code, or Nothing when it does not have that
country's number of digits once leading zeros are dropped.
-}
toInternationalNumber : CountryCode -> String -> Maybe String
toInternationalNumber code number =
    let
        national =
            trimLeadingZeros number
    in
    if String.all Char.isDigit national && String.length national == nationalNumberLength code then
        Just <| "+" ++ countryCodeToString code ++ national

    else
        Nothing


{-| Digits in a mobile number after the country code: Rwanda 788123456.
-}
nationalNumberLength : CountryCode -> Int
nationalNumberLength code =
    case code of
        CountryCodeBurundi ->
            8

        CountryCodeCongo ->
            9

        CountryCodeIsrael ->
            9

        CountryCodeKenya ->
            9

        CountryCodeRwanda ->
            9

        CountryCodeSomalia ->
            9

        CountryCodeTanzania ->
            9

        CountryCodeUganda ->
            9

        CountryCodeUSACanada ->
            10


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
