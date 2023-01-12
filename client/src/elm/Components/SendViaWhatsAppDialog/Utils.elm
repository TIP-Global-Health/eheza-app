module Components.SendViaWhatsAppDialog.Utils exposing (..)

import Components.SendViaWhatsAppDialog.Model exposing (..)
import EverySet exposing (EverySet)


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportWellChild ->
            "well-child"

        ReportAntenatal ->
            "antenatal"

        ReportAcuteIllness ->
            "acute-illness"

        ReportNCD ->
            "ncd"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "well-child" ->
            Just ReportWellChild

        "antenatal" ->
            Just ReportAntenatal

        "acute-illness" ->
            Just ReportAcuteIllness

        "ncd" ->
            Just ReportNCD

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
