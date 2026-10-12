module Components.ReportToWhatsAppDialog.Test exposing (all)

import Components.ReportToWhatsAppDialog.Model exposing (CountryCode(..), DialogState(..), Msg(..), ReportType(..))
import Components.ReportToWhatsAppDialog.View exposing (view)
import Expect
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes)
import TestFixtures
import Translate.Model exposing (Language(..))


{-| Click a dialog button and expect the given next state.
-}
expectNextState : Site -> DialogState -> List String -> DialogState -> Expect.Expectation
expectNextState site state buttonClasses nextState =
    view English site ( toEntityUuid "person", TestFixtures.testPerson ) ReportAntenatal Nothing { state = Just state }
        |> Query.fromHtml
        |> Query.find [ classes buttonClasses ]
        |> Event.simulate Event.click
        |> Event.expect (SetState (Just nextState))


all : Test
all =
    let
        phoneInput countryCode =
            PhoneInput { countryCode = countryCode, phone = "" }

        consentYes site countryCode =
            expectNextState site Consent [ "primary", "button" ] (phoneInput countryCode)

        verificationNo site countryCode =
            expectNextState site (PhoneVerification "0788000000") [ "velvet", "button" ] (phoneInput countryCode)

        continueWith site countryCode phone storedNumber =
            expectNextState site
                (PhoneInput { countryCode = countryCode, phone = phone })
                [ "primary", "button" ]
                (PhoneUpdateAtProfile storedNumber)
    in
    describe "Report to WhatsApp"
        [ describe "a new number starts with the country code of the site"
            [ test "Rwanda: consent with no number on the profile" <|
                \_ -> consentYes SiteRwanda CountryCodeRwanda
            , test "Burundi: consent with no number on the profile" <|
                \_ -> consentYes SiteBurundi CountryCodeBurundi
            , test "Somalia: consent with no number on the profile" <|
                \_ -> consentYes SiteSomalia CountryCodeSomalia
            , test "Rwanda: declining the number on the profile" <|
                \_ -> verificationNo SiteRwanda CountryCodeRwanda
            , test "Burundi: declining the number on the profile" <|
                \_ -> verificationNo SiteBurundi CountryCodeBurundi
            , test "Somalia: declining the number on the profile" <|
                \_ -> verificationNo SiteSomalia CountryCodeSomalia
            ]
        , describe "a number of the site's own country is kept the way that site's profiles store it"
            [ test "Rwanda: with a leading 0" <|
                \_ -> continueWith SiteRwanda CountryCodeRwanda "788123456" "0788123456"
            , test "Burundi: 8 digits, no leading 0" <|
                \_ -> continueWith SiteBurundi CountryCodeBurundi "79123456" "79123456"
            , test "Burundi: a typed leading 0 is dropped" <|
                \_ -> continueWith SiteBurundi CountryCodeBurundi "079123456" "79123456"
            , test "Somalia: 9 digits, no leading 0" <|
                \_ -> continueWith SiteSomalia CountryCodeSomalia "612345678" "612345678"
            , test "Burundi: a number of another country keeps its country code" <|
                \_ -> continueWith SiteBurundi CountryCodeRwanda "0788123456" "+250788123456"
            ]
        ]
