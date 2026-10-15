module Components.ReportToWhatsAppDialog.Test exposing (all)

import Backend.Person.Model exposing (Person)
import Components.ReportToWhatsAppDialog.Model exposing (CountryCode(..), DialogState(..), Msg(..), ReportType(..))
import Components.ReportToWhatsAppDialog.Utils exposing (toWhatsAppNumber)
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


{-| The dialog button with the given classes, for a person and a dialog state.
-}
findButton : Site -> Person -> DialogState -> List String -> Query.Single (Msg msg)
findButton site person state buttonClasses =
    view English site ( toEntityUuid "person", person ) ReportAntenatal Nothing { state = Just state }
        |> Query.fromHtml
        |> Query.find [ classes buttonClasses ]


{-| Click a dialog button and expect the given message.
-}
expectClick : Site -> Person -> DialogState -> List String -> Msg msg -> Expect.Expectation
expectClick site person state buttonClasses msg =
    findButton site person state buttonClasses
        |> Event.simulate Event.click
        |> Event.expect msg


{-| Click a dialog button and expect the given next state.
-}
expectNextState : Site -> DialogState -> List String -> DialogState -> Expect.Expectation
expectNextState site state buttonClasses nextState =
    expectClick site TestFixtures.testPerson state buttonClasses (SetState (Just nextState))


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

        continueDisabled site countryCode phone =
            findButton site
                TestFixtures.testPerson
                (PhoneInput { countryCode = countryCode, phone = phone })
                [ "primary", "button" ]
                |> Query.has [ classes [ "disabled" ] ]

        consentWithProfileNumber site profileNumber nextState =
            expectClick site
                { testPerson | telephoneNumber = Just profileNumber }
                Consent
                [ "primary", "button" ]
                (SetState (Just nextState))

        sendTo site phoneNumber sentNumber =
            expectClick site
                TestFixtures.testPerson
                (ConfirmationBeforeExecuting phoneNumber)
                [ "primary", "button" ]
                (Execute English ReportAntenatal (toEntityUuid "person") sentNumber)

        testPerson =
            TestFixtures.testPerson

        whatsAppNumber site number expected =
            toWhatsAppNumber site number
                |> Expect.equal expected
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
        , describe "a number needs its country's number of digits"
            [ test "Rwanda: with a leading 0" <|
                \_ -> whatsAppNumber SiteRwanda "0788123456" (Just "+250788123456")
            , test "Rwanda: without a leading 0" <|
                \_ -> whatsAppNumber SiteRwanda "788123456" (Just "+250788123456")
            , test "Rwanda: one digit short" <|
                \_ -> whatsAppNumber SiteRwanda "078812345" Nothing
            , test "Rwanda: all zeros" <|
                \_ -> whatsAppNumber SiteRwanda "0000000000" Nothing
            , test "Rwanda: a single zero" <|
                \_ -> whatsAppNumber SiteRwanda "0" Nothing
            , test "Rwanda: country code without +" <|
                \_ -> whatsAppNumber SiteRwanda "250788123456" Nothing
            , test "Rwanda: not only digits" <|
                \_ -> whatsAppNumber SiteRwanda "0788-12345" Nothing
            , test "Burundi: 8 digits" <|
                \_ -> whatsAppNumber SiteBurundi "79123456" (Just "+25779123456")
            , test "Burundi: 9 digits" <|
                \_ -> whatsAppNumber SiteBurundi "791234567" Nothing
            , test "Somalia: 9 digits" <|
                \_ -> whatsAppNumber SiteSomalia "612345678" (Just "+252612345678")
            , test "international: a known country" <|
                \_ -> whatsAppNumber SiteBurundi "+250788123456" (Just "+250788123456")
            , test "international: a known country, one digit short" <|
                \_ -> whatsAppNumber SiteBurundi "+25078812345" Nothing
            , test "international: country code only" <|
                \_ -> whatsAppNumber SiteRwanda "+250" Nothing
            , test "international: a leading 0 after the country code is dropped" <|
                \_ -> whatsAppNumber SiteRwanda "+2500788123456" (Just "+250788123456")
            , test "international: USA and Canada" <|
                \_ -> whatsAppNumber SiteRwanda "+12025550123" (Just "+12025550123")
            , test "international: an unknown country needs at least 6 digits" <|
                \_ -> whatsAppNumber SiteRwanda "+44207946" (Just "+44207946")
            , test "international: an unknown country with too few digits" <|
                \_ -> whatsAppNumber SiteRwanda "+44207" Nothing
            ]
        , describe "an invalid number on the profile is not offered"
            [ test "Rwanda: a valid number is offered" <|
                \_ -> consentWithProfileNumber SiteRwanda "0788123456" (PhoneVerification "0788123456")
            , test "Rwanda: all zeros" <|
                \_ -> consentWithProfileNumber SiteRwanda "0000000000" (phoneInput CountryCodeRwanda)
            , test "Rwanda: one digit short" <|
                \_ -> consentWithProfileNumber SiteRwanda "078812345" (phoneInput CountryCodeRwanda)
            , test "Burundi: a valid number is offered" <|
                \_ -> consentWithProfileNumber SiteBurundi "79123456" (PhoneVerification "79123456")
            ]
        , describe "a typed number needs its country's number of digits"
            [ test "Rwanda: all zeros" <|
                \_ -> continueDisabled SiteRwanda CountryCodeRwanda "000000"
            , test "Rwanda: one digit short" <|
                \_ -> continueDisabled SiteRwanda CountryCodeRwanda "078812345"
            , test "Burundi: a Burundi number at Rwanda's length" <|
                \_ -> continueDisabled SiteBurundi CountryCodeBurundi "791234567"
            , test "empty" <|
                \_ -> continueDisabled SiteRwanda CountryCodeRwanda ""
            ]
        , describe "the report is sent to the number with its country code"
            [ test "Rwanda: a local number" <|
                \_ -> sendTo SiteRwanda "0788123456" "+250788123456"
            , test "Burundi: a local number" <|
                \_ -> sendTo SiteBurundi "79123456" "+25779123456"
            , test "Burundi: a number of another country" <|
                \_ -> sendTo SiteBurundi "+250788123456" "+250788123456"
            ]
        ]
