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


{-| Click a dialog button and expect it to open the phone input with an empty
number and the given country code.
-}
expectPhoneInput : Site -> DialogState -> List String -> CountryCode -> Expect.Expectation
expectPhoneInput site state buttonClasses countryCode =
    view English site ( toEntityUuid "person", TestFixtures.testPerson ) ReportAntenatal Nothing { state = Just state }
        |> Query.fromHtml
        |> Query.find [ classes buttonClasses ]
        |> Event.simulate Event.click
        |> Event.expect (SetState (Just (PhoneInput { countryCode = countryCode, phone = "" })))


all : Test
all =
    let
        consentYes site =
            expectPhoneInput site Consent [ "primary", "button" ]

        verificationNo site =
            expectPhoneInput site (PhoneVerification "0788000000") [ "velvet", "button" ]
    in
    describe "Report to WhatsApp - a new number starts with the country code of the site"
        [ test "Rwanda: consent with no number on the profile" <|
            \_ -> consentYes SiteRwanda CountryCodeRwanda
        , test "Burundi: consent with no number on the profile" <|
            \_ -> consentYes SiteBurundi CountryCodeBurundi
        , test "Somalia: consent with no number on the profile" <|
            \_ -> consentYes SiteSomalia CountryCodeSomalia
        , test "Burundi: declining the number on the profile" <|
            \_ -> verificationNo SiteBurundi CountryCodeBurundi
        ]
