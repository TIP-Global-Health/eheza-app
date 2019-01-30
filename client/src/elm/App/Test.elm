module App.Test exposing (all)

import App.Model exposing (..)
import App.Update exposing (loginConfig)
import App.View exposing (view)
import Dict
import Maybe exposing (withDefault)
import Pages.Device.Model
import Pusher.Model exposing (Cluster(..), PusherAppKey)
import RemoteData exposing (RemoteData(..))
import Restful.Login exposing (checkCachedCredentials)
import Rollbar
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, tag, text)
import Translate exposing (Language(..))


viewConfigErrorTest : Test
viewConfigErrorTest =
    describe "Config error view"
        [ test "Correct error message appears when config has errored" <|
            \() ->
                view { testModel | configuration = Failure "some error" }
                    |> Query.fromHtml
                    |> Query.has [ text "Configuration error" ]
        ]


exampleFlags : Flags
exampleFlags =
    { credentials = ""
    , hostname = ""
    , activeLanguage = "en"
    , activeServiceWorker = False
    }


testModel : Model
testModel =
    emptyModel exampleFlags


viewLanguageSwitcherTest : Test
viewLanguageSwitcherTest =
    describe "Language switcher view"
        [ test "The language switcher appears with the English language" <|
            \() ->
                view { testModel | configuration = Success testConfigModel }
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "language-switcher" ]
                    |> Query.find [ Selector.class "english" ]
                    |> Query.has [ Selector.class "active" ]
        , test "The language switcher appears with the Kinyarwanda language" <|
            \() ->
                view { testModel | configuration = Success testConfigModel, language = Kinyarwanda }
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "language-switcher" ]
                    |> Query.find [ Selector.class "kinyarwanda" ]
                    |> Query.has [ Selector.class "active" ]
        ]


testConfigModel : ConfiguredModel
testConfigModel =
    let
        testConfig =
            { backendUrl = "http://ihanagane.local"
            , name = "local"
            , pusherKey = PusherAppKey "" UsEast1
            , debug = True
            , rollbarToken = Rollbar.token "the_token"
            , sandbox = False
            }

        ( loginStatus, cmd ) =
            checkCachedCredentials loginConfig testConfig.backendUrl (Just "")
    in
    { config = testConfig
    , device = NotAsked
    , devicePage = Pages.Device.Model.emptyModel
    }


all : Test
all =
    describe "App tests"
        [ viewConfigErrorTest
        , viewLanguageSwitcherTest
        ]
