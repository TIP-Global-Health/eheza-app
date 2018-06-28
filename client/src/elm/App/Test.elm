module App.Test exposing (all)

import App.Model exposing (..)
import App.Update exposing (loginConfig)
import App.View exposing (view)
import Dict
import Maybe exposing (withDefault)
import Pages.Login.Model
import Pusher.Model exposing (Cluster(..), PusherAppKey)
import RemoteData exposing (RemoteData(..))
import Restful.Login exposing (checkCachedCredentials)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, tag, text)
import Translate exposing (Language(..))


viewConfigErrorTest : Test
viewConfigErrorTest =
    describe "Config error view"
        [ test "Correct error message appears when config has errored" <|
            \() ->
                view { emptyModel | configuration = Failure "some error" }
                    |> Query.fromHtml
                    |> Query.has [ text "Configuration error" ]
        ]


viewLanguageSwitcherTest : Test
viewLanguageSwitcherTest =
    describe "Language switcher view"
        [ test "The language switcher appears with the English language" <|
            \() ->
                view { emptyModel | configuration = Success testConfigModel }
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "language-icon" ]
                    |> Query.has [ Selector.class "gb" ]
        , test "The language switcher appears with the Kinyarwanda language" <|
            \() ->
                view { emptyModel | configuration = Success testConfigModel, language = Kinyarwanda }
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "language-icon" ]
                    |> Query.has [ Selector.class "rw" ]
        ]


testConfigModel : ConfiguredModel
testConfigModel =
    let
        testConfig =
            { backendUrl = "http://ihanagane.local"
            , name = "local"
            , pusherKey = PusherAppKey "" UsEast1
            , debug = True
            }

        ( loginStatus, cmd ) =
            checkCachedCredentials loginConfig testConfig.backendUrl ""
    in
    { config = testConfig
    , loginPage = Pages.Login.Model.emptyModel
    , login = loginStatus
    }


all : Test
all =
    describe "App tests"
        [ viewConfigErrorTest
        , viewLanguageSwitcherTest
        ]
