module App.Test exposing (all)

import App.Model exposing (..)
import App.Update exposing (..)
import App.View exposing (view)
import Expect
import Http
import Pages.Page exposing (Page(..))
import Test exposing (describe, test, Test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag)
import RemoteData exposing (RemoteData(..))


viewConfigErrorTest : Test
viewConfigErrorTest =
    describe "Config error view"
        [ test "Correct error message appears when config has errored" <|
            \() ->
                view { emptyModel | configuration = Failure "some error" }
                    |> Query.fromHtml
                    |> Query.has [ text "Configuration error" ]
        ]


all : Test
all =
    describe "App tests"
        [ viewConfigErrorTest
        ]
