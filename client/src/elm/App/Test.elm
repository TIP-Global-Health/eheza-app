module App.Test exposing (all)

import App.Model exposing (..)
import App.View exposing (view)
import RemoteData exposing (RemoteData(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)


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
