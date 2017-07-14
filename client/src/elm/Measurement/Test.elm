module Measurement.Test exposing (all)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Fixtures exposing (exampleChild, exampleUser)
import Measurement.Model exposing (..)
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (tag, text)
import Translate exposing (..)


viewHeightFormTest : Test
viewHeightFormTest =
    describe "Pages Patient View Tests" <|
        [ test "Weight form should display when selected" <|
            \() ->
                viewChild "" "" exampleUser English ( 5, exampleChild ) (Just <| Child Height) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "height" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Height:" ]
        ]


all : Test
all =
    describe "Measurement forms tests"
        [ viewHeightFormTest
        ]
