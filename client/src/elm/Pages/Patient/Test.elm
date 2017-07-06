module Pages.Patient.Test exposing (all)

import Translate exposing (..)
import Pages.Patient.View exposing (..)
import Test exposing (describe, test, Test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text, tag)


viewWeightSelectedVisibleText : Test
viewWeightSelectedVisibleText =
    describe "Pages Patient View Tests" <|
        [ test "Weight form should display when selected" <|
            \() ->
                viewSelectedActivity English "weight"
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "weightEntryForm" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Weight:" ]
        ]


all : Test
all =
    describe "Pages Patient tests"
        [ viewWeightSelectedVisibleText
        ]
