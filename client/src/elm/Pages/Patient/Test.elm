module Pages.Patient.Test exposing (all)

import Pages.Patient.Model exposing (ActivityOptions)
import Pages.Patient.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (tag, text)
import Translate exposing (..)


viewWeightSelectedVisibleText : Test
viewWeightSelectedVisibleText =
    describe "Pages Patient View Tests" <|
        [ test "Weight form should display when selected" <|
            \() ->
                viewSelectedActivity English (Just Pages.Patient.Model.Weight)
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
