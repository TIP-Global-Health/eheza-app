module Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (ColorAlertIndication(..))
import Expect
import Measurement.View exposing (viewColorAlertIndication)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes, text)
import Translate.Model exposing (Language(..))


viewChildFormsTest : Test
viewChildFormsTest =
    test "Re-implement viewChildFormsTest" <|
        always Expect.pass


viewMotherFormsTest : Test
viewMotherFormsTest =
    test "Re-implement viewMotherFormsTest" <|
        always Expect.pass


viewColorAlertIndicationTest : Test
viewColorAlertIndicationTest =
    describe "viewColorAlertIndication"
        [ test "red" <|
            \_ ->
                viewColorAlertIndication English ColorAlertRed
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-red" ]
                        , text "RED"
                        ]
        , test "yellow" <|
            \_ ->
                viewColorAlertIndication English ColorAlertYellow
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-yellow" ]
                        , text "YELLOW"
                        ]
        , test "green" <|
            \_ ->
                viewColorAlertIndication English ColorAlertGreen
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-green" ]
                        , text "GREEN"
                        ]
        ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        , viewMotherFormsTest
        , viewColorAlertIndicationTest
        ]
