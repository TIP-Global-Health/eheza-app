module Measurement.Test exposing (all)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChild, exampleUser)
import Measurement.Model exposing (..)
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (tag, text)
import Translate exposing (..)


viewChildFormsTest : Test
viewChildFormsTest =
    describe "A nurse visits the assesment of a Child" <|
        [ test "Then a weight form should be displayed when selected" <|
            \() ->
                viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) (Just <| Child Height) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "height" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Height:" ]
        , test "Then a MUAC form should be displayed when selected" <|
            \() ->
                viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) (Just <| Child Muac) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "muac" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Mid Upper Arm Circumference (MUAC):" ]
        , test "Then a Nutrition form should be displayed when selected" <|
            \() ->
                viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) (Just <| Child NutritionSigns) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "ui full segment nutrition" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Nutrition:" ]
        , test "Then a Weight form should be displayed when selected" <|
            \() ->
                viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) (Just <| Child Weight) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "weight" ]
                    |> Query.find [ tag "h1" ]
                    |> Query.has [ text "Weight:" ]
        ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        ]
