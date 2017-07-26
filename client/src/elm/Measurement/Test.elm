module Measurement.Test exposing (all)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChild, exampleUser)
import Measurement.Model exposing (..)
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, classes, id, tag, text)
import Translate exposing (..)


viewChildFormsTest : Test
viewChildFormsTest =
    let
        viewChildWithActivity selectedActivity model =
            viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) Nothing selectedActivity model
    in
        describe "A nurse visits the assesment of a Child" <|
            [ test "Then a height form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child Height) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.class "height" ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Height:" ]
            , test "Then a MUAC form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child Muac) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.class "muac" ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Mid Upper Arm Circumference (MUAC):" ]
            , test "Then a Nutrition form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child NutritionSigns) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.classes [ "ui", "full", "segment", "nutrition" ] ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Nutrition:" ]
            , test "Then a Weight form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child Weight) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Weight:" ]
            , test "Then a Photo form with disabled Save button should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child ChildPicture) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ id "save-form" ]
                        |> Query.has [ classes [ "disabled" ] ]
            , test "Then a Photo form with enabled Save button should be displayed when selected and there is one photo" <|
                \() ->
                    viewChildWithActivity (Just <| Child ChildPicture) { emptyModel | photo = ( Just 1, Nothing ) }
                        |> Query.fromHtml
                        |> Query.find [ id "save-form" ]
                        |> Query.hasNot [ classes [ "disabled" ] ]
            , test "Then a Photo form with disabled Retake button should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child ChildPicture) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ class "retake" ]
                        |> Query.has [ classes [ "disabled" ] ]
            , test "Then a Photo form with enabled Retake button should be displayed when selected and there is one photo" <|
                \() ->
                    viewChildWithActivity (Just <| Child ChildPicture) { emptyModel | photo = ( Just 1, Nothing ) }
                        |> Query.fromHtml
                        |> Query.find [ class "retake" ]
                        |> Query.hasNot [ classes [ "disabled" ] ]
            ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        ]
