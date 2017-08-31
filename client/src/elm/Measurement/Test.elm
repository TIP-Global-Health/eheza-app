module Measurement.Test exposing (all)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Expect
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChild, exampleUser)
import Html
import Html.Attributes as Attr
import Measurement.Model exposing (..)
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, classes, id, tag, text, className)
import Translate exposing (..)


viewChildFormsTest : Test
viewChildFormsTest =
    let
        viewChildWithActivity selectedActivity model =
            Html.div [ Attr.class "test-container" ]
                [ viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) Nothing selectedActivity model
                ]
    in
        describe "A nurse visits the assesment of a Child" <|
            [ test "Then a height form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child Height) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.classes [ "ui", "full", "segment", "height" ] ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Height:" ]
            , test "Then a MUAC form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child Muac) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.classes [ "ui", "full", "segment", "muac" ] ]
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
                        |> Query.find [ Selector.classes [ "ui", "full", "segment", "weight" ] ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Weight:" ]
            , test "Then a Photo form should be displayed when selected" <|
                \() ->
                    viewChildWithActivity (Just <| Child ChildPicture) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.classes [ "ui", "full", "segment", "photo" ] ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Photo:" ]
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
            ]


viewMotherFormsTest : Test
viewMotherFormsTest =
    let
        viewMotherWithActivity selectedActivity model =
            Html.div [ Attr.class "test-container" ]
                [ viewMother exampleBackendUrl exampleAccessToken exampleUser English selectedActivity model ]
    in
        describe "A nurse visits the assesment of a Mother" <|
            [ test "Then a family planning form should be displayed when selected" <|
                \() ->
                    viewMotherWithActivity (Just <| Activity.Model.Mother FamilyPlanning) emptyModel
                        |> Query.fromHtml
                        |> Query.find [ Selector.class "family-planning" ]
                        |> Query.find [ tag "h3" ]
                        |> Query.has [ text "Planning:" ]
            ]


muacIndicationTest : Test
muacIndicationTest =
    describe "MuacIndication tests"
        [ describe "muacIndication"
            [ test "red" <|
                \_ ->
                    muacIndication 11.5
                        |> Expect.equal MuacRed
            , test "yellow1" <|
                \_ ->
                    muacIndication 11.6
                        |> Expect.equal MuacYellow
            , test "yellow2" <|
                \_ ->
                    muacIndication 12.5
                        |> Expect.equal MuacYellow
            , test "green" <|
                \_ ->
                    muacIndication 12.6
                        |> Expect.equal MuacGreen
            ]
        , describe "viewMuacIndication"
            [ test "red" <|
                \_ ->
                    viewMuacIndication English MuacRed
                        |> Query.fromHtml
                        |> Query.has
                            [ classes [ "label-red" ]
                            , text "RED"
                            ]
            , test "yellow" <|
                \_ ->
                    viewMuacIndication English MuacYellow
                        |> Query.fromHtml
                        |> Query.has
                            [ classes [ "label-yellow" ]
                            , text "YELLOW"
                            ]
            , test "green" <|
                \_ ->
                    viewMuacIndication English MuacGreen
                        |> Query.fromHtml
                        |> Query.has
                            [ classes [ "label-green" ]
                            , text "GREEN"
                            ]
            ]
        ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        , viewMotherFormsTest
        , muacIndicationTest
        ]
