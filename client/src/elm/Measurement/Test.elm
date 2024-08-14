module Measurement.Test exposing (all)

import Backend.Measurement.Model exposing (ColorAlertIndication(..))
import Expect
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes, text)
import Translate.Model exposing (Language(..))


viewChildFormsTest : Test
viewChildFormsTest =
    test "Re-implement viewChildFormsTest" <|
        always Expect.pass



{-
   let
       viewChildWithActivity selectedActivity model =
           Html.div [ Attr.class "test-container" ]
               [ viewChild English (Date.fromTime 1504858608000) ( toEntityUuid 5, exampleChildA ) Nothing selectedActivity model
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
           , test "Then the Weight form contains Z-Score for Age" <|
               \() ->
                   viewChildWithActivity (Just <| Child Weight) { emptyModel | weight = Just "10.0" }
                       |> Query.fromHtml
                       |> Query.find [ classes [ "z-score", "age" ] ]
                       |> Query.has [ classes [ "header" ] ]
           , test "Then the Weight form contains Z-Score for Height" <|
               \() ->
                   viewChildWithActivity (Just <| Child Weight) { emptyModel | weight = Just "10.0", height = Just "69.0" }
                       |> Query.fromHtml
                       |> Query.find [ classes [ "z-score", "height" ] ]
                       |> Query.has [ text "-2" ]
           ]
-}


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



{-
   let
       viewMotherWithActivity selectedActivity model =
           Html.div [ Attr.class "test-container" ]
               [ viewMother English selectedActivity model ]
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
-}


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        , viewMotherFormsTest
        , viewColorAlertIndicationTest
        ]
