module Measurement.View
    exposing
        ( viewChild
        )

import Activity.Encoder exposing (encodeChildNutritionSign)
import Activity.Model exposing (ActivityType(..), ChildActivityType(..), ChildNutritionSign(..))
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..))
import Measurement.Utils exposing (getInputConstraintsWeight)
import Child.Model exposing (Child, ChildId)
import Translate as Trans exposing (Language(..), translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showMaybe)


-- @todo: Change ActivityType to ChildActivityType ?

viewChild : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language ( childId, child ) selectedActivity model =
    showMaybe <|
        (Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            Weight ->
                                viewWeight backendUrl accessToken user language ( childId, child ) model

                            NutritionSigns ->
                               viewNutritionSigns backendUrl accessToken user  language (childId, child) model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )


viewWeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewWeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsWeight

        language =
            English
    in
        div []
            [ divider
            , div
                [ class "ui card"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesWeightTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesWeightHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesWeightLabel ]
                    , input
                        [ type_ "number"
                        , name "weight"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        ]
                        []
                    , span [] [ text <| translate language Trans.KilogramShorthand ]
                    ]
                ]
            ]
viewNutritionSigns: BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewNutritionSigns backendUrl accessToken user language ( childId, child ) model =
    div []
        [ div
            [ class "ui divider" ]
            []
        , div
            [ class "ui card"
            , id "nutritionSignsEntryForm"
            ]
            [ h1
                []
                [ text <| translate language Trans.ActivitiesNutritionSignsTitle
                ]
            , span
                []
                [ text <| translate language Trans.ActivitiesNutritionSignsHelp ]
            , div
                []
                [ span []
                    [ text <| translate language Trans.ActivitiesNutritionSignsLabel
                    , viewNutritionSignsSelector language
                    ]
                ]
            , button [ type_ "button" ] [ text <| translate language Trans.Save ]
            ]
        ]


viewNutritionSignsSelector : Language -> Html Msg
viewNutritionSignsSelector language =
    ul [ class "checkboxes" ]
        [ li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign Edema
                ]
                []
            , span []
                [ text <| translate language Trans.ActivitiesNutritionSignsEdemaLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign AbdominalDisortion
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsAbdominalDisortionLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign DrySkin
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsDrySkinLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign PoorAppetite
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsPoorAppetiteLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign Apathy
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsApathyLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign BrittleHair
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsBrittleHairLabel ]
            ]
        , li []
            [ input
                [ type_ "checkbox"
                , name <| encodeChildNutritionSign None
                ]
                []
            , span [] [ text <| translate language Trans.ActivitiesNutritionSignsNoneLabel ]
            ]
        ]
