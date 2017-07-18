module Measurement.View
    exposing
        ( viewChild
        )

import Activity.Encoder exposing (encodeChildNutritionSign)
import Activity.Model exposing (ActivityType(..), ChildActivityType(..), ChildNutritionSign(..))
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Measurement.Model exposing (Model, Msg(..), getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight)
import RemoteData exposing (RemoteData(..), isFailure, isLoading)
import Translate as Trans exposing (Language(..), TranslationId, translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showIf, showMaybe)


viewChild : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language ( childId, child ) selectedActivity model =
    showMaybe <|
        (Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            ChildPicture ->
                                viewPhoto backendUrl accessToken user language ( childId, child ) model

                            Height ->
                                viewHeight backendUrl accessToken user language ( childId, child ) model

                            Muac ->
                                viewMuac backendUrl accessToken user language ( childId, child ) model

                            NutritionSigns ->
                                viewNutritionSigns backendUrl accessToken user language ( childId, child ) model

                            Weight ->
                                viewWeight backendUrl accessToken user language ( childId, child ) model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )


viewHeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewHeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsHeight
    in
        div []
            [ divider
            , div
                [ class "ui card height"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesHeightTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesHeightHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesHeightLabel ]
                    , input
                        [ type_ "number"
                        , name "height"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.weight.value
                        , onInput <| (\v -> HeightUpdate <| Result.withDefault 0.0 <| String.toFloat v)
                        ]
                        []
                    , span [] [ text <| translate language Trans.CentimeterShorthand ]
                    ]
                , button [ type_ "button" ] [ text <| translate language Trans.Save ]
                ]
            ]


viewMuac : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewMuac backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsMuac
    in
        div []
            [ divider
            , div
                [ class "ui card muac"
                ]
                [ h1
                    []
                    [ text <| translate language Trans.ActivitiesMuacTitle
                    ]
                , span
                    []
                    [ text <| translate language Trans.ActivitiesMuacHelp ]
                , div
                    []
                    [ span [] [ text <| translate language Trans.ActivitiesMuacLabel ]
                    , input
                        [ type_ "number"
                        , name "muac"
                        , Attr.min <| toString constraints.minVal
                        , Attr.max <| toString constraints.maxVal
                        , value <| toString model.muac.value
                        , onInput <| (\v -> MuacUpdate <| Result.withDefault 0.0 <| String.toFloat v)
                        ]
                        []
                    , span [] [ text <| translate language Trans.CentimeterShorthand ]
                    ]
                , button [ type_ "button" ] [ text <| translate language Trans.Save ]
                ]
            ]


viewWeight : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewWeight backendUrl accessToken user language ( childId, child ) model =
    let
        constraints =
            getInputConstraintsWeight
    in
        div [ class "ui full segment" ]
            [ div
                [ class "content"
                ]
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language Trans.ActivitiesWeightTitle
                    ]
                , p
                    []
                    [ text <| translate language Trans.ActivitiesWeightHelp ]
                , div
                    [ class "ui form" ]
                    [ div
                        [ class "ui grid" ]
                        [ div
                            [ class "ten wide column" ]
                            [ div
                                [ class "ui right labeled input" ]
                                [ div
                                    [ class "ui basic label" ]
                                    [ text <| translate language Trans.ActivitiesWeightLabel ]
                                , input
                                    [ type_ "number"
                                    , name "weight"
                                    , Attr.min <| toString constraints.minVal
                                    , Attr.max <| toString constraints.maxVal
                                    , value <| toString model.weight.value
                                    , onInput
                                        (\v ->
                                            String.toFloat v
                                                |> Result.withDefault constraints.defaultValue
                                                |> clamp constraints.minVal constraints.maxVal
                                                |> WeightUpdate
                                        )
                                    ]
                                    []
                                , div
                                    [ class "ui basic label" ]
                                    [ text <| translate language Trans.KilogramShorthand ]
                                ]
                            ]
                        ]
                    , p [] [ text <| translate language (Trans.PriorWeight 0.0) ]
                    ]
                ]
            , div
                [ class "actions" ]
                [ saveButton language WeightSave model Nothing
                ]
            ]


viewPhoto : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewPhoto backendUrl accessToken user language ( childId, child ) model =
    div []
        [ divider
        , div
            [ class "ui full segment"
            ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language Trans.ActivitiesPhotoTitle
                ]
            , p
                []
                [ text <| translate language Trans.ActivitiesPhotoHelp ]
            , div
                [ class "dropzone" ]
                []
            , div [ class "actions" ]
                [ div [ class "ui two column grid" ]
                    [ div
                        [ class "column" ]
                        [ button
                            [ class "ui fluid basic button" ]
                            [ text <| translate language Trans.Retake ]
                        ]
                    , saveButton language PhotoSave model (Just "column")
                    ]
                ]
            ]
        ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Msg -> Model -> Maybe String -> Html Msg
saveButton language msg model maybeDivClass =
    let
        isLoading =
            model.status == Loading

        isSuccess =
            RemoteData.isSuccess model.status

        isFailure =
            RemoteData.isFailure model.status

        saveAttr =
            if isLoading then
                []
            else
                [ onClick msg ]

        classes =
            Maybe.map (\divClass -> [ ( divClass, True ) ]) maybeDivClass
                |> Maybe.withDefault []
    in
        div [ classList classes ]
            [ button
                ([ classList
                    [ ( "ui fluid basic button", True )
                    , ( "loading", isLoading )
                    , ( "basic", not isSuccess )
                    , ( "negative", isFailure )
                    ]
                 ]
                    ++ saveAttr
                )
                [ text <| translate language Trans.Save
                ]
            , showIf isFailure <| div [] [ text <| translate language Trans.SaveError ]
            ]


viewNutritionSigns : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewNutritionSigns backendUrl accessToken user language ( childId, child ) model =
    div []
        [ div
            [ class "ui divider" ]
            []
        , div
            [ class "ui card nutrition"
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
            , saveButton language NutritionSignsSave model Nothing
            ]
        ]


viewNutritionSignsSelector : Language -> Html Msg
viewNutritionSignsSelector language =
    let
        nutrionSignsAndTranslationIds =
            [ ( Edema, Trans.ActivitiesNutritionSignsEdemaLabel )
            , ( AbdominalDisortion, Trans.ActivitiesNutritionSignsAbdominalDisortionLabel )
            , ( DrySkin, Trans.ActivitiesNutritionSignsDrySkinLabel )
            , ( PoorAppetite, Trans.ActivitiesNutritionSignsPoorAppetiteLabel )
            , ( Apathy, Trans.ActivitiesNutritionSignsApathyLabel )
            , ( BrittleHair, Trans.ActivitiesNutritionSignsBrittleHairLabel )
            , ( None, Trans.ActivitiesNutritionSignsNoneLabel )
            ]
    in
        ul [ class "checkboxes" ]
            (List.map (\( nutritionSign, translateId ) -> viewNutritionSignsSelectorItem language nutritionSign translateId) nutrionSignsAndTranslationIds)


viewNutritionSignsSelectorItem : Language -> ChildNutritionSign -> TranslationId -> Html Msg
viewNutritionSignsSelectorItem language sign translationId =
    li []
        [ input
            [ type_ "checkbox"
            , name <| encodeChildNutritionSign sign
            ]
            []
        , span [] [ text <| translate language translationId ]
        ]
