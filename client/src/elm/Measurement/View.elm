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
        Maybe.map
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
                        , onInput <| \v -> HeightUpdate <| Result.withDefault 0.0 <| String.toFloat v
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
                        , onInput <| \v -> MuacUpdate <| Result.withDefault 0.0 <| String.toFloat v
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
        div []
            [ divider
            , div
                [ class "ui segment weight"
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
                        , step "0.5"
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
                    , span [] [ text <| translate language Trans.KilogramShorthand ]
                    ]
                , saveButton language WeightSave model
                ]
            ]


viewPhoto : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewPhoto backendUrl accessToken user language ( childId, child ) model =
    div []
        [ divider
        , div
            [ class "ui segment"
            ]
            [ h1
                []
                [ text <| translate language Trans.ActivitiesPhotoTitle
                ]
            , span
                []
                [ text <| translate language Trans.ActivitiesPhotoHelp ]
            , div
                [ class "dropzone" ]
                []
            , saveButton language PhotoSave model
            , div
                [ class "ui button" ]
                [ text <| translate language Trans.Retake ]
            ]
        ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Msg -> Model -> Html Msg
saveButton language msg model =
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
    in
        div []
            [ div
                ([ classList
                    [ ( "ui fluid basic button", True )
                    , ( "loading", isLoading )
                    , ( "positive", isSuccess )
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
            [ class "ui full segment nutrition"
            , id "nutritionSignsEntryForm"
            ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language Trans.ActivitiesNutritionSignsTitle
                ]
            , p
                []
                [ text <| translate language Trans.ActivitiesNutritionSignsHelp ]
            , div
                [ class "ui form" ]
                [ p []
                    [ text <| translate language Trans.ActivitiesNutritionSignsLabel
                    ]
                , viewNutritionSignsSelector language
                ]
            , div [ class "actions" ]
                [ saveButton language NutritionSignsSave model
                ]
            ]
        ]


viewNutritionSignsSelector : Language -> Html Msg
viewNutritionSignsSelector language =
    let
        nutrionSignsAndTranslationIdsFirst =
            [ Edema, AbdominalDisortion, DrySkin, PoorAppetite ]

        nutrionSignsAndTranslationIdsSecond =
            [ Apathy, BrittleHair, None ]
    in
        div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                (List.map
                    (viewNutritionSignsSelectorItem language)
                    nutrionSignsAndTranslationIdsFirst
                )
            , div [ class "eight wide column" ]
                (List.map
                    (viewNutritionSignsSelectorItem language)
                    nutrionSignsAndTranslationIdsSecond
                )
            ]


{-| Helper function to return a tuples of checkbox label and attributes value.

For each nutrition sign the function will return a the translaed label of the
checkbox and a value for the id and for attributes.

-}
viewNutritionSignsSelectorItem : Language -> ChildNutritionSign -> Html Msg
viewNutritionSignsSelectorItem language sign =
    let
        ( body, attributeValue ) =
            case sign of
                Edema ->
                    ( Trans.ActivitiesNutritionSignsEdemaLabel, "edema" )

                AbdominalDisortion ->
                    ( Trans.ActivitiesNutritionSignsAbdominalDisortionLabel, "abdominal-distrortion" )

                DrySkin ->
                    ( Trans.ActivitiesNutritionSignsDrySkinLabel, "dry-skin" )

                PoorAppetite ->
                    ( Trans.ActivitiesNutritionSignsPoorAppetiteLabel, "poor-appetites" )

                Apathy ->
                    ( Trans.ActivitiesNutritionSignsApathyLabel, "apathy" )

                BrittleHair ->
                    ( Trans.ActivitiesNutritionSignsBrittleHairLabel, "brittle-hair" )

                None ->
                    ( Trans.ActivitiesNutritionSignsNoneLabel, "none-of-these" )
    in
        div [ class "ui checkbox" ]
            [ input
                [ type_ "checkbox"
                , id attributeValue
                , name <| encodeChildNutritionSign sign
                ]
                []
            , label [ for attributeValue ]
                [ text <| translate language body ]
            ]
