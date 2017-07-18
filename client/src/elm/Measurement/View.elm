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
import Measurement.Model exposing (FloatMeasurements(..), Model, Msg(..), getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight)
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
                                viewFloatForm backendUrl accessToken user language HeightFloat ( childId, child ) model

                            Muac ->
                                viewFloatForm backendUrl accessToken user language MuacFloat ( childId, child ) model

                            NutritionSigns ->
                                viewNutritionSigns backendUrl accessToken user language ( childId, child ) model

                            Weight ->
                                viewFloatForm backendUrl accessToken user language WeightFloat ( childId, child ) model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity
        )


viewFloatForm : BackendUrl -> String -> User -> Language -> FloatMeasurements -> ( ChildId, Child ) -> Model -> Html Msg
viewFloatForm backendUrl accessToken user language floatMeasurements ( childId, child ) model =
    let
        ( constraints, headerText, helpText, labelText, measurementValue, measurementType, updateMsg, saveMsg ) =
            case floatMeasurements of
                HeightFloat ->
                    ( getInputConstraintsHeight
                    , Trans.ActivitiesHeightTitle
                    , Trans.ActivitiesHeightHelp
                    , Trans.ActivitiesHeightLabel
                    , model.height.value
                    , Trans.CentimeterShorthand
                    , HeightUpdate
                    , HeightSave
                    )

                MuacFloat ->
                    ( getInputConstraintsMuac
                    , Trans.ActivitiesMuacTitle
                    , Trans.ActivitiesMuacHelp
                    , Trans.ActivitiesMuacLabel
                    , model.muac.value
                    , Trans.CentimeterShorthand
                    , MuacUpdate
                    , MuacSave
                    )

                WeightFloat ->
                    ( getInputConstraintsWeight
                    , Trans.ActivitiesWeightTitle
                    , Trans.ActivitiesWeightHelp
                    , Trans.ActivitiesWeightLabel
                    , model.weight.value
                    , Trans.KilogramShorthand
                    , WeightUpdate
                    , WeightSave
                    )
    in
        div []
            [ divider
            , div
                [ class "ui full segment height"
                ]
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language headerText
                    ]
                , p
                    []
                    [ text <| translate language helpText ]
                , div
                    [ class "ui form" ]
                    [ div [ class "ui grid" ]
                        [ div [ class "ten wide column" ]
                            [ div [ class "ui right labeled input" ]
                                [ div [ class "ui basic label" ] [ text <| translate language labelText ]
                                , input
                                    [ type_ "number"
                                    , name "height"
                                    , Attr.min <| toString constraints.minVal
                                    , Attr.max <| toString constraints.maxVal
                                    , value <| toString measurementValue
                                    , onInput <| (\v -> updateMsg <| Result.withDefault 0.0 <| String.toFloat v)
                                    ]
                                    []
                                , div [ class "ui basic label" ] [ text <| translate language measurementType ]
                                ]
                            ]
                        ]
                    ]
                , saveButton language saveMsg model
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
                    [ ( "ui fluid button", True )
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
            , saveButton language NutritionSignsSave model
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
