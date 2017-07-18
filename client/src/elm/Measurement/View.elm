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
        Maybe.map
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


viewFloatForm : BackendUrl -> String -> User -> Language -> FloatMeasurements -> ( ChildId, Child ) -> Model -> Html Msg
viewFloatForm backendUrl accessToken user language floatMeasurement ( childId, child ) model =
    let
        ( blockName, headerText, helpText, labelText, constraints, measurementValue, measurementType, updateMsg, saveMsg ) =
            case floatMeasurement of
                HeightFloat ->
                    ( "height"
                    , Trans.ActivitiesHeightTitle
                    , Trans.ActivitiesHeightHelp
                    , Trans.ActivitiesHeightLabel
                    , getInputConstraintsHeight
                    , model.height.value
                    , Trans.CentimeterShorthand
                    , HeightUpdate
                    , HeightSave
                    )

                MuacFloat ->
                    ( "muac"
                    , Trans.ActivitiesMuacTitle
                    , Trans.ActivitiesMuacHelp
                    , Trans.ActivitiesMuacLabel
                    , getInputConstraintsMuac
                    , model.muac.value
                    , Trans.CentimeterShorthand
                    , MuacUpdate
                    , MuacSave
                    )

                WeightFloat ->
                    ( "weight"
                    , Trans.ActivitiesWeightTitle
                    , Trans.ActivitiesWeightHelp
                    , Trans.ActivitiesWeightLabel
                    , getInputConstraintsWeight
                    , model.weight.value
                    , Trans.KilogramShorthand
                    , WeightUpdate
                    , WeightSave
                    )
    in
        div []
            [ divider
            , div
                [ class <| "ui full segment " ++ blockName ]
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
                                    , name blockName
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
