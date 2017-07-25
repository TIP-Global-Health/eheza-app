module Measurement.View
    exposing
        ( viewChild
        )

import Activity.Encoder exposing (encodeChildNutritionSign)
import Activity.Model exposing (ActivityType(..), ChildActivityType(..), ChildNutritionSign(..))
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import Examination.Model exposing (ExaminationChild)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (FloatInput, FloatMeasurements(..), Model, Msg(..), getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight)
import RemoteData exposing (RemoteData(..), isFailure, isLoading)
import Translate as Trans exposing (Language(..), TranslationId, translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showIf, showMaybe)


viewChild : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Maybe ExaminationChild -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language ( childId, child ) maybePreviousExamination selectedActivity model =
    showMaybe <|
        Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            ChildPicture ->
                                viewPhoto backendUrl accessToken user language ( childId, child ) model

                            Height ->
                                viewFloatForm backendUrl accessToken user language HeightFloat ( childId, child ) maybePreviousExamination model

                            Muac ->
                                viewFloatForm backendUrl accessToken user language MuacFloat ( childId, child ) maybePreviousExamination model

                            NutritionSigns ->
                                viewNutritionSigns backendUrl accessToken user language ( childId, child ) model

                            Weight ->
                                viewFloatForm backendUrl accessToken user language WeightFloat ( childId, child ) maybePreviousExamination model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity


viewFloatForm : BackendUrl -> String -> User -> Language -> FloatMeasurements -> ( ChildId, Child ) -> Maybe ExaminationChild -> Model -> Html Msg
viewFloatForm backendUrl accessToken user language floatMeasurement ( childId, child ) maybePreviousExamination model =
    let
        ( blockName, headerText, helpText, labelText, constraints, measurementValue, measurementType, updateMsg, saveMsg ) =
            case floatMeasurement of
                HeightFloat ->
                    ( "height"
                    , Trans.ActivitiesHeightTitle
                    , Trans.ActivitiesHeightHelp
                    , Trans.ActivitiesHeightLabel
                    , getInputConstraintsHeight
                    , model.height
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
                    , model.muac
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
                    , model.weight
                    , Trans.KilogramShorthand
                    , WeightUpdate
                    , WeightSave
                    )

        defaultAttr =
            Maybe.map (\val -> [ value <| toString val ]) measurementValue
                |> Maybe.withDefault []

        inputAttrs =
            [ type_ "number"
            , name blockName
            , Attr.min <| toString constraints.minVal
            , Attr.max <| toString constraints.maxVal
            , onInput <| (\v -> updateMsg <| Result.withDefault 0.0 <| String.toFloat v)
            ]
                ++ defaultAttr
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
                                    inputAttrs
                                    []
                                , div [ class "ui basic label" ] [ text <| translate language measurementType ]
                                ]
                            ]
                        , div [ class "six wide column" ]
                            [ viewFloatDiff language floatMeasurement maybePreviousExamination measurementType model ]
                        ]
                    , viewPreviousMeasurement language floatMeasurement maybePreviousExamination
                    ]
                , div
                    [ class "actions" ]
                    [ saveButton language saveMsg model (isJust measurementValue) Nothing
                    ]
                ]
            ]


viewPreviousMeasurement : Language -> FloatMeasurements -> Maybe ExaminationChild -> Html Msg
viewPreviousMeasurement language floatMeasurement maybePreviousExamination =
    case maybePreviousExamination of
        Nothing ->
            emptyNode

        Just previousExamination ->
            let
                maybePreviousValue =
                    case floatMeasurement of
                        HeightFloat ->
                            previousExamination.height

                        MuacFloat ->
                            previousExamination.muac

                        WeightFloat ->
                            previousExamination.weight
            in
                Maybe.map
                    (\previousValue ->
                        div [] [ text <| translate language <| Trans.PreviousFloatMeasurement previousValue ]
                    )
                    maybePreviousValue
                    |> Maybe.withDefault emptyNode


{-| Show a diff of values, if they were gained or lost.
|
-}
viewFloatDiff : Language -> FloatMeasurements -> Maybe ExaminationChild -> TranslationId -> Model -> Html Msg
viewFloatDiff language floatMeasurement maybePreviousExamination measurementType model =
    let
        maybePreviousValue =
            case maybePreviousExamination of
                Just previousExamination ->
                    case floatMeasurement of
                        HeightFloat ->
                            previousExamination.height

                        MuacFloat ->
                            previousExamination.muac

                        WeightFloat ->
                            previousExamination.weight

                Nothing ->
                    Nothing

        maybeCurrentValue =
            case floatMeasurement of
                HeightFloat ->
                    model.height

                MuacFloat ->
                    model.muac

                WeightFloat ->
                    model.weight
    in
        case ( maybePreviousValue, maybeCurrentValue ) of
            ( Just previousValue, Just currentValue ) ->
                let
                    diff =
                        toString <| abs (currentValue - previousValue)
                in
                    if currentValue == previousValue then
                        -- No change in the values.
                        emptyNode
                    else if currentValue > previousValue then
                        p [ class "label-up label-with-icon" ]
                            [ span [ class "icon-up" ] []
                            , text <| translate language Trans.Gained
                            , br [] []
                            , text <| diff ++ " " ++ translate language measurementType
                            ]
                    else
                        p [ class "label-down label-with-icon" ]
                            [ span [ class "icon-down" ] []
                            , text <| translate language Trans.Lost
                            , br [] []
                            , text <| diff ++ " " ++ translate language measurementType
                            ]

            _ ->
                emptyNode


viewPhoto : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewPhoto backendUrl accessToken user language ( childId, child ) model =
    let
        hasPhoto =
            (model.photo > 0)
    in
        div []
            [ divider
            , div
                [ class "ui full segment photo"
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
                                [ classList
                                    [ ( "ui fluid basic button retake", True )
                                    , ( "disabled", not hasPhoto )
                                    ]
                                ]
                                [ text <| translate language Trans.Retake ]
                            ]
                        , saveButton language PhotoSave model hasPhoto (Just "column")
                        ]
                    ]
                ]
            ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Msg -> Model -> Bool -> Maybe String -> Html Msg
saveButton language msg model hasInput maybeDivClass =
    let
        isLoading =
            model.status == Loading

        isSuccess =
            RemoteData.isSuccess model.status

        isFailure =
            RemoteData.isFailure model.status

        saveAttr =
            if isLoading || not hasInput then
                []
            else
                [ onClick msg ]

        attrs =
            Maybe.map (\divClass -> [ class divClass ]) maybeDivClass
    in
        div (Maybe.withDefault [] attrs)
            [ button
                ([ classList
                    [ ( "ui fluid basic button", True )
                    , ( "loading", isLoading )
                    , ( "basic", not isSuccess )
                    , ( "negative", isFailure )
                    , ( "disabled", not hasInput )
                    ]
                 , id "save-form"
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
                [ saveButton language NutritionSignsSave model True Nothing
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
