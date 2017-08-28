module Measurement.View
    exposing
        ( viewChild
        , viewMother
        )

import Activity.Encoder exposing (encodeChildNutritionSign)
import Activity.Model
    exposing
        ( ActivityType(..)
        , ChildActivityType(..)
        , ChildNutritionSign(..)
        , FamilyPlanningSign(..)
        , ChildActivityType(..)
        , MotherActivityType(..)
        )
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import EveryDict
import Examination.Model exposing (ExaminationChild)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Maybe.Extra exposing (isJust)
import Measurement.Model
    exposing
        ( EveryDictChildNutritionSign
        , FileId
        , FloatInput
        , FloatMeasurements(..)
        , Model
        , Msg(..)
        , Photo
        , PhotoId
        , EveryDictFamilyPlanningSigns
        , getInputConstraintsHeight
        , getInputConstraintsMuac
        , getInputConstraintsWeight
        )
import Mother.Model exposing (Mother, MotherId)
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
            [ type_ "text"
            , placeholder "Enter height here…"
            , name blockName
            , Attr.min <| toString constraints.minVal
            , Attr.max <| toString constraints.maxVal
            , onInput <| (\v -> updateMsg <| Result.withDefault 0.0 <| String.toFloat v)
            ]
                ++ defaultAttr
    in
        div
            [ class <| "ui full segment " ++ blockName ]
            [ div [ class "content" ]
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
                        [ div [ class "eleven wide column" ]
                            [ div [ class "ui right labeled input" ]
                                [ input inputAttrs []
                                , div [ class "ui basic label" ] [ text <| translate language measurementType ]
                                ]
                            ]
                        , div [ class "five wide column" ]
                            [ viewFloatDiff language floatMeasurement maybePreviousExamination measurementType model ]
                        ]
                    , viewPreviousMeasurement language floatMeasurement maybePreviousExamination
                    ]
                , div
                    [ class "ui large header" ]
                    [ text "Z-Score:"
                    , span
                        [ class "sub header" ]
                        [ text "Requires implementation" ]
                    ]
                ]
            , div
                [ class "actions" ]
              <|
                saveButton
                    language
                    saveMsg
                    model
                    (isJust measurementValue)
                    Nothing
            ]


{-| Show a photo thumbnail, if it exists.
-}
viewPhotoThumb : ( Maybe FileId, Maybe ( PhotoId, Photo ) ) -> Html Msg
viewPhotoThumb maybePhoto =
    showMaybe <|
        Maybe.map
            (\( _, photo ) ->
                div []
                    [ img [ src photo.url, class "ui small image" ] []
                    ]
            )
            (Tuple.second maybePhoto)


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

                    viewMessage isGain =
                        let
                            classSuffix =
                                if isGain then
                                    "up"
                                else
                                    "down"
                        in
                            p
                                [ class <| "label-with-icon label-form" ]
                                [ span [ class <| "icon-" ++ classSuffix ] []
                                , text <| diff ++ " " ++ translate language measurementType
                                ]
                in
                    if currentValue == previousValue then
                        -- No change in the values.
                        emptyNode
                    else if currentValue > previousValue then
                        viewMessage True
                    else
                        viewMessage False

            _ ->
                emptyNode


viewPhoto : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Model -> Html Msg
viewPhoto backendUrl accessToken user language ( childId, child ) model =
    let
        hasFileId =
            isJust <| Tuple.first model.photo

        handleClick =
            if hasFileId then
                [ onClick ResetDropZone ]
            else
                []
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
                , viewPhotoThumb model.photo
                , div
                    [ class "dropzone" ]
                    []
                , div [ class "actions" ]
                    [ div [ class "ui two column grid" ]
                        [ div
                            [ class "column" ]
                            (button
                                ([ classList
                                    [ ( "ui fluid basic button retake", True )
                                    , ( "disabled", not hasFileId )
                                    ]
                                 ]
                                    ++ handleClick
                                )
                                [ text <| translate language Trans.Retake ]
                                :: saveButton language PhotoSave model hasFileId (Just "column")
                            )
                        ]
                    ]
                ]
            ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Msg -> Model -> Bool -> Maybe String -> List (Html Msg)
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
    in
        [ button
            ([ classList
                [ ( "ui fluid primary button", True )
                , ( "loading", isLoading )
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
    div
        [ class "ui full segment nutrition"
        , id "nutritionSignsEntryForm"
        ]
        [ div [ class "content" ]
            [ h3 [ class "ui header" ]
                [ text <| translate language Trans.ActivitiesNutritionSignsTitle
                ]
            , p [] [ text <| translate language Trans.ActivitiesNutritionSignsHelp ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language Trans.ActivitiesNutritionSignsLabel ]
                    :: viewNutritionSignsSelector language model.nutritionSigns
            ]
        , div [ class "actions" ] <|
            saveButton
                language
                NutritionSignsSave
                model
                True
                Nothing
        ]


viewNutritionSignsSelector : Language -> EveryDictChildNutritionSign -> List (Html Msg)
viewNutritionSignsSelector language nutritionSigns =
    let
        nutrionSignsAndTranslationIdsFirst =
            [ Edema, AbdominalDisortion, DrySkin ]

        nutrionSignsAndTranslationIdsSecond =
            [ Apathy, PoorAppetite, BrittleHair ]
    in
        [ div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                (List.map
                    (viewNutritionSignsSelectorItem language nutritionSigns)
                    nutrionSignsAndTranslationIdsFirst
                )
            , div [ class "eight wide column" ]
                (List.map
                    (viewNutritionSignsSelectorItem language nutritionSigns)
                    nutrionSignsAndTranslationIdsSecond
                )
            ]
        , div [ class "ui divider" ] []
        , viewNutritionSignsSelectorItem language nutritionSigns None
        ]


{-| Helper function to return a tuples of checkbox label and attributes value.

For each nutrition sign the function will return a the translaed label of the
checkbox and a value for the id and for attributes.

-}
viewNutritionSignsSelectorItem : Language -> EveryDictChildNutritionSign -> ChildNutritionSign -> Html Msg
viewNutritionSignsSelectorItem language nutritionSigns sign =
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
                , onClick <| NutritionSignsToggle sign
                , checked <| EveryDict.member sign nutritionSigns
                ]
                []
            , label [ for attributeValue ]
                [ text <| translate language body ]
            ]


viewMother : BackendUrl -> String -> User -> Language -> Maybe ActivityType -> Model -> Html Msg
viewMother backendUrl accessToken user language selectedActivity model =
    showMaybe <|
        Maybe.map
            (\activity ->
                case activity of
                    Mother motherActivity ->
                        case motherActivity of
                            FamilyPlanning ->
                                viewFamilyPlanning backendUrl accessToken user language model

                    _ ->
                        emptyNode
            )
            selectedActivity


viewFamilyPlanning : BackendUrl -> String -> User -> Language -> Model -> Html Msg
viewFamilyPlanning backendUrl accessToken user language model =
    div
        [ class "ui full segment family-planning"
        , id "familyPlanningEntryForm"
        ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language Trans.ActivitiesFamilyPlanningSignsTitle
                ]
            , p [] [ text <| translate language Trans.ActivitiesFamilyPlanningSignsHelp ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language Trans.ActivitiesFamilyPlanningSignsLabel ]
                    :: viewFamilyPlanningSelector language model.familyPlanningSigns
            ]
        , div [ class "actions" ] <|
            saveButton language FamilyPlanningSignsSave model True Nothing
        ]


viewFamilyPlanningSelector : Language -> EveryDictFamilyPlanningSigns -> List (Html Msg)
viewFamilyPlanningSelector language familyPlanningSigns =
    let
        familyPlanningSignFirst =
            [ Pill, Condoms, IUD ]

        familyPlanningSignSecond =
            [ Injection, Necklace ]
    in
        [ div [ class "ui grid" ]
            [ div [ class "eight wide column" ] <|
                List.map
                    (viewFamilyPlanningSelectorItem language familyPlanningSigns)
                    familyPlanningSignFirst
            , div [ class "eight wide column" ] <|
                List.map
                    (viewFamilyPlanningSelectorItem language familyPlanningSigns)
                    familyPlanningSignSecond
            ]
        , div [ class "ui divider" ] []
        , viewFamilyPlanningSelectorItem language familyPlanningSigns NoFamilyPlanning
        ]


viewFamilyPlanningSelectorItem : Language -> EveryDictFamilyPlanningSigns -> FamilyPlanningSign -> Html Msg
viewFamilyPlanningSelectorItem language familyPlanningSigns sign =
    let
        ( body, attributeValue ) =
            case sign of
                Condoms ->
                    ( Trans.ActivitiesFamilyPlanningSignsCondomsLabel, "condoms" )

                IUD ->
                    ( Trans.ActivitiesFamilyPlanningSignsIUDLabel, "iud" )

                Injection ->
                    ( Trans.ActivitiesFamilyPlanningSignsInjectionLabel, "injection" )

                Necklace ->
                    ( Trans.ActivitiesFamilyPlanningSignsNecklaceLabel, "necklace" )

                NoFamilyPlanning ->
                    ( Trans.ActivitiesFamilyPlanningSignsNoneLabel, "no-family-planning-sign" )

                Pill ->
                    ( Trans.ActivitiesFamilyPlanningSignsPillLabel, "pill" )
    in
        div [ class "ui checkbox" ]
            [ input
                [ type_ "checkbox"
                , id attributeValue

                --, name <| encodeChildNutritionSign sign
                , onClick <| FamilyPlanningSignsToggle sign
                , checked <| EveryDict.member sign familyPlanningSigns
                ]
                []
            , label [ for attributeValue ]
                [ text <| translate language body ]
            ]
