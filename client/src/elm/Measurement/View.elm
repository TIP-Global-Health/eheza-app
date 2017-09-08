module Measurement.View
    exposing
        ( muacIndication
        , viewChild
        , viewMother
        , viewMuacIndication
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
import Date exposing (Date)
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
        , MuacIndication(..)
        , Msg(..)
        , Photo
        , PhotoId
        , EveryDictFamilyPlanningSigns
        , getFloatInputValue
        , getInputConstraintsHeight
        , getInputConstraintsMuac
        , getInputConstraintsWeight
        )
import Participant.Model
import Participant.Utils exposing (getParticipantAge)
import RemoteData exposing (RemoteData(..), isFailure, isLoading)
import Round
import Translate as Trans exposing (Language(..), TranslationId, translate)
import User.Model exposing (..)
import Utils.Html exposing (divider, emptyNode, showIf, showMaybe)
import ZScore.Model exposing (Centimetres, Kilograms)
import ZScore.Utils exposing (viewZScore, zScoreForHeight, zScoreForMuac, zScoreForWeight, zScoreWeightForHeight)


viewChild : BackendUrl -> String -> User -> Language -> Date -> ( ChildId, Child ) -> Maybe ExaminationChild -> Maybe ActivityType -> Model -> Html Msg
viewChild backendUrl accessToken user language currentDate ( childId, child ) maybePreviousExamination selectedActivity model =
    showMaybe <|
        Maybe.map
            (\activity ->
                case activity of
                    Child childActivity ->
                        case childActivity of
                            ChildPicture ->
                                viewPhoto backendUrl accessToken user language ( childId, child ) model

                            Height ->
                                viewFloatForm backendUrl accessToken user language currentDate HeightFloat ( childId, child ) maybePreviousExamination model

                            Muac ->
                                viewFloatForm backendUrl accessToken user language currentDate MuacFloat ( childId, child ) maybePreviousExamination model

                            NutritionSigns ->
                                viewNutritionSigns backendUrl accessToken user language ( childId, child ) model

                            Weight ->
                                viewFloatForm backendUrl accessToken user language currentDate WeightFloat ( childId, child ) maybePreviousExamination model

                            _ ->
                                emptyNode

                    _ ->
                        emptyNode
            )
            selectedActivity


viewFloatForm : BackendUrl -> String -> User -> Language -> Date -> FloatMeasurements -> ( ChildId, Child ) -> Maybe ExaminationChild -> Model -> Html Msg
viewFloatForm backendUrl accessToken user language currentDate floatMeasurement ( childId, child ) maybePreviousExamination model =
    let
        ( blockName, headerText, helpText, labelText, placeholderText, constraints, measurementValue, measurementType, ( updateMsg, saveMsg ) ) =
            case floatMeasurement of
                HeightFloat ->
                    ( "height"
                    , Trans.ActivitiesHeightTitle
                    , Trans.ActivitiesHeightHelp
                    , Trans.ActivitiesHeightLabel
                    , Trans.PlaceholderEnterHeight
                    , getInputConstraintsHeight
                    , model.height
                    , Trans.CentimeterShorthand
                    , ( HeightUpdate, HeightSave )
                    )

                MuacFloat ->
                    ( "muac"
                    , Trans.ActivitiesMuacTitle
                    , Trans.ActivitiesMuacHelp
                    , Trans.ActivitiesMuacLabel
                    , Trans.PlaceholderEnterMUAC
                    , getInputConstraintsMuac
                    , model.muac
                    , Trans.CentimeterShorthand
                    , ( MuacUpdate, MuacSave )
                    )

                WeightFloat ->
                    ( "weight"
                    , Trans.ActivitiesWeightTitle
                    , Trans.ActivitiesWeightHelp
                    , Trans.ActivitiesWeightLabel
                    , Trans.PlaceholderEnterWeight
                    , getInputConstraintsWeight
                    , model.weight
                    , Trans.KilogramShorthand
                    , ( WeightUpdate, WeightSave )
                    )

        childParticipant =
            { info = Participant.Model.ParticipantChild child }

        viewDiff =
            case ( floatMeasurement, measurementValue ) of
                ( MuacFloat, Just value ) ->
                    viewMuacIndication language (muacIndication <| getFloatInputValue value)

                _ ->
                    viewFloatDiff language floatMeasurement maybePreviousExamination measurementType model

        defaultAttr =
            Maybe.map (\val -> [ value val ]) measurementValue
                |> Maybe.withDefault [ value "" ]

        inputAttrs =
            [ type_ "text"
            , placeholder <| translate language placeholderText
            , name blockName
            , Attr.min <| toString constraints.minVal
            , Attr.max <| toString constraints.maxVal
            , onInput updateMsg
            ]
                ++ defaultAttr

        calculatedZScoreForAge =
            case ( floatMeasurement, measurementValue ) of
                ( _, Just value ) ->
                    case floatMeasurement of
                        HeightFloat ->
                            zScoreForHeight (getParticipantAge childParticipant currentDate) child.gender (ZScore.Model.Centimetres <| getFloatInputValue value)

                        MuacFloat ->
                            zScoreForMuac (getParticipantAge childParticipant currentDate) child.gender (ZScore.Model.Centimetres <| getFloatInputValue value)

                        WeightFloat ->
                            zScoreForWeight (getParticipantAge childParticipant currentDate) child.gender (ZScore.Model.Kilograms <| getFloatInputValue value)

                _ ->
                    Nothing

        calculatedZScoreForHeight =
            case ( floatMeasurement, measurementValue ) of
                ( _, Just value ) ->
                    case floatMeasurement of
                        HeightFloat ->
                            Nothing

                        MuacFloat ->
                            Nothing

                        WeightFloat ->
                            case maybePreviousExamination of
                                Nothing ->
                                    Nothing

                                Just previousExamination ->
                                    case previousExamination.height of
                                        Just height ->
                                            zScoreWeightForHeight (ZScore.Model.Centimetres height) child.gender (ZScore.Model.Kilograms <| getFloatInputValue value)

                                        Nothing ->
                                            Nothing

                _ ->
                    Nothing

        renderedZScoreForAge =
            case calculatedZScoreForAge of
                Just val ->
                    viewZScore val

                Nothing ->
                    translate language Trans.NotAvailable

        renderedZScoreForHeight =
            case calculatedZScoreForHeight of
                Just val ->
                    Just <| viewZScore val

                Nothing ->
                    Nothing
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
                        , div [ class "five wide column" ] [ viewDiff ]
                        ]
                    , viewPreviousMeasurement language floatMeasurement maybePreviousExamination measurementType
                    ]
                , div
                    [ class "ui large header" ]
                    [ text <| translate language Trans.ZScoreForAge
                    , span
                        [ class "sub header" ]
                        [ text renderedZScoreForAge ]
                    ]
                , case renderedZScoreForHeight of
                    Just forHeight ->
                        (div
                            [ class "ui large header" ]
                            [ text <| translate language Trans.ZScoreForAge
                            , span
                                [ class "sub header" ]
                                [ text forHeight ]
                            ]
                        )

                    Nothing ->
                        emptyNode
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


{-| Given a MUAC in cm, classify according to the measurement tool shown
at <https://github.com/Gizra/ihangane/issues/282>
-}
muacIndication : Float -> MuacIndication
muacIndication value =
    if value <= 11.5 then
        MuacRed
    else if value <= 12.5 then
        MuacYellow
    else
        MuacGreen


muacColor : MuacIndication -> Attribute any
muacColor muac =
    class <|
        case muac of
            MuacRed ->
                "label-red"

            MuacYellow ->
                "label-yellow"

            MuacGreen ->
                "label-green"


viewMuacIndication : Language -> MuacIndication -> Html any
viewMuacIndication language muac =
    p
        [ muacColor muac
        , class "label-form"
        ]
        [ translate language (Trans.MuacIndication muac)
            |> String.toUpper
            |> text
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


viewPreviousMeasurement : Language -> FloatMeasurements -> Maybe ExaminationChild -> TranslationId -> Html Msg
viewPreviousMeasurement language floatMeasurement maybePreviousExamination measurementType =
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
                        div []
                            [ text <|
                                (translate language <| Trans.PreviousFloatMeasurement previousValue)
                                    ++ " "
                                    ++ (translate language measurementType)
                            ]
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
            ( Just previousValue, Just currentInput ) ->
                let
                    currentValue =
                        getFloatInputValue currentInput

                    diff =
                        Round.round 2 <| abs (currentValue - previousValue)

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
        div
            [ class "ui full segment photo" ]
            [ div [ class "content" ]
                [ h3 [ class "ui header" ]
                    [ text <| translate language Trans.ActivitiesPhotoTitle ]
                , p [] [ text <| translate language Trans.ActivitiesPhotoHelp ]
                , viewPhotoThumb model.photo
                , div [ class "dropzone" ] []
                ]
            , div [ class "actions" ] <|
                saveButton language PhotoSave model hasFileId (Just "column")
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
                (not (EveryDict.isEmpty model.nutritionSigns))
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

        isChecked =
            EveryDict.member sign nutritionSigns
    in
        div [ class "ui checkbox" ]
            [ input
                ([ type_ "checkbox"
                 , id attributeValue
                 , name <| encodeChildNutritionSign sign
                 , onClick <| NutritionSignsToggle sign
                 , checked isChecked
                 ]
                    ++ if isChecked then
                        [ class "checked" ]
                       else
                        []
                )
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
            saveButton language FamilyPlanningSignsSave model (not (EveryDict.isEmpty model.familyPlanningSigns)) Nothing
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

        isChecked =
            EveryDict.member sign familyPlanningSigns
    in
        div [ class "ui checkbox" ]
            [ input
                ([ type_ "checkbox"
                 , id attributeValue
                 , onClick <| FamilyPlanningSignsToggle sign
                 , checked isChecked
                 ]
                    ++ if isChecked then
                        [ class "checked" ]
                       else
                        []
                )
                []
            , label [ for attributeValue ]
                [ text <| translate language body ]
            ]
