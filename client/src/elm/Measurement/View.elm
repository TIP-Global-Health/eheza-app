module Measurement.View
    exposing
        ( viewChild
        , viewMother
        , viewMuacIndication
        )

{-| This module provides a form for entering measurements.
-}

import Activity.Model
    exposing
        ( ActivityType(..)
        , ChildActivityType(..)
        , MotherActivityType(..)
        )
import Backend.Child.Model exposing (Child, Gender)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (encodeNutritionSignAsString, encodeFamilyPlanningSignAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit, muacIndication)
import Backend.Mother.Model exposing (Mother)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import Participant.Model
import RemoteData exposing (RemoteData(..), WebData, isFailure, isLoading)
import Restful.Endpoint exposing (Entity)
import Round
import Translate as Trans exposing (Language(..), TranslationId, translate)
import Utils.NominalDate exposing (diffDays)
import ZScore.Model exposing (AgeInDays(..), Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreForHeight, zScoreForMuac, zScoreForWeight, zScoreWeightForHeight)


{-| This is just a convenience for the function below ... otherwise we need
rather a lot of parameters! Basically, we ask the caller to assemble this data,
so we can focus on other things here.

The `status` indicates whether we're currently saving measurements, or any
error from the last save. We don't ask for a `WebData` for the
ChildMeasurements or the ChildEdits, because if we don't have them then some
caller should show something else -- we'll insist on having those.

-}
type alias MeasurementData data edits =
    { previous : data
    , current : data
    , edits : edits
    , status : WebData ()
    }


mapMeasurementData : (d1 -> d2) -> (e1 -> e2) -> MeasurementData d1 e1 -> MeasurementData d2 e2
mapMeasurementData dataFunc editFunc measurements =
    { previous = dataFunc measurements.previous
    , current = dataFunc measurements.current
    , edits = editFunc measurements.edits
    , status = measurements.status
    }


{-| We need the current date in order to immediately construct a ZScore for the
child when we enter something.
-}
viewChild : Language -> Date -> Child -> ChildActivityType -> MeasurementData ChildMeasurements ChildEdits -> ModelChild -> Html MsgChild
viewChild language currentDate child activity measurements model =
    case activity of
        ChildPicture ->
            viewPhoto language measurements.status model.photo

        Height ->
            viewHeight language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .height) .height measurements) model

        Muac ->
            viewMuac language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .muac) .muac measurements) model

        NutritionSigns ->
            viewNutritionSigns language measurements.status model.nutritionSigns

        Weight ->
            viewWeight language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .weight) .weight measurements) model

        ProgressReport ->
            -- TODO: Show something here? Possibly with a button to indicate that we've completed the "activity"
            -- of showing the mother the progress report?
            emptyNode


{-| Some configuration for the `viewFloatForm` function, which handles several
different types of `Float` inputs.
-}
type alias FloatFormConfig value =
    { blockName : String
    , headerText : TranslationId
    , helpText : TranslationId
    , labelText : TranslationId
    , placeholderText : TranslationId
    , zScoreLabelForAge : TranslationId
    , zScoreForAge : AgeInDays -> Gender -> Float -> Maybe ZScore
    , zScoreForHeight : Maybe (Centimetres -> Gender -> Float -> Maybe ZScore)
    , constraints : FloatInputConstraints
    , unit : TranslationId
    , inputValue : ModelChild -> String
    , storedValue : value -> Float
    , dateMeasured : value -> NominalDate
    , viewIndication : Maybe (Language -> Float -> Html MsgChild)
    , updateMsg : String -> MsgChild
    , saveMsg : MsgChild
    }


heightFormConfig : FloatFormConfig Height
heightFormConfig =
    { blockName = "height"
    , headerText = Trans.ActivitiesHeightTitle
    , helpText = Trans.ActivitiesHeightHelp
    , labelText = Trans.ActivitiesHeightLabel
    , placeholderText = Trans.PlaceholderEnterHeight
    , zScoreLabelForAge = Trans.ZScoreHeightForAge
    , zScoreForAge = \age gender height -> zScoreForHeight age gender (Centimetres height)
    , zScoreForHeight = Nothing
    , constraints = getInputConstraintsHeight
    , unit = Trans.CentimeterShorthand
    , inputValue = .height
    , storedValue = .value >> \(HeightInCm val) -> val
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateHeight
    , saveMsg = SendOutMsgChild SaveHeight
    }


muacFormConfig : FloatFormConfig Muac
muacFormConfig =
    { blockName = "muac"
    , headerText = Trans.ActivitiesMuacTitle
    , helpText = Trans.ActivitiesMuacHelp
    , labelText = Trans.ActivitiesMuacLabel
    , placeholderText = Trans.PlaceholderEnterMUAC
    , zScoreLabelForAge = Trans.ZScoreMuacForAge
    , zScoreForAge = \age gender muac -> zScoreForMuac age gender (Centimetres muac)
    , zScoreForHeight = Nothing
    , constraints = getInputConstraintsMuac
    , unit = Trans.CentimeterShorthand
    , inputValue = .muac
    , storedValue = .value >> \(MuacInCm val) -> val
    , dateMeasured = .dateMeasured
    , viewIndication = Just <| \language val -> viewMuacIndication language (muacIndication (MuacInCm val))
    , updateMsg = UpdateMuac
    , saveMsg = SendOutMsgChild SaveMuac
    }


weightFormConfig : FloatFormConfig Weight
weightFormConfig =
    { blockName = "weight"
    , headerText = Trans.ActivitiesWeightTitle
    , helpText = Trans.ActivitiesWeightHelp
    , labelText = Trans.ActivitiesWeightLabel
    , placeholderText = Trans.PlaceholderEnterWeight
    , zScoreLabelForAge = Trans.ZScoreWeightForAge
    , zScoreForAge = \age gender weight -> zScoreForWeight age gender (Kilograms weight)
    , zScoreForHeight = Just <| \height gender weight -> zScoreWeightForHeight height gender (Kilograms weight)
    , constraints = getInputConstraintsWeight
    , unit = Trans.KilogramShorthand
    , inputValue = .weight
    , storedValue = .value >> \(WeightInKg val) -> val
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateWeight
    , saveMsg = SendOutMsgChild SaveWeight
    }


viewHeight : Language -> Date -> Child -> MeasurementData (Maybe Height) (Edit Height) -> ModelChild -> Html MsgChild
viewHeight language date child measurements model =
    viewFloatForm heightFormConfig language date child measurements model


viewWeight : Language -> Date -> Child -> MeasurementData (Maybe Weight) (Edit Weight) -> ModelChild -> Html MsgChild
viewWeight language date child measurements model =
    viewFloatForm weightFormConfig language date child measurements model


viewMuac : Language -> Date -> Child -> MeasurementData (Maybe Muac) (Edit Muac) -> ModelChild -> Html MsgChild
viewMuac language date child measurements model =
    viewFloatForm muacFormConfig language date child measurements model


viewFloatForm : FloatFormConfig value -> Language -> Date -> Child -> MeasurementData (Maybe value) (Edit value) -> ModelChild -> Html MsgChild
viewFloatForm config language currentDate child measurements model =
    let
        -- What is the string input value from the form?
        inputValue =
            config.inputValue model

        -- Our input is a string, which may or may not be a valid float,
        -- since we want to let users enter things like "." to start with
        -- without clobbering what they type. Query: whether tye `type_`
        -- ought to be "number" instead of "text"? Should test that once this
        -- is running again, to see how it affects browser behaviour.
        inputAttrs =
            [ type_ "text"
            , placeholder <| translate language config.placeholderText
            , name config.blockName
            , Attr.min <| toString config.constraints.minVal
            , Attr.max <| toString config.constraints.maxVal
            , onInput config.updateMsg
            , value inputValue
            ]

        -- What float value does our input corresopnd to, if any? Will be
        -- `Nothing` if our input value doesn't convert to a string
        -- successfully at the moment ... in which case we won't bother
        -- with the various interpretations yet. (Or allow saving).
        floatValue =
            inputValue
                |> String.toFloat
                |> Result.toMaybe

        -- What is the most recent measurement we've saved, either locally or
        -- to the backend (we don't care at the moment which). If this is a new
        -- measurement we haven't saved yet, this will be Nothing.
        savedMeasurement =
            measurements.current
                |> applyEdit measurements.edits

        -- What measurement should we be comparing to? That is, what's the most
        -- recent measurement of this kind that we're **not** editing?
        previousMeasurement =
            measurements.previous

        -- For calculating ZScores, we need to know how old the child was at
        -- the time of the **measurement**. If we have an existing value that
        -- we're modifying, we can get that from the value. (In that case,
        -- we're assuming that we're editing that value, rather than making a
        -- new measurement today). Otherwise, we assume we're makeing the
        -- measurement today.
        --
        -- TODO: We should probably surface this in the UI ... that is, display
        -- what day we think the measurement was made on, and allow the nurse
        -- to change that if necessary.
        dateMeasured =
            savedMeasurement
                |> Maybe.map config.dateMeasured
                |> Maybe.withDefault (fromLocalDateTime currentDate)

        -- And, we'll need the child's age.
        ageInDays =
            AgeInDays <|
                diffDays child.birthDate dateMeasured

        zScoreForAgeText =
            floatValue
                |> Maybe.andThen (\val -> config.zScoreForAge ageInDays child.gender val)
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Trans.NotAvailable)

        -- We always show something about the ZScore ... if we can't
        -- calculate one, we say that it's unavailable.
        renderedZScoreForAge =
            div
                [ class "ui large header z-score age" ]
                [ text <| translate language config.zScoreLabelForAge
                , span
                    [ class "sub header" ]
                    [ text zScoreForAgeText ]
                ]

        -- In some cases (weight) we also calculate a ZScore based on the
        -- height (rather than age). In order to do that, we need both the height and the weight.
        renderedZScoreForHeight =
            config.zScoreForHeight
                |> Maybe.map
                    (\func ->
                        let
                            -- We get the height from the model, so we'll use
                            -- the height only if it has been entered in this
                            -- session. (The previous height will have been at
                            -- a previous date). So, I suppose we should ask
                            -- the nurse to measure height before weight, so we
                            -- can see the ZScore when entering the weight.
                            zScoreText =
                                model.height
                                    |> String.toFloat
                                    |> Result.toMaybe
                                    |> Maybe.andThen
                                        (\height ->
                                            Maybe.andThen
                                                (\weight -> func (Centimetres height) child.gender weight)
                                                floatValue
                                        )
                                    |> Maybe.map viewZScore
                                    |> Maybe.withDefault (translate language Trans.NotAvailable)
                        in
                            div
                                [ class "ui large header z-score height" ]
                                [ text <| translate language Trans.ZScoreWeightForHeight
                                , span
                                    [ class "sub header" ]
                                    [ text zScoreText
                                    ]
                                ]
                    )
    in
        div
            [ class <| "ui full segment " ++ config.blockName ]
            [ div [ class "content" ]
                [ h3
                    [ class "ui header" ]
                    [ text <| translate language config.headerText
                    ]
                , p
                    []
                    [ text <| translate language config.helpText ]
                , div
                    [ class "ui form" ]
                    [ div [ class "ui grid" ]
                        [ div [ class "eleven wide column" ]
                            [ div [ class "ui right labeled input" ]
                                [ input inputAttrs []
                                , div
                                    [ class "ui basic label" ]
                                    [ text <| translate language config.unit ]
                                ]
                            ]
                        , div
                            [ class "five wide column" ]
                            [ showMaybe <|
                                Maybe.map2 (viewFloatDiff config language)
                                    measurements.previous
                                    floatValue
                            , showMaybe <|
                                Maybe.map2 (\func value -> func language value)
                                    config.viewIndication
                                    floatValue
                            ]
                        ]
                    , previousMeasurement
                        |> Maybe.map (viewPreviousMeasurement config language)
                        |> showMaybe
                    ]
                , renderedZScoreForAge
                , showMaybe renderedZScoreForHeight
                ]
            , div [ class "actions" ] <|
                saveButton language
                    config.saveMsg
                    (isJust floatValue)
                    measurements.status
                    Nothing
            ]


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


{-| Show a photo thumbnail.
-}
viewPhotoThumb : PhotoValue -> Html any
viewPhotoThumb (PhotoValue url) =
    div []
        [ img
            [ src url
            , class "ui small image"
            ]
            []
        ]


viewPreviousMeasurement : FloatFormConfig value -> Language -> value -> Html any
viewPreviousMeasurement config language previousValue =
    [ previousValue
        |> config.storedValue
        |> Trans.PreviousFloatMeasurement
        |> translate language
    , " "
    , translate language config.unit
    ]
        |> List.map text
        |> div []


{-| Show a diff of values, if they were gained or lost.
-}
viewFloatDiff : FloatFormConfig value -> Language -> value -> Float -> Html any
viewFloatDiff config language previousValue currentValue =
    let
        previousFloatValue =
            config.storedValue previousValue

        diff =
            Round.round 2 <|
                abs (currentValue - previousFloatValue)

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
                    , text <| diff ++ " " ++ translate language config.unit
                    ]
    in
        if currentValue == previousFloatValue then
            -- No change in the values.
            emptyNode
        else if currentValue > previousFloatValue then
            viewMessage True
        else
            viewMessage False


viewPhoto : Language -> WebData () -> ( Maybe FileId, Maybe PhotoValue ) -> Html MsgChild
viewPhoto language saveStatus ( fileId, photoValue ) =
    let
        hasFileId =
            isJust fileId

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
                , Maybe.map viewPhotoThumb photoValue
                    |> showMaybe
                , div [ class "dropzone" ] []
                ]
            , div [ class "actions" ] <|
                saveButton language
                    (SendOutMsgChild SavePhoto)
                    hasFileId
                    saveStatus
                    (Just "column")
            ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> msg -> Bool -> WebData () -> Maybe String -> List (Html msg)
saveButton language msg hasInput saveStatus maybeDivClass =
    let
        isLoading =
            saveStatus == Loading

        isFailure =
            RemoteData.isFailure saveStatus

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
        , [ text <| translate language Trans.SaveError ]
            |> div []
            |> showIf isFailure
        ]


viewNutritionSigns : Language -> WebData () -> EverySet ChildNutritionSign -> Html MsgChild
viewNutritionSigns language saveStatus signs =
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
                    :: viewNutritionSignsSelector language signs
            ]
        , div [ class "actions" ] <|
            saveButton
                language
                (SendOutMsgChild SaveChildNutritionSigns)
                (not (EverySet.isEmpty signs))
                saveStatus
                Nothing
        ]


viewNutritionSignsSelector : Language -> EverySet ChildNutritionSign -> List (Html MsgChild)
viewNutritionSignsSelector language nutritionSigns =
    let
        nutrionSignsAndTranslationIdsFirst =
            [ Edema, AbdominalDisortion, DrySkin ]

        nutrionSignsAndTranslationIdsSecond =
            [ Apathy, PoorAppetite, BrittleHair ]
    in
        [ div [ class "ui grid" ]
            [ nutrionSignsAndTranslationIdsFirst
                |> List.map (viewNutritionSignsSelectorItem language nutritionSigns)
                |> div [ class "eight wide column" ]
            , nutrionSignsAndTranslationIdsSecond
                |> List.map (viewNutritionSignsSelectorItem language nutritionSigns)
                |> div [ class "eight wide column" ]
            ]
        , div [ class "ui divider" ] []
        , viewNutritionSignsSelectorItem language nutritionSigns None
        ]


{-| Helper function to return a tuples of checkbox label and attributes value.

For each nutrition sign the function will return a the translaed label of the
checkbox and a value for the id and for attributes.

-}
viewNutritionSignsSelectorItem : Language -> EverySet ChildNutritionSign -> ChildNutritionSign -> Html MsgChild
viewNutritionSignsSelectorItem language nutritionSigns sign =
    let
        inputId =
            encodeNutritionSignAsString sign

        isChecked =
            EverySet.member sign nutritionSigns
    in
        div [ class "ui checkbox" ]
            [ input
                ([ type_ "checkbox"
                 , id inputId
                 , name inputId
                 , onClick <| SelectNutritionSign (not isChecked) sign
                 , checked isChecked
                 ]
                    ++ (if isChecked then
                            [ class "checked" ]
                        else
                            []
                       )
                )
                []
            , label [ for inputId ]
                [ text <| translate language (Trans.ChildNutritionSignLabel sign) ]
            ]


type alias MotherMeasurementData =
    { previous : MotherMeasurements
    , current : MotherMeasurements
    , edits : MotherEdits
    , status : WebData ()
    }


viewMother : Language -> MotherActivityType -> MeasurementData MotherMeasurements MotherEdits -> ModelMother -> Html MsgMother
viewMother language activity measurements model =
    case activity of
        FamilyPlanning ->
            viewFamilyPlanning language measurements.status model.familyPlanningSigns


viewFamilyPlanning : Language -> WebData () -> EverySet FamilyPlanningSign -> Html MsgMother
viewFamilyPlanning language saveStatus signs =
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
                    :: viewFamilyPlanningSelector language signs
            ]
        , div [ class "actions" ] <|
            saveButton language
                (SendOutMsgMother SaveFamilyPlanningSigns)
                (not (EverySet.isEmpty signs))
                saveStatus
                Nothing
        ]


viewFamilyPlanningSelector : Language -> EverySet FamilyPlanningSign -> List (Html MsgMother)
viewFamilyPlanningSelector language familyPlanningSigns =
    let
        familyPlanningSignFirst =
            [ Pill, Condoms, IUD ]

        familyPlanningSignSecond =
            [ Injection, Necklace ]
    in
        [ div [ class "ui grid" ]
            [ familyPlanningSignFirst
                |> List.map (viewFamilyPlanningSelectorItem language familyPlanningSigns)
                |> div [ class "eight wide column" ]
            , familyPlanningSignSecond
                |> List.map (viewFamilyPlanningSelectorItem language familyPlanningSigns)
                |> div [ class "eight wide column" ]
            ]
        , div [ class "ui divider" ] []
        , viewFamilyPlanningSelectorItem language familyPlanningSigns NoFamilyPlanning
        ]


viewFamilyPlanningSelectorItem : Language -> EverySet FamilyPlanningSign -> FamilyPlanningSign -> Html MsgMother
viewFamilyPlanningSelectorItem language familyPlanningSigns sign =
    let
        inputId =
            encodeFamilyPlanningSignAsString sign

        isChecked =
            EverySet.member sign familyPlanningSigns
    in
        div [ class "ui checkbox" ]
            [ input
                ([ type_ "checkbox"
                 , id inputId
                 , name inputId
                 , onClick <| SelectFamilyPlanningSign (not isChecked) sign
                 , checked isChecked
                 ]
                    ++ (if isChecked then
                            [ class "checked" ]
                        else
                            []
                       )
                )
                []
            , label [ for inputId ]
                [ text <| translate language (Trans.FamilyPlanningSignLabel sign) ]
            ]
