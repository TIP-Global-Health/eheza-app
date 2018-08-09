module Measurement.View exposing (viewChild, viewMother, viewMuacIndication)

{-| This module provides a form for entering measurements.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Backend.Child.Model exposing (Child, Gender)
import Backend.Measurement.Encoder exposing (encodeFamilyPlanningSignAsString, encodeNutritionSignAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (applyEdit, mapMeasurementData, muacIndication)
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Json.Decode
import Maybe.Extra exposing (isJust)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import RemoteData exposing (RemoteData(..), WebData, isFailure, isLoading)
import Round
import Translate as Trans exposing (Language(..), TranslationId, translate)
import Utils.Html exposing (script)
import Utils.NominalDate exposing (Days(..), diffDays)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreHeightForAge, zScoreWeightForAge, zScoreWeightForHeight)


{-| We need the current date in order to immediately construct a ZScore for the
child when we enter something.
-}
viewChild : Language -> NominalDate -> Child -> ChildActivity -> MeasurementData ChildMeasurements ChildEdits -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewChild language currentDate child activity measurements zscores model =
    case activity of
        ChildPicture ->
            viewPhoto language (mapMeasurementData (Maybe.map Tuple.second << .photo) .photo measurements) model.photo

        Height ->
            viewHeight language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .height) .height measurements) zscores model

        Muac ->
            viewMuac language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .muac) .muac measurements) zscores model

        NutritionSigns ->
            viewNutritionSigns language (mapMeasurementData (Maybe.map Tuple.second << .nutrition) .nutrition measurements) model.nutritionSigns

        Weight ->
            viewWeight language currentDate child (mapMeasurementData (Maybe.map Tuple.second << .weight) .weight measurements) zscores model


{-| Some configuration for the `viewFloatForm` function, which handles several
different types of `Float` inputs.
-}
type alias FloatFormConfig value =
    { blockName : String
    , activity : Activity
    , placeholderText : TranslationId
    , zScoreLabelForAge : TranslationId
    , zScoreForAge : Maybe (ZScore.Model.Model -> Days -> Gender -> Float -> Maybe ZScore)
    , zScoreForHeight : Maybe (ZScore.Model.Model -> Centimetres -> Gender -> Float -> Maybe ZScore)
    , constraints : FloatInputConstraints
    , unit : TranslationId
    , inputValue : ModelChild -> String
    , storedValue : value -> Float
    , dateMeasured : value -> NominalDate
    , viewIndication : Maybe (Language -> Float -> Html MsgChild)
    , updateMsg : String -> MsgChild
    , saveMsg : Float -> MsgChild
    }


heightFormConfig : FloatFormConfig Height
heightFormConfig =
    { blockName = "height"
    , activity = ChildActivity Height
    , placeholderText = Trans.PlaceholderEnterHeight
    , zScoreLabelForAge = Trans.ZScoreHeightForAge
    , zScoreForAge = Just <| \model age gender height -> zScoreHeightForAge model age gender (Centimetres height)
    , zScoreForHeight = Nothing
    , constraints = getInputConstraintsHeight
    , unit = Trans.CentimeterShorthand
    , inputValue = .height
    , storedValue = .value >> (\(HeightInCm val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateHeight
    , saveMsg = SendOutMsgChild << SaveHeight << HeightInCm
    }


muacFormConfig : FloatFormConfig Muac
muacFormConfig =
    { blockName = "muac"
    , activity = ChildActivity Muac
    , placeholderText = Trans.PlaceholderEnterMUAC
    , zScoreLabelForAge = Trans.ZScoreMuacForAge
    , zScoreForAge = Nothing
    , zScoreForHeight = Nothing
    , constraints = getInputConstraintsMuac
    , unit = Trans.CentimeterShorthand
    , inputValue = .muac
    , storedValue = .value >> (\(MuacInCm val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Just <| \language val -> viewMuacIndication language (muacIndication (MuacInCm val))
    , updateMsg = UpdateMuac
    , saveMsg = SendOutMsgChild << SaveMuac << MuacInCm
    }


weightFormConfig : FloatFormConfig Weight
weightFormConfig =
    { blockName = "weight"
    , activity = ChildActivity Weight
    , placeholderText = Trans.PlaceholderEnterWeight
    , zScoreLabelForAge = Trans.ZScoreWeightForAge
    , zScoreForAge = Just <| \model age gender weight -> zScoreWeightForAge model age gender (Kilograms weight)
    , zScoreForHeight = Just <| \model height gender weight -> zScoreWeightForHeight model height gender (Kilograms weight)
    , constraints = getInputConstraintsWeight
    , unit = Trans.KilogramShorthand
    , inputValue = .weight
    , storedValue = .value >> (\(WeightInKg val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateWeight
    , saveMsg = SendOutMsgChild << SaveWeight << WeightInKg
    }


viewHeight : Language -> NominalDate -> Child -> MeasurementData (Maybe Height) (Edit Height) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewHeight =
    viewFloatForm heightFormConfig


viewWeight : Language -> NominalDate -> Child -> MeasurementData (Maybe Weight) (Edit Weight) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewWeight =
    viewFloatForm weightFormConfig


viewMuac : Language -> NominalDate -> Child -> MeasurementData (Maybe Muac) (Edit Muac) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewMuac =
    viewFloatForm muacFormConfig


viewFloatForm : FloatFormConfig value -> Language -> NominalDate -> Child -> MeasurementData (Maybe value) (Edit value) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewFloatForm config language currentDate child measurements zscores model =
    let
        -- What is the string input value from the form?
        inputValue =
            config.inputValue model

        -- Our input is a string, which may or may not be a valid float,
        -- since we want to let users enter things like "." to start with
        -- without clobbering what they type.
        inputAttrs =
            [ type_ "number"
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
                |> Maybe.withDefault currentDate

        -- And, we'll need the child's age.
        ageInDays =
            diffDays child.birthDate dateMeasured

        renderedZScoreForAge =
            config.zScoreForAge
                |> Maybe.map
                    (\zScoreForAge ->
                        let
                            zScoreText =
                                floatValue
                                    |> Maybe.andThen (\val -> zScoreForAge zscores ageInDays child.gender val)
                                    |> Maybe.map viewZScore
                                    |> Maybe.withDefault (translate language Trans.NotAvailable)
                        in
                        div
                            [ class "ui large header z-score age" ]
                            [ text <| translate language config.zScoreLabelForAge
                            , span
                                [ class "sub header" ]
                                [ text zScoreText ]
                            ]
                    )

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
                                                (\weight -> func zscores (Centimetres height) child.gender weight)
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
                [ text <| translate language (Trans.ActivitiesTitle config.activity)
                ]
            , p
                []
                [ text <| translate language (Trans.ActivitiesLabel config.activity) ]
            , div
                [ class "ui form" ]
                [ div [ class "ui grid" ]
                    [ div
                        [ class "eleven wide column" ]
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
            , showMaybe renderedZScoreForAge
            , showMaybe renderedZScoreForHeight
            ]
        , div [ class "actions" ] <|
            saveButton language
                (Maybe.map config.saveMsg floatValue)
                measurements
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
viewPhotoThumb photo =
    div []
        [ img
            [ src photo.url
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


viewPhoto : Language -> MeasurementData (Maybe Photo) (Edit Photo) -> Maybe PhotoValue -> Html MsgChild
viewPhoto language measurement photo =
    let
        activity =
            ChildActivity ChildPicture
    in
    divKeyed
        [ class "ui full segment photo" ]
        [ keyedDivKeyed "content"
            [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Trans.ActivitiesTitle activity) ]
                |> keyed "title"
            , p [] [ text <| translate language (Trans.ActivitiesHelp activity) ]
                |> keyed "help"
            , keyedDivKeyed "grid"
                [ class "ui grid" ]
                [ Maybe.map viewPhotoThumb photo
                    |> showMaybe
                    |> List.singleton
                    |> div [ class "eight wide column" ]
                    |> keyed "thumbnail"
                , div
                    [ id "dropzone"
                    , class "eight wide column dropzone"
                    , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
                    ]
                    [ div
                        [ class "dz-message"
                        , attribute "data-dz-message" ""
                        ]
                        [ span
                            []
                            [ text <| translate language Trans.DropzoneDefaultMessage ]
                        ]
                    ]
                    |> keyed "dropzone"

                -- This runs the function from our `app.js` at the precise moment this gets
                -- written to the DOM. Isn't that convenient?
                , script "bindDropZone()"
                    |> keyed "script"
                ]
            ]
        , keyed "button" <|
            div [ class "actions" ] <|
                saveButton language
                    (Maybe.map (SendOutMsgChild << SavePhoto) photo)
                    measurement
                    (Just "column")
        ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Maybe msg -> MeasurementData (Maybe a) (Edit a) -> Maybe String -> List (Html msg)
saveButton language msg measurement maybeDivClass =
    let
        isLoading =
            measurement.update == Loading

        isFailure =
            RemoteData.isFailure measurement.update

        ( updateText, errorText ) =
            if isJust <| applyEdit measurement.edits measurement.current then
                ( Trans.Update, Trans.UpdateError )
            else
                ( Trans.Save, Trans.SaveError )

        saveAttr =
            if isLoading then
                []
            else
                Maybe.map onClick msg
                    |> Maybe.Extra.toList
    in
    [ button
        ([ classList
            [ ( "ui fluid primary button", True )
            , ( "loading", isLoading )
            , ( "negative", isFailure )
            , ( "disabled", Maybe.Extra.isNothing msg )
            ]
         , id "save-form"
         ]
            ++ saveAttr
        )
        [ text <| translate language updateText
        ]
    , [ text <| translate language errorText ]
        |> div []
        |> showIf isFailure
    ]


viewNutritionSigns : Language -> MeasurementData (Maybe ChildNutrition) (Edit ChildNutrition) -> EverySet ChildNutritionSign -> Html MsgChild
viewNutritionSigns language measurement signs =
    let
        activity =
            ChildActivity NutritionSigns

        saveMsg =
            if EverySet.isEmpty signs then
                Nothing
            else
                Just <| SendOutMsgChild <| SaveChildNutritionSigns signs
    in
    div
        [ class "ui full segment nutrition"
        , id "nutritionSignsEntryForm"
        ]
        [ div [ class "content" ]
            [ h3 [ class "ui header" ]
                [ text <| translate language (Trans.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Trans.ActivitiesHelp activity) ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language (Trans.ActivitiesLabel activity) ]
                    :: viewNutritionSignsSelector language signs
            ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
                Nothing
        ]


viewNutritionSignsSelector : Language -> EverySet ChildNutritionSign -> List (Html MsgChild)
viewNutritionSignsSelector language nutritionSigns =
    let
        nutrionSignsAndTranslationIdsFirst =
            [ Edema, AbdominalDistension, DrySkin ]

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
    div [ class "ui checkbox activity" ]
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


viewMother : Language -> MotherActivity -> MeasurementData MotherMeasurements MotherEdits -> ModelMother -> Html MsgMother
viewMother language activity measurements model =
    case activity of
        FamilyPlanning ->
            viewFamilyPlanning language (mapMeasurementData (Maybe.map Tuple.second << .familyPlanning) .familyPlanning measurements) model.familyPlanningSigns


viewFamilyPlanning : Language -> MeasurementData (Maybe FamilyPlanning) (Edit FamilyPlanning) -> EverySet FamilyPlanningSign -> Html MsgMother
viewFamilyPlanning language measurement signs =
    let
        activity =
            MotherActivity FamilyPlanning

        saveMsg =
            if EverySet.isEmpty signs then
                Nothing
            else
                Just <| SendOutMsgMother <| SaveFamilyPlanningSigns signs
    in
    div
        [ class "ui full segment family-planning"
        , id "familyPlanningEntryForm"
        ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Trans.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Trans.ActivitiesHelp activity) ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language (Trans.ActivitiesLabel activity) ]
                    :: viewFamilyPlanningSelector language signs
            ]
        , div [ class "actions" ] <|
            saveButton language
                saveMsg
                measurement
                Nothing
        ]


viewFamilyPlanningSelector : Language -> EverySet FamilyPlanningSign -> List (Html MsgMother)
viewFamilyPlanningSelector language familyPlanningSigns =
    let
        familyPlanningSignFirst =
            [ Pill, Condoms, IUD ]

        familyPlanningSignSecond =
            [ Implant, Injection, Necklace ]
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
    div [ class "ui checkbox activity" ]
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
