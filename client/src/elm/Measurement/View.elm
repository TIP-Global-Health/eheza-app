module Measurement.View exposing (viewChild, viewMeasurementFloatDiff, viewMother, viewMuacIndication, zScoreForHeightOrLength)

{-| This module provides a form for entering measurements.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTiming(..), CounselingTopic)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (encodeFamilyPlanningSignAsString, encodeNutritionSignAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValues, fbfFormToValue, lactationFormToSigns, mapMeasurementData, muacIndication)
import Backend.Person.Model exposing (Gender, Person)
import Backend.Session.Model exposing (EditableSession)
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.Parser.Util exposing (toVirtualDom)
import Json.Decode
import Maybe.Extra exposing (isJust)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model exposing (..)
import Measurement.Utils exposing (..)
import Pages.Utils exposing (viewBoolInput, viewCheckBoxSelectInput, viewLabel, viewMeasurementInput, viewPhotoThumbFromPhotoUrl, viewQuestionLabel)
import RemoteData exposing (RemoteData(..), WebData, isFailure, isLoading)
import Restful.Endpoint exposing (fromEntityUuid)
import Round
import Translate as Trans exposing (Language, TranslationId, translate)
import Translate.Utils exposing (selectLanguage)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (Days(..), diffDays)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreLengthHeightForAge, zScoreWeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


{-| We need the current date in order to immediately construct a ZScore for the
child when we enter something.
-}
viewChild :
    Language
    -> NominalDate
    -> Bool
    -> Person
    -> ChildActivity
    -> MeasurementData ChildMeasurements
    -> ZScore.Model.Model
    -> EditableSession
    -> ModelChild
    -> PreviousMeasurementsValue
    -> Html MsgChild
viewChild language currentDate isChw child activity measurements zscores session model previousIndividualMeasurements =
    case activity of
        ChildFbf ->
            viewChildFbf language currentDate child session.offlineSession.session.clinicType (mapMeasurementData .fbf measurements) model.fbfForm

        ChildPicture ->
            viewPhoto language (mapMeasurementData .photo measurements) model.photo

        Height ->
            let
                previousIndividualHeight =
                    previousIndividualMeasurements.height
                        |> Maybe.map (\( date, HeightInCm val ) -> ( date, val ))
            in
            viewHeight language currentDate isChw child (mapMeasurementData .height measurements) previousIndividualHeight zscores model

        Muac ->
            let
                previousIndividualMuac =
                    previousIndividualMeasurements.muac
                        |> Maybe.map (\( date, MuacInCm val ) -> ( date, val ))
            in
            viewMuac language currentDate isChw child (mapMeasurementData .muac measurements) previousIndividualMuac zscores model

        NutritionSigns ->
            viewNutritionSigns language (mapMeasurementData .nutrition measurements) model.nutritionSigns

        -- Counseling ->
        --    viewCounselingSession language (mapMeasurementData .counselingSession measurements) session model.counseling
        Weight ->
            let
                previousIndividualWeight =
                    previousIndividualMeasurements.weight
                        |> Maybe.map (\( date, WeightInKg val ) -> ( date, val ))
            in
            viewWeight language currentDate isChw child (mapMeasurementData .weight measurements) previousIndividualWeight zscores model


{-| Some configuration for the `viewFloatForm` function, which handles several
different types of `Float` inputs.
-}
type alias FloatFormConfig id value =
    { blockName : String
    , activity : Activity
    , placeholderText : TranslationId
    , zScoreLabelForAge : TranslationId
    , zScoreForAge : Maybe (ZScore.Model.Model -> Days -> Gender -> Float -> Maybe ZScore)
    , zScoreForHeightOrLength : Maybe (ZScore.Model.Model -> Days -> Centimetres -> Gender -> Float -> Maybe ZScore)
    , constraints : FloatInputConstraints
    , unit : TranslationId
    , inputValue : ModelChild -> String
    , storedValue : value -> Float
    , dateMeasured : value -> NominalDate
    , viewIndication : Maybe (Language -> Float -> Html MsgChild)
    , updateMsg : String -> MsgChild
    , saveMsg : Maybe id -> Float -> MsgChild
    }


heightFormConfig : FloatFormConfig HeightId Height
heightFormConfig =
    { blockName = "height"
    , activity = ChildActivity Height
    , placeholderText = Trans.PlaceholderEnterHeight
    , zScoreLabelForAge = Trans.ZScoreHeightForAge
    , zScoreForAge = Just <| \model age gender height -> zScoreLengthHeightForAge model age gender (Centimetres height)
    , zScoreForHeightOrLength = Nothing
    , constraints = getInputConstraintsHeight
    , unit = Trans.CentimeterShorthand
    , inputValue = .height
    , storedValue = .value >> (\(HeightInCm val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateHeight
    , saveMsg = \id value -> SendOutMsgChild <| SaveHeight id (HeightInCm value)
    }


muacFormConfig : FloatFormConfig MuacId Muac
muacFormConfig =
    { blockName = "muac"
    , activity = ChildActivity Muac
    , placeholderText = Trans.PlaceholderEnterMUAC
    , zScoreLabelForAge = Trans.ZScoreMuacForAge
    , zScoreForAge = Nothing
    , zScoreForHeightOrLength = Nothing
    , constraints = getInputConstraintsMuac
    , unit = Trans.CentimeterShorthand
    , inputValue = .muac
    , storedValue = .value >> (\(MuacInCm val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Just <| \language val -> viewMuacIndication language (muacIndication (MuacInCm val))
    , updateMsg = UpdateMuac
    , saveMsg = \id value -> SendOutMsgChild <| SaveMuac id (MuacInCm value)
    }


weightFormConfig : FloatFormConfig WeightId Weight
weightFormConfig =
    { blockName = "weight"
    , activity = ChildActivity Weight
    , placeholderText = Trans.PlaceholderEnterWeight
    , zScoreLabelForAge = Trans.ZScoreWeightForAge
    , zScoreForAge = Just <| \model age gender weight -> zScoreWeightForAge model age gender (Kilograms weight)
    , zScoreForHeightOrLength = Just zScoreForHeightOrLength
    , constraints = getInputConstraintsWeight
    , unit = Trans.KilogramShorthand
    , inputValue = .weight
    , storedValue = .value >> (\(WeightInKg val) -> val)
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateWeight
    , saveMsg = \id value -> SendOutMsgChild <| SaveWeight id (WeightInKg value)
    }


zScoreForHeightOrLength : ZScore.Model.Model -> Days -> Centimetres -> Gender -> Float -> Maybe ZScore
zScoreForHeightOrLength model (Days days) (Centimetres cm) gender weight =
    if days < 731 then
        zScoreWeightForLength model (ZScore.Model.Length cm) gender (Kilograms weight)

    else
        zScoreWeightForHeight model (ZScore.Model.Height cm) gender (Kilograms weight)


viewHeight : Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( HeightId, Height )) -> Maybe ( NominalDate, Float ) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewHeight =
    viewFloatForm heightFormConfig


viewWeight : Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( WeightId, Weight )) -> Maybe ( NominalDate, Float ) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewWeight =
    viewFloatForm weightFormConfig


viewMuac : Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( MuacId, Muac )) -> Maybe ( NominalDate, Float ) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewMuac =
    viewFloatForm muacFormConfig


viewFloatForm : FloatFormConfig id value -> Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( id, value )) -> Maybe ( NominalDate, Float ) -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewFloatForm config language currentDate isChw child measurements previousIndividualValue zscores model =
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
            , Attr.min <| String.fromFloat config.constraints.minVal
            , Attr.max <| String.fromFloat config.constraints.maxVal
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

        -- What is the most recent measurement we've saved, either locally or
        -- to the backend (we don't care at the moment which). If this is a new
        -- measurement we haven't saved yet, this will be Nothing.
        savedMeasurement =
            measurements.current

        previousGroupValue =
            measurements.previous
                |> Maybe.map
                    (\( _, measurement ) ->
                        ( measurement |> config.dateMeasured
                        , measurement |> config.storedValue
                        )
                    )

        previousValue =
            resolvePreviousValueInCommonContext previousGroupValue previousIndividualValue

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
                |> Maybe.map (Tuple.second >> config.dateMeasured)
                |> Maybe.withDefault currentDate

        -- And, we'll need the child's age.
        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate dateMeasured)
                child.birthDate

        renderedZScoreForAge =
            config.zScoreForAge
                |> Maybe.map
                    (\zScoreForAge ->
                        let
                            zScoreText =
                                floatValue
                                    |> Maybe.andThen
                                        (\val ->
                                            Maybe.andThen
                                                (\ageInDays ->
                                                    zScoreForAge zscores ageInDays child.gender val
                                                )
                                                maybeAgeInDays
                                        )
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
            if isChw then
                Nothing

            else
                config.zScoreForHeightOrLength
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
                                        |> Maybe.andThen
                                            (\height ->
                                                Maybe.andThen
                                                    (\weight ->
                                                        Maybe.andThen
                                                            (\ageInDays ->
                                                                func zscores ageInDays (Centimetres height) child.gender weight
                                                            )
                                                            maybeAgeInDays
                                                    )
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

        saveMsg =
            floatValue
                |> Maybe.andThen
                    (\value ->
                        if not <| withinConstraints config.constraints value then
                            Nothing

                        else
                            config.saveMsg (Maybe.map Tuple.first measurements.current) value |> Just
                    )
    in
    div
        [ class <| "ui full segment " ++ config.blockName ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Trans.ActivitiesTitle config.activity)
                ]
            , p [ class "activity-helper" ] [ text <| translate language (Trans.ActivitiesHelp config.activity) ]
            , p [ class "range-helper" ] [ text <| translate language (Trans.AllowedValuesRangeHelper config.constraints) ]
            , div
                [ class "ui form" ]
                [ div [ class "ui grid" ]
                    [ div
                        [ class "eleven wide column" ]
                        [ div [ class "ui right labeled input" ]
                            [ div [ class "input-wrapper" ] [ input inputAttrs [] ]
                            , div
                                [ class "ui basic label" ]
                                [ text <| translate language config.unit ]
                            ]
                        ]
                    , div
                        [ class "five wide column" ]
                        [ showMaybe <|
                            Maybe.map2 (viewFloatDiff config language)
                                previousValue
                                floatValue
                        , showMaybe <|
                            Maybe.map2 (\func value -> func language value)
                                config.viewIndication
                                floatValue
                        ]
                    ]
                , previousValue
                    |> Maybe.map (viewPreviousMeasurement config language)
                    |> showMaybe
                ]
            , showMaybe renderedZScoreForAge
            , showMaybe renderedZScoreForHeight
            ]
        , div [ class "actions" ] <|
            saveButton language saveMsg measurements Nothing
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


viewPreviousMeasurement : FloatFormConfig id value -> Language -> Float -> Html any
viewPreviousMeasurement config language previousValue =
    [ previousValue
        |> Trans.PreviousFloatMeasurement
        |> translate language
    , " "
    , translate language config.unit
    ]
        |> List.map text
        |> div []


{-| Show a diff of values, if they were gained or lost.
-}
viewFloatDiff : FloatFormConfig id value -> Language -> Float -> Float -> Html any
viewFloatDiff config language previousValue currentValue =
    viewMeasurementFloatDiff language config.unit currentValue previousValue


viewMeasurementFloatDiff : Language -> TranslationId -> Float -> Float -> Html any
viewMeasurementFloatDiff language unit currentValue previousValue =
    let
        diff =
            Round.round 2 <|
                abs (currentValue - previousValue)

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
                , text <| diff ++ " " ++ translate language unit
                ]
    in
    if currentValue == previousValue then
        -- No change in the values.
        emptyNode

    else if currentValue > previousValue then
        viewMessage True

    else
        viewMessage False


viewPhoto : Language -> MeasurementData (Maybe ( PhotoId, Photo )) -> Maybe PhotoUrl -> Html MsgChild
viewPhoto language measurement photo =
    let
        activity =
            ChildActivity ChildPicture

        photoId =
            Maybe.map Tuple.first measurement.current

        -- If we have a photo that we've just taken, but not saved, that is in
        -- `photo`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        displayPhoto =
            case photo of
                Just url ->
                    Just url

                Nothing ->
                    Maybe.map (Tuple.second >> .value)
                        measurement.current
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
                [ Maybe.map viewPhotoThumbFromPhotoUrl displayPhoto
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
                ]
            ]
        , keyed "button" <|
            div [ class "actions" ] <|
                saveButton language
                    (Maybe.map (SendOutMsgChild << SavePhoto photoId) photo)
                    measurement
                    (Just "column")
        ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Maybe msg -> MeasurementData (Maybe a) -> Maybe String -> List (Html msg)
saveButton language msg measurement maybeDivClass =
    let
        isLoading =
            measurement.update == Loading

        isFailure =
            RemoteData.isFailure measurement.update

        ( updateText, errorText ) =
            if isJust measurement.current then
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


viewNutritionSigns : Language -> MeasurementData (Maybe ( ChildNutritionId, ChildNutrition )) -> EverySet ChildNutritionSign -> Html MsgChild
viewNutritionSigns language measurement signs =
    let
        activity =
            ChildActivity NutritionSigns

        existingId =
            Maybe.map Tuple.first measurement.current

        saveMsg =
            if EverySet.isEmpty signs then
                Nothing

            else
                Just <| SendOutMsgChild <| SaveChildNutritionSigns existingId signs
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
    , viewNutritionSignsSelectorItem language nutritionSigns NormalChildNutrition
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
            [ type_ "checkbox"
            , id inputId
            , name inputId
            , onClick <| SelectNutritionSign (not isChecked) sign
            , checked isChecked
            , classList [ ( "checked", isChecked ) ]
            ]
            []
        , label [ for inputId ]
            [ text <| translate language (Trans.ChildNutritionSignLabel sign) ]
        ]


viewChildFbf : Language -> NominalDate -> Person -> ClinicType -> MeasurementData (Maybe ( ChildFbfId, Fbf )) -> FbfForm -> Html MsgChild
viewChildFbf language currentDate child clinicType measurement form =
    let
        activity =
            ChildActivity ChildFbf

        existingId =
            Maybe.map Tuple.first measurement.current

        notice =
            if isJust form.distributionNotice then
                form.distributionNotice

            else
                Just DistributedFully

        saveMsg =
            form.distributedAmount
                |> Maybe.map
                    (\_ ->
                        { form | distributionNotice = notice }
                            |> fbfFormToValue
                            |> SaveChildFbf existingId
                            |> SendOutMsgChild
                    )
    in
    viewFbfForm language
        measurement
        activity
        clinicType
        SetDistributedAmountForChild
        SetDistributoinNoticeForChild
        saveMsg
        form



{-
   viewCounselingSession : Language -> MeasurementData (Maybe ( CounselingSessionId, CounselingSession )) -> EditableSession -> Maybe ( CounselingTiming, EverySet CounselingTopicId ) -> Html MsgChild
   viewCounselingSession language measurement session value =
       let
           existingId =
               Maybe.map Tuple.first measurement.current
       in
       case value of
           Nothing ->
               emptyNode

           Just ( timing, topics ) ->
               let
                   activity =
                       ChildActivity Counseling

                   allTopicsChecked =
                       Dict.all
                           (\id _ -> EverySet.member id topics)
                           expected

                   completed =
                       isJust measurement.current

                   -- For counseling sessions, we don't allow saves unless all the
                   -- topics are checked. Also, we don't allow an update if the
                   -- activity has been completed. That is, once the nurse says the
                   -- counseling session is done, the nurse cannot correct that.
                   saveMsg =
                       if allTopicsChecked && not completed then
                           SaveCounselingSession existingId timing topics
                               |> SendOutMsgChild
                               |> Just

                       else
                           Nothing

                   expected =
                       session.offlineSession.everyCounselingSchedule
                           |> Dict.get timing
                           |> Maybe.withDefault Dict.empty
               in
               div
                   [ class "ui full segment counseling"
                   , id "counselingSessionEntryForm"
                   ]
                   [ div [ class "content" ]
                       [ h3 [ class "ui header" ]
                           [ text <| translate language (Trans.ActivitiesTitle activity)
                           ]
                       , h3 [ class "ui header" ]
                           [ text <| translate language (Trans.CounselingTimingHeading timing)
                           ]
                       , div [ class "ui form" ] <|
                           p [] [ text <| translate language (Trans.ActivitiesLabel activity) ]
                               :: viewCounselingTopics language completed expected topics
                       ]
                   , div [ class "actions" ] <|
                       saveButton
                           language
                           saveMsg
                           measurement
                           Nothing
                   ]
-}


viewCounselingTopics : Language -> Bool -> Dict CounselingTopicId CounselingTopic -> EverySet CounselingTopicId -> List (Html MsgChild)
viewCounselingTopics language completed expectedTopics selectedTopics =
    expectedTopics
        |> Dict.map
            (\topicId topic ->
                let
                    inputId =
                        "counseling-checkbox-" ++ fromEntityUuid topicId

                    isChecked =
                        EverySet.member topicId selectedTopics
                in
                div
                    [ class "ui checkbox activity" ]
                    [ input
                        [ type_ "checkbox"
                        , id inputId
                        , name inputId
                        , onClick <| SelectCounselingTopic (not isChecked) topicId
                        , checked isChecked
                        , classList [ ( "checked", isChecked ) ]
                        , disabled completed
                        ]
                        []
                    , label
                        [ for inputId ]
                        [ text <| translate language (Trans.CounselingTopic topic) ]
                    ]
            )
        |> Dict.values


type alias MotherMeasurementData =
    { previous : MotherMeasurements
    , current : MotherMeasurements
    , status : WebData ()
    }


viewMother : Language -> NominalDate -> Person -> MotherActivity -> ClinicType -> MeasurementData MotherMeasurements -> ModelMother -> Html MsgMother
viewMother language currentDate mother activity clinicType measurements model =
    case activity of
        FamilyPlanning ->
            viewFamilyPlanning language (mapMeasurementData .familyPlanning measurements) model.familyPlanningSigns

        Lactation ->
            viewLactation language (mapMeasurementData .lactation measurements) model.lactationForm

        MotherFbf ->
            viewMotherFbf language currentDate mother clinicType (mapMeasurementData .fbf measurements) model.fbfForm

        ParticipantConsent ->
            viewParticipantConsent language (mapMeasurementData .consent measurements) model.participantConsent


viewParticipantConsent : Language -> MeasurementData (Dict ParticipantConsentId ParticipantConsent) -> ParticipantFormUI -> Html MsgMother
viewParticipantConsent language measurement ui =
    let
        activity =
            MotherActivity ParticipantConsent

        viewParticipantForm formId form =
            let
                completed =
                    EverySet.member formId completedFormIds

                iconVisibility =
                    if completed then
                        "visible"

                    else
                        "hidden"
            in
            div
                [ class "item"
                , onClick <| ViewParticipantForm <| Just formId
                ]
                [ div
                    [ class "ui image icon-item icon-item-forms"
                    , style "height" "140px"
                    , style "width" "140px"
                    ]
                    []
                , div
                    [ class "middle aligned content" ]
                    [ div
                        [ style "float" "right" ]
                        [ i
                            [ class "icon check circle outline green large"
                            , style "visibility" iconVisibility
                            ]
                            []
                        ]
                    , div
                        [ class "consent-form-list" ]
                        [ text <| selectLanguage language form.title ]
                    ]
                ]

        viewFormList expected =
            let
                completedLast =
                    expected
                        |> Dict.partition (\id _ -> EverySet.member id completedFormIds)
                        |> (\( completed, todo ) -> Dict.union todo completed)
            in
            div
                [ class "ui full segment participant-consent"
                , id "participantConsentForm"
                ]
                [ div [ class "content" ]
                    [ h3
                        [ class "ui header" ]
                        [ text <| translate language (Trans.ActivitiesTitle activity)
                        ]
                    , p [] [ text <| translate language (Trans.ActivitiesHelp activity) ]
                    , completedLast
                        |> Dict.map viewParticipantForm
                        |> Dict.values
                        |> div [ class "ui items" ]
                    ]
                ]

        completedFormIds =
            currentValues measurement
                |> List.map (Tuple.second >> .value >> .formId)
                |> EverySet.fromList

        viewForm formId expected =
            let
                backIcon =
                    a
                        [ class "icon-back"
                        , onClick <| ViewParticipantForm Nothing
                        ]
                        []

                completed =
                    EverySet.member formId completedFormIds

                form =
                    Dict.get formId expected

                progress =
                    Dict.get formId ui.progress
                        |> Maybe.withDefault emptyParticipantFormProgress

                progressCompleted =
                    progress.participantSigned && progress.counselorSigned

                titleText =
                    Maybe.map (selectLanguage language << .title) form
                        |> Maybe.withDefault ""

                title =
                    h1
                        [ class "ui report header"
                        , style "padding-left" "60px"
                        , style "padding-right" "60px"
                        ]
                        [ text titleText ]

                body =
                    Maybe.map (toVirtualDom << .parsed << selectLanguage language << .body) form
                        |> Maybe.withDefault []
                        |> div []

                counselorReviewed =
                    div [ class "ui checkbox activity" ]
                        [ input
                            [ type_ "checkbox"
                            , id "counselor-reviewed"
                            , name "counselor-reviewed"
                            , onClick <| SetCounselorSigned formId (not progress.counselorSigned)
                            , checked progress.counselorSigned
                            , classList [ ( "checked", progress.counselorSigned ) ]
                            , disabled completed
                            ]
                            []
                        , label
                            [ for "counselor-reviewed" ]
                            [ text <| translate language Trans.CounselorReviewed ]
                        ]

                participantReviewed =
                    div [ class "ui checkbox activity" ]
                        [ input
                            [ type_ "checkbox"
                            , id "participant-reviewed"
                            , name "participant-reviewed"
                            , onClick <| SetParticipantSigned formId (not progress.participantSigned)
                            , checked progress.participantSigned
                            , classList [ ( "checked", progress.participantSigned ) ]
                            , disabled completed
                            ]
                            []
                        , label
                            [ for "participant-reviewed" ]
                            [ text <| translate language Trans.ParticipantReviewed ]
                        ]

                msg =
                    if completed || not progressCompleted then
                        Nothing

                    else
                        -- If the form has already been consented to (i.e.
                        -- completed), we don't allow any edits. So, if we get
                        -- here, we don't have a current ParticipantConsentId
                        -- to supply -- it's always new.
                        SaveCompletedForm Nothing formId language
                            |> SendOutMsgMother
                            |> Just

                isLoading =
                    measurement.update == Loading

                isFailure =
                    RemoteData.isFailure measurement.update

                ( updateText, errorText ) =
                    if EverySet.member formId completedFormIds then
                        ( Trans.Update, Trans.UpdateError )

                    else
                        ( Trans.Save, Trans.SaveError )

                saveAttr =
                    if isLoading then
                        []

                    else
                        Maybe.map onClick msg
                            |> Maybe.Extra.toList

                saveButton_ =
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
            in
            viewModal <|
                Just <|
                    divKeyed
                        [ class "page-form" ]
                        [ div
                            [ class "wrap-form"
                            ]
                            [ div
                                [ class "form-title" ]
                                [ backIcon
                                , title
                                ]
                            , div
                                [ class "form-body" ]
                                [ body
                                , hr [] []
                                , h2 [] [ text <| translate language Trans.ParticipantSignature ]
                                , participantReviewed
                                , h2 [] [ text <| translate language Trans.CounselorSignature ]
                                , counselorReviewed
                                ]
                            , div [ class "actions" ] saveButton_
                            ]
                            -- Use keys to force the browser to re-scroll when we switch forms.
                            |> keyed ("consent-form-" ++ fromEntityUuid formId)
                        ]
    in
    case ui.view of
        Nothing ->
            viewFormList ui.expected

        Just formId ->
            viewForm formId ui.expected


viewFamilyPlanning : Language -> MeasurementData (Maybe ( FamilyPlanningId, FamilyPlanning )) -> EverySet FamilyPlanningSign -> Html MsgMother
viewFamilyPlanning language measurement signs =
    let
        activity =
            MotherActivity FamilyPlanning

        existingId =
            Maybe.map Tuple.first measurement.current

        saveMsg =
            if EverySet.isEmpty signs then
                Nothing

            else
                Just <| SendOutMsgMother <| SaveFamilyPlanningSigns existingId signs
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
            [ AutoObservation, Condoms, CycleBeads, CycleCounting, Hysterectomy, Implants, Injectables ]

        familyPlanningSignSecond =
            [ IUD, LactationAmenorrhea, OralContraceptives, Spermicide, TubalLigatures, Vasectomy ]
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


viewLactation : Language -> MeasurementData (Maybe ( LactationId, Lactation )) -> LactationForm -> Html MsgMother
viewLactation language measurement form =
    let
        activity =
            MotherActivity Lactation

        existingId =
            Maybe.map Tuple.first measurement.current

        saveMsg =
            form.breastfeeding
                |> Maybe.map
                    (\_ ->
                        lactationFormToSigns form
                            |> SaveLactation existingId
                            |> SendOutMsgMother
                    )
    in
    div
        [ class "ui full segment lactation"
        , id "lactationEntryForm"
        ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Trans.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Trans.ActivitiesHelp activity) ]
            , div [ class "ui form" ] <|
                [ viewQuestionLabel language Trans.IsCurrentlyBreastfeeding
                , viewBoolInput language
                    form.breastfeeding
                    (SelectLactationSign Breastfeeding)
                    "breastfeeding"
                    Nothing
                ]
            ]
        , div [ class "actions" ] <|
            saveButton language
                saveMsg
                measurement
                Nothing
        ]


viewMotherFbf : Language -> NominalDate -> Person -> ClinicType -> MeasurementData (Maybe ( MotherFbfId, Fbf )) -> FbfForm -> Html MsgMother
viewMotherFbf language currentDate mother clinicType measurement form =
    let
        activity =
            MotherActivity MotherFbf

        existingId =
            Maybe.map Tuple.first measurement.current

        notice =
            if isJust form.distributionNotice then
                form.distributionNotice

            else
                Just DistributedFully

        saveMsg =
            form.distributedAmount
                |> Maybe.map
                    (\_ ->
                        { form | distributionNotice = notice }
                            |> fbfFormToValue
                            |> SaveMotherFbf existingId
                            |> SendOutMsgMother
                    )
    in
    viewFbfForm language
        measurement
        activity
        clinicType
        SetDistributedAmountForMother
        SetDistributoinNoticeForMother
        saveMsg
        form


viewFbfForm :
    Language
    -> MeasurementData (Maybe a)
    -> Activity
    -> ClinicType
    -> (String -> msg)
    -> (DistributionNotice -> msg)
    -> Maybe msg
    -> FbfForm
    -> Html msg
viewFbfForm language measurement activity clinicType setDistributedAmountMsg setDistributoinNoticeMsg saveMsg form =
    let
        selectQuantityInput =
            option
                [ value ""
                , selected (form.distributedAmount == Nothing)
                ]
                [ text "" ]
                :: (List.repeat 11 ""
                        |> List.indexedMap
                            (\index _ ->
                                let
                                    indexAsString =
                                        String.fromInt index
                                in
                                option
                                    [ value indexAsString
                                    , selected (form.distributedAmount == Just (toFloat index))
                                    ]
                                    [ text indexAsString ]
                            )
                   )
                |> select [ onInput setDistributedAmountMsg, class "fbf-distirbution" ]

        formContentCommon =
            div [ class "form-input measurement quantity" ]
                [ selectQuantityInput
                , div [ class "unit" ]
                    [ text <| translate language quantityUnitLabel ]
                ]

        formContentByClinicType =
            case clinicType of
                Achi ->
                    []

                _ ->
                    [ viewLabel language <| Trans.WasFbfDistirbuted activity
                    , viewCheckBoxSelectInput language
                        [ DistributedPartiallyLackOfStock, DistributedPartiallyOther ]
                        []
                        form.distributionNotice
                        setDistributoinNoticeMsg
                        Trans.DistributionNotice
                    ]

        ( quantityUnitLabel, activityLabel ) =
            case clinicType of
                Achi ->
                    ( Trans.KilogramShorthand, Trans.ActivitityLabelAchi )

                _ ->
                    ( Trans.PackagesPerMonth, Trans.ActivitiesLabel activity )
    in
    div
        [ class "ui full segment fbf"
        , id "fbfEntryForm"
        ]
        [ div [ class "content" ]
            [ h3 [ class "ui header" ]
                [ text <| translate language (Trans.FbfDistribution clinicType) ]
            , p [] [ text <| translate language activityLabel ]
            , formContentCommon
                :: formContentByClinicType
                |> div [ class "ui form" ]
            ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
                Nothing
        ]
