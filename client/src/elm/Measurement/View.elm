module Measurement.View exposing (FloatFormConfig, birthWeightInputsAndTasks, contributingFactorsFormInutsAndTasks, followUpFormInputsAndTasks, healthEducationFormInutsAndTasks, heightFormAndTasks, muacFormInputsAndTasks, nutritionCaringInputsAndTasks, nutritionFeedingInputsAndTasks, nutritionFoodSecurityInputsAndTasks, nutritionFormInputsAndTasks, nutritionHygieneInputsAndTasks, referToProgramFormInputsAndTasks, sendToFacilityInputsAndTasks, viewActionTakenLabel, viewChild, viewColorAlertIndication, viewContributingFactorsForm, viewCorePhysicalExamForm, viewFamilyPlanningForm, viewFamilyPlanningInput, viewFollowUpForm, viewHealthEducationForm, viewHeightForm, viewMeasurementFloatDiff, viewMedicationAdministrationForm, viewMother, viewMuacForm, viewMultipleTreatmentWithDosage, viewNCDAContent, viewNutritionFollowUpForm, viewNutritionForm, viewReferToProgramForm, viewSendToHealthCenterForm, viewSendToHospitalForm, viewTreatmentOptionWithDosage, viewVitalsForm, viewWeightForm, vitalsFormInputsAndTasks, weightFormAndTasks)

{-| This module provides a form for entering measurements.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Activity.Utils exposing (generateNutritionAssessment)
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (encodeFamilyPlanningSignAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( currentValues
        , getMeasurementValueFunc
        , mapMeasurementData
        , muacIndication
        , nutritionSignToString
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model
import Backend.NutritionEncounter.Utils
    exposing
        ( calculateZScoreWeightForAge
        , getNewbornExamPregnancySummary
        , nutritionAssessmentForBackend
        , resolveAllWeightMeasurementsForChild
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
        )
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.PrenatalEncounter.Utils exposing (eddToLmpDate)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Html.Parser.Util exposing (toVirtualDom)
import Json.Decode
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model exposing (ContributingFactorsForm, CorePhysicalExamForm, CorePhysicalExamFormConfig, FamilyPlanningForm, FbfForm, FloatInputConstraints, GroupOfFoods(..), HealthEducationForm, HeightForm, InvokationModule(..), MedicationAdministrationForm, MedicationAdministrationFormConfig, ModelChild, ModelMother, MsgChild(..), MsgMother(..), MuacForm, NCDAContentConfig, NCDAData, NCDAForm, NCDAStep(..), NutritionCaringForm, NutritionFeedingForm, NutritionFollowUpForm, NutritionFoodSecurityForm, NutritionForm, NutritionHygieneForm, OutMsgChild(..), OutMsgMother(..), ParticipantFormUI, SendToHCForm, VitalsForm, VitalsFormConfig, VitalsFormMode(..), WeightForm, emptyParticipantFormProgress)
import Measurement.Utils exposing (contributingFactorsFormWithDefault, fbfFormToValue, getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight, healthEducationFormWithDefault, isBehindOnVaccinationsByProgress, lactationFormToSigns, medicationAdministrationFormInputsAndTasks, muacMeasurementIsOff, ncdaFormWithDefault, nutritionFollowUpFormWithDefault, renderDatePart, resoloveLastScheduledImmunizationVisitDate, resolveChildANCPregnancyData, resolveNCDASteps, sendToHCFormWithDefault, toContributingFactorsValueWithDefault, toHealthEducationValueWithDefault, toNCDAValueWithDefault, toNutritionFollowUpValueWithDefault, toSendToHCValueWithDefault, withinConstraints)
import Pages.Utils
    exposing
        ( concatInputsAndTasksSections
        , isTaskCompleted
        , maybeToBoolTask
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromImageUrl
        , viewQuestionLabel
        , viewRedAlertForBool
        , viewRedAlertForSelect
        , viewTasksCount
        )
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Round
import SyncManager.Model exposing (Site(..))
import Translate exposing (Language, TranslationId, translate)
import Translate.Utils exposing (selectLanguage)
import Utils.Html exposing (viewModal)
import ZScore.Model exposing (Centimetres(..), Days(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (diffDays, viewZScore, zScoreLengthHeightForAge, zScoreWeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


{-| We need the current date in order to immediately construct a ZScore for the
child when we enter something.
-}
viewChild :
    Language
    -> NominalDate
    -> Site
    -> Bool
    -> ( PersonId, Person )
    -> ChildActivity
    -> MeasurementData ChildMeasurements
    -> ZScore.Model.Model
    -> EditableSession
    -> ModelIndexedDb
    -> ModelChild
    -> PreviousValuesSet
    -> Html MsgChild
viewChild language currentDate site isChw ( childId, child ) activity measurements zscores session db model previousValuesSet =
    case activity of
        ChildFbf ->
            viewChildFbf language session.offlineSession.session.clinicType (mapMeasurementData .fbf measurements) model.fbfForm

        ChildPicture ->
            viewPhoto language (mapMeasurementData .photo measurements) model.photo

        Height ->
            viewHeight language currentDate isChw child (mapMeasurementData .height measurements) previousValuesSet.height zscores model

        Muac ->
            viewMuac site language currentDate isChw child (mapMeasurementData .muac measurements) previousValuesSet.muac zscores model

        NutritionSigns ->
            viewNutritionSigns language (mapMeasurementData .nutrition measurements) model.nutrition

        -- Counseling ->
        --    viewCounselingSession language (mapMeasurementData .counselingSession measurements) session model.counseling
        Weight ->
            viewWeight language currentDate isChw child (mapMeasurementData .weight measurements) previousValuesSet.weight zscores model

        ContributingFactors ->
            viewContributingFactors language (mapMeasurementData .contributingFactors measurements) model.contributingFactorsForm

        FollowUp ->
            viewFollowUp language currentDate zscores childId (mapMeasurementData .followUp measurements) session.offlineSession db model.followUpForm

        Activity.Model.HealthEducation ->
            viewHealthEducation language (mapMeasurementData .healthEducation measurements) model.healthEducationForm

        Activity.Model.SendToHC ->
            viewSendToHC language currentDate (mapMeasurementData .sendToHC measurements) model.sendToHCForm

        Activity.Model.NCDA ->
            viewNCDA language currentDate site childId child (mapMeasurementData .ncda measurements) model.ncdaData db


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
    , toBackendValue : String -> Maybe Float
    , dateMeasured : value -> NominalDate
    , viewIndication : Maybe (Language -> Float -> Html MsgChild)
    , updateMsg : String -> MsgChild
    , saveMsg : Maybe id -> Float -> MsgChild
    }


heightFormConfig : FloatFormConfig HeightId Height
heightFormConfig =
    { blockName = "height"
    , activity = ChildActivity Height
    , placeholderText = Translate.PlaceholderEnterHeight
    , zScoreLabelForAge = Translate.ZScoreHeightForAge
    , zScoreForAge = Just <| \model age gender height -> zScoreLengthHeightForAge model age gender (Centimetres height)
    , zScoreForHeightOrLength = Nothing
    , constraints = getInputConstraintsHeight
    , unit = Translate.UnitCentimeter
    , inputValue = .height
    , toBackendValue = String.toFloat
    , dateMeasured = .dateMeasured
    , viewIndication = Nothing
    , updateMsg = UpdateHeight
    , saveMsg = \id value -> SendOutMsgChild <| SaveHeight id (HeightInCm value)
    }


muacFormConfig : Site -> FloatFormConfig MuacId Muac
muacFormConfig site =
    let
        ( toBackendValue, unit ) =
            case site of
                SiteBurundi ->
                    ( -- At Burundi, value is entered as mm, but we need to store it
                      -- as cm. Therefore, we multiply by 0.1.
                      String.toFloat >> Maybe.map ((*) 0.1 >> Round.roundNum 1)
                    , Translate.UnitMillimeter
                    )

                _ ->
                    ( String.toFloat
                    , Translate.UnitCentimeter
                    )
    in
    { blockName = "muac"
    , activity = ChildActivity Muac
    , placeholderText = Translate.PlaceholderEnterMUAC
    , zScoreLabelForAge = Translate.ZScoreMuacForAge
    , zScoreForAge = Nothing
    , zScoreForHeightOrLength = Nothing
    , constraints = getInputConstraintsMuac site
    , unit = unit
    , inputValue = .muac
    , toBackendValue = toBackendValue
    , dateMeasured = .dateMeasured
    , viewIndication = Just <| \language val -> viewColorAlertIndication language (muacIndication (MuacInCm val))
    , updateMsg = UpdateMuac
    , saveMsg = \id value -> SendOutMsgChild <| SaveMuac id (MuacInCm value)
    }


weightFormConfig : FloatFormConfig WeightId Weight
weightFormConfig =
    { blockName = "weight"
    , activity = ChildActivity Weight
    , placeholderText = Translate.PlaceholderEnterWeight
    , zScoreLabelForAge = Translate.ZScoreWeightForAge
    , zScoreForAge = Just <| \model age gender weight -> zScoreWeightForAge model age gender (Kilograms weight)
    , zScoreForHeightOrLength = Just zScoreForHeightOrLength
    , constraints = getInputConstraintsWeight
    , unit = Translate.KilogramShorthand
    , inputValue = .weight
    , toBackendValue = String.toFloat
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


viewHeight : Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( HeightId, Height )) -> Maybe Float -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewHeight =
    viewFloatForm heightFormConfig


viewWeight : Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( WeightId, Weight )) -> Maybe Float -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewWeight =
    viewFloatForm weightFormConfig


viewMuac : Site -> Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( MuacId, Muac )) -> Maybe Float -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewMuac site =
    viewFloatForm (muacFormConfig site)


viewFloatForm : FloatFormConfig id value -> Language -> NominalDate -> Bool -> Person -> MeasurementData (Maybe ( id, value )) -> Maybe Float -> ZScore.Model.Model -> ModelChild -> Html MsgChild
viewFloatForm config language currentDate isChw child measurements previousValue zscores model =
    let
        -- What is the string input value from the form?
        inputValue =
            config.inputValue model

        inputAsFloat =
            String.toFloat inputValue

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
        backendValue =
            config.toBackendValue inputValue

        -- What is the most recent measurement we've saved, either locally or
        -- to the backend (we don't care at the moment which). If this is a new
        -- measurement we haven't saved yet, this will be Nothing.
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
        -- And, we'll need the child's age.
        maybeAgeInDays =
            Maybe.map
                (\birthDate ->
                    let
                        savedMeasurement =
                            measurements.current

                        dateMeasured =
                            savedMeasurement
                                |> Maybe.map (Tuple.second >> config.dateMeasured)
                                |> Maybe.withDefault currentDate
                    in
                    diffDays birthDate dateMeasured
                )
                child.birthDate

        renderedZScoreForAge =
            config.zScoreForAge
                |> Maybe.map
                    (\zScoreForAge ->
                        let
                            zScoreText =
                                Maybe.andThen
                                    (\val ->
                                        Maybe.andThen
                                            (\ageInDays ->
                                                zScoreForAge zscores ageInDays child.gender val
                                            )
                                            maybeAgeInDays
                                    )
                                    backendValue
                                    |> Maybe.map viewZScore
                                    |> Maybe.withDefault (translate language Translate.NotAvailable)
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
                                                    backendValue
                                            )
                                        |> Maybe.map viewZScore
                                        |> Maybe.withDefault (translate language Translate.NotAvailable)
                            in
                            div
                                [ class "ui large header z-score height" ]
                                [ text <| translate language Translate.ZScoreWeightForHeight
                                , span
                                    [ class "sub header" ]
                                    [ text zScoreText
                                    ]
                                ]
                        )

        saveMsg =
            Maybe.Extra.andThen2
                (\asFloat forBackend ->
                    if not <| withinConstraints config.constraints asFloat then
                        Nothing

                    else
                        config.saveMsg (Maybe.map Tuple.first measurements.current) forBackend |> Just
                )
                inputAsFloat
                backendValue
    in
    div
        [ class <| "ui full segment " ++ config.blockName ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Translate.ActivitiesTitle config.activity)
                ]
            , p [ class "activity-helper" ] [ text <| translate language (Translate.ActivitiesHelp config.activity) ]
            , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper config.constraints) ]
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
                                inputAsFloat
                        , showMaybe <|
                            Maybe.map2 (\func value -> func language value)
                                config.viewIndication
                                backendValue
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
            saveButton language saveMsg measurements
        ]


muacColor : ColorAlertIndication -> Attribute any
muacColor muac =
    class <|
        case muac of
            ColorAlertRed ->
                "label-red"

            ColorAlertYellow ->
                "label-yellow"

            ColorAlertGreen ->
                "label-green"


viewColorAlertIndication : Language -> ColorAlertIndication -> Html any
viewColorAlertIndication language muac =
    p
        [ muacColor muac
        , class "label-form"
        ]
        [ translate language (Translate.ColorAlertIndication muac)
            |> String.toUpper
            |> text
        ]


viewPreviousMeasurement : FloatFormConfig id value -> Language -> Float -> Html any
viewPreviousMeasurement config language previousValue =
    [ previousValue
        |> Translate.PreviousFloatMeasurement
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
                [ class "label-with-icon label-form" ]
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


viewPhoto : Language -> MeasurementData (Maybe ( PhotoId, Photo )) -> Maybe ImageUrl -> Html MsgChild
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
                    getMeasurementValueFunc
                        measurement.current
    in
    divKeyed
        [ class "ui full segment photo" ]
        [ keyedDivKeyed "content"
            [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Translate.ActivitiesTitle activity) ]
                |> keyed "title"
            , p [] [ text <| translate language (Translate.ActivitiesHelp activity) ]
                |> keyed "help"
            , keyedDivKeyed "grid"
                [ class "ui grid" ]
                [ Maybe.map viewPhotoThumbFromImageUrl displayPhoto
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
                            [ text <| translate language Translate.DropzoneDefaultMessage ]
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
        ]


{-| Helper function to create a Save button.

Button will also take care of preventing double submission,
and showing success and error indications.

-}
saveButton : Language -> Maybe msg -> MeasurementData (Maybe a) -> List (Html msg)
saveButton language msg measurement =
    let
        isLoading =
            measurement.update == Loading

        isFailure =
            RemoteData.isFailure measurement.update

        ( updateText, errorText ) =
            if isJust measurement.current then
                ( Translate.Update, Translate.UpdateError )

            else
                ( Translate.Save, Translate.SaveError )

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


viewNutritionSigns :
    Language
    -> MeasurementData (Maybe ( ChildNutritionId, ChildNutrition ))
    -> NutritionValue
    -> Html MsgChild
viewNutritionSigns language measurement value =
    let
        activity =
            ChildActivity NutritionSigns

        saveMsg =
            if EverySet.isEmpty value.signs then
                Nothing

            else
                let
                    existingId =
                        Maybe.map Tuple.first measurement.current
                in
                Just <| SendOutMsgChild <| SaveNutrition existingId value
    in
    div [ class "ui full segment nutrition" ]
        [ div [ class "content" ]
            [ h3 [ class "ui header" ]
                [ text <| translate language (Translate.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Translate.ActivitiesHelp activity) ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language (Translate.ActivitiesLabel activity) ]
                    :: viewNutritionSignsSelector language value.signs
            ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
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
            nutritionSignToString sign

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
            [ text <| translate language (Translate.ChildNutritionSignLabel sign) ]
        ]


viewChildFbf : Language -> ClinicType -> MeasurementData (Maybe ( ChildFbfId, Fbf )) -> FbfForm -> Html MsgChild
viewChildFbf language clinicType measurement form =
    let
        activity =
            ChildActivity ChildFbf

        saveMsg =
            form.distributedAmount
                |> Maybe.map
                    (\_ ->
                        let
                            existingId =
                                Maybe.map Tuple.first measurement.current

                            notice =
                                if isJust form.distributionNotice then
                                    form.distributionNotice

                                else
                                    Just DistributedFully
                        in
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
                           [ text <| translate language (Translate.ActivitiesTitle activity)
                           ]
                       , h3 [ class "ui header" ]
                           [ text <| translate language (Translate.CounselingTimingHeading timing)
                           ]
                       , div [ class "ui form" ] <|
                           p [] [ text <| translate language (Translate.ActivitiesLabel activity) ]
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


viewContributingFactors : Language -> MeasurementData (Maybe ( ContributingFactorsId, ContributingFactors )) -> ContributingFactorsForm -> Html MsgChild
viewContributingFactors language measurement form =
    let
        existingId =
            Maybe.map Tuple.first measurement.current

        saved =
            getMeasurementValueFunc measurement.current

        formContent =
            contributingFactorsFormWithDefault form saved
                |> viewContributingFactorsForm language SetContributingFactorsSign

        saveMsg =
            toContributingFactorsValueWithDefault saved form
                |> Maybe.map (SaveContributingFactors existingId >> SendOutMsgChild)
    in
    div [ class "ui full segment contributing-factors" ]
        [ div [ class "content" ] [ formContent ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
        ]


viewContributingFactorsForm :
    Language
    -> (ContributingFactorsSign -> msg)
    -> ContributingFactorsForm
    -> Html msg
viewContributingFactorsForm language setContributingFactorsSignMsg form =
    let
        ( inputs, _ ) =
            contributingFactorsFormInutsAndTasks language setContributingFactorsSignMsg form
    in
    div [ class "ui form contributing-factors" ]
        inputs


contributingFactorsFormInutsAndTasks :
    Language
    -> (ContributingFactorsSign -> msg)
    -> ContributingFactorsForm
    -> ( List (Html msg), List (Maybe Bool) )
contributingFactorsFormInutsAndTasks language setContributingFactorsSignMsg form =
    ( [ viewQuestionLabel language Translate.ContributingFactorsQuestion
      , viewCheckBoxMultipleSelectInput language
            [ FactorLackOfBreastMilk, FactorMaternalMastitis, FactorPoorSuck, FactorDiarrheaOrVomiting ]
            []
            (Maybe.withDefault [] form.signs)
            (Just NoContributingFactorsSign)
            setContributingFactorsSignMsg
            Translate.ContributingFactor
      ]
    , [ maybeToBoolTask form.signs ]
    )


viewFollowUp :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> PersonId
    -> MeasurementData (Maybe ( FollowUpId, FollowUp ))
    -> OfflineSession
    -> ModelIndexedDb
    -> NutritionFollowUpForm
    -> Html MsgChild
viewFollowUp language currentDate zscores childId measurement offlineSession db form =
    let
        existingId =
            Maybe.map Tuple.first measurement.current

        saved =
            getMeasurementValueFunc measurement.current

        assesment =
            generateNutritionAssessment currentDate zscores childId db offlineSession
                |> nutritionAssessmentForBackend

        form_ =
            { form | assesment = Just assesment }

        formContent =
            nutritionFollowUpFormWithDefault form_ saved
                |> viewNutritionFollowUpForm language SetFollowUpOption

        saveMsg =
            toNutritionFollowUpValueWithDefault saved form_
                |> Maybe.map (SaveFollowUp existingId >> SendOutMsgChild)
    in
    div [ class "ui full segment follow-up" ]
        [ div [ class "content" ] [ formContent ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
        ]


viewNutritionFollowUpForm :
    Language
    -> (FollowUpOption -> msg)
    -> { f | option : Maybe FollowUpOption }
    -> Html msg
viewNutritionFollowUpForm language setFollowUpOptionMsg form =
    viewFollowUpForm language
        [ OneDay, ThreeDays, OneWeek, TwoWeeks, FollowUpNotNeeded ]
        setFollowUpOptionMsg
        form


viewFollowUpForm :
    Language
    -> List FollowUpOption
    -> (FollowUpOption -> msg)
    -> { f | option : Maybe FollowUpOption }
    -> Html msg
viewFollowUpForm language options setFollowUpOptionMsg form =
    let
        ( inputs, _ ) =
            followUpFormInputsAndTasks language options setFollowUpOptionMsg form
    in
    div [ class "ui form follow-up" ]
        inputs


followUpFormInputsAndTasks :
    Language
    -> List FollowUpOption
    -> (FollowUpOption -> msg)
    -> { f | option : Maybe FollowUpOption }
    -> ( List (Html msg), List (Maybe Bool) )
followUpFormInputsAndTasks language options setFollowUpOptionMsg form =
    ( [ viewLabel language Translate.FollowUpLabel
      , viewCheckBoxSelectInput language
            options
            []
            form.option
            setFollowUpOptionMsg
            Translate.FollowUpOption
      ]
    , [ maybeToBoolTask form.option ]
    )


viewHealthEducation : Language -> MeasurementData (Maybe ( GroupHealthEducationId, GroupHealthEducation )) -> HealthEducationForm -> Html MsgChild
viewHealthEducation language measurement form_ =
    let
        saved =
            getMeasurementValueFunc measurement.current

        form =
            healthEducationFormWithDefault form_ saved

        formContent =
            viewHealthEducationForm language
                SetProvidedEducationForDiagnosis
                SetReasonForNotProvidingHealthEducation
                form

        ( completed, total ) =
            let
                ( reasonForProvidingEducationActive, reasonForProvidingEducationCompleted ) =
                    form.educationForDiagnosis
                        |> Maybe.map
                            (\providedHealthEducation ->
                                if not providedHealthEducation then
                                    if isJust form.reasonForNotProvidingHealthEducation then
                                        ( 1, 1 )

                                    else
                                        ( 0, 1 )

                                else
                                    ( 0, 0 )
                            )
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( reasonForProvidingEducationActive + taskCompleted form.educationForDiagnosis
            , reasonForProvidingEducationCompleted + 1
            )

        saveMsg =
            if completed < total then
                Nothing

            else
                let
                    existingId =
                        Maybe.map Tuple.first measurement.current
                in
                toHealthEducationValueWithDefault saved form_
                    |> Maybe.map (SaveHealthEducation existingId >> SendOutMsgChild)
    in
    div [ class "ui full segment health-education" ]
        [ div [ class "content" ] [ formContent ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
        ]


viewHealthEducationForm :
    Language
    -> (Bool -> msg)
    -> (ReasonForNotProvidingHealthEducation -> msg)
    -> HealthEducationForm
    -> Html msg
viewHealthEducationForm language setProvidedEducationForDiagnosisMsg setReasonForNotProvidingHealthEducationMsg form =
    let
        ( inputs, _ ) =
            healthEducationFormInutsAndTasks language setProvidedEducationForDiagnosisMsg setReasonForNotProvidingHealthEducationMsg form
    in
    div [ class "ui form health-education" ]
        inputs


healthEducationFormInutsAndTasks :
    Language
    -> (Bool -> msg)
    -> (ReasonForNotProvidingHealthEducation -> msg)
    -> HealthEducationForm
    -> ( List (Html msg), List (Maybe Bool) )
healthEducationFormInutsAndTasks language setProvidedEducationForDiagnosisMsg setReasonForNotProvidingHealthEducationMsg form =
    let
        healthEducationSection =
            let
                providedHealthEducation =
                    Maybe.withDefault True form.educationForDiagnosis

                reasonForNotProvidingHealthEducationSection =
                    if not providedHealthEducation then
                        ( [ viewQuestionLabel language Translate.WhyNot
                          , viewCheckBoxSelectInput language
                                [ PatientNeedsEmergencyReferral
                                , ReceivedEmergencyCase
                                , LackOfAppropriateEducationUserGuide
                                , PatientRefused
                                , PatientTooIll
                                ]
                                []
                                form.reasonForNotProvidingHealthEducation
                                setReasonForNotProvidingHealthEducationMsg
                                Translate.ReasonForNotProvidingHealthEducation
                          ]
                        , [ maybeToBoolTask form.reasonForNotProvidingHealthEducation ]
                        )

                    else
                        ( [], [] )
            in
            concatInputsAndTasksSections
                [ ( [ div [ class "label" ]
                        [ text <| translate language Translate.ProvidedPreventionEducationQuestionShort
                        , text "?"
                        ]
                    , viewBoolInput
                        language
                        form.educationForDiagnosis
                        setProvidedEducationForDiagnosisMsg
                        "education-for-diagnosis"
                        Nothing
                    ]
                  , [ form.educationForDiagnosis ]
                  )
                , reasonForNotProvidingHealthEducationSection
                ]
    in
    concatInputsAndTasksSections
        [ ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
            , div [ class "instructions" ]
                [ viewHealthEducationLabel language Translate.ProvideHealthEducationShort "icon-open-book" Nothing
                ]
            ]
          , []
          )
        , healthEducationSection
        ]


viewHealthEducationLabel : Language -> TranslationId -> String -> Maybe NominalDate -> Html any
viewHealthEducationLabel language actionTranslationId iconClass maybeDate =
    let
        message =
            div [] <|
                (text <| translate language actionTranslationId)
                    :: renderDatePart language maybeDate
                    ++ [ text "." ]
    in
    div [ class "header icon-label" ]
        [ i [ class iconClass ] []
        , message
        ]


viewSendToHC : Language -> NominalDate -> MeasurementData (Maybe ( GroupSendToHCId, GroupSendToHC )) -> SendToHCForm -> Html MsgChild
viewSendToHC language currentDate measurement form_ =
    let
        saved =
            getMeasurementValueFunc measurement.current

        form =
            sendToHCFormWithDefault form_ saved

        formContent =
            viewSendToHealthCenterForm language
                currentDate
                SetReferToHealthCenter
                SetReasonForNonReferral
                SetHandReferralForm
                Nothing
                form

        ( completed, total ) =
            let
                ( reasonForNotSentActive, reasonForNotSentCompleted ) =
                    form.referToHealthCenter
                        |> Maybe.map
                            (\sentToHC ->
                                if not sentToHC then
                                    if isJust form.reasonForNotSendingToHC then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( reasonForNotSentActive + taskCompleted form.handReferralForm
            , reasonForNotSentCompleted + 1
            )

        saveMsg =
            if completed < total then
                Nothing

            else
                let
                    existingId =
                        Maybe.map Tuple.first measurement.current
                in
                toSendToHCValueWithDefault saved form_
                    |> Maybe.map (SaveSendToHC existingId >> SendOutMsgChild)
    in
    div [ class "ui full segment send-to-hc" ]
        [ div [ class "content" ] [ formContent ]
        , div [ class "actions" ] <|
            saveButton
                language
                saveMsg
                measurement
        ]


viewSendToHealthCenterForm :
    Language
    -> NominalDate
    -> (Bool -> msg)
    -> (ReasonForNonReferral -> msg)
    -> (Bool -> msg)
    -> Maybe (Bool -> msg)
    -> SendToHCForm
    -> Html msg
viewSendToHealthCenterForm language _ =
    viewSendToFacilityForm language FacilityHealthCenter


viewSendToHospitalForm :
    Language
    -> NominalDate
    -> (Bool -> msg)
    -> (ReasonForNonReferral -> msg)
    -> (Bool -> msg)
    -> Maybe (Bool -> msg)
    -> SendToHCForm
    -> Html msg
viewSendToHospitalForm language _ =
    viewSendToFacilityForm language FacilityHospital


viewSendToFacilityForm :
    Language
    -> ReferralFacility
    -> (Bool -> msg)
    -> (ReasonForNonReferral -> msg)
    -> (Bool -> msg)
    -> Maybe (Bool -> msg)
    -> SendToHCForm
    -> Html msg
viewSendToFacilityForm language facility setReferToHealthCenterMsg setReasonForNonReferralMsg setHandReferralFormMsg setAccompanyToHCMsg form =
    let
        ( inputs, _ ) =
            sendToFacilityInputsAndTasks language
                facility
                setReferToHealthCenterMsg
                setReasonForNonReferralMsg
                setHandReferralFormMsg
                setAccompanyToHCMsg
                form
    in
    div [ class "ui form send-to-hc" ]
        inputs


sendToFacilityInputsAndTasks :
    Language
    -> ReferralFacility
    -> (Bool -> msg)
    -> (ReasonForNonReferral -> msg)
    -> (Bool -> msg)
    -> Maybe (Bool -> msg)
    -> SendToHCForm
    -> ( List (Html msg), List (Maybe Bool) )
sendToFacilityInputsAndTasks language facility setReferToHealthCenterMsg setReasonForNonReferralMsg setHandReferralFormMsg setAccompanyToHCMsg form =
    let
        headerHelper =
            case facility of
                FacilityHospital ->
                    [ viewCustomLabel language Translate.HighRiskCaseHelper "." "instructions" ]

                _ ->
                    []

        sendToHCSection =
            let
                sentToHealthCenter =
                    Maybe.withDefault True form.referToHealthCenter

                reasonForNotSendingToHCSection =
                    if not sentToHealthCenter then
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ ClientRefused
                                    , NoAmbulance
                                    , ClientUnableToAffordFees
                                    , ReasonForNonReferralNotIndicated
                                    , ReasonForNonReferralOther
                                    ]
                                    []
                                    form.reasonForNotSendingToHC
                                    setReasonForNonReferralMsg
                                    Translate.ReasonForNonReferral
                                ]
                          ]
                        , [ maybeToBoolTask form.reasonForNotSendingToHC ]
                        )

                    else
                        ( [], [] )
            in
            concatInputsAndTasksSections
                [ ( [ viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion facility
                    , viewBoolInput
                        language
                        form.referToHealthCenter
                        setReferToHealthCenterMsg
                        "refer-to-hc"
                        Nothing
                    ]
                  , [ form.referToHealthCenter ]
                  )
                , reasonForNotSendingToHCSection
                ]

        handReferralFormSection =
            ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
              , viewBoolInput
                    language
                    form.handReferralForm
                    setHandReferralFormMsg
                    "hand-referral-form"
                    Nothing
              ]
            , [ form.handReferralForm ]
            )

        accompanyToHCSection =
            Maybe.map
                (\msg ->
                    ( [ viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion facility
                      , viewBoolInput
                            language
                            form.accompanyToHealthCenter
                            msg
                            "accompany-to-hc"
                            Nothing
                      ]
                    , [ form.accompanyToHealthCenter ]
                    )
                )
                setAccompanyToHCMsg
                |> Maybe.withDefault ( [], [] )
    in
    concatInputsAndTasksSections
        [ ( headerHelper
                ++ [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                   , div [ class "instructions" ]
                        [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing
                        , viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" Nothing
                        ]
                   ]
          , []
          )
        , sendToHCSection
        , handReferralFormSection
        , accompanyToHCSection
        ]


viewReferToProgramForm :
    Language
    -> (Bool -> msg)
    -> (Bool -> msg)
    -> SendToHCForm
    -> Html msg
viewReferToProgramForm language setEnrollToNutritionProgramMsg setReferToNutritionProgramMsg form =
    let
        ( inputs, _ ) =
            referToProgramFormInputsAndTasks language setEnrollToNutritionProgramMsg setReferToNutritionProgramMsg form
    in
    div [ class "ui form send-to-hc" ]
        inputs


referToProgramFormInputsAndTasks :
    Language
    -> (Bool -> msg)
    -> (Bool -> msg)
    -> SendToHCForm
    -> ( List (Html msg), List (Maybe Bool) )
referToProgramFormInputsAndTasks language setEnrollToNutritionProgramMsg setReferToNutritionProgramMsg form =
    ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewActionTakenLabel language Translate.ReferToProgramAction "icon-forms" Nothing
            , viewActionTakenLabel language Translate.EnrollToProgramAction "icon-shuttle" Nothing
            ]
      , viewQuestionLabel language Translate.EnrollToProgramQuestion
      , viewBoolInput
            language
            form.enrollToNutritionProgram
            setEnrollToNutritionProgramMsg
            "enroll-to-program"
            Nothing
      , viewQuestionLabel language Translate.ReferToProgramQuestion
      , viewBoolInput
            language
            form.referToNutritionProgram
            setReferToNutritionProgramMsg
            "refer-to-program"
            Nothing
      ]
    , [ form.enrollToNutritionProgram, form.referToNutritionProgram ]
    )


viewVitalsForm : Language -> NominalDate -> VitalsFormConfig msg -> VitalsForm -> Html msg
viewVitalsForm language currentDate config form =
    let
        ( inputs, _ ) =
            vitalsFormInputsAndTasks language currentDate config form
    in
    div [ class <| "ui form " ++ config.formClass ]
        inputs


vitalsFormInputsAndTasks : Language -> NominalDate -> VitalsFormConfig msg -> VitalsForm -> ( List (Html msg), List (Maybe Bool) )
vitalsFormInputsAndTasks language currentDate config form =
    let
        sysBloodPressureUpdateFunc value form_ =
            { form_ | sysBloodPressure = value, sysBloodPressureDirty = True }

        diaBloodPressureUpdateFunc value form_ =
            { form_ | diaBloodPressure = value, diaBloodPressureDirty = True }

        heartRateUpdateFunc value form_ =
            { form_ | heartRate = value, heartRateDirty = True }

        respiratoryRateUpdateFunc value form_ =
            { form_ | respiratoryRate = value, respiratoryRateDirty = True }

        bodyTemperatureUpdateFunc value form_ =
            { form_ | bodyTemperature = value, bodyTemperatureDirty = True }

        sysRepeatedUpdateFunc value form_ =
            { form_ | sysRepeated = value, sysRepeatedDirty = True }

        diaRepeatedUpdateFunc value form_ =
            { form_ | diaRepeated = value, diaRepeatedDirty = True }

        ageInYears =
            Maybe.map
                (\birthDate -> Gizra.NominalDate.diffYears birthDate currentDate)
                config.birthDate

        viewBloodPressureSection sys dia sysUpdateFunc diaUpdateFunc sysPrevValue diaPrevValue addSeparator =
            Maybe.map
                (\ageYears ->
                    if ageYears < 12 then
                        -- Blood presure is taken for patients that are
                        -- 12 years old, or older.
                        ( [], [] )

                    else
                        let
                            ( redAlertsSys, redAlertsDia ) =
                                let
                                    ( redAlertHighSys, redAlertHighDia ) =
                                        if ageYears < 14 then
                                            ( 135, 88 )

                                        else
                                            ( 140, 90 )
                                in
                                ( [ [ (<) redAlertHighSys ]
                                  , [ (>) 100 ]
                                  ]
                                , [ [ (<) redAlertHighDia ]
                                  , [ (>) 60 ]
                                  ]
                                )
                        in
                        ( [ div [ class "ui grid" ]
                                [ div [ class "eleven wide column" ]
                                    [ viewLabel language Translate.BloodPressure ]
                                ]
                          , div [ class "ui grid systolic" ]
                                [ div [ class "twelve wide column" ]
                                    [ div [ class "title sys" ] [ text <| translate language Translate.BloodPressureSysLabel ] ]
                                , div [ class "four wide column" ]
                                    [ viewConditionalAlert sys
                                        redAlertsSys
                                        []
                                    ]
                                ]
                          , viewMeasurementInput
                                language
                                sys
                                (config.setFloatInputMsg sysUpdateFunc)
                                "sys-blood-pressure"
                                Translate.MMHGUnit
                          , Pages.Utils.viewPreviousMeasurement language sysPrevValue Translate.MMHGUnit
                          , div [ class "ui grid" ]
                                [ div [ class "twelve wide column" ]
                                    [ div [ class "title dia" ] [ text <| translate language Translate.BloodPressureDiaLabel ] ]
                                , div [ class "four wide column" ]
                                    [ viewConditionalAlert dia
                                        redAlertsDia
                                        []
                                    ]
                                ]
                          , viewMeasurementInput
                                language
                                dia
                                (config.setFloatInputMsg diaUpdateFunc)
                                "dia-blood-pressure"
                                Translate.MMHGUnit
                          , Pages.Utils.viewPreviousMeasurement language diaPrevValue Translate.MMHGUnit
                          , separator |> showIf addSeparator
                          ]
                        , [ maybeToBoolTask sys, maybeToBoolTask dia ]
                        )
                )
                ageInYears
                |> Maybe.withDefault ( [], [] )

        respiratoryRateSection =
            let
                ( redAlerts, yellowAlerts ) =
                    case config.invokationModule of
                        InvokationModulePrenatal ->
                            ( [ [ (>) 12 ], [ (<) 30 ] ]
                            , [ [ (<=) 21, (>=) 30 ] ]
                            )

                        _ ->
                            Maybe.map
                                (\ageYears ->
                                    let
                                        ( redAlertMinValue, redAlertMaxValue ) =
                                            if ageYears < 1 then
                                                ( 30, 49 )

                                            else if ageYears < 5 then
                                                ( 24, 39 )

                                            else
                                                ( 18, 30 )
                                    in
                                    ( [ [ (>) redAlertMinValue ], [ (<) redAlertMaxValue ] ], [] )
                                )
                                ageInYears
                                |> Maybe.withDefault ( [], [] )
            in
            ( [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.RespiratoryRate ]
                    , div [ class "four wide column" ]
                        [ viewConditionalAlert form.respiratoryRate
                            redAlerts
                            yellowAlerts
                        ]
                    ]
              , viewMeasurementInput
                    language
                    (Maybe.map toFloat form.respiratoryRate)
                    (config.setIntInputMsg respiratoryRateUpdateFunc)
                    "respiratory-rate"
                    Translate.BreathsPerMinuteUnitLabel
              , Pages.Utils.viewPreviousMeasurement language config.respiratoryRatePreviousValue Translate.BreathsPerMinuteUnitLabel
              , separator
              ]
            , [ maybeToBoolTask form.respiratoryRate ]
            )

        bodyTemperatureSection =
            ( [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.BodyTemperature ]
                    , div [ class "four wide column" ]
                        [ viewConditionalAlert form.bodyTemperature
                            [ [ (>) 35 ], [ (<=) 37.5 ] ]
                            []
                        ]
                    ]
              , viewMeasurementInput
                    language
                    form.bodyTemperature
                    (config.setFloatInputMsg bodyTemperatureUpdateFunc)
                    "body-temperature"
                    Translate.Celsius
              , Pages.Utils.viewPreviousMeasurement language config.bodyTemperaturePreviousValue Translate.Celsius
              ]
            , [ maybeToBoolTask form.bodyTemperature ]
            )

        separator =
            div [ class "separator" ] []
    in
    case config.mode of
        VitalsFormBasic ->
            concatInputsAndTasksSections [ respiratoryRateSection, bodyTemperatureSection ]

        VitalsFormFull ->
            let
                bloodPressureSection =
                    viewBloodPressureSection
                        form.sysBloodPressure
                        form.diaBloodPressure
                        sysBloodPressureUpdateFunc
                        diaBloodPressureUpdateFunc
                        config.sysBloodPressurePreviousValue
                        config.diaBloodPressurePreviousValue
                        True

                heartRateSection =
                    let
                        ( redAlerts, yellowAlerts ) =
                            case config.invokationModule of
                                InvokationModulePrenatal ->
                                    ( [ [ (>) 40 ], [ (<=) 120 ] ]
                                    , [ [ (<=) 40, (>=) 50 ], [ (<) 100, (>) 120 ] ]
                                    )

                                _ ->
                                    Maybe.map
                                        (\ageYears ->
                                            let
                                                ( redAlertMinValue, redAlertMaxValue ) =
                                                    if ageYears < 1 then
                                                        ( 110, 160 )

                                                    else if ageYears < 2 then
                                                        ( 100, 150 )

                                                    else if ageYears < 5 then
                                                        ( 95, 140 )

                                                    else if ageYears < 12 then
                                                        ( 80, 120 )

                                                    else
                                                        ( 60, 100 )
                                            in
                                            ( [ [ (>) redAlertMinValue ], [ (<) redAlertMaxValue ] ], [] )
                                        )
                                        ageInYears
                                        |> Maybe.withDefault ( [], [] )
                    in
                    ( [ div [ class "ui grid" ]
                            [ div [ class "twelve wide column" ]
                                [ viewLabel language Translate.HeartRate ]
                            , div [ class "four wide column" ]
                                [ viewConditionalAlert form.heartRate
                                    redAlerts
                                    yellowAlerts
                                ]
                            ]
                      , viewMeasurementInput
                            language
                            (Maybe.map toFloat form.heartRate)
                            (config.setIntInputMsg heartRateUpdateFunc)
                            "heart-rate"
                            Translate.BeatsPerMinuteUnitLabel
                      , Pages.Utils.viewPreviousMeasurement language config.heartRatePreviousValue Translate.BeatsPerMinuteUnitLabel
                      , separator
                      ]
                    , [ maybeToBoolTask form.heartRate ]
                    )
            in
            concatInputsAndTasksSections
                [ bloodPressureSection
                , heartRateSection
                , respiratoryRateSection
                , bodyTemperatureSection
                ]

        VitalsFormRepeated ->
            viewBloodPressureSection
                form.sysRepeated
                form.diaRepeated
                sysRepeatedUpdateFunc
                diaRepeatedUpdateFunc
                form.sysBloodPressure
                form.diaBloodPressure
                False


viewActionTakenLabel : Language -> TranslationId -> String -> Maybe NominalDate -> Html any
viewActionTakenLabel language actionTranslationId iconClass maybeDate =
    let
        message =
            div [] <|
                (text <| translate language actionTranslationId)
                    :: renderDatePart language maybeDate
                    ++ [ text "." ]
    in
    div [ class "header icon-label" ]
        [ i [ class iconClass ] []
        , message
        ]


viewMother : Language -> MotherActivity -> ClinicType -> MeasurementData MotherMeasurements -> ModelMother -> Html MsgMother
viewMother language activity clinicType measurements model =
    case activity of
        FamilyPlanning ->
            viewFamilyPlanning language (mapMeasurementData .familyPlanning measurements) model.familyPlanningSigns

        Lactation ->
            viewLactation language (mapMeasurementData .lactation measurements) model.lactationForm

        MotherFbf ->
            viewMotherFbf language clinicType (mapMeasurementData .fbf measurements) model.fbfForm

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
                        [ text <| translate language (Translate.ActivitiesTitle activity)
                        ]
                    , p [] [ text <| translate language (Translate.ActivitiesHelp activity) ]
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
                            [ text <| translate language Translate.CounselorReviewed ]
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
                            [ text <| translate language Translate.ParticipantReviewed ]
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
                        ( Translate.Update, Translate.UpdateError )

                    else
                        ( Translate.Save, Translate.SaveError )

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
                                , h2 [] [ text <| translate language Translate.ParticipantSignature ]
                                , participantReviewed
                                , h2 [] [ text <| translate language Translate.CounselorSignature ]
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

        saveMsg =
            if EverySet.isEmpty signs then
                Nothing

            else
                let
                    existingId =
                        Maybe.map Tuple.first measurement.current
                in
                Just <| SendOutMsgMother <| SaveFamilyPlanningSigns existingId signs
    in
    div
        [ class "ui full segment family-planning"
        , id "familyPlanningEntryForm"
        ]
        [ div [ class "content" ]
            [ h3
                [ class "ui header" ]
                [ text <| translate language (Translate.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Translate.ActivitiesHelp activity) ]
            , div [ class "ui form" ] <|
                p [] [ text <| translate language (Translate.ActivitiesLabel activity) ]
                    :: viewFamilyPlanningSelector language signs
            ]
        , div [ class "actions" ] <|
            saveButton language
                saveMsg
                measurement
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
            [ text <| translate language (Translate.FamilyPlanningSignLabel sign) ]
        ]


viewLactation : Language -> MeasurementData (Maybe ( LactationId, Lactation )) -> LactationForm -> Html MsgMother
viewLactation language measurement form =
    let
        activity =
            MotherActivity Lactation

        saveMsg =
            form.breastfeeding
                |> Maybe.map
                    (\_ ->
                        let
                            existingId =
                                Maybe.map Tuple.first measurement.current
                        in
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
                [ text <| translate language (Translate.ActivitiesTitle activity)
                ]
            , p [] [ text <| translate language (Translate.ActivitiesHelp activity) ]
            , div [ class "ui form" ]
                [ viewQuestionLabel language Translate.IsCurrentlyBreastfeeding
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
        ]


viewMotherFbf : Language -> ClinicType -> MeasurementData (Maybe ( MotherFbfId, Fbf )) -> FbfForm -> Html MsgMother
viewMotherFbf language clinicType measurement form =
    let
        activity =
            MotherActivity MotherFbf

        saveMsg =
            form.distributedAmount
                |> Maybe.map
                    (\_ ->
                        let
                            existingId =
                                Maybe.map Tuple.first measurement.current

                            notice =
                                if isJust form.distributionNotice then
                                    form.distributionNotice

                                else
                                    Just DistributedFully
                        in
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
        options =
            List.repeat 11 ""
                |> List.indexedMap (\index _ -> toFloat index)

        selectQuantityInput =
            viewCustomSelectListInput form.distributedAmount
                options
                String.fromFloat
                setDistributedAmountMsg
                String.fromFloat
                "fbf-distirbution"
                True

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
                    [ viewLabel language <| Translate.WasFbfDistirbuted activity
                    , viewCheckBoxSelectInput language
                        [ DistributedPartiallyLackOfStock, DistributedPartiallyOther ]
                        []
                        form.distributionNotice
                        setDistributoinNoticeMsg
                        Translate.DistributionNotice
                    ]

        ( quantityUnitLabel, activityLabel ) =
            case clinicType of
                Achi ->
                    ( Translate.KilogramShorthand, Translate.ActivitityLabelAchi )

                _ ->
                    ( Translate.PackagesPerMonth, Translate.ActivitiesLabel activity )
    in
    div
        [ class "ui full segment fbf"
        , id "fbfEntryForm"
        ]
        [ div [ class "content" ]
            [ h3 [ class "ui header" ]
                [ text <| translate language (Translate.FbfDistribution clinicType) ]
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
        ]


viewFamilyPlanningForm : Language -> TranslationId -> (FamilyPlanningSign -> msg) -> FamilyPlanningForm -> Html msg
viewFamilyPlanningForm language questionTransId setFamilyPlanningSignMsg form =
    div [ class "ui form family-planning" ]
        [ viewQuestionLabel language questionTransId
        , viewFamilyPlanningInput language setFamilyPlanningSignMsg form.signs
        ]


viewFamilyPlanningInput : Language -> (FamilyPlanningSign -> msg) -> Maybe (List FamilyPlanningSign) -> Html msg
viewFamilyPlanningInput language setFamilyPlanningSignMsg currentValue =
    viewCheckBoxMultipleSelectInput language
        [ AutoObservation, Condoms, CycleBeads, CycleCounting, Hysterectomy, Implants, Injectables ]
        [ IUD, LactationAmenorrhea, OralContraceptives, Spermicide, TubalLigatures, Vasectomy ]
        (Maybe.withDefault [] currentValue)
        (Just NoFamilyPlanning)
        setFamilyPlanningSignMsg
        Translate.FamilyPlanningSignLabel


viewCorePhysicalExamForm : Language -> CorePhysicalExamFormConfig msg -> CorePhysicalExamForm -> Html msg
viewCorePhysicalExamForm language config form =
    let
        brittleHairUpdateFunc value form_ =
            { form_ | brittleHair = Just value }

        paleConjuctivaUpdateFunc value form_ =
            { form_ | paleConjuctiva = Just value }

        heartMurmurUpdateFunc value form_ =
            { form_ | heartMurmur = Just value }
    in
    div [ class "ui form examination core-physical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HeadHair ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.brittleHair False ]
            ]
        , viewBoolInput
            language
            form.brittleHair
            (config.setBoolInputMsg brittleHairUpdateFunc)
            "head-hair"
            (Just ( Translate.BrittleHair, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Eyes ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.paleConjuctiva False ]
            ]
        , viewBoolInput
            language
            form.paleConjuctiva
            (config.setBoolInputMsg paleConjuctivaUpdateFunc)
            "eyes"
            (Just ( Translate.PaleConjuctiva, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Neck ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.neck |> Maybe.withDefault [])
                    [ NormalNeck ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ EnlargedThyroid, EnlargedLymphNodes ]
            [ NormalNeck ]
            (form.neck |> Maybe.withDefault [])
            Nothing
            config.setNeckMsg
            Translate.NeckCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Heart ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.heart |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ NormalRateAndRhythm ]
                ]
            ]
        , viewCheckBoxSelectInput language
            [ IrregularRhythm, SinusTachycardia ]
            [ NormalRateAndRhythm ]
            form.heart
            config.setHeartMsg
            Translate.HeartCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HeartMurmur ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.heartMurmur False ]
            ]
        , viewBoolInput
            language
            form.heartMurmur
            (config.setBoolInputMsg heartMurmurUpdateFunc)
            "heart-murmur"
            Nothing
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Lungs ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.lungs |> Maybe.withDefault [])
                    [ NormalLungs ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Wheezes, Crackles ]
            [ NormalLungs ]
            (form.lungs |> Maybe.withDefault [])
            Nothing
            config.setLungsMsg
            Translate.LungsCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Abdomen ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.abdomen |> Maybe.withDefault [])
                    [ NormalAbdomen ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Hepatomegaly, Splenomegaly, TPRightUpper, TPLeftUpper ]
            [ NormalAbdomen, Hernia, TPRightLower, TPLeftLower ]
            (form.abdomen |> Maybe.withDefault [])
            Nothing
            config.setAbdomenMsg
            Translate.AbdomenCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Extremities ]
            ]
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title hands" ] [ text (translate language Translate.Hands ++ ":") ] ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.hands |> Maybe.withDefault [])
                    [ NormalHands ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ PallorHands, EdemaHands ]
            [ NormalHands ]
            (form.hands |> Maybe.withDefault [])
            Nothing
            config.setHandsMsg
            Translate.HandsCPESign
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title legs" ] [ text (translate language Translate.Legs ++ ":") ] ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.legs |> Maybe.withDefault [])
                    [ NormalLegs ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ PallorLegs, EdemaLegs ]
            [ NormalLegs ]
            (form.legs |> Maybe.withDefault [])
            Nothing
            config.setLegsMsg
            Translate.LegsCPESign
        ]


viewMultipleTreatmentWithDosage : Language -> List RecommendedTreatmentSign -> Html any
viewMultipleTreatmentWithDosage language =
    List.map (viewTreatmentWithDosage language)
        >> List.intersperse [ b [] [ text <| " " ++ (String.toUpper <| translate language Translate.And) ++ " " ] ]
        >> List.concat
        >> label []


viewTreatmentOptionWithDosage : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionWithDosage language sign =
    if
        List.member sign
            [ NoTreatmentForHypertension
            , NoTreatmentForMalaria
            , NoTreatmentForSyphilis
            , NoTreatmentForMastitis
            , NoTreatmentForDiabetes
            ]
    then
        label [] [ text <| translate language <| Translate.RecommendedTreatmentSignLabel sign ]

    else
        viewTreatmentWithDosage language sign
            |> label []


viewTreatmentWithDosage : Language -> RecommendedTreatmentSign -> List (Html any)
viewTreatmentWithDosage language sign =
    [ span [ class "treatment" ] [ text <| (translate language <| Translate.RecommendedTreatmentSignLabel sign) ++ ":" ]
    , span [ class "dosage" ] [ text <| translate language <| Translate.RecommendedTreatmentSignDosage sign ]
    ]


viewNCDAContent :
    Language
    -> NominalDate
    -> Site
    -> PersonId
    -> Person
    -> NCDAContentConfig msg
    -> Maybe NCDASign
    -> NCDAForm
    -> ModelIndexedDb
    -> List (Html msg)
viewNCDAContent language currentDate site personId person config helperState form db =
    let
        steps =
            resolveNCDASteps currentDate person config.ncdaNeverFilled config.atHealthCenter

        currentStep =
            Maybe.Extra.or form.step (List.head steps)

        viewTask step =
            let
                iconClass =
                    case step of
                        NCDAStepAntenatalCare ->
                            "ncda-antenatal"

                        NCDAStepUniversalInterventions ->
                            "ncda-universal-intervention"

                        NCDAStepNutritionBehavior ->
                            "ncda-nutrition-behavior"

                        NCDAStepNutritionAssessment ->
                            "nutrition-assessment"

                        NCDAStepTargetedInterventions ->
                            "ncda-targeted-intervention"

                        NCDAStepInfrastructureEnvironment ->
                            "ncda-infrastructure-environment"

                isActive =
                    currentStep == Just step

                isCompleted =
                    isTaskCompleted tasksCompletedFromTotalDict step

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| config.setStepMsg step ]
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NCDAStep step)
                    ]
                ]

        formHtmlAndTasks =
            List.map
                (\step ->
                    ( step
                    , ncdaFormInputsAndTasks language
                        currentDate
                        site
                        personId
                        person
                        config
                        form
                        step
                        db
                    )
                )
                steps
                |> Dict.fromList

        tasksCompletedFromTotalDict =
            Dict.map
                (\_ ( _, tasks_ ) ->
                    ( List.map taskCompleted tasks_
                        |> List.sum
                    , List.length tasks_
                    )
                )
                formHtmlAndTasks

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.map
                (\step ->
                    let
                        html =
                            Dict.get step formHtmlAndTasks
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault []

                        ( completed, total ) =
                            Dict.get step tasksCompletedFromTotalDict
                                |> Maybe.withDefault ( 0, 0 )
                    in
                    ( html, completed, total )
                )
                currentStep
                |> Maybe.withDefault ( [], 0, 0 )

        ( header, actions ) =
            Maybe.map
                (\step ->
                    let
                        actionButton =
                            Pages.Utils.saveButton language (tasksCompleted == totalTasks)
                    in
                    if config.showTasksTray then
                        let
                            actionMsg =
                                List.filter
                                    (\step_ ->
                                        (Just step_ /= currentStep)
                                            && (not <| isTaskCompleted tasksCompletedFromTotalDict step_)
                                    )
                                    steps
                                    |> List.head
                                    |> Maybe.map config.setStepMsg
                                    |> Maybe.withDefault config.saveMsg
                        in
                        ( div [ class "ui task segment blue", Attr.id tasksBarId ]
                            [ div [ class "ui five column grid" ] <|
                                List.map viewTask steps
                            ]
                        , div [ class "actions" ]
                            [ actionButton actionMsg ]
                        )

                    else
                        ( emptyNode
                        , List.Extra.elemIndex step steps
                            |> Maybe.map
                                (\stepIndex ->
                                    let
                                        backButton backStep =
                                            button
                                                [ class "ui fluid primary button"
                                                , onClick <| config.setStepMsg backStep
                                                ]
                                                [ text ("< " ++ translate language Translate.Back) ]

                                        previousStep =
                                            List.Extra.getAt (stepIndex - 1) steps

                                        nextStep =
                                            List.Extra.getAt (stepIndex + 1) steps
                                    in
                                    case ( previousStep, nextStep ) of
                                        ( Nothing, Just next ) ->
                                            div [ class "actions" ]
                                                [ actionButton (config.setStepMsg next) ]

                                        ( Just prev, Just next ) ->
                                            div [ class "actions two" ]
                                                [ backButton prev
                                                , actionButton (config.setStepMsg next)
                                                ]

                                        ( Just prev, Nothing ) ->
                                            div [ class "actions two" ]
                                                [ backButton prev
                                                , actionButton config.saveMsg
                                                ]

                                        ( Nothing, Nothing ) ->
                                            div [ class "actions" ]
                                                [ actionButton config.saveMsg ]
                                )
                            |> Maybe.withDefault emptyNode
                        )
                )
                currentStep
                |> Maybe.withDefault ( emptyNode, emptyNode )
    in
    [ header
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form ncda" ] viewForm ]
        , actions
        ]
    , viewModal <|
        viewNCDAHelperDialog language (config.setHelperStateMsg Nothing) helperState
    ]


ncdaFormInputsAndTasks :
    Language
    -> NominalDate
    -> Site
    -> PersonId
    -> Person
    -> NCDAContentConfig msg
    -> NCDAForm
    -> NCDAStep
    -> ModelIndexedDb
    -> ( List (Html msg), List (Maybe Bool) )
ncdaFormInputsAndTasks language currentDate site personId person config form currentStep db =
    let
        inputsAndTasksForSign sign =
            case sign of
                SupplementsDuringPregnancy ->
                    let
                        updateFunc value form_ =
                            { form_ | supplementsDuringPregnancy = Just value, takenSupplementsPerGuidance = Nothing }

                        ( derivedInputs, derivedTasks ) =
                            if form.supplementsDuringPregnancy == Just True then
                                inputsAndTasksForSign TakenSupplementsPerGuidance

                            else
                                ( [], [] )

                        counseling =
                            if
                                (form.supplementsDuringPregnancy == Just False)
                                    || (form.takenSupplementsPerGuidance == Just False)
                            then
                                [ viewCounselingLabel SupplementsDuringPregnancy ]

                            else
                                []
                    in
                    ( viewNCDAInput SupplementsDuringPregnancy form.supplementsDuringPregnancy updateFunc
                        ++ derivedInputs
                        ++ counseling
                    , form.supplementsDuringPregnancy :: derivedTasks
                    )

                TakenSupplementsPerGuidance ->
                    let
                        updateFunc value form_ =
                            { form_ | takenSupplementsPerGuidance = Just value }
                    in
                    ( viewNCDAInput TakenSupplementsPerGuidance form.takenSupplementsPerGuidance updateFunc
                    , [ form.takenSupplementsPerGuidance ]
                    )

                ChildBehindOnVaccination ->
                    if isBehindOnVaccinationsByProgress currentDate site personId db then
                        let
                            updateFunc value form_ =
                                { form_ | childBehindOnVaccination = Just value }

                            counseling =
                                Maybe.map
                                    (\childBehind ->
                                        if childBehind then
                                            [ viewCounselingLabel ChildBehindOnVaccination ]

                                        else
                                            [ viewCustomLabel language Translate.NCDAUpdateVaccineRecordMessage "." "label counseling" ]
                                    )
                                    form.childBehindOnVaccination
                                    |> Maybe.withDefault []

                            lastScheduledImmunizationVisitDate =
                                resoloveLastScheduledImmunizationVisitDate personId db
                        in
                        ( viewCustomLabel language (Translate.NCDANumberImmunizationAppointmentLabel lastScheduledImmunizationVisitDate) "." "label"
                            :: viewNCDAInput ChildBehindOnVaccination form.childBehindOnVaccination updateFunc
                            ++ counseling
                        , [ form.childBehindOnVaccination ]
                        )

                    else
                        ( [], [] )

                ShowsEdemaSigns ->
                    let
                        updateFunc value form_ =
                            { form_ | showsEdemaSigns = Just value }
                    in
                    ( viewNCDAInput ShowsEdemaSigns form.showsEdemaSigns updateFunc
                    , [ form.showsEdemaSigns ]
                    )

                OngeraMNP ->
                    let
                        updateFunc value form_ =
                            { form_
                                | ongeraMNP = Just value
                                , takingOngeraMNP = Nothing
                            }

                        ( derivedInputs, derivedTasks ) =
                            if not config.atHealthCenter && form.ongeraMNP == Just True then
                                inputsAndTasksForSign TakingOngeraMNP

                            else
                                ( [], [] )

                        counseling =
                            if form.ongeraMNP == Just False || form.takingOngeraMNP == Just False then
                                [ viewCounselingLabel OngeraMNP ]

                            else
                                []
                    in
                    ( viewNCDAInput OngeraMNP form.ongeraMNP updateFunc
                        ++ derivedInputs
                        ++ counseling
                    , form.ongeraMNP :: derivedTasks
                    )

                TakingOngeraMNP ->
                    let
                        updateFunc value form_ =
                            { form_ | takingOngeraMNP = Just value }
                    in
                    ( viewNCDAInput TakingOngeraMNP form.takingOngeraMNP updateFunc
                    , [ form.takingOngeraMNP ]
                    )

                FiveFoodGroups ->
                    let
                        updateFunc value form_ =
                            { form_ | fiveFoodGroups = Just value }

                        counseling =
                            if form.fiveFoodGroups == Just False then
                                [ viewCounselingLabel FiveFoodGroups ]

                            else
                                []
                    in
                    ( [ div [ class "label-with-helper" ]
                            [ viewQuestionLabel language <| Translate.NCDASignQuestion FiveFoodGroups
                            , div
                                [ class "label-helper"
                                , onClick <| config.setHelperStateMsg (Just FiveFoodGroups)
                                ]
                                [ img [ src "assets/images/question-mark.svg" ] [] ]
                            ]
                      , viewBoolInput
                            language
                            form.fiveFoodGroups
                            (config.setBoolInputMsg updateFunc)
                            ""
                            Nothing
                      ]
                        ++ counseling
                    , [ form.fiveFoodGroups ]
                    )

                BreastfedForSixMonths ->
                    let
                        updateFunc value form_ =
                            { form_ | breastfedForSixMonths = Just value }

                        counseling =
                            if form.breastfedForSixMonths == Just False then
                                [ viewCounselingLabel BreastfedForSixMonths ]

                            else
                                []
                    in
                    ( viewNCDAInput BreastfedForSixMonths form.breastfedForSixMonths updateFunc ++ counseling
                    , [ form.breastfedForSixMonths ]
                    )

                AppropriateComplementaryFeeding ->
                    let
                        updateFunc value form_ =
                            { form_ | appropriateComplementaryFeeding = Just value }

                        counseling =
                            if form.appropriateComplementaryFeeding == Just False then
                                [ viewCounselingLabel AppropriateComplementaryFeeding ]

                            else
                                []
                    in
                    ( viewNCDAInput AppropriateComplementaryFeeding form.appropriateComplementaryFeeding updateFunc ++ counseling
                    , [ form.appropriateComplementaryFeeding ]
                    )

                MealsAtRecommendedTimes ->
                    let
                        updateFunc value form_ =
                            { form_ | mealsAtRecommendedTimes = Just value }

                        counseling =
                            if form.mealsAtRecommendedTimes == Just False then
                                [ viewCounselingLabel MealsAtRecommendedTimes ]

                            else
                                []
                    in
                    ( [ div [ class "label-with-helper" ]
                            [ viewQuestionLabel language <| Translate.NCDASignQuestion MealsAtRecommendedTimes
                            , div
                                [ class "label-helper"
                                , onClick <| config.setHelperStateMsg (Just MealsAtRecommendedTimes)
                                ]
                                [ img [ src "assets/images/question-mark.svg" ] [] ]
                            ]
                      , viewBoolInput
                            language
                            form.mealsAtRecommendedTimes
                            (config.setBoolInputMsg updateFunc)
                            ""
                            Nothing
                      ]
                        ++ counseling
                    , [ form.mealsAtRecommendedTimes ]
                    )

                ChildReceivesFBF ->
                    let
                        updateFunc value form_ =
                            { form_ | childReceivesFBF = Just value, childTakingFBF = Nothing }

                        ( derivedInputs, derivedTasks ) =
                            if form.childReceivesFBF == Just True then
                                inputsAndTasksForSign ChildTakingFBF

                            else
                                ( [], [] )

                        counseling =
                            if form.childReceivesFBF == Just False || form.childTakingFBF == Just False then
                                [ viewCounselingLabel ChildReceivesFBF ]

                            else
                                []
                    in
                    ( viewNCDAInput ChildReceivesFBF form.childReceivesFBF updateFunc
                        ++ derivedInputs
                        ++ counseling
                    , form.childReceivesFBF :: derivedTasks
                    )

                ChildTakingFBF ->
                    let
                        updateFunc value form_ =
                            { form_ | childTakingFBF = Just value }
                    in
                    ( viewNCDAInput ChildTakingFBF form.childTakingFBF updateFunc
                    , [ form.childTakingFBF ]
                    )

                ChildReceivesVitaminA ->
                    let
                        childReceivesVitaminAInput =
                            [ viewQuestionLabel language <| Translate.NCDASignQuestion ChildReceivesVitaminA
                            , viewCheckBoxSelectInput language
                                [ OptionReceive, OptionNotReceive ]
                                [ OptionNotApplicable ]
                                form.childReceivesVitaminA
                                config.setChildReceivesVitaminAMsg
                                Translate.ReceiveOption
                            ]

                        counseling =
                            if form.childReceivesVitaminA == Just OptionNotReceive then
                                [ viewCounselingLabel ChildReceivesVitaminA ]

                            else
                                []
                    in
                    ( childReceivesVitaminAInput
                        ++ counseling
                    , [ maybeToBoolTask form.childReceivesVitaminA ]
                    )

                ChildReceivesDewormer ->
                    let
                        updateFunc value form_ =
                            { form_ | childReceivesDewormer = Just value }

                        counseling =
                            if form.childReceivesDewormer == Just False then
                                [ viewCounselingLabel ChildReceivesDewormer ]

                            else
                                []
                    in
                    ( viewNCDAInput ChildReceivesDewormer form.childReceivesDewormer updateFunc
                        ++ counseling
                    , [ form.childReceivesDewormer ]
                    )

                ChildReceivesECD ->
                    let
                        updateFunc value form_ =
                            { form_ | childReceivesECD = Just value }

                        counseling =
                            if form.childReceivesECD == Just False then
                                [ viewCounselingLabel ChildReceivesECD ]

                            else
                                []
                    in
                    ( viewNCDAInput ChildReceivesECD form.childReceivesECD updateFunc
                        ++ counseling
                    , [ form.childReceivesECD ]
                    )

                BeneficiaryCashTransfer ->
                    let
                        updateFunc value form_ =
                            { form_ | beneficiaryCashTransfer = Just value, receivingCashTransfer = Nothing }

                        ( derivedInputs, derivedTasks ) =
                            if form.beneficiaryCashTransfer == Just True then
                                inputsAndTasksForSign ReceivingCashTransfer

                            else
                                ( [], [] )

                        counseling =
                            if form.receivingCashTransfer == Just False then
                                [ viewCounselingLabel BeneficiaryCashTransfer ]

                            else
                                []
                    in
                    ( viewNCDAInput BeneficiaryCashTransfer form.beneficiaryCashTransfer updateFunc
                        ++ derivedInputs
                        ++ counseling
                    , form.beneficiaryCashTransfer :: derivedTasks
                    )

                ReceivingCashTransfer ->
                    let
                        updateFunc value form_ =
                            { form_ | receivingCashTransfer = Just value }
                    in
                    ( viewNCDAInput ReceivingCashTransfer form.receivingCashTransfer updateFunc
                    , [ form.receivingCashTransfer ]
                    )

                ConditionalFoodItems ->
                    let
                        updateFunc value form_ =
                            { form_ | conditionalFoodItems = Just value }

                        counseling =
                            if form.conditionalFoodItems == Just False then
                                [ viewCounselingLabel ConditionalFoodItems ]

                            else
                                []
                    in
                    ( viewNCDAInput ConditionalFoodItems form.conditionalFoodItems updateFunc ++ counseling
                    , [ form.conditionalFoodItems ]
                    )

                TreatedForAcuteMalnutrition ->
                    let
                        updateFunc value form_ =
                            { form_ | treatedForAcuteMalnutrition = Just value }

                        counseling =
                            if form.treatedForAcuteMalnutrition == Just False then
                                [ viewCounselingLabel TreatedForAcuteMalnutrition ]

                            else
                                []
                    in
                    ( viewCustomLabel language Translate.ChildHasMalnutritionPhrase "." "label red"
                        :: viewNCDAInput TreatedForAcuteMalnutrition form.treatedForAcuteMalnutrition updateFunc
                        ++ counseling
                    , [ form.treatedForAcuteMalnutrition ]
                    )

                ChildWithDisability ->
                    let
                        updateFunc value form_ =
                            { form_ | childWithDisability = Just value, receivingSupport = Nothing }

                        ( derivedInputs, derivedTasks ) =
                            if form.childWithDisability == Just True then
                                inputsAndTasksForSign ReceivingSupport

                            else
                                ( [], [] )
                    in
                    ( viewNCDAInput ChildWithDisability form.childWithDisability updateFunc
                        ++ derivedInputs
                    , form.childWithDisability :: derivedTasks
                    )

                ReceivingSupport ->
                    let
                        updateFunc value form_ =
                            { form_ | receivingSupport = Just value }

                        counseling =
                            if form.receivingSupport == Just False then
                                [ viewCounselingLabel ReceivingSupport ]

                            else
                                []
                    in
                    ( viewNCDAInput ReceivingSupport form.receivingSupport updateFunc ++ counseling
                    , [ form.receivingSupport ]
                    )

                ChildGotDiarrhea ->
                    let
                        updateFunc value form_ =
                            { form_ | childGotDiarrhea = Just value }
                    in
                    ( viewNCDAInput ChildGotDiarrhea form.childGotDiarrhea updateFunc
                    , [ form.childGotDiarrhea ]
                    )

                HasCleanWater ->
                    let
                        updateFunc value form_ =
                            { form_ | hasCleanWater = Just value }

                        counseling =
                            if form.hasCleanWater == Just False then
                                [ viewCounselingLabel HasCleanWater ]

                            else
                                []
                    in
                    ( viewNCDAInput HasCleanWater form.hasCleanWater updateFunc ++ counseling
                    , [ form.hasCleanWater ]
                    )

                HasHandwashingFacility ->
                    let
                        updateFunc value form_ =
                            { form_ | hasHandwashingFacility = Just value }

                        counseling =
                            if form.hasHandwashingFacility == Just False then
                                [ viewCounselingLabel HasHandwashingFacility ]

                            else
                                []
                    in
                    ( viewNCDAInput HasHandwashingFacility form.hasHandwashingFacility updateFunc ++ counseling
                    , [ form.hasHandwashingFacility ]
                    )

                HasToilets ->
                    let
                        updateFunc value form_ =
                            { form_ | hasToilets = Just value }

                        counseling =
                            if form.hasToilets == Just False then
                                [ viewCounselingLabel HasToilets ]

                            else
                                []
                    in
                    ( viewNCDAInput HasToilets form.hasToilets updateFunc ++ counseling
                    , [ form.hasToilets ]
                    )

                HasKitchenGarden ->
                    let
                        updateFunc value form_ =
                            { form_ | hasKitchenGarden = Just value }

                        counseling =
                            if form.hasKitchenGarden == Just False then
                                [ viewCounselingLabel HasKitchenGarden ]

                            else
                                []
                    in
                    ( viewNCDAInput HasKitchenGarden form.hasKitchenGarden updateFunc ++ counseling
                    , [ form.hasKitchenGarden ]
                    )

                InsecticideTreatedBednets ->
                    let
                        updateFunc value form_ =
                            { form_ | insecticideTreatedBednets = Just value }

                        counseling =
                            if form.insecticideTreatedBednets == Just False then
                                [ viewCounselingLabel InsecticideTreatedBednets ]

                            else
                                []
                    in
                    ( viewNCDAInput InsecticideTreatedBednets form.insecticideTreatedBednets updateFunc ++ counseling
                    , [ form.insecticideTreatedBednets ]
                    )

                BornWithBirthDefect ->
                    let
                        updateFunc value form_ =
                            { form_ | bornWithBirthDefect = Just value }
                    in
                    ( viewNCDAInput BornWithBirthDefect form.bornWithBirthDefect updateFunc
                    , [ form.bornWithBirthDefect ]
                    )

                NoNCDASigns ->
                    ( [], [] )

        viewNCDAInput sign value updateFunc =
            [ viewQuestionLabel language <| Translate.NCDASignQuestion sign
            , viewBoolInput
                language
                value
                (config.setBoolInputMsg updateFunc)
                ""
                Nothing
            ]

        viewCounselingLabel sign =
            viewCustomLabel language (Translate.NCDASignCounseling sign) "." "label counseling"
    in
    case currentStep of
        NCDAStepAntenatalCare ->
            let
                ( ancVisitsSection, ancVisitsTasks ) =
                    ancVisitsInpustAndTasks language personId person config form db

                ( signsInputs, signTasks ) =
                    inputsAndTasksForSign SupplementsDuringPregnancy

                ( newbornExamSection, newbornExamTasks ) =
                    if showNCDAQuestionsByNewbornExam config.pregnancySummary then
                        let
                            ( birthWeightSection, birthWeightTasks ) =
                                birthWeightInputsAndTasks language form.birthWeight config.setBirthWeightMsg

                            ( birthDefectSection, birthDefectTask ) =
                                inputsAndTasksForSign BornWithBirthDefect
                        in
                        ( birthWeightSection ++ birthDefectSection
                        , birthDefectTask ++ birthWeightTasks
                        )

                    else
                        ( [], [] )
            in
            ( ancVisitsSection
                ++ signsInputs
                ++ newbornExamSection
            , ancVisitsTasks
                ++ signTasks
                ++ newbornExamTasks
            )

        NCDAStepUniversalInterventions ->
            let
                signs =
                    if config.atHealthCenter then
                        [ OngeraMNP ]

                    else
                        [ ChildBehindOnVaccination
                        , ChildReceivesVitaminA
                        , ChildReceivesDewormer
                        , OngeraMNP
                        , ChildReceivesECD
                        ]

                inputsAndTasks =
                    List.map inputsAndTasksForSign signs
            in
            ( List.concatMap Tuple.first inputsAndTasks
            , List.concatMap Tuple.second inputsAndTasks
            )

        NCDAStepNutritionBehavior ->
            let
                breastfeedingSign =
                    if config.ncdaNotFilledAfterAgeOfSixMonths then
                        [ BreastfedForSixMonths ]

                    else
                        []

                signs =
                    FiveFoodGroups :: breastfeedingSign ++ [ AppropriateComplementaryFeeding, MealsAtRecommendedTimes ]

                inputsAndTasks =
                    List.map inputsAndTasksForSign signs
            in
            ( List.concatMap Tuple.first inputsAndTasks
            , List.concatMap Tuple.second inputsAndTasks
            )

        NCDAStepNutritionAssessment ->
            let
                ( stuntingLevelInput, stuntingLevelTask ) =
                    let
                        measurementNotTakenChecked =
                            form.stuntingLevelNotTaken == Just True

                        measurementNotTakenUpdateFunc value form_ =
                            { form_ | stuntingLevelNotTaken = Just value, stuntingLevel = Nothing }

                        measurementNotTakenValueWhenChecked =
                            Maybe.map not form.stuntingLevelNotTaken
                                |> Maybe.withDefault True

                        inputSection =
                            if measurementNotTakenChecked then
                                []

                            else
                                [ viewCheckBoxSelectInput language
                                    [ LevelGreen, LevelYellow ]
                                    [ LevelRed ]
                                    form.stuntingLevel
                                    config.setStuntingLevelMsg
                                    Translate.StuntingLevel
                                ]

                        notTakenCheckbox =
                            [ div
                                [ class "ui checkbox activity skip-step"
                                , onClick <| config.setBoolInputMsg measurementNotTakenUpdateFunc measurementNotTakenValueWhenChecked
                                ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked measurementNotTakenChecked
                                    , classList [ ( "checked", measurementNotTakenChecked ) ]
                                    ]
                                    []
                                , label [] [ text <| translate language Translate.MeasurementNotTaken ]
                                ]
                            ]
                    in
                    ( viewLabel language Translate.StuntingLevelLabel :: inputSection ++ notTakenCheckbox
                    , [ if measurementNotTakenChecked then
                            form.stuntingLevelNotTaken

                        else
                            maybeToBoolTask form.stuntingLevel
                      ]
                    )

                ( weightInput, weightTask ) =
                    let
                        measurementNotTakenChecked =
                            form.weightNotTaken == Just True

                        measurementNotTakenUpdateFunc value form_ =
                            { form_ | weightNotTaken = Just value, weight = Nothing }

                        measurementNotTakenValueWhenChecked =
                            Maybe.map not form.weightNotTaken
                                |> Maybe.withDefault True

                        inputSection =
                            if measurementNotTakenChecked then
                                []

                            else
                                let
                                    weightAsFloat =
                                        Maybe.map (\(WeightInKg weight) -> weight)
                                            form.weight
                                in
                                [ viewMeasurementInput
                                    language
                                    weightAsFloat
                                    config.setWeightMsg
                                    "weight"
                                    Translate.KilogramShorthand
                                ]

                        notTakenCheckbox =
                            [ div
                                [ class "ui checkbox activity skip-step"
                                , onClick <| config.setBoolInputMsg measurementNotTakenUpdateFunc measurementNotTakenValueWhenChecked
                                ]
                                [ input
                                    [ type_ "checkbox"
                                    , checked measurementNotTakenChecked
                                    , classList [ ( "checked", measurementNotTakenChecked ) ]
                                    ]
                                    []
                                , label [] [ text <| translate language Translate.MeasurementNotTaken ]
                                ]
                            ]
                    in
                    ( viewLabel language Translate.Weight :: inputSection ++ notTakenCheckbox
                    , [ if measurementNotTakenChecked then
                            form.weightNotTaken

                        else
                            maybeToBoolTask form.weight
                      ]
                    )

                ( muacInput, muacTask ) =
                    ageInMonths currentDate person
                        |> Maybe.map
                            (\ageMonths ->
                                if ageMonths >= 6 then
                                    let
                                        measurementNotTakenChecked =
                                            form.muacNotTaken == Just True

                                        measurementNotTakenUpdateFunc value form_ =
                                            { form_ | muacNotTaken = Just value, muac = Nothing }

                                        measurementNotTakenValueWhenChecked =
                                            Maybe.map not form.muacNotTaken
                                                |> Maybe.withDefault True

                                        inputSection =
                                            if measurementNotTakenChecked then
                                                []

                                            else
                                                let
                                                    muacAsFloat =
                                                        Maybe.map (\(MuacInCm muac) -> muac)
                                                            form.muac
                                                in
                                                [ div [ class "ui grid" ]
                                                    [ div [ class "eleven wide column" ]
                                                        [ viewMeasurementInput
                                                            language
                                                            muacAsFloat
                                                            config.setMuacMsg
                                                            "muac"
                                                            Translate.UnitCentimeter
                                                        ]
                                                    , div
                                                        [ class "five wide column" ]
                                                        [ showMaybe <|
                                                            Maybe.map (muacIndication >> viewColorAlertIndication language) form.muac
                                                        ]
                                                    ]
                                                ]

                                        notTakenCheckbox =
                                            [ div
                                                [ class "ui checkbox activity skip-step"
                                                , onClick <| config.setBoolInputMsg measurementNotTakenUpdateFunc measurementNotTakenValueWhenChecked
                                                ]
                                                [ input
                                                    [ type_ "checkbox"
                                                    , checked measurementNotTakenChecked
                                                    , classList [ ( "checked", measurementNotTakenChecked ) ]
                                                    ]
                                                    []
                                                , label [] [ text <| translate language Translate.MeasurementNotTaken ]
                                                ]
                                            ]
                                    in
                                    ( viewLabel language Translate.MUAC :: inputSection ++ notTakenCheckbox
                                    , [ if measurementNotTakenChecked then
                                            form.muacNotTaken

                                        else
                                            maybeToBoolTask form.muac
                                      ]
                                    )

                                else
                                    ( [], [] )
                            )
                        |> Maybe.withDefault ( [], [] )

                ( edemaInput, edemaTask ) =
                    inputsAndTasksForSign ShowsEdemaSigns
            in
            ( List.filter (List.isEmpty >> not)
                [ stuntingLevelInput
                , weightInput
                , muacInput
                , edemaInput
                ]
                |> List.intersperse [ div [ class "separator" ] [] ]
                |> List.concat
            , stuntingLevelTask
                ++ weightTask
                ++ muacTask
                ++ edemaTask
            )

        NCDAStepTargetedInterventions ->
            let
                childReceivesFBFSign =
                    if config.atHealthCenter then
                        []

                    else
                        [ ChildReceivesFBF ]

                treatedForAcuteMalnutritionSign =
                    if not config.atHealthCenter && muacMeasurementIsOff form.muac then
                        [ TreatedForAcuteMalnutrition ]

                    else
                        []

                childGotDiarrheaSign =
                    if config.atHealthCenter then
                        []

                    else
                        [ ChildGotDiarrhea ]

                signs =
                    childReceivesFBFSign
                        ++ [ BeneficiaryCashTransfer
                           , ConditionalFoodItems
                           ]
                        ++ treatedForAcuteMalnutritionSign
                        ++ [ ChildWithDisability ]
                        ++ childGotDiarrheaSign

                inputsAndTasks =
                    List.map inputsAndTasksForSign signs
            in
            ( List.concatMap Tuple.first inputsAndTasks
            , List.concatMap Tuple.second inputsAndTasks
            )

        NCDAStepInfrastructureEnvironment ->
            let
                inputsAndTasks =
                    List.map inputsAndTasksForSign
                        [ HasCleanWater
                        , HasHandwashingFacility
                        , HasToilets
                        , HasKitchenGarden
                        , InsecticideTreatedBednets
                        ]
            in
            ( List.concatMap Tuple.first inputsAndTasks
            , List.concatMap Tuple.second inputsAndTasks
            )


ancVisitsInpustAndTasks :
    Language
    -> PersonId
    -> Person
    -> NCDAContentConfig msg
    -> NCDAForm
    -> ModelIndexedDb
    -> ( List (Html msg), List (Maybe Bool) )
ancVisitsInpustAndTasks language personId person config form db =
    Maybe.map
        (\birthDate ->
            let
                ( eddDate, encountersDatesFromANCData ) =
                    resolveChildANCPregnancyData personId db

                historySection =
                    let
                        entriesForView =
                            EverySet.toList encountersDatesFromANCData
                                |> List.sortWith Date.compare
                                |> List.indexedMap
                                    (\index date -> viewHistoryEntry (String.fromInt <| index + 1) date)

                        viewHistoryEntry index date =
                            div [ class "history-entry" ]
                                [ div [ class "dose" ] [ text index ]
                                , div [ class "date" ] [ text <| formatDDMMYYYY date ]
                                ]

                        visitsForView =
                            if List.isEmpty entriesForView then
                                [ viewCustomLabel language Translate.NCDANoANVCVisitsOnRecord "." "label-normal" ]

                            else
                                entriesForView
                    in
                    div [ class "history" ]
                        visitsForView

                ( inputs, tasks ) =
                    let
                        encountersDatesFromForm =
                            -- Since ANC step of NCDA form is filled only once, we know
                            -- that current activity is the first one filled, and there's
                            -- no need to examine existing NCDA activities.
                            Maybe.withDefault EverySet.empty form.ancVisitsDates

                        ( derivedInputs, derivedTasks ) =
                            if form.updateANCVisits == Just True then
                                let
                                    pregnancyStartDate =
                                        Maybe.map eddToLmpDate eddDate
                                            |> Maybe.withDefault
                                                -- If we don't have LMP date, we'll assume that
                                                -- pregnancy was complete (lasted 9 months).
                                                (eddToLmpDate birthDate)

                                    encountersMonthsFromANCData =
                                        EverySet.toList encountersDatesFromANCData
                                            |> List.map
                                                (\encounterDate ->
                                                    Date.diff Date.Months pregnancyStartDate encounterDate + 1
                                                )

                                    encountersMonthsFromForm =
                                        EverySet.toList encountersDatesFromForm
                                            |> List.map
                                                (\encounterDate ->
                                                    Date.diff Date.Months pregnancyStartDate encounterDate + 1
                                                )

                                    content =
                                        List.range 1 9
                                            |> List.map
                                                (\monthNumber ->
                                                    div [ class "item" ]
                                                        [ div [ class "month-number" ] [ text <| String.fromInt monthNumber ]
                                                        , viewRadioButton monthNumber
                                                        ]
                                                )
                                            |> greedyGroupsOf 3
                                            |> List.map (div [ class "trimester" ])
                                            |> List.intersperse
                                                (div [ class "trimesters-separator" ]
                                                    [ div [ class "section left" ]
                                                        [ div [ class "top" ] []
                                                        , div [ class "bottom" ] []
                                                        ]
                                                    , div [ class "section right" ]
                                                        [ div [ class "top" ] []
                                                        , div [ class "bottom" ] []
                                                        ]
                                                    ]
                                                )

                                    viewRadioButton monthNumber =
                                        let
                                            isChecked =
                                                List.member monthNumber encountersMonthsFromANCData
                                                    || List.member monthNumber encountersMonthsFromForm

                                            disabled =
                                                List.member monthNumber encountersMonthsFromANCData

                                            dateForMonth =
                                                Date.add Date.Months (monthNumber - 1) pregnancyStartDate
                                                    |> Date.add Date.Days 14
                                        in
                                        div [ class "month-radio" ]
                                            [ input
                                                [ type_ "radio"
                                                , checked isChecked
                                                , classList
                                                    [ ( "checked", isChecked )
                                                    , ( "disabled", disabled )
                                                    ]
                                                ]
                                                []
                                            , label [ onClick <| config.toggleANCVisitDateMsg dateForMonth ] [ text "" ]
                                            ]
                                in
                                ( [ viewLabel language Translate.ANCIndicateVisitsMonthsPhrase
                                  , div [ class "form-input anc-months" ]
                                        content
                                  ]
                                , [ maybeToBoolTask form.ancVisitsDates ]
                                )

                            else
                                ( [], [] )

                        counseling =
                            if isJust form.updateANCVisits then
                                let
                                    ancDataVisits =
                                        EverySet.size encountersDatesFromANCData

                                    formVisists =
                                        EverySet.size encountersDatesFromForm
                                in
                                if (ancDataVisits + formVisists) < 4 then
                                    viewCustomLabel language Translate.NCDAANCVisitsCounseling "." "label counseling"

                                else
                                    emptyNode

                            else
                                emptyNode
                    in
                    ( [ viewQuestionLabel language Translate.ANCEncountersNotRecordedQuestion
                      , viewBoolInput
                            language
                            form.updateANCVisits
                            config.setUpdateANCVisitsMsg
                            ""
                            Nothing
                      ]
                        ++ derivedInputs
                        ++ [ counseling ]
                    , form.updateANCVisits :: derivedTasks
                    )
            in
            ( [ viewLabel language Translate.AntenatalVisistsHistory
              , historySection
              ]
                ++ inputs
            , tasks
            )
        )
        person.birthDate
        |> Maybe.withDefault ( [], [] )


showNCDAQuestionsByNewbornExam : Maybe PregnancySummaryValue -> Bool
showNCDAQuestionsByNewbornExam newbornExamPregnancySummary =
    -- Verify that NCDA related questions were not answered at Neborn exam.
    -- This can happen, because needed questions were added after
    -- Newborn exam was launched, so, it could have been filled
    -- without them.
    -- It's enough to check if one of the questions was answered,
    -- because both answereds are required to save the form.
    Maybe.map (.birthWeight >> isNothing) newbornExamPregnancySummary
        |> Maybe.withDefault True


birthWeightInputsAndTasks : Language -> Maybe WeightInGrm -> (String -> msg) -> ( List (Html msg), List (Maybe Bool) )
birthWeightInputsAndTasks language birthWeight setBirthWeightMsg =
    let
        colorAlertIndication =
            Maybe.map
                (\weight ->
                    if weight < 2500 then
                        div
                            [ class "four wide column" ]
                            [ viewColorAlertIndication language ColorAlertRed ]

                    else
                        emptyNode
                )
                birthWeightAsFloat

        birthWeightAsFloat =
            Maybe.map (\(WeightInGrm weight) -> weight)
                birthWeight
    in
    ( [ viewQuestionLabel language Translate.NCDABirthweightQuestion
      , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewMeasurementInput language
                    birthWeightAsFloat
                    setBirthWeightMsg
                    "birth-weight"
                    Translate.Grams
                ]
            , showMaybe colorAlertIndication
            ]
      ]
    , [ maybeToBoolTask birthWeight ]
    )


viewNCDAHelperDialog : Language -> msg -> Maybe NCDASign -> Maybe (Html msg)
viewNCDAHelperDialog language action helperState =
    Maybe.andThen
        (\sign ->
            let
                viewHelperDialog dialogContent =
                    div [ class "ui active modal ncda-helper-popup" ]
                        [ div [ class "header" ]
                            [ viewLabel language <| Translate.NCDASignHelperHeader sign ]
                        , div
                            [ class "content" ]
                            [ dialogContent ]
                        , div
                            [ class "actions" ]
                            [ button
                                [ class "ui fluid primary button"
                                , onClick action
                                ]
                                [ text <| translate language Translate.Close ]
                            ]
                        ]
            in
            case sign of
                FiveFoodGroups ->
                    fiveFoodGroupsHelperDialog language
                        |> viewHelperDialog
                        |> Just

                MealsAtRecommendedTimes ->
                    mealsAtRecommendedTimesHelperDialog language
                        |> viewHelperDialog
                        |> Just

                _ ->
                    Nothing
        )
        helperState


fiveFoodGroupsHelperDialog : Language -> Html msg
fiveFoodGroupsHelperDialog language =
    ol [] <|
        List.map
            (\foodGroup ->
                li [] [ text <| translate language <| Translate.GroupOfFoods foodGroup ]
            )
            [ Staples
            , Legumes
            , DairyProducts
            , AnimalSourceFoods
            , Eggs
            , FruitsVegetables
            , BreastMilk
            , MealsWithEdibleOil
            ]


mealsAtRecommendedTimesHelperDialog : Language -> Html msg
mealsAtRecommendedTimesHelperDialog language =
    ul []
        [ li [] [ text <| translate language Translate.NCDAMealFrequency6to9 ]
        , li [] [ text <| translate language Translate.NCDAMealFrequency9to12 ]
        , li [] [ text <| translate language Translate.NCDAMealFrequency12to24 ]
        ]


viewNCDA :
    Language
    -> NominalDate
    -> Site
    -> PersonId
    -> Person
    -> MeasurementData (Maybe ( GroupNCDAId, GroupNCDA ))
    -> NCDAData
    -> ModelIndexedDb
    -> Html MsgChild
viewNCDA language currentDate site childId child measurement data db =
    let
        existingId =
            Maybe.map Tuple.first measurement.current

        saved =
            getMeasurementValueFunc measurement.current

        form =
            ncdaFormWithDefault data.form saved

        config =
            { atHealthCenter = True
            , showTasksTray = False
            , pregnancySummary = getNewbornExamPregnancySummary childId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate childId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate childId child db
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStuntingLevelMsg = SetStuntingLevel
            , setWeightMsg = SetWeight
            , setMuacMsg = SetMuac
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg =
                toNCDAValueWithDefault saved data.form
                    |> Maybe.map (SaveNCDA existingId)
                    |> Maybe.withDefault NoOp
                    |> SendOutMsgChild
            }
    in
    viewNCDAContent language
        currentDate
        site
        childId
        child
        config
        data.helperState
        form
        db
        |> div [ class "form-content ncda" ]


nutritionFeedingInputsAndTasks :
    Language
    -> PersonId
    -> ((Bool -> NutritionFeedingForm -> NutritionFeedingForm) -> Bool -> msg)
    -> (NutritionSupplementType -> msg)
    -> (String -> msg)
    -> ModelIndexedDb
    -> NutritionFeedingForm
    -> ( List (Html msg), List (Maybe Bool) )
nutritionFeedingInputsAndTasks language personId setBoolInputMsg setNutritionSupplementTypeMsg setSachetsPerDayMsg db form =
    let
        ( receiveSupplementInputs, receiveSupplementTasks ) =
            let
                ( derivedReceiveSupplementInputs, derivedReceiveSupplementTasks ) =
                    if form.receiveSupplement == Just True then
                        let
                            rationPresentAtHomeUpdateFunc value form_ =
                                { form_ | rationPresentAtHome = Just value }

                            enoughTillNextSessionUpdateFunc value form_ =
                                { form_ | enoughTillNextSession = Just value }

                            supplementSharedUpdateFunc value form_ =
                                { form_ | supplementShared = Just value }
                        in
                        ( [ viewQuestionLabel language Translate.WhatType
                          , viewCheckBoxSelectInput language
                                [ FortifiedPorridge, Rutf, Ongera, TherapeuticMilk ]
                                []
                                form.supplementType
                                setNutritionSupplementTypeMsg
                                Translate.NutritionSupplementType
                          , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion RationPresentAtHome
                          , viewBoolInput language
                                form.rationPresentAtHome
                                (setBoolInputMsg rationPresentAtHomeUpdateFunc)
                                "ration-present-at-home"
                                Nothing
                          , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EnoughTillNextSession
                          , viewBoolInput language
                                form.enoughTillNextSession
                                (setBoolInputMsg enoughTillNextSessionUpdateFunc)
                                "enough-till-next-section"
                                Nothing
                          , viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion SupplementShared
                          , viewBoolInput language
                                form.supplementShared
                                (setBoolInputMsg supplementSharedUpdateFunc)
                                "enough-till-next-section"
                                (Just ( Translate.Shared, Translate.OnlySickChild ))
                          ]
                        , [ if form.supplementType == Just NoNutritionSupplementType then
                                Nothing

                            else
                                maybeToBoolTask form.supplementType
                          , form.rationPresentAtHome
                          , form.enoughTillNextSession
                          , form.supplementShared
                          ]
                        )

                    else
                        ( [], [] )

                receiveSupplementUpdateFunc value form_ =
                    { form_
                        | receiveSupplement = Just value
                        , supplementType = Nothing
                        , rationPresentAtHome = Nothing
                        , enoughTillNextSession = Nothing
                        , supplementShared = Nothing
                    }
            in
            ( [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion ReceiveSupplement
              , viewBoolInput language
                    form.receiveSupplement
                    (setBoolInputMsg receiveSupplementUpdateFunc)
                    "receive-supplement"
                    Nothing
              ]
                ++ derivedReceiveSupplementInputs
            , form.receiveSupplement :: derivedReceiveSupplementTasks
            )

        ( ( sachetsPerDayInput, sachetsPerDayTask ), _ ) =
            if form.receiveSupplement == Just True && form.supplementType == Just Rutf then
                let
                    sachetsPerDayHelper =
                        resolveAllWeightMeasurementsForChild personId db
                            |> List.head
                            |> Maybe.map
                                (\( _, weight ) ->
                                    let
                                        recommendation =
                                            if weight < 4 then
                                                1.5

                                            else if weight < 5.5 then
                                                2

                                            else if weight < 7 then
                                                2.5

                                            else if weight < 8.5 then
                                                3

                                            else if weight < 9.5 then
                                                3.5

                                            else if weight < 10.5 then
                                                4

                                            else if weight < 12 then
                                                4.5

                                            else
                                                5
                                    in
                                    viewCustomLabel language (Translate.SachetsPerDayHelper weight recommendation) "." "helper"
                                )
                            |> Maybe.withDefault emptyNode

                    options =
                        List.repeat 20 0.5
                            |> List.indexedMap (\index number -> toFloat index * number)

                    eatenWithWaterUpdateFunc value form_ =
                        { form_ | eatenWithWater = Just value }
                in
                ( ( [ viewQuestionLabel language Translate.SachetsPerDayQuestion
                    , sachetsPerDayHelper
                    , viewCustomSelectListInput form.sachetsPerDay
                        options
                        String.fromFloat
                        setSachetsPerDayMsg
                        String.fromFloat
                        "form-input select"
                        True
                    ]
                  , [ maybeToBoolTask form.sachetsPerDay ]
                  )
                , ( [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EatenWithWater
                    , viewBoolInput language
                        form.eatenWithWater
                        (setBoolInputMsg eatenWithWaterUpdateFunc)
                        "enough-till-next-section"
                        Nothing
                    ]
                  , [ form.eatenWithWater ]
                  )
                )

            else
                ( ( [], [] ), ( [], [] ) )

        encouragedToEatInput =
            let
                encouragedToEatUpdateFunc value form_ =
                    { form_ | encouragedToEat = Just value }
            in
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion EncouragedToEat
            , viewBoolInput language
                form.encouragedToEat
                (setBoolInputMsg encouragedToEatUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        refusingToEatInput =
            let
                refusingToEatUpdateFunc value form_ =
                    { form_ | refusingToEat = Just value }
            in
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion RefusingToEat
            , viewBoolInput language
                form.refusingToEat
                (setBoolInputMsg refusingToEatUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        breastfeedingInput =
            let
                breastfeedingUpdateFunc value form_ =
                    { form_ | breastfeeding = Just value }
            in
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion FeedingSignBreastfeeding
            , viewBoolInput language
                form.breastfeeding
                (setBoolInputMsg breastfeedingUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]

        cleanWaterAvailableInput =
            let
                cleanWaterAvailableUpdateFunc value form_ =
                    { form_ | cleanWaterAvailable = Just value }
            in
            [ viewQuestionLabel language <| Translate.NutritionFeedingSignQuestion CleanWaterAvailable
            , viewBoolInput language
                form.cleanWaterAvailable
                (setBoolInputMsg cleanWaterAvailableUpdateFunc)
                "enough-till-next-section"
                Nothing
            ]
    in
    ( receiveSupplementInputs
        ++ sachetsPerDayInput
        ++ encouragedToEatInput
        ++ refusingToEatInput
        ++ breastfeedingInput
        ++ cleanWaterAvailableInput
    , receiveSupplementTasks
        ++ sachetsPerDayTask
        ++ [ form.encouragedToEat
           , form.refusingToEat
           , form.breastfeeding
           , form.cleanWaterAvailable
           ]
    )


nutritionCaringInputsAndTasks :
    Language
    -> (Bool -> msg)
    -> (CaringOption -> msg)
    -> (Bool -> msg)
    -> NutritionCaringForm
    -> ( List (Html msg), List (Maybe Bool) )
nutritionCaringInputsAndTasks language setParentsAliveAndHealthyMsg setNutritionCaringOptionMsg setChildCleanMsg form =
    ( [ viewQuestionLabel language Translate.ParentsAliveAndHealthyQuestion
      , viewBoolInput
            language
            form.parentHealth
            setParentsAliveAndHealthyMsg
            "parents-health"
            Nothing
      , viewQuestionLabel language Translate.WhoCaresForTheChildDuringTheDay
      , viewCheckBoxSelectInput language
            [ CaredByParent
            , CaredByGrandparent
            , CaredBySibling
            , CaredByNeighbor
            , CaredByHouseHelper
            , CaredByDaycare
            ]
            []
            form.caringOption
            setNutritionCaringOptionMsg
            Translate.NutritionCaringOption
      , viewQuestionLabel language Translate.ChildCleanQuestion
      , viewBoolInput
            language
            form.childClean
            setChildCleanMsg
            "child-clean"
            Nothing
      ]
    , [ form.parentHealth, maybeToBoolTask form.caringOption, form.childClean ]
    )


nutritionHygieneInputsAndTasks :
    Language
    -> ((Bool -> NutritionHygieneForm -> NutritionHygieneForm) -> Bool -> msg)
    -> (MainWaterSource -> msg)
    -> (WaterPreparationOption -> msg)
    -> NutritionHygieneForm
    -> ( List (Html msg), List (Maybe Bool) )
nutritionHygieneInputsAndTasks language setHygieneBoolInputMsg setMainWaterSourceMsg setWaterPreparationOptionMsg form =
    let
        soapInTheHouseUpdateFunc value form_ =
            { form_ | soapInTheHouse = Just value }

        washHandsBeforeFeedingUpdateFunc value form_ =
            { form_ | washHandsBeforeFeeding = Just value }

        foodCoveredUpdateFunc value form_ =
            { form_ | foodCovered = Just value }
    in
    ( [ viewQuestionLabel language Translate.MainWaterSourceQuestion
      , viewCheckBoxSelectInput language
            [ PipedWaterToHome
            , PublicWaterTap
            , RainWaterCollectionSystem
            , NaturalSourceFlowingWater
            , NaturalSourceStandingWater
            , BottledWater
            ]
            []
            form.mainWaterSource
            setMainWaterSourceMsg
            Translate.MainWaterSource
      , viewQuestionLabel language Translate.MainWaterPreparationQuestion
      , viewCheckBoxSelectInput language
            [ Boiled
            , PurificationSolution
            , Filtered
            , Bottled
            , NoWaterPreparationOption
            ]
            []
            form.waterPreparationOption
            setWaterPreparationOptionMsg
            Translate.MainWaterPreparationOption
      , viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion SoapInTheHouse
      , viewBoolInput language
            form.soapInTheHouse
            (setHygieneBoolInputMsg soapInTheHouseUpdateFunc)
            "soap-in-the-house"
            Nothing
      , viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion WashHandsBeforeFeeding
      , viewBoolInput language
            form.washHandsBeforeFeeding
            (setHygieneBoolInputMsg washHandsBeforeFeedingUpdateFunc)
            "wash-hands-before-feeding"
            Nothing
      , viewQuestionLabel language <| Translate.NutritionHygieneSignQuestion FoodCovered
      , viewBoolInput language
            form.foodCovered
            (setHygieneBoolInputMsg foodCoveredUpdateFunc)
            "food-covered"
            Nothing
      ]
    , [ maybeToBoolTask form.mainWaterSource
      , maybeToBoolTask form.waterPreparationOption
      , form.soapInTheHouse
      , form.washHandsBeforeFeeding
      , form.foodCovered
      ]
    )


nutritionFoodSecurityInputsAndTasks :
    Language
    -> ((Bool -> NutritionFoodSecurityForm -> NutritionFoodSecurityForm) -> Bool -> msg)
    -> (MainIncomeSource -> msg)
    -> NutritionFoodSecurityForm
    -> ( List (Html msg), List (Maybe Bool) )
nutritionFoodSecurityInputsAndTasks language setFoodSecurityBoolInputMsg setMainIncomeSourceMsg form =
    let
        householdGotFoodUpdateFunc value form_ =
            { form_ | householdGotFood = Just value }
    in
    ( [ viewQuestionLabel language Translate.MainIncomeSourceQuestion
      , viewCheckBoxSelectInput language
            [ HomeBasedAgriculture
            , CommercialAgriculture
            , PublicEmployee
            , PrivateBusinessEmpployee
            ]
            []
            form.mainIncomeSource
            setMainIncomeSourceMsg
            Translate.MainIncomeSource
      , viewQuestionLabel language <| Translate.NutritionFoodSecuritySignQuestion HouseholdGotFood
      , viewBoolInput language
            form.householdGotFood
            (setFoodSecurityBoolInputMsg householdGotFoodUpdateFunc)
            "household-got-fFood"
            Nothing
      ]
    , [ maybeToBoolTask form.mainIncomeSource
      , form.householdGotFood
      ]
    )


viewHeightForm :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe Float
    -> (String -> msg)
    -> HeightForm
    -> List (Html msg)
viewHeightForm language currentDate zscores person previousValue setHeightMsg form =
    let
        ( formForView, _ ) =
            heightFormAndTasks language currentDate zscores person previousValue setHeightMsg form
    in
    formForView


heightFormAndTasks :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe Float
    -> (String -> msg)
    -> HeightForm
    -> ( List (Html msg), List (Maybe Bool) )
heightFormAndTasks language currentDate zscores person previousValue setHeightMsg form =
    let
        activity =
            Backend.NutritionActivity.Model.Height

        zScoreText =
            Maybe.andThen
                (\height ->
                    Maybe.map
                        (\birthDate -> diffDays birthDate currentDate)
                        person.birthDate
                        |> Maybe.andThen
                            (\ageInDays ->
                                zScoreLengthHeightForAge zscores ageInDays person.gender (Centimetres height)
                            )
                )
                form.height
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsHeight
    in
    ( [ div [ class "ui form height" ]
            [ viewLabel language <| Translate.NutritionActivityTitle activity
            , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
            , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
            , div [ class "ui grid" ]
                [ div [ class "eleven wide column" ]
                    [ viewMeasurementInput
                        language
                        form.height
                        setHeightMsg
                        "height"
                        Translate.UnitCentimeter
                    ]
                , div
                    [ class "five wide column" ]
                    [ showMaybe <|
                        Maybe.map2 (viewMeasurementFloatDiff language Translate.UnitCentimeter)
                            form.height
                            previousValue
                    ]
                ]
            , Pages.Utils.viewPreviousMeasurement language previousValue Translate.UnitCentimeter
            ]
      , div [ class "ui large header z-score age" ]
            [ text <| translate language Translate.ZScoreHeightForAge
            , span [ class "sub header" ]
                [ text zScoreText ]
            ]
      ]
    , [ maybeToBoolTask form.height ]
    )


viewMuacForm :
    Language
    -> Site
    -> Maybe Float
    -> (String -> msg)
    -> MuacForm
    -> List (Html msg)
viewMuacForm language site previousValue setMuacMsg form =
    let
        ( inputs, _ ) =
            muacFormInputsAndTasks language site previousValue setMuacMsg form
    in
    [ div [ class "ui form muac" ]
        inputs
    ]


muacFormInputsAndTasks :
    Language
    -> Site
    -> Maybe Float
    -> (String -> msg)
    -> MuacForm
    -> ( List (Html msg), List (Maybe Bool) )
muacFormInputsAndTasks language site previousValue setMuacMsg form =
    let
        activity =
            Backend.NutritionActivity.Model.Muac

        constraints =
            getInputConstraintsMuac site

        ( currentValue, unitTransId ) =
            case site of
                SiteBurundi ->
                    ( -- Value is stored in cm, but for Burundi, we need to
                      -- view it as mm. Therefore, multiplying by 10.
                      Maybe.map ((*) 10) form.muac
                    , Translate.UnitMillimeter
                    )

                _ ->
                    ( form.muac, Translate.UnitCentimeter )
    in
    ( [ viewLabel language <| Translate.NutritionActivityTitle activity
      , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
      , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
      , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    currentValue
                    setMuacMsg
                    "muac"
                    unitTransId
                ]
            , div
                [ class "five wide column" ]
                [ showMaybe <|
                    Maybe.map (MuacInCm >> muacIndication >> viewColorAlertIndication language) form.muac
                ]
            ]
      , Pages.Utils.viewPreviousMeasurement language previousValue unitTransId
      ]
    , [ maybeToBoolTask form.muac ]
    )


viewNutritionForm : Language -> (ChildNutritionSign -> msg) -> NutritionForm -> List (Html msg)
viewNutritionForm language setSignMsg form =
    let
        ( inputs, _ ) =
            nutritionFormInputsAndTasks language setSignMsg form
    in
    [ div [ class "ui form nutrition" ]
        inputs
    ]


nutritionFormInputsAndTasks :
    Language
    -> (ChildNutritionSign -> msg)
    -> NutritionForm
    -> ( List (Html msg), List (Maybe Bool) )
nutritionFormInputsAndTasks language setSignMsg form =
    let
        activity =
            Backend.NutritionActivity.Model.Nutrition
    in
    ( [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
      , viewLabel language Translate.SelectAllSigns
      , viewCheckBoxMultipleSelectInput language
            [ Edema, AbdominalDistension, DrySkin ]
            [ Apathy, PoorAppetite, BrittleHair ]
            (Maybe.withDefault [] form.signs)
            (Just NormalChildNutrition)
            setSignMsg
            Translate.ChildNutritionSignLabel
      ]
    , [ maybeToBoolTask form.signs ]
    )


viewWeightForm :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe HeightInCm
    -> Maybe Float
    -> Bool
    -> (String -> msg)
    -> WeightForm
    -> List (Html msg)
viewWeightForm language currentDate zscores person heightValue previousValue showWeightForHeightZScore setWeightMsg form =
    let
        ( formForView, _ ) =
            weightFormAndTasks language currentDate zscores person heightValue previousValue showWeightForHeightZScore setWeightMsg form
    in
    formForView


weightFormAndTasks :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe HeightInCm
    -> Maybe Float
    -> Bool
    -> (String -> msg)
    -> WeightForm
    -> ( List (Html msg), List (Maybe Bool) )
weightFormAndTasks language currentDate zscores person heightValue previousValue showWeightForHeightZScore setWeightMsg form =
    let
        activity =
            Backend.NutritionActivity.Model.Weight

        zScoreForAgeText =
            calculateZScoreWeightForAge currentDate zscores person form.weight
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        zScoreForHeightText =
            Maybe.andThen
                (\(HeightInCm height) ->
                    Maybe.andThen
                        (\weight ->
                            Maybe.map
                                (\birthDate -> diffDays birthDate currentDate)
                                person.birthDate
                                |> Maybe.andThen
                                    (\ageInDays ->
                                        zScoreForHeightOrLength zscores ageInDays (Centimetres height) person.gender weight
                                    )
                        )
                        form.weight
                )
                heightValue
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsWeight
    in
    ( [ div [ class "ui form weight" ]
            [ viewLabel language <| Translate.NutritionActivityTitle activity
            , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
            , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
            , div [ class "ui grid" ]
                [ div [ class "eleven wide column" ]
                    [ viewMeasurementInput
                        language
                        form.weight
                        setWeightMsg
                        "weight"
                        Translate.KilogramShorthand
                    ]
                , div
                    [ class "five wide column" ]
                    [ showMaybe <|
                        Maybe.map2 (viewMeasurementFloatDiff language Translate.KilogramShorthand)
                            form.weight
                            previousValue
                    ]
                ]
            , Pages.Utils.viewPreviousMeasurement language previousValue Translate.KilogramShorthand
            ]
      , div [ class "ui large header z-score age" ]
            [ text <| translate language Translate.ZScoreWeightForAge
            , span [ class "sub header" ]
                [ text zScoreForAgeText ]
            ]
      , showIf showWeightForHeightZScore <|
            div [ class "ui large header z-score height" ]
                [ text <| translate language Translate.ZScoreWeightForHeight
                , span [ class "sub header" ]
                    [ text zScoreForHeightText
                    ]
                ]
      ]
    , [ maybeToBoolTask form.weight ]
    )


viewMedicationAdministrationForm :
    Language
    -> NominalDate
    -> Person
    -> MedicationAdministrationFormConfig msg
    -> MedicationAdministrationForm
    -> List (Html msg)
viewMedicationAdministrationForm language currentDate person config form =
    let
        ( inputs, _ ) =
            medicationAdministrationFormInputsAndTasks language currentDate person config form
    in
    [ div [ class "ui form medication-administration" ]
        inputs
    ]
