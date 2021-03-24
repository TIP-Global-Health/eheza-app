module Pages.NutritionActivity.View exposing (view, warningPopup)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.NutritionEncounter.Utils exposing (calculateZScoreWeightForAge)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model exposing (ContributingFactorsForm, FollowUpForm, HealthEducationForm, SendToHCForm)
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( renderDatePart
        , viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewMuacIndication
        , viewSendToHCForm
        , zScoreForHeightOrLength
        )
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionActivity.Utils exposing (..)
import Pages.NutritionEncounter.Model exposing (AssembledData, NutritionAssesment(..))
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromPhotoUrl
        , viewPreviousMeasurement
        , viewQuestionLabel
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (Days(..), diffDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreLengthHeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id activity isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id activity isChw db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id activity isChw db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores id activity isChw db model data
    in
    div [ class "page-activity nutrition" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
        ]


viewHeader : Language -> NutritionEncounterId -> NutritionActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.NutritionActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id activity isChw db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate zscores id activity isChw assembled db model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> NominalDate -> (List NutritionAssesment -> msg) -> List NutritionAssesment -> Maybe (Html msg)
warningPopup language currentDate setStateMsg state =
    if List.isEmpty state then
        Nothing

    else
        let
            infoHeading =
                [ div [ class "popup-heading" ] [ text <| translate language Translate.Assessment ++ ":" ] ]

            assessments =
                List.map (\assessment -> p [] [ translateAssement assessment ]) state

            translateAssement assessment =
                case assessment of
                    AssesmentMalnutritionSigns signs ->
                        let
                            translatedSigns =
                                List.map (Translate.ChildNutritionSignLabel >> translate language) signs
                                    |> String.join ", "
                        in
                        text <| translate language (Translate.NutritionAssesment assessment) ++ ": " ++ translatedSigns

                    _ ->
                        text <| translate language <| Translate.NutritionAssesment assessment
        in
        Just <|
            div [ class "ui active modal diagnosis-popup" ]
                [ div [ class "content" ] <|
                    [ div [ class "popup-heading-wrapper" ] infoHeading
                    , div [ class "popup-title" ] assessments
                    ]
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| setStateMsg []
                        ]
                        [ text <| translate language Translate.Continue ]
                    ]
                ]


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> Bool -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores id activity isChw assembled db model =
    let
        childMeasurements =
            Dict.get assembled.participant.person db.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe

        resolvePreviousGroupValue getChildMeasurementFunc =
            childMeasurements
                |> Maybe.andThen
                    (getChildMeasurementFunc
                        >> Dict.values
                        >> List.map (\measurement -> ( measurement.dateMeasured, measurement.value ))
                        -- Most recent date to least recent date.
                        >> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare (Tuple.first m2) (Tuple.first m1))
                        >> List.head
                    )

        previousGroupHeight =
            resolvePreviousGroupValue .heights
                |> Maybe.map (\( date, HeightInCm val ) -> ( date, val ))

        previousGroupMuac =
            resolvePreviousGroupValue .muacs
                |> Maybe.map (\( date, MuacInCm val ) -> ( date, val ))

        previousGroupWeight =
            resolvePreviousGroupValue .weights
                |> Maybe.map (\( date, WeightInKg val ) -> ( date, val ))
    in
    case activity of
        Height ->
            viewHeightContent language currentDate zscores assembled model.heightData previousGroupHeight

        Muac ->
            viewMuacContent language currentDate assembled model.muacData previousGroupMuac

        Nutrition ->
            viewNutritionContent language currentDate ( assembled.participant.person, assembled.measurements ) model.nutritionData

        Photo ->
            viewPhotoContent language currentDate ( assembled.participant.person, assembled.measurements ) model.photoData

        Weight ->
            viewWeightContent language currentDate zscores isChw assembled model.weightData previousGroupWeight

        NextSteps ->
            viewNextStepsContent language currentDate id assembled model.nextStepsData


viewHeightContent : Language -> NominalDate -> ZScore.Model.Model -> AssembledData -> HeightData -> Maybe ( NominalDate, Float ) -> List (Html Msg)
viewHeightContent language currentDate zscores assembled data previousGroupValue =
    let
        activity =
            Height

        form =
            assembled.measurements.height
                |> Maybe.map (Tuple.second >> .value)
                |> heightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.height

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                assembled.person.birthDate

        previousIndividualValue =
            resolvePreviousIndividualValue assembled.previousMeasurementsWithDates .height (\(HeightInCm cm) -> cm)

        previousValue =
            resolvePreviousValueInCommonContext previousGroupValue previousIndividualValue

        zScoreText =
            form.height
                |> Maybe.andThen
                    (\height ->
                        Maybe.andThen
                            (\ageInDays ->
                                zScoreLengthHeightForAge zscores ageInDays assembled.person.gender (Centimetres height)
                            )
                            maybeAgeInDays
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsHeight

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.height
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form height" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
                , div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewMeasurementInput
                            language
                            form.height
                            SetHeight
                            "height"
                            Translate.CentimeterShorthand
                        ]
                    , div
                        [ class "five wide column" ]
                        [ showMaybe <|
                            Maybe.map2 (viewMeasurementFloatDiff language Translate.CentimeterShorthand)
                                form.height
                                previousValue
                        ]
                    ]
                , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
                ]
            , div [ class "ui large header z-score age" ]
                [ text <| translate language Translate.ZScoreHeightForAge
                , span [ class "sub header" ]
                    [ text zScoreText ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveHeight assembled.participant.person assembled.measurements.height
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewMuacContent : Language -> NominalDate -> AssembledData -> MuacData -> Maybe ( NominalDate, Float ) -> List (Html Msg)
viewMuacContent language currentDate assembled data previousGroupValue =
    let
        activity =
            Muac

        form =
            assembled.measurements.muac
                |> Maybe.map (Tuple.second >> .value)
                |> muacFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.muac

        previousIndividualValue =
            resolvePreviousIndividualValue assembled.previousMeasurementsWithDates .muac (\(MuacInCm cm) -> cm)

        previousValue =
            resolvePreviousValueInCommonContext previousGroupValue previousIndividualValue

        constraints =
            getInputConstraintsMuac

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.muac
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form muac" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
                , div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewMeasurementInput
                            language
                            form.muac
                            SetMuac
                            "muac"
                            Translate.CentimeterShorthand
                        ]
                    , div
                        [ class "five wide column" ]
                        [ showMaybe <|
                            Maybe.map (MuacInCm >> muacIndication >> viewMuacIndication language) form.muac
                        ]
                    ]
                , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveMuac assembled.participant.person assembled.measurements.muac
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewNutritionContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> NutritionData -> List (Html Msg)
viewNutritionContent language currentDate ( personId, measurements ) data =
    let
        activity =
            Nutrition

        form =
            measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form nutrition" ]
                [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , viewLabel language Translate.SelectAllSigns
                , viewCheckBoxMultipleSelectInput language
                    [ Edema, AbdominalDistension, DrySkin ]
                    [ Apathy, PoorAppetite, BrittleHair ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NormalChildNutrition)
                    SetNutritionSign
                    Translate.ChildNutritionSignLabel
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveNutrition personId measurements.nutrition
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPhotoContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> PhotoData -> List (Html Msg)
viewPhotoContent language currentDate ( personId, measurements ) data =
    let
        activity =
            Photo

        photoId =
            Maybe.map Tuple.first measurements.photo

        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case data.form.url of
                Just url ->
                    ( Just url
                    , [ onClick <| SavePhoto personId photoId url ]
                    , False
                    )

                Nothing ->
                    ( Maybe.map (Tuple.second >> .value) measurements.photo
                    , []
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , divKeyed [ class "ui full segment photo" ]
        [ keyedDivKeyed "content"
            [ class "content" ]
            [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
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
                            [ text <| translate language Translate.DropzoneDefaultMessage ]
                        ]
                    ]
                    |> keyed "dropzone"
                ]
            ]
        , keyed "button" <|
            div [ class "actions" ]
                [ button
                    ([ classList
                        [ ( "ui fluid primary button", True )
                        , ( "disabled", isDisabled )
                        ]
                     ]
                        ++ saveMsg
                    )
                    [ text <| translate language Translate.Save ]
                ]
        ]
    ]


viewWeightContent : Language -> NominalDate -> ZScore.Model.Model -> Bool -> AssembledData -> WeightData -> Maybe ( NominalDate, Float ) -> List (Html Msg)
viewWeightContent language currentDate zscores isChw assembled data previousGroupValue =
    let
        activity =
            Weight

        form =
            assembled.measurements.weight
                |> Maybe.map (Tuple.second >> .value)
                |> weightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.weight

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                assembled.person.birthDate

        previousIndividualValue =
            resolvePreviousIndividualValue assembled.previousMeasurementsWithDates .weight (\(WeightInKg kg) -> kg)

        previousValue =
            resolvePreviousValueInCommonContext previousGroupValue previousIndividualValue

        zScoreForAgeText =
            calculateZScoreWeightForAge currentDate zscores assembled.person form.weight
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        zScoreForHeightText =
            assembled.measurements.height
                |> Maybe.map (Tuple.second >> .value)
                |> Maybe.andThen
                    (\(HeightInCm height) ->
                        form.weight
                            |> Maybe.andThen
                                (\weight ->
                                    Maybe.andThen
                                        (\ageInDays ->
                                            zScoreForHeightOrLength zscores ageInDays (Centimetres height) assembled.person.gender weight
                                        )
                                        maybeAgeInDays
                                )
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        constraints =
            getInputConstraintsWeight

        disabled =
            (tasksCompleted /= totalTasks)
                || (form.weight
                        |> Maybe.map (withinConstraints constraints >> not)
                        |> Maybe.withDefault True
                   )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form weight" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [ class "activity-helper" ] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , p [ class "range-helper" ] [ text <| translate language (Translate.AllowedValuesRangeHelper constraints) ]
                , div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewMeasurementInput
                            language
                            form.weight
                            SetWeight
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
                , viewPreviousMeasurement language previousValue Translate.KilogramShorthand
                ]
            , div [ class "ui large header z-score age" ]
                [ text <| translate language Translate.ZScoreWeightForAge
                , span [ class "sub header" ]
                    [ text zScoreForAgeText ]
                ]
            , div [ class "ui large header z-score height" ]
                [ text <| translate language Translate.ZScoreWeightForHeight
                , span [ class "sub header" ]
                    [ text zScoreForHeightText
                    ]
                ]
                |> showIf (not isChw)
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
                , onClick <| SaveWeight assembled.participant.person assembled.measurements.weight
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> NutritionEncounterId -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate id assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            [ NextStepContributingFactors, NextStepsHealthEducation, NextStepsSendToHC, NextStepFollowUp ]

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        NextStepsSendToHC ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                        NextStepsHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        NextStepContributingFactors ->
                            ( "next-steps-contributing-factors"
                            , isJust measurements.contributingFactors
                            )

                        NextStepFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNextStepsTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, nextStepsTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsSendToHC ->
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHCForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNotSendingToHC
                            SetHandReferralForm

                Just NextStepsHealthEducation ->
                    measurements.healthEducation
                        |> Maybe.map (Tuple.second >> .value)
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation

                Just NextStepContributingFactors ->
                    measurements.contributingFactors
                        |> Maybe.map (Tuple.second >> .value)
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign

                Just NextStepFollowUp ->
                    measurements.followUp
                        |> Maybe.map (Tuple.second >> .value)
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate SetFollowUpOption

                Nothing ->
                    emptyNode

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveMsg =
                                case task of
                                    NextStepsSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                                    NextStepsHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    NextStepContributingFactors ->
                                        SaveContributingFactors personId measurements.contributingFactors nextTask

                                    NextStepFollowUp ->
                                        SaveFollowUp personId measurements.followUp nextTask
                        in
                        div [ class "actions next-steps" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                , onClick saveMsg
                                ]
                                [ text <| translate language Translate.Save ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui three column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]
