module Pages.PrenatalActivity.View exposing (view)

import AllDict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Date.Extra as Date exposing (Interval(Day, Month))
import DateSelector.SelectorDropdown
import EveryDict exposing (EveryDict)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import PrenatalActivity.Model exposing (PrenatalActivity(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)


view : Language -> NominalDate -> PersonId -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        content =
            AllDict.get id db.people
                |> unwrap
                    []
                    (RemoteData.toMaybe
                        >> unwrap
                            []
                            (\mother ->
                                [ div [ class "ui unstackable items" ] <|
                                    viewMotherAndMeasurements language currentDate mother
                                        ++ viewContent language currentDate id activity model
                                ]
                            )
                    )
    in
    div [ class "page-prenatal-activity" ] <|
        viewHeader language id activity
            :: content


viewHeader : Language -> PersonId -> PrenatalActivity -> Html Msg
viewHeader language motherId activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.PrenatalActivitiesTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> PersonId -> PrenatalActivity -> Model -> List (Html Msg)
viewContent language currentDate motherId activity model =
    case activity of
        PregnancyDating ->
            viewPregnancyDatingContent language currentDate motherId model.pregnancyDatingData

        History ->
            viewHistoryContent language currentDate motherId model.historyData

        _ ->
            []


viewPregnancyDatingContent : Language -> NominalDate -> PersonId -> PregnancyDatingData -> List (Html Msg)
viewPregnancyDatingContent language currentDate motherId pregnancyDatingData =
    let
        form =
            pregnancyDatingData.form

        lmpRangeInput =
            option
                [ value ""
                , selected (form.lmpRange == Nothing)
                ]
                [ text "" ]
                :: ([ OneMonth, ThreeMonth, SixMonth ]
                        |> List.map
                            (\range ->
                                option
                                    [ value (encodeLmpRange range)
                                    , selected (form.lmpRange == Just range)
                                    ]
                                    [ text <| translate language <| Translate.LmpRange range ]
                            )
                   )
                |> select [ onInput SetLmpRange, class "form-input range" ]

        today =
            toLocalDateTime currentDate 0 0 0 0

        lmpDateInput =
            form.lmpRange
                |> unwrap
                    []
                    (\range ->
                        let
                            daysBack =
                                case range of
                                    OneMonth ->
                                        -31

                                    ThreeMonth ->
                                        -92

                                    SixMonth ->
                                        -184
                        in
                        [ DateSelector.SelectorDropdown.view
                            ToggleDateSelector
                            SetLmpDate
                            form.isDateSelectorOpen
                            (Date.add Day daysBack today)
                            today
                            form.lmpDate
                        ]
                    )
                |> div [ class "form-input date" ]

        ( eddResult, egaResult ) =
            form.lmpDate
                |> unwrap
                    ( emptyNode, emptyNode )
                    (\date ->
                        let
                            eddDate =
                                Date.add Month 9 date
                                    |> fromLocalDateTime

                            lmpDate =
                                fromLocalDateTime date

                            diffInDays =
                                diffDays lmpDate currentDate

                            diffInWeeks =
                                diffInDays // 7

                            egaWeeks =
                                translate language <| Translate.WeekSinglePlural diffInWeeks

                            egaDays =
                                translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)
                        in
                        ( div [ class "value" ] [ text <| formatMMDDYYYY eddDate ]
                        , div [ class "value" ] [ text <| egaWeeks ++ ", " ++ egaDays ]
                        )
                    )

        totalTasks =
            2

        tasksCompleted =
            taskCompleted form.lmpDate + taskCompleted form.lmpDateConfident
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form pregnancy-dating" ]
                [ div [ class "label" ] [ text <| translate language Translate.LmpRangeHeader ]
                , lmpRangeInput
                , div [ class "label" ] [ text <| translate language Translate.LmpDateHeader ]
                , lmpDateInput
                , div [ class "label" ] [ text <| translate language Translate.LmpDateConfidentHeader ]
                , viewBoolInput language form.lmpDateConfident SetLmpDateConfident "is-confident" Nothing
                , div [ class "separator" ] []
                , div [ class "results" ]
                    [ div [ class "edd-result" ]
                        [ div [ class "label" ] [ text <| translate language Translate.EddHeader ]
                        , eddResult
                        ]
                    , div [ class "ega-result" ]
                        [ div [ class "label" ] [ text <| translate language Translate.EgaHeader ]
                        , egaResult
                        ]
                    ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHistoryContent : Language -> NominalDate -> PersonId -> HistoryData -> List (Html Msg)
viewHistoryContent language currentDate motherId data =
    let
        tasks =
            [ Obstetric, Medical, Social ]

        viewTask task =
            let
                ( labelTranslateId, iconClass ) =
                    case task of
                        Obstetric ->
                            ( Translate.ObstetricHistory, "obstetric" )

                        Medical ->
                            ( Translate.MedicalHistory, "medical" )

                        Social ->
                            ( Translate.SocialHistory, "social" )

                isActiveTask =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActiveTask ) ]
                        :: (if isActiveTask then
                                []

                            else
                                [ onClick <| SetActiveHistoryTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-history-task icon-" ++ iconClass ] []
                    , text <| translate language labelTranslateId
                    ]
                ]

        ( viewForm, tasksCompleted, totalTasks ) =
            case data.activeTask of
                Obstetric ->
                    case data.obstetricForm of
                        FirstStep form ->
                            let
                                intInputs =
                                    [ form.termPregnancy
                                    , form.preTermPregnancy
                                    , form.stillbirthsAtTerm
                                    , form.stillbirthsPreTerm
                                    , form.abortions
                                    , form.liveChildren
                                    ]
                            in
                            ( viewObstetricFormFirstStep language currentDate motherId form
                            , (intInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted form.currentlyPregnant
                            , 7
                            )

                        SecondStep form ->
                            let
                                boolInputs =
                                    [ form.cSectionInPreviousDelivery
                                    , form.successiveAbortions
                                    , form.successivePrimatureDeliveries
                                    , form.stillbornPreviousDelivery
                                    , form.babyDiedOnDayOfBirthPreviousDelivery
                                    , form.partialPlacentaPreviousDelivery
                                    , form.severeHemorrhagingPreviousDelivery
                                    , form.preeclampsiaPreviousPregnancy
                                    , form.convulsionsPreviousDelivery
                                    , form.convulsionsAndUnconciousPreviousDelivery
                                    , form.gestatipnalDiabetesPreviousPregnancy
                                    , form.incompleteCervixPreviousPregnancy
                                    , form.rhNegative
                                    ]
                            in
                            ( viewObstetricFormSecondStep language currentDate motherId form
                            , (boolInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted form.cSections
                                + taskCompleted form.reasonForCSection
                                + taskCompleted form.previousDeliveryPeriod
                            , 16
                            )

                _ ->
                    ( emptyNode, 0, 0 )

        actions =
            let
                ( buttons, stepIndicatoryClass ) =
                    case data.activeTask of
                        Obstetric ->
                            case data.obstetricForm of
                                FirstStep _ ->
                                    ( [ button
                                            [ classList [ ( "ui fluid primary button", True ), ( "disabledTMP", tasksCompleted /= totalTasks ) ]
                                            , onClick OBSaveFirstStep
                                            ]
                                            [ text <| translate language Translate.SaveAndNext ]
                                      ]
                                    , "first"
                                    )

                                SecondStep _ ->
                                    ( [ button [ class "ui fluid primary button" ]
                                            [ text <| translate language Translate.Back ]
                                      , button
                                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                                            ]
                                            [ text <| translate language Translate.Save ]
                                      ]
                                    , "second"
                                    )

                        _ ->
                            ( [], "" )
            in
            div [ class <| "actions history obstetric " ++ stepIndicatoryClass ]
                buttons
    in
    [ div [ class "ui task segment" ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask <|
                tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewObstetricFormFirstStep : Language -> NominalDate -> PersonId -> ObstetricFormFirstStep -> Html Msg
viewObstetricFormFirstStep language currentDate motherId form =
    let
        gravidaResult =
            span [] [ text "02" ]

        paraResult =
            span [] [ text "0102" ]

        termPregnancyUpdateFunc number form_ =
            { form_ | termPregnancy = Just number }

        preTermPregnancyUpdateFunc number form_ =
            { form_ | preTermPregnancy = Just number }

        stillbirthsAtTermUpdateFunc number form_ =
            { form_ | stillbirthsAtTerm = Just number }

        stillbirthsPreTermUpdateFunc number form_ =
            { form_ | stillbirthsPreTerm = Just number }

        abortionsUpdateFunc number form_ =
            { form_ | abortions = Just number }

        liveChildrenUpdateFunc number form_ =
            { form_ | liveChildren = Just number }
    in
    div [ class "form history obstetric first" ]
        [ viewBoolInput language form.currentlyPregnant SetCurrentlyPregnant "currently-pregnant" (Just Translate.CurrentlyPregnant)
        , viewNumberInput language form.termPregnancy (SetOBIntInput termPregnancyUpdateFunc) "term-pregnancy" Translate.TermPregnancy
        , viewNumberInput language form.preTermPregnancy (SetOBIntInput preTermPregnancyUpdateFunc) "preterm-pregnancy" Translate.PreTermPregnancy
        , viewNumberInput language form.stillbirthsAtTerm (SetOBIntInput stillbirthsAtTermUpdateFunc) "stillbirths-at-term" Translate.NumberOfStillbirthsAtTerm
        , viewNumberInput language form.stillbirthsPreTerm (SetOBIntInput stillbirthsPreTermUpdateFunc) "stillbirths-pre-term" Translate.NumberOfStillbirthsPreTerm
        , viewNumberInput language form.abortions (SetOBIntInput abortionsUpdateFunc) "abortions" Translate.NumberOfAbortions
        , viewNumberInput language form.liveChildren (SetOBIntInput liveChildrenUpdateFunc) "live-children" Translate.NumberOfLiveChildren
        , div [ class "separator" ] []
        , div [ class "results" ]
            [ div [ class "gravida-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Gravida ++ ":") ]
                , gravidaResult
                ]
            , div [ class "para-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Para ++ ":") ]
                , paraResult
                ]
            ]
        ]


viewObstetricFormSecondStep : Language -> NominalDate -> PersonId -> ObstetricFormSecondStep -> Html Msg
viewObstetricFormSecondStep language currentDate motherId form =
    let
        cSectionInPreviousDeliveryUpdateFunc value form_ =
            { form_ | cSectionInPreviousDelivery = Just value }

        successiveAbortionsUpdateFunc value form_ =
            { form_ | successiveAbortions = Just value }

        successivePrimatureDeliveriesUpdateFunc value form_ =
            { form_ | successivePrimatureDeliveries = Just value }

        stillbornPreviousDeliveryUpdateFunc value form_ =
            { form_ | stillbornPreviousDelivery = Just value }

        babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc value form_ =
            { form_ | babyDiedOnDayOfBirthPreviousDelivery = Just value }

        partialPlacentaPreviousDeliveryUpdateFunc value form_ =
            { form_ | partialPlacentaPreviousDelivery = Just value }

        severeHemorrhagingPreviousDeliveryUpdateFunc value form_ =
            { form_ | severeHemorrhagingPreviousDelivery = Just value }

        preeclampsiaPreviousPregnancyUpdateFunc value form_ =
            { form_ | preeclampsiaPreviousPregnancy = Just value }

        convulsionsPreviousDeliveryUpdateFunc value form_ =
            { form_ | convulsionsPreviousDelivery = Just value }

        convulsionsAndUnconciousPreviousDeliveryUpdateFunc value form_ =
            { form_ | convulsionsAndUnconciousPreviousDelivery = Just value }

        gestatipnalDiabetesPreviousPregnancyUpdateFunc value form_ =
            { form_ | gestatipnalDiabetesPreviousPregnancy = Just value }

        incompleteCervixPreviousPregnancyUpdateFunc value form_ =
            { form_ | incompleteCervixPreviousPregnancy = Just value }

        rhNegativeUpdateFunc value form_ =
            { form_ | rhNegative = Just value }
    in
    div [ class "form history obstetric second" ]
        [ viewNumberInput
            language
            form.cSections
            SetNumberOfCSections
            "c-sections"
            Translate.NumberOfCSections
        , div [ class "label normal" ] [ text <| (translate language Translate.CSectionInPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.cSectionInPreviousDelivery
            (SetOBBoolInput cSectionInPreviousDeliveryUpdateFunc)
            "c-section-previous-delivery"
            Nothing
        , viewCheckBoxSelectInput language
            [ Breech, Emergency, Other ]
            [ FailureToProgress, None ]
            form.reasonForCSection
            Translate.CSectionReason
            SetCSectionReason
            Translate.CSectionReasons
        , viewCheckBoxSelectInput language
            [ LessThan18Month, MoreThan5Years ]
            [ Neither ]
            form.previousDeliveryPeriod
            Translate.PreviousDelivery
            SetPreviousDeliveryPeriod
            Translate.PreviousDeliveryPeriods
        , div [ class "label normal" ] [ text <| (translate language Translate.SuccessiveAbortions ++ ":") ]
        , viewBoolInput
            language
            form.successiveAbortions
            (SetOBBoolInput successiveAbortionsUpdateFunc)
            "successive-abortions"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.SuccessivePrimatureDeliveries ++ ":") ]
        , viewBoolInput
            language
            form.successivePrimatureDeliveries
            (SetOBBoolInput successivePrimatureDeliveriesUpdateFunc)
            "successive-primature-deliveries"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.StillbornPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.stillbornPreviousDelivery
            (SetOBBoolInput stillbornPreviousDeliveryUpdateFunc)
            "stillborn-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.BabyDiedOnDayOfBirthPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.babyDiedOnDayOfBirthPreviousDelivery
            (SetOBBoolInput babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc)
            "baby-died-on-day-off-birth-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.PartialPlacentaPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.partialPlacentaPreviousDelivery
            (SetOBBoolInput partialPlacentaPreviousDeliveryUpdateFunc)
            "partial-placenta-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.SevereHemorrhagingPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.severeHemorrhagingPreviousDelivery
            (SetOBBoolInput severeHemorrhagingPreviousDeliveryUpdateFunc)
            "severe-hemorrhaging-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.PreeclampsiaPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.preeclampsiaPreviousPregnancy
            (SetOBBoolInput preeclampsiaPreviousPregnancyUpdateFunc)
            "preeclampsia-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.ConvulsionsPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.convulsionsPreviousDelivery
            (SetOBBoolInput convulsionsPreviousDeliveryUpdateFunc)
            "convulsions-previous-pelivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.ConvulsionsAndUnconciousPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.convulsionsAndUnconciousPreviousDelivery
            (SetOBBoolInput convulsionsAndUnconciousPreviousDeliveryUpdateFunc)
            "convulsions-and-unconcious-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.GestatipnalDiabetesPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.gestatipnalDiabetesPreviousPregnancy
            (SetOBBoolInput gestatipnalDiabetesPreviousPregnancyUpdateFunc)
            "gestatipnal-diabetes-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.IncompleteCervixPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.incompleteCervixPreviousPregnancy
            (SetOBBoolInput incompleteCervixPreviousPregnancyUpdateFunc)
            "incomplete-cervix-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.RhNegative ++ ":") ]
        , viewBoolInput
            language
            form.rhNegative
            (SetOBBoolInput rhNegativeUpdateFunc)
            "rh-negative"
            Nothing
        ]


viewBoolInput : Language -> Maybe Bool -> (Bool -> Msg) -> String -> Maybe TranslationId -> Html Msg
viewBoolInput language currentValue setMsg inputClass labelTranslateId =
    let
        inputLabel =
            labelTranslateId
                |> unwrap
                    emptyNode
                    (\translationId ->
                        div [ class "label" ] [ text <| (translate language translationId ++ ":") ]
                    )

        viewInput value currentValue setMsg =
            let
                isChecked =
                    currentValue == Just value
            in
            input
                [ type_ "radio"
                , checked isChecked
                , classList [ ( "checked", isChecked ) ]
                , onCheck (always (setMsg value))
                ]
                []
    in
    div [ class <| "form-input " ++ inputClass ]
        [ inputLabel
        , viewInput True currentValue setMsg
        , label [ class "yes" ] [ text <| translate language Translate.Yes ]
        , viewInput False currentValue setMsg
        , label [ class "no" ] [ text <| translate language Translate.No ]
        ]


viewNumberInput :
    Language
    -> Maybe Int
    ->
        (String
         -> Msg
        )
    -> String
    -> TranslationId
    -> Html Msg
viewNumberInput language maybeCurrentValue setMsg inputClass labelTranslationId =
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    toString
    in
    div [ class <| "form-input " ++ inputClass ]
        [ div [ class "label" ] [ text <| (translate language labelTranslationId ++ ":") ]
        , input
            [ type_ "number"
            , Html.Attributes.min "0"
            , Html.Attributes.max "99"
            , onInput setMsg
            , value currentValue
            ]
            []
        ]


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> TranslationId -> (a -> Msg) -> (a -> TranslationId) -> Html Msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue labelTranslateId setMsg translateFunc =
    div [ class "ui form" ]
        [ p [] [ text <| (translate language labelTranslateId ++ ":") ]
        , div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language currentValue setMsg translateFunc)
                |> div [ class "eight wide column" ]
            , rightOptions
                |> List.map (viewCheckBoxSelectInputItem language currentValue setMsg translateFunc)
                |> div [ class "eight wide column" ]
            ]
        ]


viewCheckBoxSelectInputItem : Language -> Maybe a -> (a -> Msg) -> (a -> TranslationId) -> a -> Html Msg
viewCheckBoxSelectInputItem language currentValue setMsg translateFunc option =
    let
        isChecked =
            currentValue == Just option
    in
    div
        [ class "ui checkbox activity"
        , onClick <| setMsg option
        ]
        [ input
            [ type_ "checkbox"

            -- , onCheck (always (setMsg option))
            , checked isChecked
            , classList [ ( "checked", isChecked ) ]
            ]
            []
        , label []
            [ text <| translate language (translateFunc option) ]
        ]


taskCompleted : Maybe a -> Int
taskCompleted maybe =
    if isJust maybe then
        1

    else
        0
