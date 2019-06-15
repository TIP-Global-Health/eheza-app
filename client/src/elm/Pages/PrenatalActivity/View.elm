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
                            ( viewObstetricFormFirstStep language currentDate motherId form
                            , ([ form.termPregnancy
                               , form.preTermPregnancy
                               , form.stillbirthsAtTerm
                               , form.stillbirthsPreTerm
                               , form.abortions
                               , form.liveChildren
                               ]
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted form.currentlyPregnant
                            , 7
                            )

                        SecondStep form ->
                            ( viewObstetricFormSecondStep language currentDate motherId form, 0, 16 )

                _ ->
                    ( emptyNode, 0, 0 )

        actionButton =
            div [ class "actions" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]

                    -- , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                    ]
                    [ text <| translate language Translate.Save ]
                ]
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
            , actionButton
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
    in
    div [ class "form history obstetric first" ]
        [ viewBoolInput language form.currentlyPregnant SetCurrentlyPregnant "currently-pregnant" (Just Translate.CurrentlyPregnant)
        , viewNumberInput language form.termPregnancy SetTermPregnancy "term-pregnancy" Translate.TermPregnancy
        , viewNumberInput language form.preTermPregnancy SetPreTermPregnancy "preterm-pregnancy" Translate.PreTermPregnancy
        , viewNumberInput language form.stillbirthsAtTerm SetStillbirthsAtTerm "stillbirths-at-term" Translate.NumberOfStillbirthsAtTerm
        , viewNumberInput language form.stillbirthsPreTerm SetStillbirthsPreTerm "stillbirths-pre-term" Translate.NumberOfStillbirthsPreTerm
        , viewNumberInput language form.abortions SetAbortions "abortions" Translate.NumberOfAbortions
        , viewNumberInput language form.liveChildren SetLiveChildren "live-children" Translate.NumberOfLiveChildren
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
    emptyNode


viewBoolInput : Language -> Maybe Bool -> (Bool -> Msg) -> String -> Maybe TranslationId -> Html Msg
viewBoolInput language currentValue setFunc inputClass labelTranslateId =
    let
        inputLabel =
            labelTranslateId
                |> unwrap
                    emptyNode
                    (\translationId ->
                        div [ class "label" ] [ text <| (translate language translationId ++ ":") ]
                    )

        viewInput value currentValue setFunc =
            let
                isChecked =
                    currentValue == Just value
            in
            input
                [ type_ "radio"
                , checked isChecked
                , classList [ ( "checked", isChecked ) ]
                , onCheck (always (setFunc value))
                ]
                []
    in
    div [ class <| "form-input " ++ inputClass ]
        [ inputLabel
        , viewInput True currentValue setFunc
        , label [ class "yes" ] [ text <| translate language Translate.Yes ]
        , viewInput False currentValue setFunc
        , label [ class "no" ] [ text <| translate language Translate.No ]
        ]


viewNumberInput : Language -> Maybe Int -> (String -> Msg) -> String -> TranslationId -> Html Msg
viewNumberInput language maybeCurrentValue setFunc inputClass labelTranslationId =
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
            , onInput setFunc
            , value currentValue
            ]
            []
        ]


taskCompleted : Maybe a -> Int
taskCompleted maybe =
    if isJust maybe then
        1

    else
        0
