module Pages.PregnancyOutcome.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalParticipant.Encoder exposing (pregnancyOutcomeToString)
import Backend.PrenatalParticipant.Model exposing (PregnancyOutcome(..), allPregnancyOutcome)
import Date.Extra as Date exposing (Interval(Day, Month))
import DateSelector.SelectorDropdown
import EveryDict
import EveryDictList
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PregnancyOutcome.Model exposing (Model, Msg(..))
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (generateAssembledData)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewLabel)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalParticipantId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        lastEncounterId =
            EveryDict.get id db.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map EveryDictList.keys
                |> RemoteData.withDefault []
                |> List.reverse
                |> List.head

        data =
            lastEncounterId
                |> Maybe.map
                    (\encounterId ->
                        generateAssembledData encounterId db
                    )
                |> Maybe.withDefault NotAsked

        header =
            viewWebData language (viewHeader language) identity data

        content =
            viewWebData language (viewContent language currentDate model) identity data
    in
    div
        [ class "page-pregnancy-outcome" ]
        [ header
        , content
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PregnancyOutcomeLabel ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Model -> AssembledData -> Html Msg
viewContent language currentDate model data =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate data False (\_ -> NoOp)
            ++ viewPregnancyOutcome language currentDate data model


viewPregnancyOutcome : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewPregnancyOutcome language currentDate data model =
    let
        pregnancyOutcomeInput =
            option
                [ value ""
                , selected (model.pregnancyOutcome == Nothing)
                ]
                [ text "" ]
                :: (allPregnancyOutcome
                        |> List.map
                            (\outcome ->
                                option
                                    [ value (pregnancyOutcomeToString outcome)
                                    , selected (model.pregnancyOutcome == Just outcome)
                                    ]
                                    [ text <| translate language <| Translate.PregnancyOutcome outcome ]
                            )
                   )
                |> select [ onInput SetPregnancyOutcome, class "form-input pregnancy-outcome" ]

        today =
            toLocalDateTime currentDate 0 0 0 0

        pregnancyConcludedDateInput =
            DateSelector.SelectorDropdown.view
                ToggleDateSelector
                SetPregnancyConcludedDate
                model.isDateSelectorOpen
                (Date.add Day -92 today)
                today
                model.pregnancyConcludedDate

        totalTasks =
            3

        tasksCompleted =
            taskCompleted model.pregnancyConcludedDate + taskCompleted model.pregnancyOutcome + taskCompleted model.isFacilityDelivery
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form pregnancy-dating" ]
                [ viewLabel language Translate.DatePregnancyConcluded
                , div [ class "form-input date" ]
                    [ pregnancyConcludedDateInput ]
                , viewLabel language Translate.PregnancyOutcomeLabel
                , pregnancyOutcomeInput
                , viewLabel language Translate.DeliveryLocation
                , viewBoolInput
                    language
                    model.isFacilityDelivery
                    SetDeliveryLocation
                    "delivery-location"
                    (Just ( Translate.Facility, Translate.Home ))
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]

                -- , onClick <| SavePregnancyDating assembled.id assembled.participant.person assembled.measurements.lastMenstrualPeriod
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
