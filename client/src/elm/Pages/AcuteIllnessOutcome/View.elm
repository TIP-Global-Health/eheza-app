module Pages.AcuteIllnessOutcome.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Encoder exposing (pregnancyOutcomeToString)
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..), allAcuteIllnessOutcome)
import Backend.Model exposing (ModelIndexedDb)
import Date exposing (Unit(..))
import DateSelector.SelectorDropdown
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (generateAssembledData)
import Pages.AcuteIllnessEncounter.View exposing (viewPersonDetailsWithAlert)
import Pages.AcuteIllnessOutcome.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewLabel)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> IndividualEncounterParticipantId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        lastEncounterId =
            Dict.get id db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map Dict.keys
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
        [ class "page-outcome acute-illness" ]
        [ header
        , content
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.AcuteIllnessOutcomeLabel ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Model -> AssembledData -> Html Msg
viewContent language currentDate model data =
    div [ class "ui unstackable items" ] <|
        viewPersonDetailsWithAlert language currentDate data Nothing
            ++ viewAcuteIllnessOutcome language currentDate data model


viewAcuteIllnessOutcome : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewAcuteIllnessOutcome language currentDate data model =
    let
        pregnancyOutcomeInput =
            option
                [ value ""
                , selected (model.pregnancyOutcome == Nothing)
                ]
                [ text "" ]
                :: (allAcuteIllnessOutcome
                        |> List.map
                            (\outcome ->
                                option
                                    [ value (pregnancyOutcomeToString outcome)
                                    , selected (model.pregnancyOutcome == Just outcome)
                                    ]
                                    [ text <| translate language <| Translate.AcuteIllnessOutcome outcome ]
                            )
                   )
                |> select [ onInput SetAcuteIllnessOutcome, class "form-input pregnancy-outcome" ]

        today =
            currentDate

        pregnancyConcludedDateInput =
            DateSelector.SelectorDropdown.view
                ToggleDateSelector
                SetPregnancyConcludedDate
                model.isDateSelectorOpen
                (Date.add Days -92 today)
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
                , viewLabel language Translate.AcuteIllnessOutcomeLabel
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
                , onClick SaveAcuteIllnessOutcome
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
