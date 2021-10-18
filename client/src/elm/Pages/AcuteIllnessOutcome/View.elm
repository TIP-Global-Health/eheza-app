module Pages.AcuteIllnessOutcome.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Encoder exposing (acuteIllnessOutcomeToString)
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


view : Language -> NominalDate -> IndividualEncounterParticipantId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        firstEncounterId =
            Dict.get id db.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map Dict.keys
                |> RemoteData.withDefault []
                |> List.head

        data =
            firstEncounterId
                |> Maybe.map
                    (\encounterId ->
                        generateAssembledData currentDate encounterId isChw db
                    )
                |> Maybe.withDefault NotAsked

        header =
            viewWebData language (viewHeader language) identity data

        content =
            viewWebData language (viewContent language currentDate isChw model) identity data
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


viewContent : Language -> NominalDate -> Bool -> Model -> AssembledData -> Html Msg
viewContent language currentDate isChw model data =
    div [ class "ui unstackable items" ] <|
        viewPersonDetailsWithAlert language currentDate isChw data model.showAlertsDialog SetAlertsDialogState
            :: viewAcuteIllnessOutcome language currentDate data model


viewAcuteIllnessOutcome : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewAcuteIllnessOutcome language currentDate data model =
    let
        acuteIllnessOutcomeInput =
            option
                [ value ""
                , selected (model.acuteIllnessOutcome == Nothing)
                ]
                [ text "" ]
                :: (allAcuteIllnessOutcome
                        |> List.map
                            (\outcome ->
                                option
                                    [ value (acuteIllnessOutcomeToString outcome)
                                    , selected (model.acuteIllnessOutcome == Just outcome)
                                    ]
                                    [ text <| translate language <| Translate.AcuteIllnessOutcome outcome ]
                            )
                   )
                |> select [ onInput SetAcuteIllnessOutcome, class "form-input acuteIllness-outcome" ]

        totalTasks =
            1

        tasksCompleted =
            taskCompleted model.acuteIllnessOutcome
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form acute-illness-dating" ]
                [ viewLabel language Translate.AcuteIllnessOutcomeLabel
                , acuteIllnessOutcomeInput
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
