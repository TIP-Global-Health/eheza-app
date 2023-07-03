module Pages.AcuteIllness.Outcome.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Encoder exposing (acuteIllnessOutcomeToString)
import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome(..), IndividualParticipantInitiator(..), allAcuteIllnessOutcome)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getAcuteIllnessEncountersForParticipant)
import Date exposing (Unit(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (generateAssembledData)
import Pages.AcuteIllness.Encounter.View exposing (viewPersonDetailsWithAlert)
import Pages.AcuteIllness.Outcome.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewLabel, viewSelectListInput)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> IndividualEncounterParticipantId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        firstEncounterId =
            getAcuteIllnessEncountersForParticipant db id
                |> List.head
                |> Maybe.map Tuple.first

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
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessParticipantPage InitiatorParticipantsPage data.participant.person
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
            viewSelectListInput language
                model.acuteIllnessOutcome
                allAcuteIllnessOutcome
                acuteIllnessOutcomeToString
                SetAcuteIllnessOutcome
                Translate.AcuteIllnessOutcome
                "acuteIllness-outcome"

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
