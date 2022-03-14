module Pages.Prenatal.Outcome.View exposing (view)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Encoder exposing (pregnancyOutcomeToString)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..), IndividualEncounterParticipantOutcome(..), IndividualParticipantInitiator(..), PregnancyOutcome(..), allPregnancyOutcome)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)
import Backend.PrenatalEncounter.Model exposing (RecordPreganancyInitiator(..))
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Outcome.Model exposing (Model, Msg(..))
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewLabel)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> IndividualEncounterParticipantId -> Bool -> RecordPreganancyInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        lastEncounterId =
            Dict.get id db.prenatalEncountersByParticipant
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
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw initiator model) identity data


viewHeaderAndContent : Language -> NominalDate -> IndividualEncounterParticipantId -> Bool -> RecordPreganancyInitiator -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw initiator model data =
    let
        header =
            viewHeader language data

        content =
            viewContent language currentDate isChw initiator model data
    in
    div
        [ class "page-outcome pregnancy" ]
        [ header
        , content
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PregnancyOutcomeLabel ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage InitiatorParticipantsPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Bool -> RecordPreganancyInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate isChw initiator model data =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate isChw data Nothing
            ++ viewPregnancyOutcome language currentDate initiator data model


viewPregnancyOutcome : Language -> NominalDate -> RecordPreganancyInitiator -> AssembledData -> Model -> List (Html Msg)
viewPregnancyOutcome language currentDate initiator data model =
    let
        currentPregnancyOutcome =
            case data.participant.outcome of
                Just (Pregnancy outcome) ->
                    Just outcome

                _ ->
                    Nothing

        form =
            { model
                | pregnancyConcludedDate = Maybe.Extra.or model.pregnancyConcludedDate data.participant.dateConcluded
                , pregnancyOutcome = Maybe.Extra.or model.pregnancyOutcome currentPregnancyOutcome
                , deliveryLocation = Maybe.Extra.or model.deliveryLocation data.participant.deliveryLocation
            }

        pregnancyOutcomeInput =
            option
                [ value ""
                , selected (form.pregnancyOutcome == Nothing)
                ]
                [ text "" ]
                :: (allPregnancyOutcome
                        |> List.map
                            (\outcome ->
                                option
                                    [ value (pregnancyOutcomeToString outcome)
                                    , selected (form.pregnancyOutcome == Just outcome)
                                    ]
                                    [ text <| translate language <| Translate.PregnancyOutcome outcome ]
                            )
                   )
                |> select [ onInput SetPregnancyOutcome, class "form-input pregnancy-outcome" ]

        pregnancyConcludedDateForView =
            Maybe.map formatDDMMYYYY form.pregnancyConcludedDate
                |> Maybe.withDefault ""

        dateSelectorConfig =
            { select = SetPregnancyConcludedDate
            , close = SetDateSelectorState Nothing
            , dateFrom = Date.add Months -3 currentDate
            , dateTo = currentDate
            , dateDefault = Nothing
            }

        totalTasks =
            3

        tasksCompleted =
            taskCompleted form.pregnancyConcludedDate
                + taskCompleted form.pregnancyOutcome
                + taskCompleted form.deliveryLocation

        destinationPage =
            case initiator of
                InitiatorParticipantPage ->
                    PinCodePage

                InitiatorWarningPopup ->
                    UserPage <| PrenatalParticipantPage InitiatorParticipantsPage data.participant.person

                InitiatorPostpartumEncounter encounterId ->
                    UserPage <| PrenatalEncounterPage encounterId
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form pregnancy-dating" ]
                [ viewLabel language Translate.DatePregnancyConcluded
                , div
                    [ class "form-input date"
                    , onClick <| SetDateSelectorState (Just dateSelectorConfig)
                    ]
                    [ text pregnancyConcludedDateForView ]
                , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.pregnancyConcludedDate
                , viewLabel language Translate.PregnancyOutcomeLabel
                , pregnancyOutcomeInput
                , viewLabel language Translate.DeliveryLocation
                , viewBoolInput
                    language
                    (Maybe.map ((==) FacilityDelivery) form.deliveryLocation)
                    SetDeliveryLocation
                    "delivery-location"
                    (Just ( Translate.Facility, Translate.Home ))
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SavePregnancyOutcome destinationPage
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
