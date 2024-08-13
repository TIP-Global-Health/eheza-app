module Pages.Prenatal.Outcome.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..), IndividualEncounterParticipantOutcome(..), IndividualParticipantInitiator(..), allPregnancyOutcome)
import Backend.IndividualEncounterParticipant.Utils exposing (pregnancyOutcomeToString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getPrenatalEncountersForParticipant)
import Backend.PrenatalEncounter.Model exposing (RecordPreganancyInitiator(..))
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData)
import Pages.Prenatal.Encounter.View exposing (viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Outcome.Model exposing (Model, Msg(..))
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewLabel, viewSelectListInput, viewTasksCount)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> IndividualEncounterParticipantId -> Bool -> RecordPreganancyInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        lastEncounterId =
            getPrenatalEncountersForParticipant db id
                |> List.map Tuple.first
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
            viewHeader language initiator data

        content =
            viewContent language currentDate isChw initiator model data
    in
    div
        [ class "page-outcome pregnancy" ]
        [ header
        , content
        ]


viewHeader : Language -> RecordPreganancyInitiator -> AssembledData -> Html Msg
viewHeader language initiator data =
    let
        goBackPage =
            case initiator of
                InitiatorPostpartumEncounter encounterId ->
                    PrenatalEncounterPage encounterId

                _ ->
                    PrenatalParticipantPage InitiatorParticipantsPage data.participant.person
    in
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.PregnancyOutcomeLabel ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] []
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
            viewSelectListInput language
                form.pregnancyOutcome
                allPregnancyOutcome
                pregnancyOutcomeToString
                SetPregnancyOutcome
                Translate.PregnancyOutcome
                "pregnancy-outcome"

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
    [ viewTasksCount language tasksCompleted totalTasks
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
