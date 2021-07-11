module Pages.GlobalCaseManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounterType(..), emptyAcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (emptyPrenatalEncounter)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.PrenatalEncounter.Utils exposing (generatePostCreateDestination)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> Maybe HealthCenterId -> Msg -> ModelIndexedDb -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate healthCenterId msg db model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetEncounterTypeFilter encounterType ->
            ( { model | encounterTypeFilter = encounterType }
            , Cmd.none
            , []
            )

        SetDialogState state ->
            ( { model | dialogState = state }
            , Cmd.none
            , []
            )

        StartFollowUpEncounter dataType ->
            let
                msgs =
                    healthCenterId
                        |> Maybe.map
                            (\selectedHealthCenter ->
                                case dataType of
                                    FollowUpNutrition data ->
                                        startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data

                                    FollowUpAcuteIllness data ->
                                        startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data

                                    -- We should never get here, as Prenatal Encounter got it's own action.
                                    FollowUpPrenatal data ->
                                        []
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = Nothing }
            , Cmd.none
            , msgs
            )

        StartPrenatalFollowUpEncounter participantId hasNurseEncounter newEncounterType ->
            let
                msgs =
                    healthCenterId
                        |> Maybe.map
                            (\selectedHealthCenter ->
                                [ emptyPrenatalEncounter participantId currentDate newEncounterType (Just selectedHealthCenter)
                                    |> Backend.Model.PostPrenatalEncounter (generatePostCreateDestination newEncounterType hasNurseEncounter)
                                    |> App.Model.MsgIndexedDb
                                ]
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = Nothing }
            , Cmd.none
            , msgs
            )


startFollowUpEncounterHomeVisit : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpNutritionData -> List App.Model.Msg
startFollowUpEncounterHomeVisit currentDate selectedHealthCenter db data =
    resolveIndividualParticipantForPerson data.personId HomeVisitEncounter db
        |> Maybe.map
            -- If home visit participant exists, create new encounter for it.
            (\sessionId ->
                [ emptyHomeVisitEncounter sessionId currentDate (Just selectedHealthCenter)
                    |> Backend.Model.PostHomeVisitEncounter
                    |> App.Model.MsgIndexedDb
                ]
            )
        -- If not, create it.
        |> Maybe.withDefault
            [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                |> Backend.Model.PostIndividualSession Backend.IndividualEncounterParticipant.Model.NoIndividualParticipantExtraData
                |> App.Model.MsgIndexedDb
            ]


startFollowUpEncounterAcuteIllness : NominalDate -> HealthCenterId -> ModelIndexedDb -> FollowUpAcuteIllnessData -> List App.Model.Msg
startFollowUpEncounterAcuteIllness currentDate selectedHealthCenter db data =
    [ emptyAcuteIllnessEncounter data.participantId currentDate data.sequenceNumber AcuteIllnessEncounterCHW (Just selectedHealthCenter)
        |> Backend.Model.PostAcuteIllnessEncounter
        |> App.Model.MsgIndexedDb
    ]
