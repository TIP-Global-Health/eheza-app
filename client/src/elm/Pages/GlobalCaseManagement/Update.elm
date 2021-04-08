module Pages.GlobalCaseManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Utils exposing (resolveIndividualParticipantForPerson)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
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

        StartFollowUpEncounter data ->
            let
                -- Resolving Home Visit participant for person.
                participantId =
                    resolveIndividualParticipantForPerson data.personId HomeVisitEncounter db

                startFollowUpEncounterMsgs =
                    healthCenterId
                        |> Maybe.map
                            (\selectedHealthCenter ->
                                participantId
                                    |> Maybe.map
                                        -- If home visit participant exists, create new encounter for it.
                                        (\sessionId ->
                                            [ Backend.HomeVisitEncounter.Model.HomeVisitEncounter sessionId currentDate Nothing (Just selectedHealthCenter)
                                                |> Backend.Model.PostHomeVisitEncounter
                                                |> App.Model.MsgIndexedDb
                                            ]
                                        )
                                    -- If not, create it.
                                    |> Maybe.withDefault
                                        [ emptyIndividualEncounterParticipant currentDate data.personId Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter selectedHealthCenter
                                            |> Backend.Model.PostIndividualSession
                                            |> App.Model.MsgIndexedDb
                                        ]
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = Nothing }
            , Cmd.none
            , startFollowUpEncounterMsgs
            )
