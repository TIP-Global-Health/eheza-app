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
            ( { model | dialogState = Nothing }
            , Cmd.none
            , startFollowUpEncounterOfTypeMsgs currentDate healthCenterId db data
            )


startFollowUpEncounterOfTypeMsgs : NominalDate -> Maybe HealthCenterId -> ModelIndexedDb -> FollowUpEncounterData -> List App.Model.Msg
startFollowUpEncounterOfTypeMsgs currentDate healthCenterId db data =
    case data.encounterType of
        HomeVisitEncounter ->
            healthCenterId
                |> Maybe.map
                    (\selectedHealthCenter ->
                        resolveIndividualParticipantForPerson data.personId HomeVisitEncounter db
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

        _ ->
            []
