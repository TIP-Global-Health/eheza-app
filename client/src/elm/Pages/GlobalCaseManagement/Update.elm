module Pages.GlobalCaseManagement.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Utils exposing (emptyIndividualEncounterParticipant)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
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
            let
                fetchMsgs =
                    state
                        |> Maybe.map
                            (\data ->
                                case data.encounterType of
                                    NutritionEncounter ->
                                        let
                                            individualParticipants =
                                                Dict.get data.personId db.individualParticipantsByPerson
                                                    |> Maybe.withDefault NotAsked
                                        in
                                        individualParticipants
                                            |> RemoteData.map
                                                (Dict.toList
                                                    >> List.filterMap
                                                        (\( participantId, participant ) ->
                                                            if participant.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter then
                                                                Just participantId

                                                            else
                                                                Nothing
                                                        )
                                                    >> List.map (FetchHomeVisitEncountersForParticipant >> App.Model.MsgIndexedDb)
                                                )
                                            |> RemoteData.withDefault []

                                    _ ->
                                        []
                            )
                        |> Maybe.withDefault []
            in
            ( { model | dialogState = state }
            , Cmd.none
            , fetchMsgs
            )

        StartFollowUpEncounter data ->
            let
                -- Resolving Home Visit participant for person.
                participantId =
                    Dict.get data.personId db.individualParticipantsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.filter
                            (\( sessionId, session ) ->
                                session.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
                            )
                        |> List.head
                        |> Maybe.map Tuple.first

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
            ( model
            , Cmd.none
            , startFollowUpEncounterMsgs
            )
