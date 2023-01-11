module Components.SendViaWhatsAppDialog.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model
import Backend.Person.Model exposing (PatchPersonInitator(..))
import Components.SendViaWhatsAppDialog.Model exposing (..)
import Components.SendViaWhatsAppDialog.Utils exposing (..)
import Restful.Endpoint exposing (fromEntityUuid)


update : Msg msg -> Model -> ( Model, Cmd msg, ( List msg, List App.Model.Msg ) )
update msg model =
    case msg of
        SetState state ->
            ( { model | state = state }, Cmd.none, ( [], [] ) )

        UpdatePhoneAtProfile personId person phoneNumber ->
            ( { model | state = Just <| PhoneUpdateConfirmation phoneNumber }
            , Cmd.none
            , ( []
              , [ Backend.Model.PatchPerson InitiatorProgressReport personId { person | telephoneNumber = Just phoneNumber }
                    |> App.Model.MsgIndexedDb
                ]
              )
            )

        SetReportComponents setComponentsMsg phoneNumber ->
            ( { model | state = Just <| ConfirmationBeforeExecuting phoneNumber }
            , Cmd.none
            , ( [ setComponentsMsg ]
              , []
              )
            )

        Execute reportType personId phoneNumber ->
            ( { model | state = Just <| ExecutionResult Nothing }
            , App.Ports.makeProgressReportScreenshot
                { reportType = reportTypeToString reportType
                , personId = fromEntityUuid personId
                , phoneNumber = phoneNumber
                }
            , ( []
              , []
              )
            )

        CancelExecute clearComponentsMsg ->
            ( { model | state = Nothing }
            , Cmd.none
            , ( Maybe.map List.singleton clearComponentsMsg
                    |> Maybe.withDefault []
              , []
              )
            )

        SetExecutionResult clearComponentsMsg value ->
            ( { model | state = Just <| ExecutionResult <| Just value }
            , Cmd.none
            , ( Maybe.map List.singleton clearComponentsMsg
                    |> Maybe.withDefault []
              , []
              )
            )
