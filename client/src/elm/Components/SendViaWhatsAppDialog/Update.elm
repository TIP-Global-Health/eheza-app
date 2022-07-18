module Components.SendViaWhatsAppDialog.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model
import Backend.Person.Model exposing (PatchPersonInitator(..))
import Components.SendViaWhatsAppDialog.Model exposing (..)


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
            ( { model | state = Just <| ConfirmationBeforeSending phoneNumber }
            , Cmd.none
            , ( [ setComponentsMsg ]
              , []
              )
            )

        Execute clearComponentsMsg phoneNumber ->
            ( { model | state = Nothing }
            , App.Ports.makeProgressReportScreenshot phoneNumber
            , ( [ clearComponentsMsg ]
              , []
              )
            )
