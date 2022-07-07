module Components.SendViaWhatsAppDialog.Update exposing (update)

import App.Model
import Backend.Model
import Backend.Person.Model exposing (PatchPersonInitator(..))
import Components.SendViaWhatsAppDialog.Model exposing (..)


update : Msg -> Model -> ( Model, List App.Model.Msg )
update msg model =
    case msg of
        SetState state ->
            ( { model | state = state }, [] )

        UpdatePhoneAtProfile personId person phoneNumber ->
            ( { model | state = Just <| PhoneUpdateConfirmation phoneNumber }
            , [ Backend.Model.PatchPerson InitiatorProgressReport personId { person | telephoneNumber = Just phoneNumber }
                    |> App.Model.MsgIndexedDb
              ]
            )
