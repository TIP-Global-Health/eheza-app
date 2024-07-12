module Components.ReportToWhatsAppDialog.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model
import Backend.Person.Model exposing (PatchPersonInitator(..))
import Components.ReportToWhatsAppDialog.Model exposing (..)
import Components.ReportToWhatsAppDialog.Utils exposing (..)
import Maybe.Extra exposing (isJust)
import Restful.Endpoint exposing (fromEntityUuid)
import Translate.Utils exposing (languageToCode)


update : Msg msg -> Model -> ( Model, Cmd msg, ( List msg, List App.Model.Msg ) )
update msg model =
    let
        noChange =
            ( model, Cmd.none, ( [], [] ) )
    in
    case msg of
        SetState state ->
            ( { model | state = state }, Cmd.none, ( [], [] ) )

        SetPhoneNumber value ->
            case model.state of
                Just (PhoneInput data) ->
                    if isJust (String.toInt value) then
                        let
                            updatedState =
                                PhoneInput { data | phone = value }
                        in
                        update (SetState <| Just updatedState) model

                    else
                        noChange

                _ ->
                    noChange

        SetCountryCode value ->
            case model.state of
                Just (PhoneInput data) ->
                    countryCodeFromString value
                        |> Maybe.map
                            (\code ->
                                let
                                    updatedState =
                                        PhoneInput { data | countryCode = code }
                                in
                                update (SetState <| Just updatedState) model
                            )
                        |> Maybe.withDefault noChange

                _ ->
                    noChange

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

        Execute language reportType personId phoneNumber ->
            ( { model | state = Just <| ExecutionResult Nothing }
            , App.Ports.makeProgressReportScreenshot
                { language = languageToCode language
                , reportType = reportTypeToString reportType
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
