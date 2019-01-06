module Pages.PatientRegistration.Utils exposing (getFormFieldValue, sequenceExtra)

import Form
import List
import Maybe.Extra exposing (unwrap)


getFormFieldValue : Form.FieldState e String -> Int
getFormFieldValue field =
    unwrap
        0
        (\value ->
            case String.toInt value of
                Ok value ->
                    value

                _ ->
                    0
        )
        field.value


{-| Like `Update.Extra.sequence`, but for `update` signatures that also
return appMsgs.
-}
sequenceExtra :
    (msg -> model -> ( model, Cmd msg, List appMsgs ))
    -> List msg
    -> ( model, Cmd msg, List appMsgs )
    -> ( model, Cmd msg, List appMsgs )
sequenceExtra updater msgs ( previousModel, previousCmd, previousAppMsgs ) =
    List.foldl
        (\eachMsg ( modelSoFar, cmdsSoFar, appMsgsSoFar ) ->
            let
                ( newModel, newCmd, newAppMsgs ) =
                    updater eachMsg modelSoFar
            in
            ( newModel
            , Cmd.batch [ cmdsSoFar, newCmd ]
            , appMsgsSoFar ++ newAppMsgs
            )
        )
        ( previousModel, previousCmd, previousAppMsgs )
        msgs
