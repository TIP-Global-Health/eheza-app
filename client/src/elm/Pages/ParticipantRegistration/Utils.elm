module Pages.ParticipantRegistration.Utils exposing (decodeStringToMaybe, getFormFieldValue, getRegistratingParticipant, sequenceExtra)

import Form
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (Decoder)
import List
import Maybe.Extra exposing (unwrap)
import Participant.Model exposing (ParticipantId(..), ParticipantType(..))
import Time.Date


getFormFieldValue : Form.FieldState e String -> Maybe Int
getFormFieldValue field =
    Maybe.andThen (String.toInt >> Result.toMaybe) field.value


{-| TODO: Remove this. Should explicitly choose between child and mother,
rather than depending on age at time of data entry.
-}
getRegistratingParticipant : NominalDate -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe ParticipantId -> Maybe ParticipantType
getRegistratingParticipant currentDate maybeDay maybeMonth maybeYear maybeRelationParticipant =
    case ( maybeDay, maybeMonth, maybeYear ) of
        ( Just birthDay, Just birthMonth, Just birthYear ) ->
            let
                delta =
                    Time.Date.delta currentDate (Time.Date.date birthYear birthMonth birthDay)
            in
            maybeRelationParticipant
                |> unwrap
                    (if delta.years > 12 then
                        Just <| MotherParticipant delta

                     else
                        Just <| ChildParticipant delta
                    )
                    (\relationParticipant ->
                        case relationParticipant of
                            ParticipantMother _ ->
                                Just <| ChildParticipant delta

                            ParticipantChild _ ->
                                Just <| MotherParticipant delta
                    )

        _ ->
            Nothing


{-| Given a string value, tries to decode it. Returns a Just if
successful, Nothing if not.
-}
decodeStringToMaybe : Decoder a -> String -> Maybe a
decodeStringToMaybe decoder value =
    -- We need to add the quotes to make it a value JSON string
    case Json.Decode.decodeString decoder ("\"" ++ value ++ "\"") of
        Err _ ->
            Nothing

        Ok result ->
            Just result


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
