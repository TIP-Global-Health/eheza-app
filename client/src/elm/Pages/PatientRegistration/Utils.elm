module Pages.PatientRegistration.Utils exposing (getCommonDetails, getFormFieldValue, getRegistratingParticipant, sequenceExtra)

import Form
import Gizra.NominalDate exposing (NominalDate)
import List
import Maybe.Extra exposing (unwrap)
import Pages.PatientRegistration.Model exposing (PatientData(..))
import Participant.Model exposing (ParticipantType(..))
import Time exposing (Time)
import Time.Date


getCommonDetails : PatientData -> { name : String, avatarUrl : Maybe String, birthDate : Maybe NominalDate, village : Maybe String }
getCommonDetails patient =
    case patient of
        PatientMother _ mother ->
            { name = mother.name
            , avatarUrl = mother.avatarUrl
            , birthDate = mother.birthDate
            , village = mother.village
            }

        PatientChild _ child ->
            { name = child.name
            , avatarUrl = child.avatarUrl
            , birthDate = Just child.birthDate
            , village = child.village
            }


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


getRegistratingParticipant : NominalDate -> Int -> Int -> Int -> Maybe PatientData -> Maybe ParticipantType
getRegistratingParticipant currentDate birthDay birthMonth birthYear maybeRelationPatient =
    if birthDay > 0 && birthMonth > 0 && birthYear > 0 then
        let
            delta =
                Time.Date.delta currentDate (Time.Date.date birthYear birthMonth birthDay)
        in
        maybeRelationPatient
            |> unwrap
                (if delta.years > 12 then
                    Just <| MotherParticipant delta

                 else
                    Just <| ChildParticipant delta
                )
                (\relationPatient ->
                    case relationPatient of
                        PatientMother _ _ ->
                            Just <| MotherParticipant delta

                        PatientChild _ _ ->
                            Just <| ChildParticipant delta
                )

    else
        Nothing


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
