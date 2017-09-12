module Pages.Activity.Utils exposing (participantsWithPendingActivity)

import Activity.Model exposing (ActivityType(Child, Mother))
import Activity.Utils exposing (getActivityIdentity, hasPendingChildActivity, hasPendingMotherActivity)
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Pages.Activity.Model exposing (Model)
import Participant.Model exposing (ParticipantType(ParticipantChild, ParticipantMother), ParticipantsDict)


participantsWithPendingActivity : Date -> ParticipantsDict -> Model -> ParticipantsDict
participantsWithPendingActivity currentDate participantsDict model =
    let
        selectedActivityIdentity =
            getActivityIdentity model.selectedActivity
    in
        participantsDict
            |> Dict.filter
                (\participantId participant ->
                    case participant.info of
                        ParticipantChild child ->
                            case selectedActivityIdentity.activityType of
                                Child activityType ->
                                    hasPendingChildActivity currentDate child activityType

                                Mother _ ->
                                    False

                        ParticipantMother mother ->
                            case selectedActivityIdentity.activityType of
                                Child _ ->
                                    False

                                Mother activityType ->
                                    hasPendingMotherActivity currentDate mother activityType
                )
