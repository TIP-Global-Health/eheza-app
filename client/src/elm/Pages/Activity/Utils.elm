module Pages.Activity.Utils exposing (participantsWithPendingActivity)

import Activity.Model exposing (ActivityType(Child, Mother))
import Activity.Utils exposing (getActivityIdentity, hasPendingChildActivity, hasPendingMotherActivity)
import EveryDict
import Pages.Activity.Model exposing (..)
import Participant.Model exposing (Participant(..))


participantsWithPendingActivity : ParticipantsDict -> Model -> ParticipantsDict
participantsWithPendingActivity participantsDict model =
    let
        selectedActivityIdentity =
            getActivityIdentity model.selectedActivity
    in
        participantsDict
            |> EveryDict.filter
                (\participantId participant ->
                    case participant of
                        ParticipantChild child ->
                            case selectedActivityIdentity.activityType of
                                Child activityType ->
                                    hasPendingChildActivity child activityType

                                Mother _ ->
                                    False

                        ParticipantMother mother ->
                            case selectedActivityIdentity.activityType of
                                Child _ ->
                                    False

                                Mother activityType ->
                                    hasPendingMotherActivity mother activityType
                )
