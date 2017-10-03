module Pages.Activity.Utils exposing (participantsWithPendingActivity)

import Activity.Model exposing (ActivityType(Child, Mother))
import Activity.Utils exposing (getActivityIdentity, hasPendingChildActivity, hasPendingMotherActivity)
import Dict
import Pages.Activity.Model exposing (Model)
import Participant.Model exposing (ParticipantType(ParticipantChild, ParticipantMother), ParticipantsDict)


{-| This will eventually need to be parameterized to focus on a particular
date ... for now, we just consult the single examination for each participant.
-}
participantsWithPendingActivity : ParticipantsDict -> Model -> ParticipantsDict
participantsWithPendingActivity participantsDict model =
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
