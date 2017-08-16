module Pages.Participant.Utils exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Date exposing (Date)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantsDict)


{-| Model update helper which applies the provided date to the specified activity type field in the
provided participant while ensuring proper Mother / Child and activity mapping type safety
-}
updateActivityDate : Date -> ActivityType -> Participant -> Participant
updateActivityDate date activityType participant =
    case ( activityType, participant.info ) of
        ( Child childActivityType, ParticipantChild child ) ->
            let
                activityDates =
                    child.activityDates

                updatedActivityDates =
                    case childActivityType of
                        ChildPicture ->
                            { activityDates | childPicture = Just date }

                        Height ->
                            { activityDates | height = Just date }

                        Muac ->
                            { activityDates | muac = Just date }

                        NutritionSigns ->
                            { activityDates | nutritionSigns = Just date }

                        ProgressReport ->
                            { activityDates | progressReport = Just date }

                        Weight ->
                            { activityDates | weight = Just date }
            in
                { info = ParticipantChild { child | activityDates = updatedActivityDates } }

        ( Mother motherActivityType, ParticipantMother mother ) ->
            let
                activityDates =
                    mother.activityDates

                updatedActivityDates =
                    case motherActivityType of
                        Aheza ->
                            { activityDates | aheza = Just date }

                        Attendance ->
                            { activityDates | attendance = Just date }

                        Education ->
                            { activityDates | education = Just date }

                        FamilyPlanning ->
                            { activityDates | familyPlanning = Just date }

                        Hiv ->
                            { activityDates | hiv = Just date }

                        MotherPicture ->
                            { activityDates | motherPicture = Just date }
            in
                { info = ParticipantMother { mother | activityDates = updatedActivityDates } }

        -- This should never be reached as it would imply a Child activity being applied to a Mother or vica versa
        ( _, _ ) ->
            participant
