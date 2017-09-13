module Pages.Participant.Utils exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Child.Model exposing (ChildId)
import Date exposing (Date)
import Dict
import Mother.Model exposing (MotherId)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantsDict)


makeLoneMotherDict : MotherId -> Mother.Model.Mother -> ParticipantsDict
makeLoneMotherDict motherId mother =
    Dict.insert motherId ({ info = Participant.Model.ParticipantMother mother }) Dict.empty


makeLoneChildDict : ChildId -> Child.Model.Child -> ParticipantsDict
makeLoneChildDict childId child =
    Dict.insert childId ({ info = Participant.Model.ParticipantChild child }) Dict.empty


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
                        FamilyPlanning ->
                            { activityDates | familyPlanning = Just date }
            in
                { info = ParticipantMother { mother | activityDates = updatedActivityDates } }

        -- This should never be reached as it would imply a Child activity being applied to a Mother or vica versa
        ( _, _ ) ->
            participant


{-| Like `Update.Extra.sequence`, but for `update` signatures that also
return a data and redirect page.
-}
sequenceExtra :
    (msg -> model -> ( data, model, Cmd msg, page ))
    -> List msg
    -> ( data, model, Cmd msg, page )
    -> ( data, model, Cmd msg, page )
sequenceExtra updater msgs ( data, previousModel, previousCmd, page ) =
    List.foldl
        (\eachMsg ( data, modelSoFar, cmdsSoFar, page ) ->
            let
                ( _, newModel, newCmd, _ ) =
                    updater eachMsg modelSoFar
            in
                ( data
                , newModel
                , Cmd.batch [ cmdsSoFar, newCmd ]
                , page
                )
        )
        ( data, previousModel, previousCmd, page )
        msgs
