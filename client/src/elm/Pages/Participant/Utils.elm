module Pages.Participant.Utils exposing (..)

import Dict
import List
import Child.Model exposing (ChildId, Child)
import Mother.Model exposing (MotherId, Mother)
import Participant.Model exposing (ParticipantsDict)


makeLoneMotherDict : MotherId -> Mother.Model.Mother -> ParticipantsDict
makeLoneMotherDict motherId mother =
    Dict.insert motherId ({ info = Participant.Model.ParticipantMother mother }) Dict.empty


makeLoneChildDict : ChildId -> Child.Model.Child -> ParticipantsDict
makeLoneChildDict childId child =
    Dict.insert childId ({ info = Participant.Model.ParticipantChild child }) Dict.empty


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
