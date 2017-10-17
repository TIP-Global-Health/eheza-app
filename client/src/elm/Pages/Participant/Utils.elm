module Pages.Participant.Utils exposing (..)

import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother)
import Dict
import Drupal.Restful exposing (fromEntityId)
import List
import Participant.Model


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
