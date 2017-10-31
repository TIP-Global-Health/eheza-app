module Pages.Update exposing (..)

import EveryDict
import Maybe.Extra
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Activities.Update
import Pages.Model exposing (..)
import Pages.Participants.Update
import Update.Extra exposing (sequence)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgActivities subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Activities.Update.update subMsg model.activitiesPage
            in
                ( { model | activitiesPage = subModel }
                , Cmd.map MsgActivities subCmd
                )
                    |> sequence update (List.map SetUserAttention (Maybe.Extra.toList subPage))

        MsgActivity activityType subMsg ->
            let
                ( subModel, subCmd ) =
                    EveryDict.get activityType model.activityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.Update.update subMsg
            in
                ( { model | activityPages = EveryDict.insert activityType subModel model.activityPages }
                , Cmd.map (MsgActivity activityType) subCmd
                )

        MsgParticipants subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Participants.Update.update subMsg model.participantsPage
            in
                ( { model | participantsPage = subModel }
                , Cmd.map MsgParticipants subCmd
                )
                    |> sequence update (List.map SetUserAttention (Maybe.Extra.toList subPage))

        SetUserAttention val ->
            ( { model | userAttention = val }
            , Cmd.none
            )
