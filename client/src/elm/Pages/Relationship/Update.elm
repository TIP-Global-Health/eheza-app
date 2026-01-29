module Pages.Relationship.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model
import Pages.Relationship.Model exposing (Model, Msg(..), emptyModel)
import Restful.Endpoint exposing (toEntityUuid)


update : PersonId -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id1 id2 msg model =
    case msg of
        AssignToClinicId id ->
            let
                clinicId =
                    if String.isEmpty id then
                        Nothing

                    else
                        Just (toEntityUuid id)
            in
            ( { model | assignToGroup = clinicId }
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        Reset ->
            ( emptyModel
            , Cmd.none
            , []
            )

        Save maybeRelatedBy assignToGroup initiator ->
            let
                extraMsg =
                    case maybeRelatedBy of
                        Just relatedBy ->
                            [ Backend.Model.PostRelationship id1
                                { relatedBy = relatedBy
                                , relatedTo = id2
                                }
                                assignToGroup
                                initiator
                                |> App.Model.MsgIndexedDb
                            ]

                        Nothing ->
                            []
            in
            ( model
            , Cmd.none
            , extraMsg
            )

        RelationshipSelected data ->
            ( { model | relatedBy = Just data }
            , Cmd.none
            , []
            )
