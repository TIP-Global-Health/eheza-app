module Pages.Relationship.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Relationship.Model exposing (..)


update : PersonId -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id1 id2 msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        Reset ->
            ( Nothing
            , Cmd.none
            , [ PersonPage id1
                    |> UserPage
                    |> App.Model.SetActivePage
              ]
            )

        Save ->
            let
                extraMsg =
                    case model of
                        Just relatedBy ->
                            [ Backend.Model.PostRelationship id1
                                { relatedBy = relatedBy
                                , relatedTo = id2
                                }
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
            ( Just data
            , Cmd.none
            , []
            )
