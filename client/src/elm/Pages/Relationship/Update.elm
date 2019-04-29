module Pages.Relationship.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
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

        Cancel ->
            ( Nothing
            , Cmd.none
            , [ PersonPage id1
                    |> UserPage
                    |> App.Model.SetActivePage
              ]
            )

        Save ->
            ( model
            , Cmd.none
            , []
            )

        RelationshipSelected data ->
            ( Just data
            , Cmd.none
            , []
            )
