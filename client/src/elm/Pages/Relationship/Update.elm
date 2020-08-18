module Pages.Relationship.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model
import Backend.Person.Model exposing (Initiator(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Relationship.Model exposing (..)
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

        Reset initiator ->
            let
                nextPage =
                    case initiator of
                        -- When at session context, we navigate to session Attendance page.
                        -- At that page, we should see newly created attendance.
                        GroupEncounterOrigin sessionId ->
                            UserPage (SessionPage sessionId AttendancePage)

                        -- For other cases, we navigate to the page of main person.
                        _ ->
                            UserPage (PersonPage id1 initiator)
            in
            ( emptyModel
            , Cmd.none
            , [ App.Model.SetActivePage nextPage ]
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
