module Pages.Person.Update exposing (update)

import AllDict
import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model
import Backend.Person.Form exposing (applyDefaultValues, birthDate, validatePerson)
import Backend.Person.Model exposing (ExpectedAge(..), ParticipantDirectoryOperation(..), Person)
import Form
import Form.Field
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD, fromLocalDateTime)
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)


update : NominalDate -> Msg -> EntityUuidDict PersonId (WebData Person) -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg people model =
    case msg of
        MsgForm relation operation subMsg ->
            let
                related =
                    relation
                        |> Maybe.andThen (\personId -> AllDict.get personId people)
                        |> Maybe.andThen RemoteData.toMaybe

                newForm =
                    Form.update (validatePerson related operation (Just currentDate)) subMsg model.form

                appMsgs =
                    case subMsg of
                        Form.Submit ->
                            case operation of
                                CreatePerson ->
                                    model.form
                                        |> Form.getOutput
                                        |> Maybe.map
                                            (\person ->
                                                [ person
                                                    |> Backend.Model.PostPerson relation
                                                    |> App.Model.MsgIndexedDb
                                                ]
                                            )
                                        -- If we submit, but can't actually submit,
                                        -- then change the request status to
                                        -- `NotAsked` (to reset network errors
                                        -- etc.)
                                        |> Maybe.withDefault
                                            [ Backend.Model.HandlePostedPerson relation NotAsked
                                                |> App.Model.MsgIndexedDb
                                            ]

                                EditPerson ->
                                    relation
                                        |> Maybe.map
                                            (\personId ->
                                                applyDefaultValues related operation currentDate model.form
                                                    |> Form.getOutput
                                                    |> Maybe.map
                                                        (\person ->
                                                            [ person
                                                                |> Backend.Model.PatchPerson personId
                                                                |> App.Model.MsgIndexedDb
                                                            ]
                                                        )
                                                    -- If we submit, but can't actually submit,
                                                    -- then change the request status to
                                                    -- `NotAsked` (to reset network errors
                                                    -- etc.)
                                                    |> Maybe.withDefault
                                                        [ Backend.Model.HandlePatchedPerson personId NotAsked
                                                            |> App.Model.MsgIndexedDb
                                                        ]
                                            )
                                        -- We should never get here, because when editing,
                                        -- we always have the ID of person being edited.
                                        |> Maybe.withDefault []

                        _ ->
                            []
            in
            ( { model | form = newForm }
            , Cmd.none
            , appMsgs
            )

        DropZoneComplete relation operation result ->
            let
                subMsg =
                    Form.Input Backend.Person.Form.photo Form.Text (Form.Field.String result.url)
            in
            update currentDate (MsgForm relation operation subMsg) people model

        ResetCreateForm ->
            ( Pages.Person.Model.emptyCreateModel
            , Cmd.none
            , []
            )

        ResetEditForm ->
            ( Pages.Person.Model.emptyEditModel
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        ToggleDateSelector ->
            ( { model | isDateSelectorOpen = not model.isDateSelectorOpen }
            , Cmd.none
            , []
            )

        DateSelected relation operation date ->
            let
                dateAsString =
                    fromLocalDateTime date |> formatYYYYMMDD

                setFieldMsg =
                    Form.Input birthDate Form.Text (Form.Field.String dateAsString) |> MsgForm relation operation
            in
            update currentDate setFieldMsg people model
