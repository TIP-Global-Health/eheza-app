module Pages.Person.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form exposing (PersonForm, applyDefaultValues, birthDate, validatePerson)
import Backend.Person.Model exposing (ExpectedAge(..), ParticipantDirectoryOperation(..), Person)
import Date
import Form
import Form.Field
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD, fromLocalDateTime)
import Maybe.Extra exposing (isJust)
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


update : NominalDate -> Msg -> ModelIndexedDb -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg db model =
    case msg of
        MsgForm operation subMsg ->
            let
                relation =
                    case operation of
                        CreatePerson maybeId ->
                            maybeId

                        EditPerson id ->
                            Just id

                related =
                    relation
                        |> Maybe.andThen (\personId -> Dict.get personId db.people)
                        |> Maybe.andThen RemoteData.toMaybe

                newForm =
                    Form.update (validatePerson related operation (Just currentDate)) subMsg model.form

                appMsgs =
                    case subMsg of
                        Form.Submit ->
                            case operation of
                                CreatePerson _ ->
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

                                EditPerson _ ->
                                    relation
                                        |> Maybe.map
                                            (\personId ->
                                                applyDefaultValues related operation currentDate model.form
                                                    |> Form.getOutput
                                                    |> Maybe.map
                                                        (\person ->
                                                            generateMsgsForPersonEdit currentDate personId person model.form db
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

        DropZoneComplete operation result ->
            let
                subMsg =
                    Form.Input Backend.Person.Form.photo Form.Text (Form.Field.String result.url)
            in
            update currentDate (MsgForm operation subMsg) db model

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

        DateSelected operation date ->
            let
                dateAsString =
                    Date.format "yyyy-MM-dd" date

                setFieldMsg =
                    Form.Input birthDate Form.Text (Form.Field.String dateAsString) |> MsgForm operation
            in
            update currentDate setFieldMsg db model


generateMsgsForPersonEdit : NominalDate -> PersonId -> Person -> PersonForm -> ModelIndexedDb -> List App.Model.Msg
generateMsgsForPersonEdit currentDate personId person form db =
    let
        province =
            Form.getFieldAsString Backend.Person.Form.province form

        district =
            Form.getFieldAsString Backend.Person.Form.district form

        sector =
            Form.getFieldAsString Backend.Person.Form.sector form

        cell =
            Form.getFieldAsString Backend.Person.Form.cell form

        village =
            Form.getFieldAsString Backend.Person.Form.village form

        updateChildrenMsgs =
            -- We do not allow childrent to edit address, therefore, we do not
            -- have to verify that edited person is an adult.
            -- What we do check is that at least one of address fields was changed.
            if List.any (.value >> isJust) [ province, district, sector, cell, village ] then
                let
                    childrenIds =
                        Dict.get personId db.relationshipsByPerson
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (Dict.values >> List.map .relatedTo)
                            |> RemoteData.withDefault []

                    updatedChildren =
                        childrenIds
                            |> List.map
                                (\childId ->
                                    Dict.get childId db.people
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.map
                                            (\child ->
                                                ( childId
                                                , { child
                                                    | province = person.province
                                                    , district = person.district
                                                    , sector = person.sector
                                                    , cell = person.cell
                                                    , village = person.village
                                                  }
                                                )
                                            )
                                )
                            |> List.filterMap
                                (\remoteChild ->
                                    case remoteChild of
                                        Success childData ->
                                            Just childData

                                        _ ->
                                            Nothing
                                )
                in
                updatedChildren
                    |> List.map
                        (\( childId, child ) ->
                            child
                                |> Backend.Model.PatchPerson childId
                                |> App.Model.MsgIndexedDb
                        )

            else
                []
    in
    (person
        |> Backend.Model.PatchPerson personId
        |> App.Model.MsgIndexedDb
    )
        :: updateChildrenMsgs
