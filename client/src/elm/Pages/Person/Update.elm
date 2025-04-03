module Pages.Person.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form exposing (PersonForm, applyDefaultValuesForPerson, birthDate, validatePerson)
import Backend.Person.Model exposing (ParticipantDirectoryOperation(..), PatchPersonInitator(..), Person)
import Backend.Village.Utils exposing (getVillageById)
import Date
import Form
import Form.Field
import GeoLocation.Model exposing (ReverseGeoInfo)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust)
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site)


update : NominalDate -> Site -> ReverseGeoInfo -> Maybe HealthCenterId -> Maybe VillageId -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site reverseGeoInfo selectedHealthCenter maybeVillageId isChw db msg model =
    case msg of
        MsgForm operation initiator subMsg ->
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
                    Form.update (validatePerson site related operation (Just currentDate)) subMsg model.form

                ( appMsgs, extraMsgs ) =
                    case subMsg of
                        Form.Submit ->
                            let
                                formWithDefaults =
                                    let
                                        maybeVillage =
                                            Maybe.andThen (getVillageById db) maybeVillageId
                                    in
                                    applyDefaultValuesForPerson currentDate
                                        site
                                        reverseGeoInfo
                                        maybeVillage
                                        isChw
                                        related
                                        operation
                                        initiator
                                        model.form
                            in
                            case operation of
                                CreatePerson _ ->
                                    Form.getOutput formWithDefaults
                                        |> Maybe.map
                                            (\person ->
                                                let
                                                    personWithShard =
                                                        { person | shard = selectedHealthCenter }
                                                in
                                                Maybe.andThen
                                                    (\nationalId ->
                                                        Dict.get nationalId db.personSearchesByNationalId
                                                            |> Maybe.andThen RemoteData.toMaybe
                                                            |> Maybe.andThen (Dict.values >> List.head)
                                                            |> Maybe.map
                                                                (\suspectedDuplicate ->
                                                                    ( []
                                                                    , [ SetDialogState <|
                                                                            Just <|
                                                                                ( suspectedDuplicate
                                                                                , PostPerson relation initiator personWithShard
                                                                                )
                                                                      ]
                                                                    )
                                                                )
                                                    )
                                                    personWithShard.nationalIdNumber
                                                    |> Maybe.withDefault
                                                        ( [ Backend.Model.PostPerson relation initiator personWithShard
                                                                |> App.Model.MsgIndexedDb
                                                          ]
                                                        , []
                                                        )
                                            )
                                        -- If we submit, but can't actually submit,
                                        -- then change the request status to
                                        -- `NotAsked` (to reset network errors
                                        -- etc.)
                                        |> Maybe.withDefault
                                            ( [ Backend.Model.HandlePostedPerson relation initiator NotAsked
                                                    |> App.Model.MsgIndexedDb
                                              ]
                                            , []
                                            )

                                EditPerson _ ->
                                    relation
                                        |> Maybe.map
                                            (\personId ->
                                                formWithDefaults
                                                    |> Form.getOutput
                                                    |> Maybe.map
                                                        (\person ->
                                                            let
                                                                personWithShard =
                                                                    { person | shard = selectedHealthCenter }
                                                            in
                                                            ( generateMsgsForPersonEdit currentDate
                                                                personId
                                                                personWithShard
                                                                model.form
                                                                db
                                                            , []
                                                            )
                                                        )
                                                    -- If we submit, but can't actually submit,
                                                    -- then change the request status to
                                                    -- `NotAsked` (to reset network errors
                                                    -- etc.)
                                                    |> Maybe.withDefault
                                                        ( [ Backend.Model.HandlePatchedPerson InitiatorEditForm personId NotAsked
                                                                |> App.Model.MsgIndexedDb
                                                          ]
                                                        , []
                                                        )
                                            )
                                        -- We should never get here, because when editing,
                                        -- we always have the ID of person being edited.
                                        |> Maybe.withDefault ( [], [] )

                        Form.Input _ _ (Form.Field.String value) ->
                            if String.length value > 13 then
                                ( [ Backend.Model.FetchPeopleByNationalId value
                                        |> App.Model.MsgIndexedDb
                                  ]
                                , []
                                )

                            else
                                ( [], [] )

                        _ ->
                            ( [], [] )
            in
            ( { model | form = newForm }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra
                    (update currentDate
                        site
                        reverseGeoInfo
                        selectedHealthCenter
                        maybeVillageId
                        isChw
                        db
                    )
                    extraMsgs

        DropZoneComplete operation initiator result ->
            let
                subMsg =
                    Form.Input Backend.Person.Form.photo Form.Text (Form.Field.String result.url)
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra
                    (update currentDate
                        site
                        reverseGeoInfo
                        selectedHealthCenter
                        maybeVillageId
                        isChw
                        db
                    )
                    [ MsgForm operation initiator subMsg ]

        ResetCreateForm ->
            ( Pages.Person.Model.emptyCreateModel site
            , Cmd.none
            , []
            )

        ResetEditForm ->
            ( Pages.Person.Model.emptyEditModel site
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        DateSelected operation initiator date ->
            let
                setFieldMsg =
                    let
                        dateAsString =
                            Date.format "yyyy-MM-dd" date
                    in
                    Form.Input birthDate Form.Text (Form.Field.String dateAsString)
                        |> MsgForm operation initiator
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra
                    (update currentDate
                        site
                        reverseGeoInfo
                        selectedHealthCenter
                        maybeVillageId
                        isChw
                        db
                    )
                    [ setFieldMsg ]

        SetDateSelectorState state ->
            ( { model | dateSelectorPopupState = state }
            , Cmd.none
            , []
            )

        SetDialogState state ->
            ( { model | dialogState = state }
            , Cmd.none
            , []
            )

        PostPerson relation initiator person ->
            ( model
            , Cmd.none
            , [ Backend.Model.PostPerson relation initiator person
                    |> App.Model.MsgIndexedDb
              ]
            )


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
                            Backend.Model.PatchPerson InitiatorEditForm childId child
                                |> App.Model.MsgIndexedDb
                        )

            else
                []
    in
    (Backend.Model.PatchPerson InitiatorEditForm personId person
        |> App.Model.MsgIndexedDb
    )
        :: updateChildrenMsgs
