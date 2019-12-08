module Pages.Person.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PersonId)
import Backend.Model
import Backend.Person.Form exposing (ExpectedAge(..), birthDate, validatePerson)
import Backend.Person.Model exposing (Person)
import Date exposing (toRataDie)
import Form
import Form.Field
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD, fromLocalDateTime)
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


update : NominalDate -> Msg -> Dict PersonId (WebData Person) -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg people model =
    case msg of
        MsgForm relation subMsg ->
            let
                log =
                    Debug.log "subMsg" subMsg

                related =
                    relation
                        |> Maybe.andThen (\personId -> Dict.get personId people)
                        |> Maybe.andThen RemoteData.toMaybe

                newForm =
                    Form.update (validatePerson related (Just currentDate)) subMsg model.form

                appMsgs =
                    case subMsg of
                        Form.Submit ->
                            Form.getOutput model.form
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

                        _ ->
                            []
            in
            ( { model | form = newForm }
            , Cmd.none
            , appMsgs
            )

        DropZoneComplete relation result ->
            let
                subMsg =
                    Form.Input Backend.Person.Form.photo Form.Text (Form.Field.String result.url)
            in
            update currentDate (MsgForm relation subMsg) people model

        ResetCreateForm ->
            ( Pages.Person.Model.emptyModel
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

        DateSelected relation date ->
            let
                dateAsString =
                    Date.format "yyyy-MM-dd" date

                setFieldMsg =
                    Form.Input birthDate Form.Text (Form.Field.String dateAsString) |> MsgForm relation
            in
            update currentDate setFieldMsg people model
