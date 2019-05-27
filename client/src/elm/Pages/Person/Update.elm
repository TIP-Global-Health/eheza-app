module Pages.Person.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model
import Backend.Person.Form exposing (validatePerson)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isAdultRegistering, isPersonAnAdult)
import EveryDict exposing (EveryDict)
import Form
import Form.Field
import Gizra.NominalDate exposing (NominalDate)
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


update : NominalDate -> Msg -> EveryDict PersonId (WebData Person) -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg people model =
    case msg of
        MsgForm relation subMsg ->
            let
                birthDateField =
                    Form.getFieldAsString Backend.Person.Form.birthDate model

                isAdult =
                    relation
                        |> Maybe.andThen (\personId -> EveryDict.get personId people)
                        |> Maybe.andThen RemoteData.toMaybe
                        -- We register an adul,t if person we relate to is a child.
                        |> Maybe.map (isPersonAnAdult currentDate >> not)
                        -- Or, by checking birth date field, if we don't have relation info.
                        |> Maybe.withDefault (isAdultRegistering currentDate birthDateField)

                newModel =
                    Form.update (validatePerson isAdult (Just currentDate)) subMsg model

                appMsgs =
                    case subMsg of
                        Form.Submit ->
                            Form.getOutput model
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
            ( newModel
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
            ( Backend.Person.Form.emptyForm
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
