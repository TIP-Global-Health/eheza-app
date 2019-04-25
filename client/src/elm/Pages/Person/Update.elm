module Pages.Person.Update exposing (update)

import App.Model
import Backend.Model
import Backend.Person.Form exposing (validatePerson)
import Form
import Form.Field
import Pages.Person.Model exposing (..)
import RemoteData exposing (RemoteData(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        MsgForm relation subMsg ->
            let
                newModel =
                    Form.update validatePerson subMsg model

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
            update (MsgForm relation subMsg) model

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
