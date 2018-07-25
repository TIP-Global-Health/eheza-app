module Pages.Admin.Update exposing (..)

import App.Model
import Backend.Model exposing (ModelBackend)
import Backend.Session.Form exposing (validateSession)
import EveryDictList
import Form
import Gizra.NominalDate exposing (NominalDate)
import Pages.Admin.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Time.Date exposing (addDays)


{-| For simplicity's sake, we just pass in the whole backend. In theory, we
might not need to consult all of it here. Any **modifications** to the backend
are done by passing up `msgs` in our third return param.
-}
update : NominalDate -> ModelBackend -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update date backend msg model =
    let
        -- If we don't have a list of clinics yet, we just pretend that all
        -- clinic ID's are known. It will arrive eventually!
        knownClinic clinicId =
            backend.clinics
                |> RemoteData.map (EveryDictList.member clinicId)
                |> RemoteData.withDefault True
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        MsgBackend subMsg ->
            ( model
            , Cmd.none
            , [ App.Model.MsgLoggedIn (App.Model.MsgBackend subMsg) ]
            )

        MsgCreateSession subMsg ->
            let
                createSession =
                    model.createSession
                        |> Maybe.map (Form.update (validateSession knownClinic) subMsg)

                appMsgs =
                    case subMsg of
                        Form.Submit ->
                            model.createSession
                                |> Maybe.andThen Form.getOutput
                                |> Maybe.map
                                    (\session ->
                                        [ session
                                            |> Backend.Model.PostSession
                                            |> App.Model.MsgBackend
                                            |> App.Model.MsgLoggedIn
                                        ]
                                    )
                                -- If we submit, but can't actually submit,
                                -- then change the request status to
                                -- `NotAsked` (to reset network errors
                                -- etc.)
                                |> Maybe.withDefault
                                    [ Backend.Model.HandlePostedSession NotAsked
                                        |> App.Model.MsgBackend
                                        |> App.Model.MsgLoggedIn
                                    ]

                        _ ->
                            []
            in
            ( { model | createSession = createSession }
            , Cmd.none
            , appMsgs
            )

        ResetCreateSessionForm ->
            let
                -- We'll default the dates to start today and finish three days later
                initialDates =
                    { start = date
                    , end = addDays 3 date
                    }

                createSession =
                    model.createSession
                        |> Maybe.map (\_ -> Backend.Session.Form.emptyForm knownClinic initialDates)
            in
            ( { model | createSession = createSession }
            , Cmd.none
            , []
            )

        ShowCreateSessionForm show ->
            let
                -- We'll default the dates to start today and finish three days later
                initialDates =
                    { start = date
                    , end = addDays 3 date
                    }

                -- We reset certain successful request if we're opening or
                -- closing the form.
                resetSuccess =
                    Backend.Model.ResetSuccess
                        |> App.Model.MsgBackend
                        |> App.Model.MsgLoggedIn

                ( newModel, appMsgs ) =
                    case model.createSession of
                        Just form ->
                            if show then
                                ( model, [] )
                            else
                                ( { model | createSession = Nothing }
                                , [ resetSuccess ]
                                )

                        Nothing ->
                            if show then
                                ( { model | createSession = Just <| Backend.Session.Form.emptyForm knownClinic initialDates }
                                , [ resetSuccess ]
                                )
                            else
                                ( model, [] )
            in
            ( newModel
            , Cmd.none
            , appMsgs
            )
