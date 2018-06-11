module Pages.Admin.Update exposing (..)

import App.Model
import Backend.Model exposing (ModelBackend)
import Backend.Session.Form exposing (validateSession)
import EveryDictList
import Form
import Gizra.NominalDate exposing (NominalDate)
import Pages.Admin.Model exposing (..)
import RemoteData
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

                    newModel =
                        case model.createSession of
                            Just form ->
                                if show then
                                    model
                                else
                                    { model | createSession = Nothing }

                            Nothing ->
                                if show then
                                    { model
                                        | createSession = Just <| Backend.Session.Form.emptyForm knownClinic initialDates
                                    }
                                else
                                    model
                in
                    ( newModel
                    , Cmd.none
                    , []
                    )
