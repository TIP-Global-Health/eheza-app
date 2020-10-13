module Pages.IndividualEncounterParticipants.Update exposing (update)

import App.Model
import Debouncer.Basic as Debouncer exposing (provideInput)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra
import Pages.IndividualEncounterParticipants.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        MsgDebouncer subMsg ->
            let
                ( subModel, subCmd, extraMsg ) =
                    Debouncer.update subMsg model.debouncer
            in
            ( { model | debouncer = subModel }
            , Cmd.map MsgDebouncer subCmd
            , []
            )
                |> sequenceExtra update (Maybe.Extra.toList extraMsg)

        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSearch search ->
            let
                trimmed =
                    String.trim search

                maybeSearch =
                    if String.isEmpty trimmed then
                        Nothing

                    else
                        Just trimmed
            in
            ( { model | search = maybeSearch }
            , Cmd.none
            , []
            )

        SetInput input ->
            let
                _ =
                    Debug.log "input" input
            in
            ( { model | input = input }
            , Cmd.none
            , []
            )
                |> sequenceExtra update
                    [ MsgDebouncer <| provideInput <| SetSearch input ]
