module Components.PatientsSearchForm.Update exposing (update)

import Components.PatientsSearchForm.Model exposing (Model, Msg(..), PatientsSearchFormMode(..))
import Debouncer.Basic as Debouncer exposing (provideInput)
import Maybe.Extra exposing (isJust)
import Update.Extra exposing (sequence)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgDebouncer subMsg ->
            let
                ( subModel, subCmd, extraMsg ) =
                    Debouncer.update subMsg model.debouncer
            in
            ( { model | debouncer = subModel }
            , Cmd.map MsgDebouncer subCmd
            )
                |> sequence update (Maybe.Extra.toList extraMsg)

        SetMode byName ->
            let
                mode =
                    if byName then
                        ModeSearchByName

                    else
                        ModeSearchByNationalId
            in
            if mode == model.mode then
                -- The mode asked for is the one already in use, so what has
                -- been typed stays where it is.
                ( model, Cmd.none )

            else
                ( { model | mode = mode, input = "", search = Nothing }
                , Cmd.none
                )
                    -- The debouncer holds the last input it was given, so a
                    -- search typed a moment ago would otherwise arrive after
                    -- the switch and be run in the mode it was not typed for.
                    |> sequence update [ MsgDebouncer <| provideInput <| SetSearch "" ]

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
            )

        SetInput input ->
            case model.mode of
                ModeSearchByName ->
                    ( { model | input = input }
                    , Cmd.none
                    )
                        |> sequence update [ MsgDebouncer <| provideInput <| SetSearch input ]

                ModeSearchByNationalId ->
                    if String.isEmpty input then
                        -- An emptied field searches for nothing, so the results
                        -- of what it held go with it. The empty input goes to
                        -- the debouncer too: it holds the last input it was
                        -- given, and a number given a moment earlier would
                        -- otherwise arrive half a second later and search again.
                        ( { model | input = "", search = Nothing }
                        , Cmd.none
                        )
                            |> sequence update [ MsgDebouncer <| provideInput <| SetSearch "" ]

                    else if isJust (String.toInt input) then
                        ( { model | input = input }
                        , Cmd.none
                        )
                            |> sequence update
                                [ MsgDebouncer <| provideInput <| SetSearch input ]

                    else
                        ( model, Cmd.none )
