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
            ( { model | mode = mode, input = "", search = Nothing }
            , Cmd.none
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
            )

        SetInput input ->
            case model.mode of
                ModeSearchByName ->
                    ( { model | input = input }
                    , Cmd.none
                    )
                        |> sequence update [ MsgDebouncer <| provideInput <| SetSearch input ]

                ModeSearchByNationalId ->
                    let
                        asNumber =
                            String.toInt input
                    in
                    if isJust asNumber then
                        ( { model | input = input }
                        , Cmd.none
                        )
                            |> sequence update
                                [ MsgDebouncer <| provideInput <| SetSearch input ]

                    else
                        ( model, Cmd.none )
