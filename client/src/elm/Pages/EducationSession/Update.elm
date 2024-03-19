module Pages.EducationSession.Update exposing (update)

import App.Model
import Backend.EducationSession.Model
import Backend.Model
import Debouncer.Basic as Debouncer exposing (provideInput)
import EverySet exposing (EverySet)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra
import Pages.EducationSession.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetViewMode mode ->
            ( { model | viewMode = Just mode }
            , Cmd.none
            , []
            )

        ToggleEducationTopic currentTopics topic ->
            let
                topicsUpdated =
                    if EverySet.member topic currentTopics then
                        EverySet.remove topic currentTopics

                    else
                        EverySet.insert topic currentTopics
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra update [ SetViewMode <| ModeTopics topicsUpdated ]

        SaveTopics sessionId session ->
            ( model
            , Cmd.none
            , [ Backend.EducationSession.Model.Update session
                    |> Backend.Model.MsgEducationSession sessionId
                    |> App.Model.MsgIndexedDb
              ]
            )
                |> sequenceExtra update [ SetViewMode <| ModeAttendance session.participants ]

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
            ( { model | input = input }
            , Cmd.none
            , []
            )
                |> sequenceExtra update [ MsgDebouncer <| provideInput <| SetSearch input ]

        ToggleAttendance currentParticipants participant ->
            let
                participantsUpdated =
                    if EverySet.member participant currentParticipants then
                        EverySet.remove participant currentParticipants

                    else
                        EverySet.insert participant currentParticipants
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra update [ SetViewMode <| ModeAttendance participantsUpdated ]
