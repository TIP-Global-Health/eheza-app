module Pages.EducationSession.Update exposing (update)

import App.Model
import Backend.EducationSession.Model
import Backend.Entities exposing (..)
import Backend.Model
import Debouncer.Basic as Debouncer exposing (provideInput)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra
import Pages.EducationSession.Model exposing (..)
import Pages.Page exposing (Page(..))


update : NominalDate -> EducationSessionId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SetActivePage page ->
            ( { model | viewMode = Nothing }
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
                |> sequenceExtra (update currentDate id) [ SetViewMode <| ModeTopics topicsUpdated ]

        SaveTopics participants topics ->
            ( model
            , Cmd.none
            , [ Backend.EducationSession.Model.Update (\session -> { session | topics = topics })
                    |> Backend.Model.MsgEducationSession id
                    |> App.Model.MsgIndexedDb
              ]
            )
                |> sequenceExtra (update currentDate id) [ SetViewMode <| ModeAttendance participants ]

        MsgDebouncer subMsg ->
            let
                ( subModel, subCmd, extraMsg ) =
                    Debouncer.update subMsg model.debouncer
            in
            ( { model | debouncer = subModel }
            , Cmd.map MsgDebouncer subCmd
            , []
            )
                |> sequenceExtra (update currentDate id) (Maybe.Extra.toList extraMsg)

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
                |> sequenceExtra (update currentDate id) [ MsgDebouncer <| provideInput <| SetSearch input ]

        ToggleAttendance currentParticipants participant ->
            let
                ( participantsUpdated, fetchMsg ) =
                    if EverySet.member participant currentParticipants then
                        ( EverySet.remove participant currentParticipants, [] )

                    else
                        ( EverySet.insert participant currentParticipants
                        , [ Backend.Model.FetchPerson participant
                                |> App.Model.MsgIndexedDb
                          ]
                        )
            in
            ( model
            , Cmd.none
            , fetchMsg
            )
                |> sequenceExtra (update currentDate id)
                    [ SetViewMode <| ModeAttendance participantsUpdated
                    , SaveAttendance participantsUpdated
                    ]

        SaveAttendance participants ->
            ( model
            , Cmd.none
            , [ Backend.EducationSession.Model.Update (\session -> { session | participants = participants })
                    |> Backend.Model.MsgEducationSession id
                    |> App.Model.MsgIndexedDb
              ]
            )

        EndEncounter ->
            ( model
            , Cmd.none
            , [ Backend.EducationSession.Model.Update (\session -> { session | endDate = Just currentDate })
                    |> Backend.Model.MsgEducationSession id
                    |> App.Model.MsgIndexedDb
              ]
            )
                |> sequenceExtra (update currentDate id) [ SetActivePage PinCodePage ]
