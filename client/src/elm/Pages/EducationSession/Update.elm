module Pages.EducationSession.Update exposing (update)

import App.Model
import Backend.EducationSession.Model
import Backend.Model
import EverySet exposing (EverySet)
import Gizra.Update exposing (sequenceExtra)
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

        SetEducationTopic currentTopics topic ->
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
