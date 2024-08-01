module Pages.EducationSession.Update exposing (update)

import App.Model
import Backend.EducationSession.Model
import Backend.Entities exposing (..)
import Backend.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
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

        SetFilter filter ->
            ( { model | filter = filter }
            , Cmd.none
            , []
            )

        Reset ->
            ( { model | filter = "", initialResultsDisplay = InitialResultsShown }
            , Cmd.none
            , []
            )

        ToggleInitialResultsDisplay ->
            let
                display =
                    if model.initialResultsDisplay == InitialResultsHidden then
                        InitialResultsShown

                    else
                        InitialResultsHidden
            in
            ( { model | initialResultsDisplay = display }
            , Cmd.none
            , []
            )

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
