module Pages.WellChildEncounter.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildEncounter.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.WellChildEncounter.Model.CloseWellChildEncounter
                    |> Backend.Model.MsgWellChildEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        NavigateToAcuteIllnessParticipantPage childId encounterId ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (AcuteIllnessParticipantPage childId))
                    , SetWarningPopupState Nothing
                    , CloseEncounter encounterId
                    ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra update extraMsgs

        NavigateToActivity encounterId activity ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (WellChildActivityPage encounterId activity))
                    ]

                appMsgs =
                    if activity == WellChildNextSteps then
                        []

                    else
                        []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra update extraMsgs
