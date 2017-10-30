module Pages.Activities.Update exposing (update)

import Pages.Page exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Activities.Model exposing (Model, Msg(..))
import Participant.Model exposing (ParticipantTypeFilter(..))
import Backend.Session.Model exposing (OfflineSession)


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update msg model =
    case msg of
        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )

        SetParticipantTypeFilter participantTypeFilter ->
            ( { model | participantTypeFilter = participantTypeFilter }
            , Cmd.none
            , Nothing
            )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )
