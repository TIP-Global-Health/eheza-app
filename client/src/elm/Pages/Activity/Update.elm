module Pages.Activity.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Participant.Model exposing (ParticipantTypeFilter(..), ParticipantsDict)


update : BackendUrl -> String -> User -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg model =
    case msg of
        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetSelectedParticipant maybeId ->
            ( { model | selectedParticipantId = maybeId }, Cmd.none, Nothing )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab, selectedParticipantId = Nothing }, Cmd.none, Nothing )
