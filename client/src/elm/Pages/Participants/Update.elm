module Pages.Participants.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Pages.Participants.Model exposing (Model, Msg(..))
import Participant.Model exposing (ParticipantTypeFilter(..), ParticipantsDict)
import User.Model exposing (..)


update : BackendUrl -> String -> User -> Msg -> ParticipantsDict -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg participants model =
    case msg of
        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )
