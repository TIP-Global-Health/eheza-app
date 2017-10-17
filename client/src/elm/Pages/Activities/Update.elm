module Pages.Activities.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import User.Model exposing (..)
import Pages.Activities.Model exposing (Model, Msg(..))
import Participant.Model exposing (ParticipantTypeFilter(..))
import Backend.Session.Model exposing (OfflineSession)


update : BackendUrl -> String -> User -> Msg -> OfflineSession -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg session model =
    case msg of
        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, Nothing )

        SetParticipantTypeFilter participantTypeFilterString ->
            let
                participantTypeFilter =
                    if participantTypeFilterString == "All" then
                        All
                    else if participantTypeFilterString == "Children" then
                        Children
                    else if participantTypeFilterString == "Mothers" then
                        Mothers
                    else
                        model.participantTypeFilter
            in
                ( { model | participantTypeFilter = participantTypeFilter }
                , Cmd.none
                , Nothing
                )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )
