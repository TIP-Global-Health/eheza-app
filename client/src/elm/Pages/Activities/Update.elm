module Pages.Activities.Update exposing (update)

import Pages.Page exposing (Page(..))
import Pages.Activities.Model exposing (Model, Msg(..))


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
