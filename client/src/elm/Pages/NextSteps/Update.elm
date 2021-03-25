module Pages.NextSteps.Update exposing (update)

import Backend.Entities exposing (PersonId)
import Pages.NextSteps.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Session.Model


{-| The extra return parameter indicates our desire to change the `activePage`.
-}
update : PersonId -> Msg -> Model -> ( Model, Cmd Msg, List Pages.Session.Model.Msg )
update childId msg model =
    case msg of
        SetRedirectPage page ->
            ( model
            , Cmd.none
            , [ Pages.Session.Model.SetActivePage page ]
            )
