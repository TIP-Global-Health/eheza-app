module Pages.NCD.RecurrentEncounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (NCDEncounterId)
import Pages.NCD.RecurrentEncounter.Model exposing (Model, Msg(..))


update : NCDEncounterId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update id msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
