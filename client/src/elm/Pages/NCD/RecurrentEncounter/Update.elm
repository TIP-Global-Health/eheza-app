module Pages.NCD.RecurrentEncounter.Update exposing (update)

import App.Model
import Pages.NCD.RecurrentEncounter.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
