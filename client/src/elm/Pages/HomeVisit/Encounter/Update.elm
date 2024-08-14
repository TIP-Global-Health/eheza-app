module Pages.HomeVisit.Encounter.Update exposing (update)

import App.Model
import Backend.HomeVisitEncounter.Model
import Backend.Model
import Pages.HomeVisit.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.HomeVisitEncounter.Model.CloseHomeVisitEncounter
                    |> Backend.Model.MsgHomeVisitEncounter id
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
