module Pages.HIV.Encounter.Update exposing (update)

import App.Model
import Backend.HIVEncounter.Model
import Backend.Model
import Pages.HIV.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.HIVEncounter.Model.CloseHIVEncounter
                    |> Backend.Model.MsgHIVEncounter id
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

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )
