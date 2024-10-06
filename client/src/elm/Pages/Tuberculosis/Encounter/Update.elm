module Pages.Tuberculosis.Encounter.Update exposing (update)

import App.Model
import Backend.Model
import Backend.TuberculosisEncounter.Model
import Pages.Page exposing (Page(..))
import Pages.Tuberculosis.Encounter.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.TuberculosisEncounter.Model.CloseTuberculosisEncounter
                    |> Backend.Model.MsgTuberculosisEncounter id
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
