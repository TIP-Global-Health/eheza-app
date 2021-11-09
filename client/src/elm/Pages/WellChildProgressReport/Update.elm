module Pages.WellChildProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.WellChildEncounter.Model
import Pages.Page exposing (Page(..))
import Pages.WellChildProgressReport.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.WellChildEncounter.Model.CloseWellChildEncounter
                    |> Backend.Model.MsgWellChildEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )
