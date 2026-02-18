module Pages.FamilyEncounter.Encounter.Update exposing (update)

import App.Model
import Backend.FamilyEncounter.Model
import Pages.FamilyEncounter.Encounter.Model exposing (..)
import Pages.Page exposing (Page)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter _ ->
            -- TODO: Implement close encounter logic in #1665
            ( model, Cmd.none, [] )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }
            , Cmd.none
            , []
            )

        SkipActivity activity ->
            ( model, Cmd.none, [] )

        SetDialogState state ->
            ( { model | dialogState = state }
            , Cmd.none
            , []
            )
