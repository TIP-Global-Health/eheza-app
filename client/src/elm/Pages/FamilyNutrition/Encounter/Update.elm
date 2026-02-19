module Pages.FamilyNutrition.Encounter.Update exposing (update)

import App.Model
import Backend.FamilyNutritionEncounter.Model
import Backend.Model
import EverySet
import Gizra.Update exposing (sequenceExtra)
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.FamilyNutritionEncounter.Model.CloseFamilyNutritionEncounter
                    |> Backend.Model.MsgFamilyNutritionEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
                |> sequenceExtra update [ SetDialogState Nothing ]

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )
