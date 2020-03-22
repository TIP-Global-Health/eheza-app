module Pages.NutritionEncounter.Update exposing (update)

import App.Model
import Backend.Model
import Backend.NutritionEncounter.Model
import Pages.NutritionEncounter.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.NutritionEncounter.Model.CloseNutritionEncounter
                    |> Backend.Model.MsgNutritionEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
