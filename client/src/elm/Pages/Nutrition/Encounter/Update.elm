module Pages.Nutrition.Encounter.Update exposing (update)

import App.Model
import Backend.Model
import Backend.NutritionEncounter.Model
import EverySet
import Gizra.Update exposing (sequenceExtra)
import Pages.Nutrition.Encounter.Model exposing (Model, Msg(..))
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
                |> sequenceExtra update [ SetDialogState Nothing ]

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SkipActivity activity ->
            ( { model | skippedActivities = EverySet.insert activity model.skippedActivities }
            , Cmd.none
            , []
            )
                |> sequenceExtra update [ SetDialogState Nothing ]

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )
