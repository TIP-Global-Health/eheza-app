module Pages.FamilyNutrition.ProgressReport.Update exposing (update)

import App.Model
import Pages.FamilyNutrition.ProgressReport.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetSelectedFamilyMember familyMember ->
            ( { model | selectedFamilyMember = familyMember }, Cmd.none, [] )
