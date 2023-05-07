module Pages.Scoreboard.Update exposing (update)

import App.Model exposing (PagesReturn)
import Backend.Model exposing (ModelBackend)
import Error.Utils exposing (noError)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (DisplayMode(..), Model, Msg(..), emptyModel, emptyViewSelectionForm)
import Pages.Scoreboard.Utils exposing (..)
import Restful.Endpoint exposing (toEntityId)


update : ModelBackend -> Msg -> Model -> PagesReturn Model Msg
update modelBackend msg model =
    case msg of
        SetGeoLocation updatedFormFunc value ->
            PagesReturn
                { model | form = updatedFormFunc value model.form }
                Cmd.none
                noError
                []

        GenerateReport ->
            let
                updatedModel =
                    Maybe.map2
                        (\province district ->
                            let
                                value =
                                    { province = province
                                    , district = district
                                    , sector = model.form.sector
                                    , cell = model.form.cell
                                    , village = model.form.village
                                    }
                            in
                            { model | displayMode = DisplayResultTable value, form = emptyViewSelectionForm }
                        )
                        model.form.province
                        model.form.district
                        |> Maybe.withDefault model
            in
            PagesReturn
                updatedModel
                Cmd.none
                noError
                []

        ResetSelection ->
            PagesReturn
                emptyModel
                Cmd.none
                noError
                []

        ChaneYearGap step ->
            PagesReturn
                { model | yearSelectorGap = model.yearSelectorGap + step }
                Cmd.none
                noError
                []
