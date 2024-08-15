module Pages.Completion.Update exposing (update)

import App.Model exposing (PagesReturn)
import App.Ports
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Date exposing (Interval(..), Unit(..))
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra
import Pages.Completion.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Task exposing (Task)


update : NominalDate -> ModelBackend -> Msg -> Model -> PagesReturn Model Msg
update currentDate modelBackend msg model =
    case msg of
        NoOp ->
            PagesReturn
                model
                Cmd.none
                noError
                []
