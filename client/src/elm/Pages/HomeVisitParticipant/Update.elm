module Pages.HomeVisitParticipant.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Gizra.NominalDate exposing (NominalDate)
import Pages.HomeVisitParticipant.Model exposing (..)


update : NominalDate -> PersonId -> Msg -> ( Cmd Msg, List App.Model.Msg )
update currentDate personId msg =
    case msg of
        SetActivePage page ->
            ( Cmd.none
            , [ App.Model.SetActivePage page ]
            )
