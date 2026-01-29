module Pages.Prenatal.Participant.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Prenatal.Participant.Model exposing (Model, Msg(..))


update : NominalDate -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate personId msg model =
    case msg of
        MsgBackend msgBackend ->
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb msgBackend ]
            )

        SetActivePage page ->
            ( { model | showWarningPopup = False }
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        CloseWarningPopup ->
            ( { model | showWarningPopup = False }
            , Cmd.none
            , []
            )
