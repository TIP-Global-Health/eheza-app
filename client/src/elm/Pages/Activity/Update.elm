module Pages.Activity.Update exposing (update)

import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Page exposing (SessionPage(..))


update : Msg id -> Model id -> ( Model id, Cmd (Msg id), Maybe SessionPage )
update msg model =
    case msg of
        GoBackToActivitiesPage ->
            ( model, Cmd.none, Just ActivitiesPage )

        MsgMeasurementChild childId subMsg ->
            Debug.crash "implement"

        MsgMeasurementMother motherId subMsg ->
            Debug.crash "implement"

        SetSelectedParticipant val ->
            ( { model | selectedParticipant = val }
            , Cmd.none
            , Nothing
            )

        SetSelectedTab val ->
            ( { model | selectedTab = val }
            , Cmd.none
            , Nothing
            )
