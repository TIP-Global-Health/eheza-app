module Pages.Activity.Update exposing (update)

import Pages.Activity.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgMeasurementChild childId subMsg ->
            Debug.crash "implement"

        MsgMeasurementMother motherId subMsg ->
            Debug.crash "implement"

        SetSelectedParticipant val ->
            ( { model | selectedParticipant = val }
            , Cmd.none
            )

        SetSelectedTab val ->
            ( { model | selectedTab = val }
            , Cmd.none
            )
