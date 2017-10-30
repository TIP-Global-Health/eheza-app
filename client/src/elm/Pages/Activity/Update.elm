module Pages.Activity.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import EveryDict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isNothing)
import Measurement.Model
import Measurement.Update
import Participant.Utils exposing (getParticipantName)
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant(..), ParticipantId, ParticipantTypeFilter(..))
import Translate as Trans exposing (Language)


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
