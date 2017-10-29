port module Pages.Participant.Update exposing (update)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import EveryDict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isJust)
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..))
import Participant.Utils
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgMeasurementChild childId subMsg ->
            Debug.crash "implement"

        MsgMeasurementMother motherId subMsg ->
            Debug.crash "implement"

        SetSelectedActivity val ->
            ( { model | selectedActivity = val }
            , Cmd.none
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            )
