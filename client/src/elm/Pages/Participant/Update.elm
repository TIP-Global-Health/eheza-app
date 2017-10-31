module Pages.Participant.Update exposing (updateMother, updateChild)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import EveryDict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isJust)
import Measurement.Model
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..))
import Participant.Utils
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)


updateChild : Msg ChildActivityType Measurement.Model.MsgChild -> Model ChildActivityType -> ( Model ChildActivityType, Cmd (Msg ChildActivityType Measurement.Model.MsgChild) )
updateChild msg model =
    case msg of
        MsgMeasurement subMsg ->
            Debug.crash "implement"

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            )


updateMother : Msg MotherActivityType Measurement.Model.MsgMother -> Model MotherActivityType -> ( Model MotherActivityType, Cmd (Msg MotherActivityType Measurement.Model.MsgMother) )
updateMother msg model =
    case msg of
        MsgMeasurement subMsg ->
            Debug.crash "implement"

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            )
