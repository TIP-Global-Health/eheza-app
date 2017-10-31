module Pages.Participant.Update exposing (updateMother, updateChild)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Measurement.Model
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)


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
