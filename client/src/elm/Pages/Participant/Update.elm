module Pages.Participant.Update exposing (updateMother, updateChild)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Measurement.Model
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Page exposing (Page)


updateChild : Msg ChildActivityType Measurement.Model.MsgChild -> Model ChildActivityType -> ( Model ChildActivityType, Cmd (Msg ChildActivityType Measurement.Model.MsgChild), Maybe Page )
updateChild msg model =
    case msg of
        MsgMeasurement subMsg ->
            Debug.crash "implement"

        Redirect page ->
            ( model, Cmd.none, Just page )

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            , Nothing
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            , Nothing
            )


updateMother : Msg MotherActivityType Measurement.Model.MsgMother -> Model MotherActivityType -> ( Model MotherActivityType, Cmd (Msg MotherActivityType Measurement.Model.MsgMother), Maybe Page )
updateMother msg model =
    case msg of
        MsgMeasurement subMsg ->
            Debug.crash "implement"

        Redirect page ->
            ( model, Cmd.none, Just page )

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            , Nothing
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            , Nothing
            )
