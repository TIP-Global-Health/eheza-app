module Pages.Participant.Update exposing (updateMother, updateChild)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Measurement.Model
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Page exposing (Page)


{-| This is a bit of a variation on the usual `update` function.

The first two parameters are our own `Msg` and `Model` types ... that is, the ones we own
and are responsible for.

We also take (and return a possibly-modified) child measurements form, but
we're just the middle-man ... we don't own it, or use it ... we just pass it
on. (The reason for this structure is that we want the measurement form to show
up in two different contexts -- that is, with two different middle-men. So,
`Pages.Activity.Update` has a similar structure).

We also return a possible `OutMsgChild` for which, again, we are just the middle-man ..
someone else will know what to do with it.

And, we return a possible redirect.

TODO: Perhaps instead of returning several extra parameters, it would be better to
construct a list of messages for the caller to handle?

-}
updateChild :
    Msg ChildActivityType Measurement.Model.MsgChild
    -> Model ChildActivityType
    -> Measurement.Model.ModelChild
    -> ( Model ChildActivityType, Cmd (Msg ChildActivityType Measurement.Model.MsgChild), Measurement.Model.ModelChild, Maybe Measurement.Model.OutMsgChild, Maybe Page )
updateChild msg model childForm =
    case msg of
        MsgMeasurement subMsg ->
            let
                ( subModel, subCmd, outMsg ) =
                    Measurement.Update.updateChild subMsg childForm
            in
                ( model
                , Cmd.map MsgMeasurement subCmd
                , subModel
                , outMsg
                , Nothing
                )

        Redirect page ->
            ( model, Cmd.none, childForm, Nothing, Just page )

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            , childForm
            , Nothing
            , Nothing
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            , childForm
            , Nothing
            , Nothing
            )


{-| See comments on `updateChild` ... this has a similar structure.
-}
updateMother :
    Msg MotherActivityType Measurement.Model.MsgMother
    -> Model MotherActivityType
    -> Measurement.Model.ModelMother
    -> ( Model MotherActivityType, Cmd (Msg MotherActivityType Measurement.Model.MsgMother), Measurement.Model.ModelMother, Maybe Measurement.Model.OutMsgMother, Maybe Page )
updateMother msg model motherForm =
    case msg of
        MsgMeasurement subMsg ->
            let
                ( subModel, subCmd, outMsg ) =
                    Measurement.Update.updateMother subMsg motherForm
            in
                ( model
                , Cmd.map MsgMeasurement subCmd
                , subModel
                , outMsg
                , Nothing
                )

        Redirect page ->
            ( model, Cmd.none, motherForm, Nothing, Just page )

        SetSelectedActivity val ->
            ( { model | selectedActivity = Just val }
            , Cmd.none
            , motherForm
            , Nothing
            , Nothing
            )

        SetSelectedTab tab ->
            ( { model
                | selectedTab = tab
                , selectedActivity = Nothing
              }
            , Cmd.none
            , motherForm
            , Nothing
            , Nothing
            )
