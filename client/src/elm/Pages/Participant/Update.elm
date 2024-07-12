module Pages.Participant.Update exposing (updateChild, updateMother)

import Activity.Model exposing (ChildActivity(..), MotherActivity)
import App.Ports exposing (bindDropZone)
import Backend.Measurement.Model exposing (MeasurementData, MotherMeasurements)
import EverySet exposing (EverySet)
import Measurement.Model
import Measurement.Update
import Pages.Participant.Model exposing (ChildUpdateReturns, Model, MotherUpdateReturns, Msg(..), Tab(..))


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
    Msg ChildActivity Measurement.Model.MsgChild
    -> Model ChildActivity
    -> Measurement.Model.ModelChild
    -> ChildUpdateReturns
updateChild msg model childForm =
    case msg of
        MsgMeasurement subMsg ->
            let
                ( subModel, subCmd, outMsg ) =
                    Measurement.Update.updateChild subMsg childForm
            in
            ChildUpdateReturns
                model
                (Cmd.map MsgMeasurement subCmd)
                subModel
                outMsg
                Nothing

        Redirect page ->
            ChildUpdateReturns model Cmd.none childForm Nothing (Just page)

        SetSelectedActivity val ->
            let
                cmd =
                    case val of
                        -- When switching to ChildPicture activity, bind
                        -- DropZone to be able to take pictures.
                        ChildPicture ->
                            bindDropZone ()

                        _ ->
                            Cmd.none
            in
            ChildUpdateReturns
                { model | selectedActivity = Just val, dialogState = Nothing }
                cmd
                childForm
                Nothing
                Nothing

        SetSelectedTab tab ->
            let
                cmd =
                    case tab of
                        -- When moving to tab that may view photo as first activity,
                        -- bind DropZone to be able to take pictures.
                        Completed ->
                            bindDropZone ()

                        Pending ->
                            bindDropZone ()

                        ProgressReport ->
                            Cmd.none
            in
            ChildUpdateReturns
                { model
                    | selectedTab = tab
                    , selectedActivity = Nothing
                }
                cmd
                childForm
                Nothing
                Nothing

        SkipActivity activity ->
            ChildUpdateReturns
                { model | skippedActivities = EverySet.insert activity model.skippedActivities, dialogState = Nothing }
                Cmd.none
                childForm
                Nothing
                Nothing

        SetDialogState state ->
            ChildUpdateReturns
                { model | dialogState = state }
                Cmd.none
                childForm
                Nothing
                Nothing


{-| See comments on `updateChild` ... this has a similar structure.
-}
updateMother :
    Msg MotherActivity Measurement.Model.MsgMother
    -> Model MotherActivity
    -> Measurement.Model.ModelMother
    -> MeasurementData MotherMeasurements
    -> MotherUpdateReturns
updateMother msg model motherForm measurements =
    case msg of
        MsgMeasurement subMsg ->
            let
                ( subModel, subCmd, outMsg ) =
                    Measurement.Update.updateMother measurements subMsg motherForm
            in
            MotherUpdateReturns
                model
                (Cmd.map MsgMeasurement subCmd)
                subModel
                outMsg
                Nothing

        Redirect page ->
            MotherUpdateReturns model Cmd.none motherForm Nothing (Just page)

        SetSelectedActivity val ->
            MotherUpdateReturns
                { model | selectedActivity = Just val }
                Cmd.none
                motherForm
                Nothing
                Nothing

        SetSelectedTab tab ->
            MotherUpdateReturns
                { model
                    | selectedTab = tab
                    , selectedActivity = Nothing
                }
                Cmd.none
                motherForm
                Nothing
                Nothing

        SkipActivity activity ->
            MotherUpdateReturns
                { model | skippedActivities = EverySet.insert activity model.skippedActivities, dialogState = Nothing }
                Cmd.none
                motherForm
                Nothing
                Nothing

        SetDialogState state ->
            MotherUpdateReturns
                { model | dialogState = state }
                Cmd.none
                motherForm
                Nothing
                Nothing
