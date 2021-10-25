module App.Utils exposing (..)

import App.Model exposing (..)
import Backend.Entities exposing (HealthCenterId)
import Error.Model exposing (Error)
import Maybe.Extra exposing (unwrap)
import RemoteData
import Task


{-| Returns the logged in model and selected health center, if we're logged in.
-}
getLoggedInData : Model -> Maybe ( HealthCenterId, LoggedInModel )
getLoggedInData model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.loggedIn >> RemoteData.toMaybe)
        |> Maybe.map2 (\healthCenterId loggedIn -> ( healthCenterId, loggedIn )) model.healthCenterId


{-| If there was an error, add it to the top of the list,
and send to console.
-}
handleErrors : Maybe Error -> Model -> Model
handleErrors maybeError model =
    let
        errors =
            unwrap model.errors
                (\error ->
                    error
                        :: model.errors
                        -- Make sure list doesn't grow too much.
                        |> List.take 50
                )
                maybeError
    in
    { model | errors = errors }


{-| Helper function to call a Page, and wire Error handling into it.
-}
updateSubModel :
    subMsg
    -> subModel
    -> (subMsg -> subModel -> SubModelReturn subModel subMsg)
    -> (subModel -> Model -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( Model, Cmd Msg )
updateSubModel subMsg subModel updateFunc modelUpdateFunc msg model =
    let
        return =
            updateFunc subMsg subModel

        appCmds =
            if List.isEmpty return.appMsgs then
                Cmd.none

            else
                return.appMsgs
                    |> List.map
                        (\msg_ ->
                            Task.succeed msg_
                                |> Task.perform identity
                        )
                    |> Cmd.batch

        modelUpdatedWithError =
            handleErrors return.error model
    in
    ( modelUpdateFunc return.model modelUpdatedWithError
    , Cmd.batch
        [ Cmd.map msg return.cmd
        , appCmds
        ]
    )


{-| Like `Update.Extra.sequence`, but for `update` signatures that returns SubModelReturn.
Essentially, this allows to recursively apply a whole sequence of messages, collecting their results.
Note that the Error part of PagesReturn will contain first error that was encounterd.
-}
sequenceSubModelReturn :
    (msg -> model -> SubModelReturn model msg)
    -> List msg
    -> SubModelReturn model msg
    -> SubModelReturn model msg
sequenceSubModelReturn updater msgs startingPoint =
    List.foldl
        (\eachMsg subModelReturnSoFar ->
            let
                newPagesReturn =
                    updater eachMsg subModelReturnSoFar.model
            in
            SubModelReturn
                newPagesReturn.model
                (Cmd.batch [ subModelReturnSoFar.cmd, newPagesReturn.cmd ])
                (Maybe.Extra.or subModelReturnSoFar.error newPagesReturn.error)
                (subModelReturnSoFar.appMsgs ++ newPagesReturn.appMsgs)
        )
        startingPoint
        msgs


focusOnCalendarMsg : Msg
focusOnCalendarMsg =
    App.Model.ScrollToElement "dropdown--content-container"
