module App.Utils exposing (updateSubModel)

import App.Model exposing (Model, Msg, PagesReturn)
import Error.Model exposing (Error)
import Maybe.Extra exposing (unwrap)
import Task


{-| If there was an error, add it to the top of the list,
and send to console.
-}
handleErrors : Maybe Error -> Model -> Model
handleErrors maybeError model =
    let
        errors =
            unwrap model.errors
                (\error ->
                    error :: model.errors
                )
                maybeError
    in
    { model | errors = errors }


{-| Helper function to call a Page, and wire Error handling into it.
-}
updateSubModel :
    subMsg
    -> subModel
    -> (subMsg -> subModel -> PagesReturn subModel subMsg)
    -> (subModel -> Model -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( Model, Cmd Msg )
updateSubModel subMsg subModel updateFunc modelUpdateFunc msg model =
    let
        pagesReturn =
            updateFunc subMsg subModel

        appCmds =
            if List.isEmpty pagesReturn.appMsgs then
                Cmd.none

            else
                pagesReturn.appMsgs
                    |> List.map
                        (\msg_ ->
                            Task.succeed msg_
                                |> Task.perform identity
                        )
                    |> Cmd.batch

        modelUpdatedWithError =
            handleErrors pagesReturn.error model
    in
    ( modelUpdateFunc pagesReturn.model modelUpdatedWithError
    , Cmd.batch
        [ Cmd.map msg pagesReturn.cmd
        , appCmds
        ]
    )
