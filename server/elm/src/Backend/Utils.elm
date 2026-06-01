module Backend.Utils exposing (updateSubModel)

{-| Wire sub Entities to the Backend.
-}


updateSubModel :
    subMsg
    -> (subMsg -> model -> { model : model, cmd : Cmd subMsg, error : error, appMsgs : appMsgs })
    -> (subMsg -> outerMsg)
    -> model
    -> { model : model, cmd : Cmd outerMsg, error : error, appMsgs : appMsgs }
updateSubModel subMsg updateFunc msg model =
    let
        backendReturn =
            updateFunc subMsg model
    in
    { model = backendReturn.model
    , cmd = Cmd.map msg backendReturn.cmd
    , error = backendReturn.error
    , appMsgs = backendReturn.appMsgs
    }
