module Backend.Utils exposing (updateSubModel)

{-| Wire sub Entities to the Backend.
-}


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
