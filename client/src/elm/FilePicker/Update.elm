port module FilePicker.Update exposing (update)

import Config.Model exposing (BackendUrl)
import FilePicker.Model exposing (..)
import Translate as Trans exposing (translate, Language)


update : BackendUrl -> Language -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl language action model =
    let
        config makeActive =
            { active = makeActive
            , backendUrl = backendUrl
            , defaultMessage = translate language Trans.DropzoneDefaultMessage
            }
    in
        case action of
            Bind ->
                if model.isBound then
                    ( model, Cmd.none )
                else
                    ( { model | isBound = True }, dropzoneConfig <| config True )

            Unbind ->
                if model.isBound then
                    ( { model | isBound = False }, dropzoneConfig <| config False )
                else
                    ( model, Cmd.none )


{-| Asks the Javascript side to bind the dropZone.
-}
port dropzoneConfig : DropzoneConfig -> Cmd msg
