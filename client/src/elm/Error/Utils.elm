module Error.Utils exposing
    ( decodeError
    , httpError
    , maybeHttpError
    , noError
    , plainError
    )

import Error.Model exposing (Error, ErrorType(..))
import Http
import Json.Decode
import RemoteData exposing (RemoteData(..), WebData)



{- Helper functions for returning a `Maybe Error` from an update function. -}


{-| Return an error or Nothing, if there was no error.
-}
maybeHttpError : WebData a -> String -> String -> Maybe Error
maybeHttpError webdata module_ location =
    case webdata of
        Failure error ->
            httpError module_ location error

        _ ->
            noError


noError : Maybe Error
noError =
    Nothing


httpError : String -> String -> Http.Error -> Maybe Error
httpError module_ location error =
    Just <| Error module_ location (Http error)


plainError : String -> String -> String -> Maybe Error
plainError module_ location error =
    Just <| Error module_ location (Plain error)


decodeError : String -> String -> Json.Decode.Error -> Maybe Error
decodeError module_ location error =
    Just <| Error module_ location (Decoder error)
