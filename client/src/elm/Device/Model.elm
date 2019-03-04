module Device.Model exposing (Device)

{-| Models a device which is permitted to sync with the backend.
-}


type alias Device =
    { accessToken : String
    , refreshToken : String
    , backendUrl : String
    }
