module Device.Model exposing (Device)

{-| Models a device which is permitted to sync with the backend.
-}


type alias Device =
    { accessToken : String
    , refreshToken : String
    , backendUrl : String

    -- The node ID of the device. It's a Maybe value, as older devices might
    -- not have the ID cached in the storage cache.
    , deviceId : Maybe Int
    }
