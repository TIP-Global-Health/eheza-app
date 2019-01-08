module Device.Model exposing (Device, DeviceId)


type alias DeviceId =
    Int


{-| Models a device which is permitted to sync with the backend.
-}
type alias Device =
    { id : DeviceId
    , name : String
    , accessToken : String
    }
