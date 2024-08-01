module GeoLocation.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Restful.Endpoint exposing (EntityId)


{-| This is here to have a partially type-safe key for the Dict.
We're not actually storing these on the backend at the moment, and if
we did, they would need to be an EntityUuid rather than an EntityId.
-}
type alias GeoLocationId =
    EntityId GeoLocationIdType


type GeoLocationIdType
    = GeoLocationIdType


type alias GeoInfo =
    { provinces : Dict GeoLocationId GeoLocation
    , districts : Dict GeoLocationId GeoLocation
    , sectors : Dict GeoLocationId GeoLocation
    , cells : Dict GeoLocationId GeoLocation
    , villages : Dict GeoLocationId GeoLocation
    }


emptyGeoInfo : GeoInfo
emptyGeoInfo =
    { provinces = Dict.empty
    , districts = Dict.empty
    , sectors = Dict.empty
    , cells = Dict.empty
    , villages = Dict.empty
    }


type alias GeoLocation =
    { name : String
    , parent : Maybe GeoLocationId
    }


type alias ParentId =
    GeoLocationId


type alias ReverseGeoInfo =
    Dict (Maybe ParentId) (Dict String ( GeoLocationId, GeoLocation ))
