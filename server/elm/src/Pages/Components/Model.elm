module Pages.Components.Model exposing (..)

import Utils.GeoLocation exposing (GeoLocationId)


type alias DemographicsSelection =
    { province : Maybe GeoLocationId
    , district : Maybe GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


emptyDemographicsSelection : DemographicsSelection
emptyDemographicsSelection =
    { province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    }
