port module App.Ports exposing (bindDropZone, bindSignaturePad, cacheHealthCenter, cachePinCode, cacheVillage, clearSignaturePad, coordinates, getCoordinates, initRollbar, logByRollbar, makeProgressReportScreenshot, memoryQuota, persistentStorage, scrollToElement, setLanguage, storageQuota, storeSignature)

import App.Model exposing (GPSCoordinates, MemoryQuota, StorageQuota)


{-| Saves PIN code entered by user, so that we can use it again if
the browser is reloaded.
-}
port cachePinCode : String -> Cmd msg


{-| Set the user's current language.
-}
port setLanguage : String -> Cmd msg


{-| Requests GPS coordinates.
-}
port getCoordinates : () -> Cmd msg


{-| Let the Javascript tell us if we've successfully requested persistent
storage.
-}
port persistentStorage : (Bool -> msg) -> Sub msg


{-| Let the Javascript tell us about memory quotas.
-}
port memoryQuota : (MemoryQuota -> msg) -> Sub msg


{-| Let the Javascript tell us about our storage quota.
-}
port storageQuota : (StorageQuota -> msg) -> Sub msg


{-| Let the Javascript tell us about our GPS coordinates.
-}
port coordinates : (GPSCoordinates -> msg) -> Sub msg


{-| Saves Health center ID selected by user, so that we can use it again if
the browser is reloaded.
-}
port cacheHealthCenter : String -> Cmd msg


port cacheVillage : String -> Cmd msg


port bindDropZone : () -> Cmd msg


port bindSignaturePad : () -> Cmd msg


port clearSignaturePad : () -> Cmd msg


port storeSignature : () -> Cmd msg


port scrollToElement : String -> Cmd msg


port makeProgressReportScreenshot : { language : String, reportType : String, personId : String, phoneNumber : String } -> Cmd msg


port initRollbar : { device : String, token : String } -> Cmd msg


port logByRollbar : { source : String, message : String, md5 : String } -> Cmd msg
