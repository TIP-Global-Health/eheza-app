port module App.Ports exposing
    ( bindDropZone
    , cacheHealthCenter
    , cachePinCode
    , cacheVillage
    , memoryQuota
    , persistentStorage
    , pusherKey
    , scrollToElement
    , setLanguage
    , storageQuota
    , trySyncing
    )

import App.Model exposing (MemoryQuota, StorageQuota)


{-| Saves PIN code entered by user, so that we can use it again if
the browser is reloaded.
-}
port cachePinCode : String -> Cmd msg


{-| Manually kick off a sync event. Normally, handled automatically.
-}
port trySyncing : () -> Cmd msg


{-| Send Pusher key and cluster to JS.
-}
port pusherKey : ( String, String, List String ) -> Cmd msg


{-| Set the user's current language.
-}
port setLanguage : String -> Cmd msg


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


{-| Saves Health center ID selected by user, so that we can use it again if
the browser is reloaded.
-}
port cacheHealthCenter : String -> Cmd msg


port cacheVillage : String -> Cmd msg


port bindDropZone : () -> Cmd msg


port scrollToElement : String -> Cmd msg
