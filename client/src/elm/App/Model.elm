module App.Model exposing (ConfiguredModel, Flags, LoggedInModel, MemoryQuota, Model, Msg(..), MsgLoggedIn(..), StorageQuota, SubModelReturn, Version, emptyLoggedInModel, emptyModel)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity)
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity)
import Backend.Model
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionActivity.Model exposing (NutritionActivity)
import Backend.PrenatalActivity.Model exposing (PrenatalActivity)
import Backend.WellChildActivity.Model exposing (WellChildActivity)
import Browser
import Browser.Navigation as Nav
import Config.Model
import Device.Model exposing (Device)
import Error.Model exposing (Error)
import Http
import Json.Encode exposing (Value)
import List.Zipper as Zipper
import Pages.AcuteIllnessActivity.Model
import Pages.AcuteIllnessEncounter.Model
import Pages.AcuteIllnessOutcome.Model
import Pages.AcuteIllnessParticipant.Model
import Pages.AcuteIllnessProgressReport.Model
import Pages.Clinics.Model
import Pages.Dashboard.Model
import Pages.Device.Model
import Pages.GlobalCaseManagement.Model
import Pages.HomeVisitActivity.Model
import Pages.HomeVisitEncounter.Model
import Pages.IndividualEncounterParticipants.Model
import Pages.NutritionActivity.Model
import Pages.NutritionEncounter.Model
import Pages.NutritionProgressReport.Model
import Pages.Page exposing (DashboardPage(..), Page(..))
import Pages.People.Model
import Pages.Person.Model
import Pages.PinCode.Model
import Pages.PregnancyOutcome.Model
import Pages.PrenatalActivity.Model
import Pages.PrenatalEncounter.Model
import Pages.PrenatalParticipant.Model
import Pages.Relationship.Model
import Pages.Session.Model
import Pages.TraceContact.Model
import Pages.WellChildActivity.Model
import Pages.WellChildEncounter.Model
import Pages.WellChildProgressReport.Model
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import ServiceWorker.Model
import SyncManager.Model
import SyncManager.Utils
import Time
import Translate.Model exposing (Language(..))
import Url exposing (Url)
import Uuid exposing (Uuid)
import ZScore.Model


{-| We're now doing our model in layers, corresponding to the logic
of the startup process.

The first thing we need is a configuration, but there are a few things that
make sense to have even without a configuration. So, they are here also.

We have the `activePage` here because it really models what the user **wants**
to be seeing, and we may need to remember that whether or not we're configured
yet.

`language` is here because we always need some kind of language, if just a
default.

-}
type alias Model =
    { activePage : Page
    , navigationKey : Nav.Key
    , url : Url

    -- The DB version on the backend.
    -- This must be sent whenever we POST or PATCH an entity to the backend.
    , dbVersion : Int

    -- Access to things stored in IndexedDB. Eventually, most of this probably
    -- ought to be in LoggedInModel instead, but it's not urgent.
    , indexedDb : Backend.Model.ModelIndexedDb

    -- Have we successfully asked the browser to make our storage persistent?
    -- (This means the browser won't automatically delete our storage when
    -- it thinks space is low). It is a Maybe because in our initial state we
    -- don't know if it is true or false.
    , persistentStorage : Maybe Bool

    -- How close are we to our storage quota?
    , storageQuota : Maybe StorageQuota
    , memoryQuota : Maybe MemoryQuota
    , configuration : RemoteData String ConfiguredModel
    , currentTime : Time.Posix
    , language : Language
    , serviceWorker : ServiceWorker.Model.Model
    , zscores : ZScore.Model.Model

    -- What data did we want last time we checked? We track this so we can
    -- forget data we don't want any longer. Using an Dict relies on the
    -- relevant `Msg` values behaving well for `Debug.toString`, which should
    -- typically be fine. The time reflects the last time the data was wanted,
    -- permitting us to keep recently wanted data around for a little while
    -- after it is not wanted. (Often, it may be wanted again soon).
    , dataWanted : Dict Msg Time.Posix

    -- Should we check what data is needed? We set this at the end of every
    -- update, and clear it when we do the checking. Our subscriptions turn on
    -- animation frame events when this is on. So, as long as we keep getting
    -- updates, we'll keep checking at animation frame intervals.
    , scheduleDataWantedCheck : Bool

    -- Which health center a nurse is working at.
    , healthCenterId : Maybe HealthCenterId

    -- Which village center a nurse is working at.
    , villageId : Maybe VillageId

    -- This is outside of ModelIndexedDb, as it's a related system, which other
    -- pages/ backends shouldn't look into. It's data being synced (download or
    -- uploaded), and if some code needs it, they should access it via `ModelIndexedDb`.
    , syncManager : SyncManager.Model.Model

    -- List of errors we'll send to console.log
    , errors : List Error
    }


emptyModel : Nav.Key -> Url -> Flags -> Model
emptyModel key url flags =
    let
        healthCenterId =
            if String.isEmpty flags.healthCenterId then
                Nothing

            else
                Just (toEntityUuid flags.healthCenterId)

        villageId =
            if String.isEmpty flags.villageId then
                Nothing

            else
                Just (toEntityUuid flags.villageId)

        syncInfoAuthorities =
            flags.syncInfoAuthorities
                |> List.map SyncManager.Utils.syncInfoAuthorityFromPort
                |> Zipper.fromList

        syncManagerFlags =
            { syncInfoGeneral = SyncManager.Utils.syncInfoGeneralFromPort flags.syncInfoGeneral
            , syncInfoAuthorities = syncInfoAuthorities
            , batchSize = flags.photoDownloadBatchSize
            , syncSpeed = flags.syncSpeed
            }
    in
    { activePage = PinCodePage
    , navigationKey = key
    , url = url
    , dbVersion = flags.dbVersion
    , configuration = NotAsked
    , currentTime = Time.millisToPosix 0
    , dataWanted = Dict.empty
    , indexedDb = Backend.Model.emptyModelIndexedDb
    , language = English
    , memoryQuota = Nothing
    , persistentStorage = Nothing
    , scheduleDataWantedCheck = True
    , serviceWorker = ServiceWorker.Model.emptyModel flags.activeServiceWorker
    , storageQuota = Nothing
    , zscores = ZScore.Model.emptyModel
    , healthCenterId = healthCenterId
    , villageId = villageId
    , syncManager = SyncManager.Model.emptyModel syncManagerFlags
    , errors = []
    }


type alias StorageQuota =
    { quota : Int
    , usage : Int
    }


type alias MemoryQuota =
    { totalJSHeapSize : Int
    , usedJSHeapSize : Int
    , jsHeapSizeLimit : Int
    }


{-| Represents the version of the app. Currently, we just track the git
revision of the build. We could eventually also track a tag etc.

This is actually found in Version.version, which is a file generated
by gulp ... at src/generated/Version.elm

-}
type alias Version =
    { build : String
    }


{-| Things which depend on having a configuration.
-}
type alias ConfiguredModel =
    { config : Config.Model.Model

    -- `device` tracks the attempt to pair our device with the
    -- backend. `devicePage` handles the UI for that.
    , device : WebData Device
    , devicePage : Pages.Device.Model.Model

    -- The RemoteData tracks attempts to log in with a PIN code. The
    -- LoggedInModel tracks data which we only have if we are logged in.
    , loggedIn : WebData LoggedInModel
    , pinCodePage : Pages.PinCode.Model.Model
    }


{-| So, this is all the stuff we'll have only if we're logged in.

Part of what's nice about this is that if a function asks for this type, then
it definitely can't be called unless we're logged in ... we don't have to
do access control for that function separately. Or, to put it another way,
we've baked the access control into the types, so we're forced to deal with
it at the appropriate moment.

-}
type alias LoggedInModel =
    { createPersonPage : Pages.Person.Model.Model
    , dashboardPage : Pages.Dashboard.Model.Model
    , globalCaseManagementPage : Pages.GlobalCaseManagement.Model.Model
    , editPersonPages : Dict PersonId Pages.Person.Model.Model
    , relationshipPages : Dict ( PersonId, PersonId ) Pages.Relationship.Model.Model
    , personsPage : Pages.People.Model.Model
    , individualEncounterParticipantsPage : Pages.IndividualEncounterParticipants.Model.Model
    , clinicsPage : Pages.Clinics.Model.Model

    -- The nurse who has logged in.
    , nurse : ( NurseId, Nurse )
    , prenatalParticipantPages : Dict PersonId Pages.PrenatalParticipant.Model.Model
    , prenatalEncounterPages : Dict PrenatalEncounterId Pages.PrenatalEncounter.Model.Model
    , prenatalActivityPages : Dict ( PrenatalEncounterId, PrenatalActivity ) Pages.PrenatalActivity.Model.Model
    , pregnancyOutcomePages : Dict IndividualEncounterParticipantId Pages.PregnancyOutcome.Model.Model
    , sessionPages : Dict SessionId Pages.Session.Model.Model
    , nutritionEncounterPages : Dict NutritionEncounterId Pages.NutritionEncounter.Model.Model
    , nutritionActivityPages : Dict ( NutritionEncounterId, NutritionActivity ) Pages.NutritionActivity.Model.Model
    , nutritionProgressReportPages : Dict NutritionEncounterId Pages.NutritionProgressReport.Model.Model
    , acuteIllnessParticipantPages : Dict PersonId Pages.AcuteIllnessParticipant.Model.Model
    , acuteIllnessEncounterPages : Dict AcuteIllnessEncounterId Pages.AcuteIllnessEncounter.Model.Model
    , acuteIllnessActivityPages : Dict ( AcuteIllnessEncounterId, AcuteIllnessActivity ) Pages.AcuteIllnessActivity.Model.Model
    , acuteIllnessProgressReportPages : Dict AcuteIllnessEncounterId Pages.AcuteIllnessProgressReport.Model.Model
    , acuteIllnessOutcomePages : Dict IndividualEncounterParticipantId Pages.AcuteIllnessOutcome.Model.Model
    , homeVisitEncounterPages : Dict HomeVisitEncounterId Pages.HomeVisitEncounter.Model.Model
    , homeVisitActivityPages : Dict ( HomeVisitEncounterId, HomeVisitActivity ) Pages.HomeVisitActivity.Model.Model
    , wellChildEncounterPages : Dict WellChildEncounterId Pages.WellChildEncounter.Model.Model
    , wellChildActivityPages : Dict ( WellChildEncounterId, WellChildActivity ) Pages.WellChildActivity.Model.Model
    , wellChildProgressReportPages : Dict WellChildEncounterId Pages.WellChildProgressReport.Model.Model
    , traceContactPages : Dict AcuteIllnessTraceContactId Pages.TraceContact.Model.Model
    }


emptyLoggedInModel : Maybe VillageId -> ( NurseId, Nurse ) -> LoggedInModel
emptyLoggedInModel villageId nurse =
    { createPersonPage = Pages.Person.Model.emptyCreateModel
    , dashboardPage = Pages.Dashboard.Model.emptyModel villageId
    , globalCaseManagementPage = Pages.GlobalCaseManagement.Model.emptyModel
    , editPersonPages = Dict.empty
    , personsPage = Pages.People.Model.emptyModel
    , individualEncounterParticipantsPage = Pages.IndividualEncounterParticipants.Model.emptyModel
    , clinicsPage = Pages.Clinics.Model.emptyModel
    , relationshipPages = Dict.empty
    , nurse = nurse
    , prenatalParticipantPages = Dict.empty
    , prenatalEncounterPages = Dict.empty
    , prenatalActivityPages = Dict.empty
    , pregnancyOutcomePages = Dict.empty
    , sessionPages = Dict.empty
    , nutritionEncounterPages = Dict.empty
    , nutritionActivityPages = Dict.empty
    , nutritionProgressReportPages = Dict.empty
    , acuteIllnessParticipantPages = Dict.empty
    , acuteIllnessEncounterPages = Dict.empty
    , acuteIllnessActivityPages = Dict.empty
    , acuteIllnessProgressReportPages = Dict.empty
    , acuteIllnessOutcomePages = Dict.empty
    , homeVisitEncounterPages = Dict.empty
    , homeVisitActivityPages = Dict.empty
    , wellChildEncounterPages = Dict.empty
    , wellChildActivityPages = Dict.empty
    , wellChildProgressReportPages = Dict.empty
    , traceContactPages = Dict.empty
    }


type Msg
    = -- Manage data we get from IndexedDb, and communication with the service
      -- worker
      MsgIndexedDb Backend.Model.MsgIndexedDb
    | MsgServiceWorker ServiceWorker.Model.Msg
    | MsgSyncManager SyncManager.Model.Msg
      -- Messages that require login, or manage the login process
    | MsgLoggedIn MsgLoggedIn
    | MsgPagePinCode Pages.PinCode.Model.Msg
    | TryPinCode String
    | SetLoggedIn (WebData ( NurseId, Nurse ))
      -- Manage device pairing
    | MsgPageDevice Pages.Device.Model.Msg
    | TryPairingCode String
    | HandlePairedDevice (WebData Device)
      -- Manage ZScore data
    | MsgZScore ZScore.Model.Msg
      -- Manage our own model
    | ScrollToElement String
    | SetActivePage Page
    | SetLanguage Language
    | SetPersistentStorage Bool
    | SetStorageQuota StorageQuota
    | SetMemoryQuota MemoryQuota
    | SetHealthCenter (Maybe HealthCenterId)
    | SetVillage (Maybe VillageId)
    | Tick Time.Posix
    | CheckDataWanted
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


{-| Messages we can only handle if we're logged in.
-}
type MsgLoggedIn
    = MsgPageClinics Pages.Clinics.Model.Msg
    | MsgPageCreatePerson Pages.Person.Model.Msg
    | MsgPageDashboard DashboardPage Pages.Dashboard.Model.Msg
    | MsgPageGlobalCaseManagement Pages.GlobalCaseManagement.Model.Msg
    | MsgPageEditPerson PersonId Pages.Person.Model.Msg
    | MsgPagePersons Pages.People.Model.Msg
    | MsgPagePrenatalParticipant PersonId Pages.PrenatalParticipant.Model.Msg
    | MsgPageIndividualEncounterParticipants Pages.IndividualEncounterParticipants.Model.Msg
    | MsgPageRelationship PersonId PersonId Pages.Relationship.Model.Msg
    | MsgPageAcuteIllnessParticipant PersonId Pages.AcuteIllnessParticipant.Model.Msg
    | MsgPageSession SessionId Pages.Session.Model.Msg
    | MsgPagePrenatalEncounter PrenatalEncounterId Pages.PrenatalEncounter.Model.Msg
    | MsgPageNutritionEncounter NutritionEncounterId Pages.NutritionEncounter.Model.Msg
    | MsgPageAcuteIllnessEncounter AcuteIllnessEncounterId Pages.AcuteIllnessEncounter.Model.Msg
    | MsgPageHomeVisitEncounter HomeVisitEncounterId Pages.HomeVisitEncounter.Model.Msg
    | MsgPageWellChildEncounter WellChildEncounterId Pages.WellChildEncounter.Model.Msg
    | MsgPagePrenatalActivity PrenatalEncounterId PrenatalActivity Pages.PrenatalActivity.Model.Msg
    | MsgPageNutritionActivity NutritionEncounterId NutritionActivity Pages.NutritionActivity.Model.Msg
    | MsgPageAcuteIllnessActivity AcuteIllnessEncounterId AcuteIllnessActivity Pages.AcuteIllnessActivity.Model.Msg
    | MsgPageHomeVisitActivity HomeVisitEncounterId HomeVisitActivity Pages.HomeVisitActivity.Model.Msg
    | MsgPageWellChildActivity WellChildEncounterId WellChildActivity Pages.WellChildActivity.Model.Msg
    | MsgPagePregnancyOutcome IndividualEncounterParticipantId Pages.PregnancyOutcome.Model.Msg
    | MsgPageAcuteIllnessProgressReport AcuteIllnessEncounterId Pages.AcuteIllnessProgressReport.Model.Msg
    | MsgPageNutritionProgressReport NutritionEncounterId Pages.NutritionProgressReport.Model.Msg
    | MsgPageWellChildProgressReport WellChildEncounterId Pages.WellChildProgressReport.Model.Msg
    | MsgPageAcuteIllnessOutcome IndividualEncounterParticipantId Pages.AcuteIllnessOutcome.Model.Msg
    | MsgPageTraceContact AcuteIllnessTraceContactId Pages.TraceContact.Model.Msg


type alias Flags =
    { activeLanguage : String
    , activeServiceWorker : Bool
    , hostname : String
    , dbVersion : Int
    , pinCode : String
    , healthCenterId : String
    , villageId : String
    , syncInfoGeneral : SyncManager.Model.SyncInfoGeneralForPort
    , syncInfoAuthorities : List SyncManager.Model.SyncInfoAuthorityForPort
    , photoDownloadBatchSize : Int
    , syncSpeed : SyncManager.Model.SyncSpeed
    }


{-| This is what Pages and Backend should return. Currently we're adding it
gradually, as code base was written without it.

Along with the Model and Cmd, we return:

  - error: a Maybe error to indicate for example a Failure in HTTP request.
  - appMsgs - that allow sub models to call MSGs of other submodules.

-}
type alias SubModelReturn subModel subMsg =
    { model : subModel
    , cmd : Cmd subMsg
    , error : Maybe Error
    , appMsgs : List Msg
    }
