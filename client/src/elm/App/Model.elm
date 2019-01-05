module App.Model exposing (ConfiguredModel, Flags, LoggedInModel, Model, Msg(..), MsgLoggedIn(..), Version, emptyLoggedInModel, emptyModel)

import Backend.Model
import Config.Model
import Device.Model exposing (Device)
import Dict exposing (Dict)
import Http
import Json.Encode exposing (Value)
import Pages.Admin.Model
import Pages.Login.Model
import Pages.Model
import Pages.Page exposing (Page(LoginPage))
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Login exposing (UserAndData)
import Rollbar
import ServiceWorker.Model
import Time exposing (Time)
import Translate exposing (Language(..))
import User.Model exposing (User)
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

TODO: Should review this layering at some point to see whether it was too much
/ too little.

-}
type alias Model =
    { activePage : Page

    -- We don't need a configuration or to be logged in to access the cache, at
    -- least for the moment.
    , cache : Backend.Model.ModelCached

    -- TODO: This doesn't really belong here ... we shouldn't have this unless
    -- we have a session ... but I've done enough restructuring for now!
    , sessionPages : Pages.Model.SessionPages
    , configuration : RemoteData String ConfiguredModel
    , currentTime : Time
    , language : Language
    , serviceWorker : ServiceWorker.Model.Model
    , offline : Bool
    , zscores : ZScore.Model.Model
    }


{-| Represents the version of the app. Currently, we just track the git
revision of the build. We could eventually also track a tag etc.

This is actually found in Version.version, which is a file generated
by gulp ... at src/generated/Version.elm

-}
type alias Version =
    { build : String
    }


{-| Yay, we're configured!

  - We'll definitely have a config
  - We have a login UI
  - We have the data showing whether we're logged in or not, and, if we're
    logged in, some data that we only keep for people who are logged in.

We could put additional fields here if there is some state that
is common to people who are logged in or logged out.

Note that our `Backend` and `Pages` aren't in here, becuase they are only
for people who are logged in. But, `Pages.Login.Model` is here, of course,
since we need a UI for logging in.

-}
type alias ConfiguredModel =
    { config : Config.Model.Model
    , loginPage : Pages.Login.Model.Model
    , login : UserAndData () User LoggedInModel
    , device : WebData Device
    }


{-| So, this is all the stuff we'll have only if we're logged in.

Part of what's nice about this is that if a function asks for this type, then
it definitely can't be called unless we're logged in ... we don't have to
do access control for that function separately. Or, to put it another way,
we've baked the access control into the types, so we're forced to deal with
it at the appropriate moment.

-}
type alias LoggedInModel =
    { backend : Backend.Model.ModelBackend
    , adminPage : Pages.Admin.Model.Model
    }


emptyLoggedInModel : LoggedInModel
emptyLoggedInModel =
    { backend = Backend.Model.emptyModelBackend
    , adminPage = Pages.Admin.Model.emptyModel
    }


{-| We'll subdivide the `Msg` type to roughly correspond to our `Model`
subdivisions. In principle, that allows us to specify in function signatures
what kind of msg can be generated from certain states. We'll see how helpful
that is.

The three `Login` related messages handle:

  - Messages for the Login UI (MsgPageLogin)
  - Messages for the login process (MsgLogin)
  - Messages we can only handle once we've logged in (MsgLoggedIn)

In this app, there isn't much you can do unless you're logged in, so most of
the action is in `MsgLoggedIn`.

TODO: We remember our login information even if we're offline, by caching it
locally, so you can continue to work offline and be considered logged-in by the
app. However, we'll need to be careful about logout while you're offline, since
there won't be any way to log back in until you're online again.

  - Perhaps we'll have to prohibit logout while offline? Or, while an offline
    session is in progress (since you could then go offline **after** logging
    out)?

  - Or, we could implement a special "partial logout" which forgets the access
    token but still lets you work offline? Then, you could at least continue to
    interact with the offline data, but you'd have to log in again to upload it.

  - Or, we could somehow allow you do login and logout while offline ...
    presumably that would mean generating a salt, obtaining a password hashed
    with that salt, storing it (and the salt) locally, and then authenticating
    against the salted password while offline. (The purpose of the salt would be
    to avoid storing the actual password locally).

In any event, that will need some thought at some point.

-}
type Msg
    = MsgCache Backend.Model.MsgCached
    | MsgLoggedIn MsgLoggedIn
    | MsgLogin (Restful.Login.Msg User)
    | MsgPageLogin Pages.Login.Model.Msg
    | MsgSession Pages.Model.MsgSession
    | MsgServiceWorker ServiceWorker.Model.Msg
    | MsgZScore ZScore.Model.Msg
    | SendRollbar Rollbar.Level String (Dict String Value)
    | HandleRollbar (Result Http.Error Uuid)
    | SetActivePage Page
    | SetLanguage Language
    | SetOffline Bool
    | Tick Time


{-| Messages we can only handle if we're logged in.
-}
type MsgLoggedIn
    = MsgBackend Backend.Model.MsgBackend
    | MsgPageAdmin Pages.Admin.Model.Msg


type alias Flags =
    { credentials : String
    , hostname : String
    , activeLanguage : String
    , activeServiceWorker : Bool
    }


emptyModel : Flags -> Model
emptyModel flags =
    -- I suppose the login page is as logical as any.
    -- if we auto-login, we'll transition to something
    -- sensible anyway.
    { activePage = LoginPage
    , cache = Backend.Model.emptyModelCached
    , configuration = NotAsked

    -- We start at 1970, which might be nice to avoid, but probably more
    -- trouble than it's worth ... this will almost immediately get updated
    -- with the real date.
    , currentTime = 0
    , language = English
    , offline = False
    , sessionPages = Pages.Model.emptySessionPages
    , serviceWorker = ServiceWorker.Model.emptyModel flags.activeServiceWorker
    , zscores = ZScore.Model.emptyModel
    }
