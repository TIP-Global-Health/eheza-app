module Restful.Login
    exposing
        ( AppConfig
        , Config
        , Credentials
        , Login
        , LoginError(..)
        , LoginProgress(..)
        , LoginStatus(..)
        , Msg
        , accessTokenAccepted
        , accessTokenRejected
        , checkCachedCredentials
        , drupalConfig
        , getError
        , hasAccessToken
        , hasValidAccessToken
        , isProgressing
        , loggedOut
        , logout
        , mapData
        , maybeData
        , tryLogin
        , update
        )

{-| This module models the state associated with the login process,
but not the UI -- the idea is that the UI will vary more than the basic logic
of logging in does.

Of course, this model will probably be supplied as a parameter to functions
that manipulate the UI, and those functions will probably return messages that
can be handled here.


## Types

@docs LoginStatus, Credentials, Login, LoginProgress, LoginError


## Initialization

@docs loggedOut, checkCachedCredentials


## Actions

@docs tryLogin, logout, accessTokenRejected, accessTokenAccepted


## Integration with your app

@docs Config, AppConfig, drupalConfig, Msg, update


## Accessing the data associated with the login

@docs maybeData, mapData, hasAccessToken, hasValidAccessToken, getError, isProgressing

-}

import Base64
import Http exposing (Error(..), expectJson)
import HttpBuilder exposing (withExpect, withHeader, withExpect, withQueryParams)
import Json.Decode exposing (Decoder, field)
import Json.Encode exposing (Value)
import RemoteData exposing (WebData, RemoteData(..))
import Restful.Endpoint exposing (BackendUrl, AccessToken, (</>))
import Task


{-| What a successful login ultimately results in is:

-- an access token
-- which is valid for a particular backend URL
-- and some information about the logged-in user

We parameterize the user information, since it will vary from app to app.

You'll have to supply the backendUrl, of course, but it's handy to group it
here with the credentials, since you'll often want to supply the backend URL
and access token together as a parameter. And, this accessToken is valid for
this backendUrl.

-}
type alias Credentials user =
    { accessToken : AccessToken
    , backendUrl : BackendUrl
    , user : user
    }


{-| Models the state of the login process, from beginning to end.

  - `user` is the type we use to model information about the user
    (e.g. name etc.). If we have a cached access token, we check whether
    it's still valid by making a request for user data.

  - `data` is a type for data that we only keep for logged-in users. It
    facilitates forgetting that data when we logout ... it's baked into the type.
    If you don't want to bother with that, you can use a Tuple0 here.

  - CheckingCachedCredentials

    We might have some cached credentials, and we're currently trying to
    validate them against the backend.

      - If we don't have any cached credentials, we'll transition to `Anonymous`

      - If we have cached credentials, we'll transition to `LoggedIn` even if we
        can't validate them with the backend. A validation failure will be reflected
        in the `relogin` field.

  - Anonymous

    We don't have credentials. The `LoginProgress` parameter indicates whatever
    progress we might be making towards having credentials.

    We could parameterize this to add some data that only anonymous users use.
    However, it's probably the case that any such data would also be relevant
    for logged-in users, so it's probably not necessary -- it can be handled
    at a higher level.

  - LoggedIn

    We've got credentials. In addition to the credentials themsevles, we track
    what we know about the validity of the credentials, and any app-specific
    data that only applies where we have credentials (i.e. that should be
    thrown away upon logout).

-}
type LoginStatus user data
    = Anonymous LoginProgress
    | CheckingCachedCredentials
    | LoggedIn (Login user data)


{-| Is there currently something in progress to advance the
login process, or are we at rest?
-}
isProgressing : LoginStatus user data -> Bool
isProgressing model =
    case model of
        Anonymous progress ->
            loginProgressIsProgressing progress

        CheckingCachedCredentials ->
            True

        LoggedIn login ->
            Maybe.map loginProgressIsProgressing login.relogin
                |> Maybe.withDefault False


loginProgressIsProgressing : LoginProgress -> Bool
loginProgressIsProgressing loginProgress =
    case loginProgress of
        LoginFailed _ ->
            False

        LoginRequired ->
            False

        TryingPassword ->
            True


{-| Do we have an error to report?
-}
getError : LoginStatus user data -> Maybe LoginError
getError model =
    case model of
        Anonymous progress ->
            loginProgressToError progress

        CheckingCachedCredentials ->
            Nothing

        LoggedIn login ->
            Maybe.andThen loginProgressToError login.relogin


loginProgressToError : LoginProgress -> Maybe LoginError
loginProgressToError loginProgress =
    case loginProgress of
        LoginFailed err ->
            Just err

        LoginRequired ->
            Nothing

        TryingPassword ->
            Nothing


{-| Extract the data as a Maybe, which will be `Just` if the user is logged in.
-}
maybeData : LoginStatus user data -> Maybe data
maybeData model =
    case model of
        Anonymous _ ->
            Nothing

        CheckingCachedCredentials ->
            Nothing

        LoggedIn login ->
            Just login.data


{-| Map over the data, if the user is logged in.
-}
mapData : (data -> data) -> LoginStatus user data -> LoginStatus user data
mapData func model =
    case model of
        Anonymous _ ->
            model

        CheckingCachedCredentials ->
            model

        LoggedIn login ->
            LoggedIn { login | data = func login.data }


{-| If we have no credentials, are we currently doing anything to get them?

  - LoginRequired

    Nothing is in progress ... if the user wants to login, they'll need
    to enter a password.

  - TryingPassword

    We've sent a request to the backend with a password, and are waiting to
    see if it succeeds.

  - FailedPassword

    We sent a request to the backend with a password, and it failed wih the
    specified error. Note that the error may or may not mean that the password
    is bad ... for instance, it might be a transient network error. So, you'll
    need to interpret the error.

  - FailedAccessToken

    We sent a request to the backend with an access token, and it failed with
    the specified error.

-}
type LoginProgress
    = LoginFailed LoginError
    | LoginRequired
    | TryingPassword


{-| Represents an error which occured while trying to login.

  - PasswordRejected

    We successfully contacted the server, and it indicated that our username/password
    combination was rejected.

  - AccessTokenRejected

    We successfully contacted the server, but it rejected our access token.

  - Timeout

    The login request timed out.

  - NetworkError

    There was some other kind of network error.

  - InternalError

    Some other kind of problem occurred, which probably represents a bug in the logic
    of the app or the backend. We include the original `Http.Error` for further
    diagnosis.

-}
type LoginError
    = AccessTokenRejected
    | InternalError Error
    | NetworkError
    | PasswordRejected
    | Timeout


{-| Represents the data we have if we're logged in.

  - credentials

    What credentials did we log in with?

  - logout

    Tracks a request-in-progress to logout. In some cases, we need to contact
    the server in order to logout, because it maintains an HTTP-only session
    cookie which we can only delete via an HTTP request.

  - relogin

    Do we need to re-login? If our credentials are rejected, we don't
    transition back to `Anonymous` immediately, since that would prematurely
    throw away some information that we may want to keep. Instead, we mark that
    relogin is `Just LoginRequired`. We can then track the relogin process
    without disturbing the other data.

    Note that we shouldn't switch relogin to `Just` if there some kind of
    transient network error ... only if our access token is definitely
    rejected because it is permanently invalid.

  - data

    The app-specific data that only pertains to logged-in users, which we should
    throw away when the user logs out.

-}
type alias Login user data =
    { credentials : Credentials user
    , logout : WebData ()
    , relogin : Maybe LoginProgress
    , data : data
    }


{-| Given that some progress has been made towards login (or re-login),
reflect that progress in our model.

This won't flip you from `Anonymous` to `LoggedIn` or vice-versa ... it
only records **partial** progress.

However, if you are `LoggedIn`, this will make `relogin` a `Just` ...
that is, it will record that relogin is necessary.

-}
setProgress : LoginProgress -> LoginStatus user data -> LoginStatus user data
setProgress progress model =
    case model of
        Anonymous _ ->
            Anonymous progress

        CheckingCachedCredentials ->
            model

        LoggedIn login ->
            LoggedIn
                { login | relogin = Just progress }


{-| Record successfuly obtained credentials. The config will be used
to generate initial data if we didn't have some already (i.e. if this isn't
a re-login). If it is a re-login, we just keep the data.
-}
setCredentials : Config user data msg -> Credentials user -> LoginStatus user data -> LoginStatus user data
setCredentials config credentials model =
    case model of
        Anonymous _ ->
            LoggedIn
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = config.initialData credentials.user
                }

        CheckingCachedCredentials ->
            LoggedIn
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = config.initialData credentials.user
                }

        LoggedIn login ->
            LoggedIn
                { credentials = credentials
                , logout = NotAsked
                , relogin = Nothing
                , data = login.data
                }


{-| A constant which represents the state in which the user is logged out, and
no progress is currently being made towards login.

This is one possible "starting point" for initializing the LoginStatus. The other
main starting point would be `checkCachedCredentials`.

Note that you should use `logout` to actually perform the action of logging
out, since that will also clear the cached credentials.

-}
loggedOut : LoginStatus user data
loggedOut =
    Anonymous LoginRequired


{-| Initializes a LoginStatus by indicating that we're checking the cache for
credentials, and return a `Cmd` that will do that.

  - BackendUrl is the backend to check the cached credentials against.

  - Value is the JSON string which your `cacheCredentials` function (from Config)
    has cached. So, it's up to you to fetch that value somehow, either via
    flags at startup, or via ports. If you've cached credentials for multiple backends,
    it's up to you to match your backendURL and your credentials.

The LoginStatus will start as `CheckingCachedCredentials`. At this point, your UI
should treat the login process as unresolved ... it will soon resolve one way
or another. So, you might show a "checking for cached login" message, or just
nothing.

  - If we can decode the credentials, we'll try to use the access token against
    the backend to get updated user information. Whether or not that succeeds,
    we'll be in `LoggedIn` state ... the result of checking the credentials will
    affect whether `relogin` is required.

  - If we can't decode the credentials, we'll be in `Anonymous LoginRequired`
    state.

-}
checkCachedCredentials : Config user data msg -> BackendUrl -> String -> ( LoginStatus user data, Cmd msg )
checkCachedCredentials config backendUrl value =
    let
        -- The third return parameter will necessarily be false, since we're
        -- just kicking off the credential check here.
        ( loginStatus, cmd, _ ) =
            update config (CheckCachedCredentials backendUrl value) CheckingCachedCredentials
    in
        ( loginStatus, cmd )


{-| Some static configuration which we need to integrate with your app.
You should be able to define this once, as a constant, and then use it
where needed.

  - user

    The type you use for your user.

  - data

    The type of data that is only for users ... that is, which you'd like
    to throw away upon logout.

  - msg

    Your Msg type.

  - loginPath

    Relative to a backendUrl, what's the path to the endpoint for getting an
    access token? e.g. "api/login-token"

  - logoutPath

    Relative to a backendUrl, what's the path we can send a GET to in order
    to logout? E.g. to destroy a session cookie, if it's HTTP only, so we can't
    destroy it from Javascript.

  - userPath

    Once we have an access token, what's the path to the endpoint from which we
    can request information about the current user?

  - decodeAccessToken

    A decoder for an access token, given the response from the loginPath

  - decodeUser

    A decoder for the `user` type, as send from userPath.

  - encodeUser

    A function that will produce JSON that `decodeUser` can decode.

  - initialData

    Given the newly logged-in user, what initial data should we store for that
    user?

  - cacheCredentials

    A function which, when given a backendURL and a JSON string, will return a
    command that caches that string. Exactly how you do that is up to you ... it
    will probably be via ports.

    We provide the backendUrl in case you want to store the credentials for multiple
    backends and pick amongst them when needed. But you can ignore it if you like ...
    the important part for us is the JSON string.

    However you store the JSON string, you can provide it to `checkCachedCredentials`
    and we'll use it.

  - tag

    What tag do you use in your `Msg` type to wrap our `Msg` type? This allows
    our `update` function to work in the context of your `Msg` type ... in
    effect, we'll do the mapping, rather than making you do it.

-}
type alias Config user data msg =
    { loginPath : String
    , logoutPath : Maybe String
    , userPath : String
    , decodeAccessToken : Decoder AccessToken
    , decodeUser : Decoder user
    , encodeUser : user -> Value
    , initialData : user -> data
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| The parts of `Config` that tend to vary from one app to the next.
-}
type alias AppConfig user data msg =
    { decodeUser : Decoder user
    , encodeUser : user -> Value
    , initialData : user -> data
    , cacheCredentials : BackendUrl -> String -> Cmd msg
    , tag : Msg user -> msg
    }


{-| Make a `Config` that uses default values oriented towards Drupal's
restful implementation.
-}
drupalConfig : AppConfig user data msg -> Config user data msg
drupalConfig appConfig =
    { loginPath = "api/login-token"
    , logoutPath = Just "user/logout"
    , userPath = "api/me"
    , decodeAccessToken = field "access_token" Json.Decode.string
    , decodeUser = appConfig.decodeUser
    , encodeUser = appConfig.encodeUser
    , initialData = appConfig.initialData
    , cacheCredentials = appConfig.cacheCredentials
    , tag = appConfig.tag
    }


{-| An opaque type representing messages we handle. You can create
these messages with various functions (e.g. `tryLogin`, `logout`) and handle
them with the `update` function.
-}
type Msg user
    = CheckCachedCredentials BackendUrl String
    | HandleAccessTokenCheck (Credentials user) (Result Error user)
    | HandleLoginAttempt (Result Error (Credentials user))
    | HandleLogoutAttempt (Result Error ())
    | Logout
    | TryLogin BackendUrl String String


{-| Message which will try logging in against the specified backendUrl
-}
tryLogin : BackendUrl -> String -> String -> Msg user
tryLogin =
    TryLogin


{-| Message which will log out and clear cached credentials.
-}
logout : Msg user
logout =
    Logout


{-| Specializes an HTTP error to our `LoginError` type.

The first parameter is the `LoginError` we ought to use if the attempt to
authenticate reached the server, and we got a response, but the response was a
rejection. So, typically `PasswordRejected` or `AccessTokenRejected`, depending
on which we were trying.

-}
classifyHttpError : LoginError -> Error -> LoginProgress
classifyHttpError rejected error =
    LoginFailed <|
        case error of
            Http.BadUrl _ ->
                InternalError error

            Http.Timeout ->
                Timeout

            Http.NetworkError ->
                NetworkError

            Http.BadStatus response ->
                if response.status.code == 401 then
                    rejected
                else
                    InternalError error

            Http.BadPayload _ _ ->
                InternalError error


{-| Our update function. Note that the `Cmd` we return is in terms of
your own msg type. So, you can integrate it into your app roughly
as follows:

    ...

The third return parameter will be `True` at the very moment at which
a successful login has been made. But only at that very moment ... it's
not reflecting state, but instead a kind of notification that we've
just logged in.

-}
update : Config user data msg -> Msg user -> LoginStatus user data -> ( LoginStatus user data, Cmd msg, Bool )
update config msg model =
    -- Ultimately, it might be easier to work in the **caller's** `Msg` type,
    -- and have a "mapper" in the `Config` so we can do some internal messages.
    -- But we'll start this way and see how it goes.
    case msg of
        HandleLoginAttempt result ->
            case result of
                Err err ->
                    ( setProgress (classifyHttpError PasswordRejected err) model
                    , Cmd.none
                    , False
                    )

                Ok credentials ->
                    ( setCredentials config credentials model
                    , config.cacheCredentials credentials.backendUrl (encodeCredentials config credentials)
                    , True
                    )

        TryLogin backendUrl name password ->
            let
                -- TODO: Perhaps the login method ought to be parameterized in the config,
                -- with this as a default?
                credentials =
                    Base64.encode (name ++ ":" ++ password)

                requestAccessToken =
                    HttpBuilder.get (backendUrl </> config.loginPath)
                        |> withHeader "Authorization" ("Basic " ++ credentials)
                        |> withExpect (expectJson config.decodeAccessToken)
                        |> HttpBuilder.toTask

                requestUser accessToken =
                    HttpBuilder.get (backendUrl </> config.userPath)
                        |> withQueryParams [ ( "access_token", accessToken ) ]
                        |> withExpect (expectJson config.decodeUser)
                        |> HttpBuilder.toTask
                        |> Task.map
                            (\user ->
                                { accessToken = accessToken
                                , backendUrl = backendUrl
                                , user = user
                                }
                            )

                cmd =
                    requestAccessToken
                        |> Task.andThen requestUser
                        |> Task.attempt HandleLoginAttempt
            in
                ( setProgress TryingPassword model
                , Cmd.map config.tag cmd
                , False
                )

        HandleAccessTokenCheck credentials result ->
            case result of
                Err err ->
                    -- This is actually a kind of successful login, in that we have
                    -- credentials ... we just mark them as needing relogin.
                    ( setCredentials config credentials model
                        |> accessTokenRejected err
                    , Cmd.none
                    , True
                    )

                Ok user ->
                    ( setCredentials config { credentials | user = user } model
                    , Cmd.none
                    , True
                    )

        CheckCachedCredentials backendUrl cachedValue ->
            case Json.Decode.decodeString (decodeCredentials config backendUrl) cachedValue of
                Err _ ->
                    -- If we can't decode the cached credentials, we just
                    -- give up and say that login is needed. This will, for
                    -- instance, happen where we had logged out and cleared
                    -- the cached credentials.
                    --
                    -- It could also happen if our user decoder has changed
                    -- (i.e. a new version of the program is in place). We can
                    -- either handle that in the user decoder itself (i.e. by
                    -- using `oneOf` and detecting versions), or just failing
                    -- here isn't so bad, as it just means we have to log in.
                    ( loggedOut, Cmd.none, False )

                Ok credentials ->
                    -- If we have credentials, then we will check the access
                    -- token against the backend, to make sure that it is still
                    -- valid. Any error will result in a `relogin` being
                    -- recorded.
                    let
                        cmd =
                            HttpBuilder.get (backendUrl </> config.userPath)
                                |> withQueryParams [ ( "access_token", credentials.accessToken ) ]
                                |> withExpect (expectJson config.decodeUser)
                                |> HttpBuilder.toTask
                                |> Task.attempt (HandleAccessTokenCheck credentials)
                                |> Cmd.map config.tag
                    in
                        ( CheckingCachedCredentials, cmd, False )

        Logout ->
            case model of
                Anonymous _ ->
                    ( loggedOut
                    , Cmd.none
                    , False
                    )

                CheckingCachedCredentials ->
                    -- TODO: Do something sensible here. But it's not entirely
                    -- clear what that would be. Ideally, people shouldn't
                    -- logout while we're checking credentials...
                    ( model
                    , Cmd.none
                    , False
                    )

                LoggedIn login ->
                    case config.logoutPath of
                        Just logoutPath ->
                            -- See comment below ... in this case, we can't
                            -- really logout unless we send a request to the
                            -- backend to do so.
                            ( LoggedIn { login | logout = Loading }
                            , HttpBuilder.get (login.credentials.backendUrl </> logoutPath)
                                |> withQueryParams [ ( "access_token", login.credentials.accessToken ) ]
                                |> HttpBuilder.toTask
                                |> Task.attempt HandleLogoutAttempt
                                |> Cmd.map config.tag
                            , False
                            )

                        Nothing ->
                            -- In this case, we can just forget our credentials
                            -- locally.  We'll just call ourselves recursively
                            -- as if the logout request succeeded.
                            update config (HandleLogoutAttempt (Ok ())) model

        -- If there is an HTTP-only session cookie, and you're serving the app
        -- from a sub-path on the Drupal site, then you can only **really**
        -- logout if you're online and we get a successful response here. The
        -- reason is that the session cookie can only be deleted via HTTP, and
        -- that won't have happened unless your request succeeds here. Otherwise,
        -- we'll still have the ssession cookie, and future attempts to login
        -- will simply use the existing session.
        --
        -- For this reason, we only locally record a logout when this request
        -- succeeds ... otherwise, we show an error.
        HandleLogoutAttempt result ->
            let
                -- A 403 Forbidden response is actually success, in this case!
                adjustedResult =
                    case result of
                        Err (BadStatus response) ->
                            if response.status.code == 403 then
                                Ok ()
                            else
                                result

                        _ ->
                            result
            in
                case ( adjustedResult, model ) of
                    ( Ok _, LoggedIn login ) ->
                        -- We tell the app to cache credentials consisting of an empty object.
                        -- This is simpler than telling the app to delete credentials.
                        ( loggedOut
                        , config.cacheCredentials login.credentials.backendUrl "{}"
                        , False
                        )

                    ( Err err, LoggedIn login ) ->
                        -- Just record the error
                        ( LoggedIn { login | logout = Failure err }
                        , Cmd.none
                        , False
                        )

                    _ ->
                        -- If we weren't logged in anyway, there's nothing to do.
                        ( model, Cmd.none, False )


encodeCredentials : Config user data msg -> Credentials user -> String
encodeCredentials config credentials =
    -- We only encode the accessToken and the user ... we provide the
    -- backendURL separately, so the app can decide whether to record
    -- this separately for different configured backends etc.
    Json.Encode.encode 0 <|
        Json.Encode.object
            [ ( "access_token", Json.Encode.string credentials.accessToken )
            , ( "user", config.encodeUser credentials.user )
            ]


decodeCredentials : Config user data msg -> BackendUrl -> Decoder (Credentials user)
decodeCredentials config backendUrl =
    Json.Decode.map2
        (\accessToken user ->
            { backendUrl = backendUrl
            , accessToken = accessToken
            , user = user
            }
        )
        (field "access_token" Json.Decode.string)
        (field "user" config.decodeUser)


{-| As far as we know, do we have a still-valid access token?

If we don't know yet, we indicate `False`.

-}
hasValidAccessToken : LoginStatus user data -> Bool
hasValidAccessToken status =
    case status of
        Anonymous _ ->
            False

        CheckingCachedCredentials ->
            False

        LoggedIn login ->
            login.relogin == Nothing


{-| Do we have an access token, whether or not we think it's valid?

If we're still checking, we say `False`.

-}
hasAccessToken : LoginStatus user data -> Bool
hasAccessToken status =
    case status of
        Anonymous _ ->
            False

        CheckingCachedCredentials ->
            False

        LoggedIn login ->
            True


{-| Record the fact that our access token was rejected.

If we're in a `LoggedIn` state, we'll stay in that state ... we'll
merely record that re-login is required.

-}
accessTokenRejected : Error -> LoginStatus user data -> LoginStatus user data
accessTokenRejected =
    setProgress << classifyHttpError AccessTokenRejected


{-| If you previously recorded `accessTokenRejected` but it was a transient
problem, and now it has been accepted, you can record that with this function.

You don't need to call this every time the access token is accepted (though
it won't do any harm, either).

Note that this doesn't switch our state from `Anonymous` to `LoggedIn` ...
it only resets `LoggedIn` (if that's what we are) to show that `relogin`
is not required.

-}
accessTokenAccepted : LoginStatus user data -> LoginStatus user data
accessTokenAccepted status =
    -- We return `status` unchanged as often as possible, for the sake of
    -- preserving referential equality where we can.
    case status of
        Anonymous _ ->
            status

        CheckingCachedCredentials ->
            status

        LoggedIn login ->
            case login.relogin of
                Just _ ->
                    LoggedIn { login | relogin = Nothing }

                Nothing ->
                    status
