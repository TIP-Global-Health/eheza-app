module Restful.RestfulData exposing (..)

{-| When dealing wih values that are edited and persisted to a backend, (or
values derived from such values), there is a cluster of concerns that one has.

  - What was the value on the backend?

  - What change have we made locally, not yet persisted to the backend?

  - What edit is currently in progress in the UI, possibly in a secondary type
    suitable for editing? (For instance, when editing a float value, we track
    strings, which may or may not represent a valid float at any given moment).

  - Is there a CRUD operation in progress that will affect this value?

  - Did we try a CRUD operation which failed with an error?

The beginning of all wisdom on such matters is krisajenkins/remotedata, but it
only really covers "read" operations (and perhaps "create") -- it's not
suitable for "update" and "delete".

Another related package is Gizra/editable-webdata. This module addresses similar
concerns, but adds additional state-tracking and the possibly distinct type
used for editing in the UI.

TODO: This is a work-in-progress ... I'm not actually using it yet, but I
clearly conceived of part of it one day and wanted to get that down. I like how
it's going so far, but the validation part needs some more thinking ... I
should try integrating with an existing validation library and see how that
goes. Also, we'll need something like `RemoteData.map` to split things up, and
then possibly to combine things together -- I think that's a problem which the
validator libraries will already have a structure for, since they will also
need to "pick apart" structures to validate them, and then put them back
together.

-}

import Result


{-| A wrapper around a value which encapsulates various information about
its status on the backend, locally, and in the UI, along with the status
of a request to modify the backend.


#### Type parameters

  - `r` is the type of the request, if you wish to remember it (or you
    could use a `Tuple0` if you don't care).

  - `b` is the type of an error associated with a request to update the
    backend. In many cases, it may be `Http.Error`, but you're not limited to
    that. For the sake of simplicity, we don't have separate error types for
    different CRUD operations, but you could of course use a unifying type to tag
    multiple sub-types if you wish.

  - `v` is the type of an error associated with validating `edit` type.

  - `e` is an associated type which you use in the UI for editing the
    value. For instance, you might edit a `Float` value via the `String` type.
    If you don't need an associated type, you can repeat the `value` type.

  - `s` is the type of the value itself (i.e. the `Success` type).


#### Tags

  - `NotAsked` means that we haven't issued any CRUD operations yet, and we
    don't have a value. So, it's one logical starting point ...

  - `Success` means that we've either had a successful fetch, or a successful
    create, update or delete that results in the specified value. So, in a sense,
    `Success` is a "collapsed" state ... we forget how we got there, once we
    succeed.

  - `Pending` means that we've sent a CRUD request, and we're waiting for the
    result.

    So, this is sort of like `Loading` in `RemoteData`, except that we remember
    what our state was just before we sent the request.

  - `Failure` means that our CRUD request failed, with the specified error. So,
    it is like `Failure` in RemoteData, but we remember what our state was before
    the request.

  - `Editing` means that we're editing this data locally, and haven't tried to
    save it yet. The previous value we remember is our state before we started
    editing (that is, we're not trying to remember all intermediate states).
    The `Result` is the result of validating the raw edit, and `e` is the raw edit.

-}
type RestfulData r b v e s
    = NotAsked
    | Success s
    | Pending (RestfulData r b v e s) r
    | Failure (RestfulData r b v e s) b
    | Editing (RestfulData r b v e s) (Result v s) e


{-| A simple type to track the kind of request we're waiting for (or have sent).

If you don't care to remember this, you could use a `Tuple0` for the `r`
type parameter.

If you want to remember more, you can use your own type. Or, I suppose
you could use `Http.Request` if you just want to be able to send it
again (i.e. retry).

-}
type CrudRequest
    = Create
    | Read
    | Update
    | Delete


{-| Get any request information we have for a pending request or a failure.
-}
request : RestfulData r b v e s -> Maybe r
request data =
    case data of
        Pending _ r ->
            Just r

        Failure oldData _ ->
            request oldData

        _ ->
            Nothing


{-| Record some successful data.
-}
succeed : s -> RestfulData r b v e s
succeed =
    Success


{-| Record that the given request is now pending.
-}
pending : r -> RestfulData r b v e s -> RestfulData r b v e s
pending request data =
    case data of
        NotAsked ->
            Pending data request

        Success _ ->
            Pending data request

        Pending oldData _ ->
            -- In this case, we were already pending, and now we're recording
            -- a new request. So, we forget the old request, and just remember
            -- what the previous data was.
            Pending oldData request

        Failure oldData _ ->
            -- Again, we collapse things in this case ... once we're pending
            -- again, we no longer remember the failure, just the old data.
            Pending oldData request

        Editing _ _ _ ->
            -- If we were editing, then we remember everything about our
            -- editing state while the request is pending ... we don't collapse
            -- until we get a response.
            Pending data request


{-| Record that a request has failed.
-}
failure : b -> RestfulData r b v e s -> RestfulData r b v e s
failure err data =
    case data of
        Failure oldData _ ->
            -- If we were already in a failed state, we collapse things and
            -- substitue our new failure.
            Failure oldData err

        _ ->
            -- Otherwise, we remember our state before failure
            Failure data err


{-| Record that an edit has been made in the UI.
-}
edit : (e -> Result v s) -> e -> RestfulData r b v e s -> RestfulData r b v e s
edit validator edited data =
    case data of
        NotAsked ->
            -- We're editing a value we haven't asked about, so I guess we're
            -- creating a new one.
            Editing data (validator edited) edited

        Success _ ->
            -- We're editing a value we had, so we'll eventually be interested
            -- in updating it ...
            Editing data (validator edited) edited

        Pending _ _ ->
            -- This is an interesting case ... we're trying to edit a value while
            -- a request is pending. Let's say that this should actually be disallowed ...
            -- that is, we won't modify a value while the request is pending.
            data

        Failure oldData _ ->
            -- Another interesting case. If we were in failure mode, and the user
            -- starts editing, then let's forget the failure ... e.g. in the UI,
            -- we'd no longer want to show the error once editing has begun again.
            -- But, of course, we keep the data that the failure case was remembering.
            Editing oldData (validator edited) edited

        Editing oldData _ _ ->
            -- If we were editing, collapse the previous state so that we don't track
            -- all intermediate states!
            Editing oldData (validator edited) edited


{-| What do we think the value on the backend is, if any?
-}
backend : RestfulData r b v e s -> Maybe s
backend data =
    case data of
        NotAsked ->
            Nothing

        Success s ->
            Just s

        Pending oldData _ ->
            backend oldData

        Failure oldData _ ->
            backend oldData

        Editing oldData _ _ ->
            backend oldData


{-| What current and validated, but possibly unsaved, value do we have?
-}
current : RestfulData r b v e s -> Maybe s
current data =
    case data of
        NotAsked ->
            Nothing

        Success s ->
            Just s

        Pending oldData _ ->
            current oldData

        Failure oldData _ ->
            current oldData

        Editing _ validated _ ->
            -- If we're editing, we don't fall back to the old value ...
            -- we use `Nothing` if we don't validate.
            case validated of
                Ok value ->
                    Just value

                Err _ ->
                    Nothing


{-| What value should we show in an editor?

We return a `Maybe` so you can decide what to do when there isn't
a value to show yet ... you could supply a default, or you could
disallow editing, or whatever.

-}
editor : (s -> e) -> RestfulData r b v e s -> Maybe e
editor func data =
    case data of
        NotAsked ->
            Nothing

        Success s ->
            Just (func s)

        Pending oldData _ ->
            -- We supply a value to show ... but we shouldn't allow editing ...
            -- we'll have another function for that.
            editor func oldData

        Failure oldData _ ->
            editor func oldData

        Editing _ _ e ->
            Just e


{-| Is this data in a state in which we should allow editing in the UI?

We disabled editing when requests are pending.

-}
enableEditing : RestfulData r b v e s -> Bool
enableEditing data =
    case data of
        NotAsked ->
            True

        Success _ ->
            True

        Pending _ _ ->
            False

        Failure _ _ ->
            True

        Editing _ _ _ ->
            True


{-| Should we enable a "Save" button for this data in the UI?
-}
enableSave : RestfulData r b v e s -> Bool
enableSave data =
    case data of
        NotAsked ->
            False

        Success _ ->
            False

        Pending _ _ ->
            False

        Failure _ _ ->
            -- This one's a little complicated. In theory, there
            -- would be some cases where a `retry` might make sense
            -- without editing anything first, but we'll say
            -- `False` for now.
            False

        Editing _ validated _ ->
            case validated of
                Ok _ ->
                    True

                Err _ ->
                    False
