module Restful.UpdatableData exposing (..)

{-| This is a simpler alternative to `RestfulData`. It handles cases where we
want to track both a possible request to "fetch" some data and a possible
request to update that data. `RestfulData` does that more completely, but
at the cost of some complexity. Here, we just carry around a second `WebData`
to track the update.

I'm not actually using this one yet, either. Instead, I've put a `WebData ()`
inside the underlying type ... that is, there's a `WebData` nested inside
the data, to track updates. That seems even simpler, at lesat for the moment ...
ultimately, something more generic is likely to be nice.

-}

import Http exposing (Error)
import RemoteData exposing (RemoteData(..), WebData)


{-| Like `RemoteData`, but tracks a second `WebData` to track a possible
request in progress to update the data.
-}
type alias UpdatableData e a =
    { value : RemoteData e a
    , update : RemoteData e ()
    }


type alias UpdatableWebData a =
    UpdatableData Error a


{-| Start with both the value and its update as `NotAsked`.
-}
notAsked : UpdatableData e a
notAsked =
    { value = NotAsked
    , update = NotAsked
    }
