module LocalData exposing (LocalData(..), ReadyStatus(..), isNotNeeded, map, toMaybe, unwrap, withDefault)

{-| Data which we have locally, but takes time to calculate.
-}


type LocalData a
    = NotNeeded
    | Ready a ReadyStatus


{-| When LocalData was calculated, and we want to re-calculate it, we will mark it as `Ready a Recalculate`.
-}
type ReadyStatus
    = Recalculate
    | NoRecalculate


toMaybe : LocalData a -> Maybe a
toMaybe data =
    case data of
        Ready a _ ->
            Just a

        _ ->
            Nothing


withDefault : a -> LocalData a -> a
withDefault default data =
    toMaybe data |> Maybe.withDefault default


map : (a -> b) -> LocalData a -> LocalData b
map func data =
    case data of
        Ready a status ->
            Ready (func a) status

        NotNeeded ->
            NotNeeded


unwrap : b -> (a -> b) -> LocalData a -> b
unwrap default func data =
    map func data
        |> withDefault default


isNotNeeded : LocalData a -> Bool
isNotNeeded data =
    case data of
        Ready _ Recalculate ->
            True

        NotNeeded ->
            True

        _ ->
            False
