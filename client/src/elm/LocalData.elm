module LocalData exposing (LocalData(..), map, toMaybe, withDefault)


type LocalData a
    = NotNeeded
    | Calcualting
    | Ready a


toMaybe : LocalData a -> Maybe a
toMaybe data =
    case data of
        Ready a ->
            Just a

        _ ->
            Nothing


withDefault : a -> LocalData a -> a
withDefault default data =
    toMaybe data |> Maybe.withDefault default


map : (a -> b) -> LocalData a -> LocalData b
map func data =
    case data of
        Ready a ->
            func a |> Ready

        NotNeeded ->
            NotNeeded

        Calcualting ->
            Calcualting
