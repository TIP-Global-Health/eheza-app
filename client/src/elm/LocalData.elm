module LocalData exposing (LocalData(..), map, toMaybe, unwrap, withDefault)


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


unwrap : b -> (a -> b) -> LocalData a -> b
unwrap default func data =
    map func data
        |> withDefault default
