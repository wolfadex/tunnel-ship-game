module Util.Maybe exposing (andThen2)


andThen2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
andThen2 fn maybeA maybeB =
    case maybeA of
        Nothing ->
            Nothing

        Just a ->
            case maybeB of
                Nothing ->
                    Nothing

                Just b ->
                    fn a b
