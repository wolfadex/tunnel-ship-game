module Util.Function exposing
    ( applyIf
    , applyMaybe
    )


applyIf : Bool -> (a -> a) -> a -> a
applyIf condition f x =
    if condition then
        f x

    else
        x


applyMaybe : Maybe a -> (a -> a) -> a -> a
applyMaybe maybe f x =
    case maybe of
        Just a ->
            f a

        Nothing ->
            x
