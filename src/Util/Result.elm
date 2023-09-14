module Util.Result exposing (combine)


combine : List (Result e a) -> Result e (List a)
combine =
    combineHelper []


combineHelper : List a -> List (Result e a) -> Result e (List a)
combineHelper acc results =
    case results of
        [] ->
            acc
                |> List.reverse
                |> Ok

        next :: rest ->
            case next of
                Err err ->
                    Err err

                Ok value ->
                    combineHelper (value :: acc) rest
