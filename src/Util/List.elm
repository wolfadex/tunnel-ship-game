module Util.List exposing (extractIf)


extractIf : (a -> Bool) -> List a -> ( Maybe a, List a )
extractIf predicate list =
    extractIfHelp predicate list []


extractIfHelp : (a -> Bool) -> List a -> List a -> ( Maybe a, List a )
extractIfHelp predicate list acc =
    case list of
        [] ->
            ( Nothing, List.reverse acc )

        x :: xs ->
            if predicate x then
                ( Just x, List.reverse acc ++ xs )

            else
                extractIfHelp predicate xs (x :: acc)
