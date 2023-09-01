module Util.Random exposing (fromList)

import Random


fromList : List a -> Random.Generator (Maybe a)
fromList list =
    case list of
        [] ->
            Random.constant Nothing

        first :: rest ->
            Random.uniform first rest
                |> Random.map Just
