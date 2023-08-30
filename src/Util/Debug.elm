module Util.Debug exposing (logMap)


logMap : (a -> b) -> String -> a -> a
logMap fn msg a =
    let
        _ =
            Debug.log msg (fn a)
    in
    a
