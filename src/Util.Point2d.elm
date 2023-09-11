module Util.Point2d exposing (encode)


import Json.Encode
import Point2d exposing (Point2d)
import Length exposing (Length)



encode : (Length -> Float) -> Point2d units coordinates -> Json.Encode.Value
encode fromUnits point =
    let
        { x, y } =
            Point2d.toRecord point
    in
    Json.Encode.object
        [ ( "x", Json.Encode.float (fromUnits x) )
        , ( "y", Json.Encode.float (fromUnits y) )
        ]
