module Util.Point2d exposing
    ( decode
    , encode
    )

import Json.Decode
import Json.Encode
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


encode : (Quantity Float units -> Float) -> Point2d units coordinates -> Json.Encode.Value
encode fromUnits point =
    let
        { x, y } =
            Point2d.toRecord fromUnits point
    in
    Json.Encode.object
        [ ( "x", Json.Encode.float x )
        , ( "y", Json.Encode.float y )
        ]


decode : (Float -> Quantity Float units) -> Json.Decode.Decoder (Point2d units coordinates)
decode toUnits =
    Json.Decode.map2 (\x y -> Point2d.fromRecord toUnits { x = x, y = y })
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
