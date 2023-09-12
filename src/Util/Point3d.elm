module Util.Point3d exposing
    ( decode
    , encode
    )

import Json.Decode
import Json.Encode
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)


encode : (Quantity Float units -> Float) -> Point3d units coordinates -> Json.Encode.Value
encode fromUnits point =
    let
        { x, y, z } =
            Point3d.toRecord fromUnits point
    in
    Json.Encode.object
        [ ( "x", Json.Encode.float x )
        , ( "y", Json.Encode.float y )
        , ( "z", Json.Encode.float z )
        ]


decode : (Float -> Quantity Float units) -> Json.Decode.Decoder (Point3d units coordinates)
decode toUnits =
    Json.Decode.map3 (\x y z -> Point3d.fromRecord toUnits { x = x, y = y, z = z })
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
        (Json.Decode.field "z" Json.Decode.float)
