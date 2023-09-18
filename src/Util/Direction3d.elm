module Util.Direction3d exposing
    ( decode
    , encode
    )

import Direction3d exposing (Direction3d)
import Json.Decode
import Json.Encode
import Quantity exposing (Quantity)


encode : Direction3d coordinates -> Json.Encode.Value
encode dir =
    let
        { x, y, z } =
            Direction3d.unwrap dir
    in
    Json.Encode.object
        [ ( "x", Json.Encode.float x )
        , ( "y", Json.Encode.float y )
        , ( "z", Json.Encode.float z )
        ]


decode : Json.Decode.Decoder (Direction3d coordinates)
decode =
    Json.Decode.map3 (\x y z -> Direction3d.unsafe { x = x, y = y, z = z })
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
        (Json.Decode.field "z" Json.Decode.float)
