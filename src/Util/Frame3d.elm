module Util.Frame3d exposing
    ( decode
    , encode
    )

import Direction3d
import Frame3d exposing (Frame3d)
import Json.Decode
import Json.Encode
import Point3d
import Quantity exposing (Quantity)
import Util.Direction3d
import Util.Point3d


encode : (Quantity Float units -> Float) -> Frame3d units coordinates defines -> Json.Encode.Value
encode fromUnits frame =
    Json.Encode.object
        [ ( "origin"
          , frame
                |> Frame3d.originPoint
                |> Util.Point3d.encode fromUnits
          )
        , ( "xDir"
          , frame
                |> Frame3d.xDirection
                |> Util.Direction3d.encode
          )
        , ( "yDir"
          , frame
                |> Frame3d.yDirection
                |> Util.Direction3d.encode
          )
        , ( "zDir"
          , frame
                |> Frame3d.zDirection
                |> Util.Direction3d.encode
          )
        ]


decode : (Float -> Quantity Float units) -> Json.Decode.Decoder (Frame3d units coordinates defines)
decode toUnits =
    Json.Decode.map4
        (\origin xDir yDir zDir ->
            Frame3d.unsafe
                { originPoint = origin
                , xDirection = xDir
                , yDirection = yDir
                , zDirection = zDir
                }
        )
        (Json.Decode.field "origin" (Util.Point3d.decode toUnits))
        (Json.Decode.field "xDir" Util.Direction3d.decode)
        (Json.Decode.field "yDir" Util.Direction3d.decode)
        (Json.Decode.field "zDir" Util.Direction3d.decode)
