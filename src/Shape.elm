module Shape exposing
    ( Shape
    , custom
    , newRegular
    , encode
    , sideCount
    )

import Length exposing (Meters)
import Json.Encode
import Point2d
import Polygon2d exposing (Polygon2d)
import Util.Point2d


type alias Shape coordinates =
    Polygon2d Meters coordinates


newRegular : Int -> Shape coordinates
newRegular sides =
    Polygon2d.regular
        { centerPoint = Point2d.origin
        , circumradius = Length.meters 1
        , numSides = sides
        }


sideCount : Shape coordinates -> Int
sideCount shape =
    shape
        |> Polygon2d.vertices
        |> List.length


custom : Shape coordinates
custom =
    Polygon2d.singleLoop
        [ Point2d.meters 0.75 -0.75
        , Point2d.meters 0.75 0.75
        , Point2d.meters 0 0.5
        , Point2d.meters -0.75 0.75
        , Point2d.meters -0.75 -0.75
        , Point2d.meters 0 -0.5
        ]


encode : Shape coordinates -> Json.Encode.Value
encode shape =
    shape
        |> Polygon2d.vertices
        |> Json.Encode.list (Util.Point2d.encode Length.toMeters)