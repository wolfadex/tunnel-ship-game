module Shape exposing
    ( Shape
    , new
    , sideCount
    )

import Length exposing (Meters)
import Point2d
import Polygon2d exposing (Polygon2d)


type alias Shape coordinates =
    Polygon2d Meters coordinates


new : Int -> Shape coordinates
new sides =
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
