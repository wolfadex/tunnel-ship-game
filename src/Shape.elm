module Shape exposing (..)

import Length exposing (Meters)
import Point2d
import Polygon2d exposing (Polygon2d)


octagon : Polygon2d Meters coordinates
octagon =
    Polygon2d.regular
        { centerPoint = Point2d.origin
        , circumradius = Length.meters 1
        , numSides = 8
        }
