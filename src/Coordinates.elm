module Coordinates exposing
    ( DefinesLocal
    , Flat
    , Screen
    , World
    )


type World
    = World Never


type Flat
    = Flat Never


type Screen
    = Screen Never


type alias DefinesLocal =
    { defines : World }
