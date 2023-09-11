module DebugFlags exposing (DebugFlags)

import Visible exposing (Visible)


type alias DebugFlags =
    { tunnelVisible : Visible
    , trackPathVisible : Visible
    , trackPathDownDirectionVisible : Visible
    }
