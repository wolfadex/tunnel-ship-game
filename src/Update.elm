module Update exposing
    ( Update
    , save, complete
    , withCmd, withMsg
    , map, mapModel, mapMsg
    , andThen
    )

{-|

@docs Update

@docs save, complete

@docs withCmd, withMsg
@docs map, mapModel, mapMsg
@docs andThen

-}

import Task


type Update model msg
    = Update { model : model, cmds : List (Cmd msg) }


save : model -> Update model msg
save model =
    Update { model = model, cmds = [] }


withCmd : Cmd msg -> Update model msg -> Update model msg
withCmd cmd (Update update) =
    Update { update | cmds = cmd :: update.cmds }


withMsg : msg -> Update model msg -> Update model msg
withMsg msg (Update update) =
    Update { update | cmds = msgToCmd msg :: update.cmds }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    msg
        |> Task.succeed
        |> Task.perform identity


mapModel : (model1 -> model2) -> Update model1 msg -> Update model2 msg
mapModel fn (Update update) =
    Update
        { model = fn update.model
        , cmds = update.cmds
        }


mapMsg : (msg1 -> msg2) -> Update model msg1 -> Update model msg2
mapMsg fn (Update update) =
    Update
        { model = update.model
        , cmds = List.map (Cmd.map fn) update.cmds
        }


map : (model1 -> model2) -> (msg1 -> msg2) -> Update model1 msg1 -> Update model2 msg2
map fnModel fnMsg (Update update) =
    Update
        { model = fnModel update.model
        , cmds = List.map (Cmd.map fnMsg) update.cmds
        }


andThen : (model1 -> Update model2 msg) -> Update model1 msg -> Update model2 msg
andThen fn (Update update1) =
    let
        (Update update2) =
            fn update1.model
    in
    Update
        { model = update2.model
        , cmds = update2.cmds ++ update1.cmds
        }


complete : Update model msg -> ( model, Cmd msg )
complete (Update update) =
    ( update.model, Cmd.batch update.cmds )
