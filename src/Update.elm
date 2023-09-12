module Update exposing
    ( Update
    , save, complete
    , withCmd, withMsg, withEffect
    , mapModel, mapMsg
    , andThen
    , applyEffects
    )

{-|

@docs Update

@docs save, complete

@docs withCmd, withMsg, withEffect
@docs mapModel, mapMsg
@docs andThen
@docs applyEffects

-}

import Task


type Update model msg effect
    = Update { model : model, cmds : List (Cmd msg), effects : List effect }


save : model -> Update model msg effect
save model =
    Update { model = model, cmds = [], effects = [] }


withCmd : Cmd msg -> Update model msg effect -> Update model msg effect
withCmd cmd (Update update) =
    Update { update | cmds = cmd :: update.cmds }


withMsg : msg -> Update model msg effect -> Update model msg effect
withMsg msg (Update update) =
    Update { update | cmds = msgToCmd msg :: update.cmds }


withEffect : effect -> Update model msg effect -> Update model msg effect
withEffect effect (Update update) =
    Update { update | effects = effect :: update.effects }


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    msg
        |> Task.succeed
        |> Task.perform identity


mapModel : (model1 -> model2) -> Update model1 msg effect -> Update model2 msg effect
mapModel fn (Update update) =
    Update
        { model = fn update.model
        , cmds = update.cmds
        , effects = update.effects
        }


mapMsg : (msg1 -> msg2) -> Update model msg1 effect -> Update model msg2 effect
mapMsg fn (Update update) =
    Update
        { model = update.model
        , cmds = List.map (Cmd.map fn) update.cmds
        , effects = update.effects
        }


andThen : (model1 -> Update model2 msg effects) -> Update model1 msg effects -> Update model2 msg effects
andThen fn (Update update1) =
    let
        (Update update2) =
            fn update1.model
    in
    Update
        { model = update2.model
        , cmds = update2.cmds ++ update1.cmds
        , effects = update2.effects
        }


applyEffects :
    (childEffect -> Update model msg parentEffect -> Update model msg parentEffect)
    -> Update model msg childEffect
    -> Update model msg parentEffect
applyEffects fn (Update update) =
    List.foldr fn
        (Update
            { model = update.model
            , cmds = update.cmds
            , effects = []
            }
        )
        update.effects


fromChild : (childMsg -> parentMsg) -> (childModel -> parentModel) -> (childEffect -> Update parentModel parentMsg parentEffect -> Update parentModel parentMsg parentEffect) -> Update childModel childMsg childEffect -> Update parentModel parentMsg parentEffect
fromChild msgFn modelFn effectFn update =
    update
        |> mapModel modelFn
        |> mapMsg msgFn
        |> applyEffects effectFn


complete : Update model msg effect -> ( model, Cmd msg )
complete (Update update) =
    ( update.model, Cmd.batch update.cmds )
