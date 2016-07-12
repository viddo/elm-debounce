effect module Debounce where { command = MyCmd } exposing
  ( debounce
  )


import Platform exposing (Router, ProcessId)
import Process exposing (sleep, spawn)
import Dict exposing (Dict)
import Task exposing (Task)
import Time exposing (Time)


type MyCmd msg =
  MyCmd Time String msg


type alias State =
  Dict String Float


type alias Effect msg =
  (String, Time, msg)


debounce : Time -> String -> msg -> Cmd msg
debounce timeout key msg =
  command (MyCmd timeout key msg)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func (MyCmd timeout key msg) =
    MyCmd timeout key (func msg)


init : Task Never State
init =
  Task.succeed Dict.empty


onEffects : Router msg (Effect msg) -> List (MyCmd msg) -> State -> Task Never State
onEffects router cmds state =
  Time.now
  |> andThen (\time ->
    Task.sequence (List.map (spawnDelayedCommand time router) cmds)
      |> andThen (\_ -> Task.succeed (List.foldl (updateState time) state cmds))
  )


onSelfMsg : Router msg (Effect msg) -> (Effect msg) -> State -> Task Never State
onSelfMsg router effect state =
  let
    (key, createdAtTime, msg) = effect
    time = Maybe.withDefault -1 (Dict.get key state)
  in
    if time == createdAtTime then
      (Platform.sendToApp router msg)
        |> andThen (\_ -> Task.succeed (Dict.remove key state))
    else
      Task.succeed state


spawnDelayedCommand : Time -> Router msg (Effect msg) -> MyCmd msg -> Task Never ProcessId
spawnDelayedCommand currentTime router cmd =
  let
    (MyCmd timeout key msg) = cmd
    effect = (key, currentTime, msg)
    sendMsg = (Platform.sendToSelf router effect)
  in
    spawn (sleep timeout |> andThen (\_ -> sendMsg))


updateState : Time -> MyCmd msg -> State -> State
updateState currentTime cmd state =
  let
    (MyCmd _ key _) = cmd
  in
    Dict.insert key currentTime state


andThen : (a -> Task Never b) -> Task Never a -> Task Never b
andThen fn t1 =
  t1 `Task.andThen` fn
