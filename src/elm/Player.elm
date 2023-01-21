module Player exposing (takeTurn)

import Action
import Warrior exposing (Warrior)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Map exposing (Map)


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        currentHeading =
            History.previousActions warrior history
                |> List.filter Action.isMovement
                |> (List.head >> Maybe.andThen Action.direction)
                |> Maybe.withDefault Direction.Right
    in
    if didPlayerMove warrior history then
        Warrior.Move currentHeading

    else
        Warrior.Move (cycleDirectionCW currentHeading)


didPlayerMove : Warrior -> History -> Bool
didPlayerMove warrior history =
    let
        previousPosition =
            History.previousStates warrior history
                |> List.head
                |> Maybe.map (Tuple.first >> Warrior.position)
    in
    previousPosition
        |> Maybe.map ((/=) (Warrior.position warrior))
        |> Maybe.withDefault True


cycleDirectionCW : Direction -> Direction
cycleDirectionCW d =
    case d of
        Direction.Right ->
            Direction.Down

        Direction.Down ->
            Direction.Left

        Direction.Left ->
            Direction.Up

        Direction.Up ->
            Direction.Right
