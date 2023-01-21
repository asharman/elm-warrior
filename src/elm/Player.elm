module Player exposing (takeTurn)

import Warrior exposing (Warrior)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Map exposing (Map)


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        currentHeading =
            History.previousActions warrior history
                |> List.filter isMovement
                |> (List.head >> Maybe.andThen actionDirection)
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


isMovement : Warrior.Action -> Bool
isMovement action =
    case action of
        Warrior.Move _ ->
            True

        _ ->
            False


actionDirection : Warrior.Action -> Maybe Direction
actionDirection action =
    case action of
        Warrior.Move dir ->
            Just dir

        Warrior.Attack dir ->
            Just dir

        _ ->
            Nothing


mapDirection : (Direction -> Direction) -> Warrior.Action -> Warrior.Action
mapDirection fn action =
    case action of
        Warrior.Move dir ->
            Warrior.Move (fn dir)

        Warrior.Attack dir ->
            Warrior.Attack (fn dir)

        _ ->
            action


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
