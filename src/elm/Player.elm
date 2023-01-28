module Player exposing (takeTurn)

import Action
import Map
import Model
import Warrior exposing (Warrior)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Map exposing (Map)
import Warrior.Map.Tile as Tile


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        model =
            Model.buildModel warrior map history

        interestingNeighbors =
            Map.neighboringTiles warrior map
                |> List.filter (Tuple.second >> Map.greaterThan Tile.Empty)
    in
    List.head interestingNeighbors
        |> Maybe.map (Tuple.first >> Warrior.Move)
        |> Maybe.withDefault (wander warrior history)


wander : Warrior -> History -> Warrior.Action
wander warrior history =
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
