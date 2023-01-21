module Action exposing (direction, isMovement, mapDirection)

import Warrior exposing (Action(..))
import Warrior.Direction exposing (Direction(..))


isMovement : Action -> Bool
isMovement action =
    case action of
        Move _ ->
            True

        _ ->
            False


direction : Action -> Maybe Direction
direction action =
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
