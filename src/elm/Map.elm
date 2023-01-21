module Map exposing (compareTile, greaterThan, lessThan, neighboringTiles)

import Warrior exposing (Warrior)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile exposing (Tile(..))


neighboringTiles : Warrior -> Map -> List ( Direction, Tile )
neighboringTiles warrior map =
    let
        lookInDirection d =
            Map.look d warrior map
    in
    List.filterMap
        (\direction ->
            lookInDirection direction
                |> List.head
                |> Maybe.map (Tuple.mapFirst (always direction))
        )
        Direction.all



{-
   Ordering:
   Exit > Warrior = Item > Empty = SpawnPoint > Wall
-}


greaterThan : Tile -> Tile -> Bool
greaterThan a b =
    compareTile a b |> (==) GT


lessThan : Tile -> Tile -> Bool
lessThan a b =
    compareTile a b |> (==) LT


compareTile : Tile -> Tile -> Order
compareTile a b =
    case ( a, b ) of
        ( Exit, _ ) ->
            LT

        ( Warrior _, Exit ) ->
            GT

        ( Warrior _, Item _ ) ->
            EQ

        ( Warrior _, Warrior _ ) ->
            EQ

        ( Warrior _, _ ) ->
            LT

        ( Item _, Exit ) ->
            GT

        ( Item _, Item _ ) ->
            EQ

        ( Item _, Warrior _ ) ->
            EQ

        ( Item _, _ ) ->
            LT

        ( Empty, Wall ) ->
            LT

        ( Empty, Empty ) ->
            EQ

        ( Empty, SpawnPoint ) ->
            EQ

        ( Empty, _ ) ->
            GT

        ( SpawnPoint, Wall ) ->
            LT

        ( SpawnPoint, Empty ) ->
            EQ

        ( SpawnPoint, SpawnPoint ) ->
            EQ

        ( SpawnPoint, _ ) ->
            GT

        ( Wall, _ ) ->
            GT
