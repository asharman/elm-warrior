module Player exposing (takeTurn)

import Action
import Dict exposing (Dict)
import Map
import Set exposing (Set)
import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.History as History exposing (History)
import Warrior.Item
import Warrior.Map exposing (Map)
import Warrior.Map.Tile as Tile


tileToString : Tile.Tile -> Maybe String
tileToString tile =
    case tile of
        Tile.Item item ->
            Just (Warrior.Item.toString item)

        Tile.Warrior warrior ->
            Just warrior

        Tile.Exit ->
            Just "exit"

        Tile.SpawnPoint ->
            Just "spawnpoint"

        Tile.Empty ->
            Nothing

        Tile.Wall ->
            Nothing


type alias Graph a =
    Dict ComparableCoordinate (Node a)


type alias Node a =
    ( a, List ( ComparableCoordinate, a ) )


type Cell
    = Undiscovered
    | Discovered Tile.Tile


toTile : Cell -> Maybe Tile.Tile
toTile cell =
    case cell of
        Discovered tile ->
            Just tile

        Undiscovered ->
            Nothing


type alias ComparableCoordinate =
    ( Int, Int )


toComparable : Coordinate -> ComparableCoordinate
toComparable { x, y } =
    ( x, y )


neighboringCoordinates : ComparableCoordinate -> List ComparableCoordinate
neighboringCoordinates ( x, y ) =
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


type alias Grid =
    { entities : Dict String (Set ComparableCoordinate)
    , graph : Graph Cell
    }


visibleTiles : Warrior -> Map -> List ( ComparableCoordinate, Tile.Tile )
visibleTiles warrior map =
    let
        belowFeet =
            ( toComparable (Warrior.position warrior)
            , Warrior.Map.lookDown warrior map
            )

        cellsInDirection d =
            Warrior.Map.look d warrior map
                |> List.map (Tuple.mapFirst toComparable)
    in
    belowFeet :: List.concatMap cellsInDirection Direction.all


updateWithDefault : comparable -> (Maybe v -> Maybe v) -> v -> Dict comparable v -> Dict comparable v
updateWithDefault key fn default dict =
    if Dict.member key dict then
        Dict.update key fn dict

    else
        Dict.insert key default dict


discoverCell : ( ComparableCoordinate, Tile.Tile ) -> Grid -> Grid
discoverCell ( coord, tile ) grid =
    let
        updatedEntities =
            tileToString tile
                |> Maybe.map
                    (\entity ->
                        updateWithDefault
                            entity
                            (Maybe.map <| Set.insert coord)
                            (Set.singleton coord)
                            grid.entities
                    )
                |> Maybe.withDefault grid.entities

        neighboringCells =
            neighboringCoordinates coord
                |> List.map
                    (\c ->
                        Dict.get c grid.graph
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault Undiscovered
                            |> Tuple.pair c
                    )

        updatedGraph =
            Dict.insert coord
                ( Discovered tile
                , if not (Tile.isWall tile) then
                    neighboringCells
                        |> List.filter
                            (Tuple.second
                                >> toTile
                                >> Maybe.map (not << Tile.isWall)
                                >> Maybe.withDefault True
                            )

                  else
                    []
                )
                grid.graph
    in
    { graph =
        List.foldl
            (\( c, _ ) g ->
                Dict.update c
                    (Maybe.map
                        (Tuple.mapSecond
                            (List.filterMap
                                (\( c2, cell2 ) ->
                                    if coord == c2 then
                                        if not (Tile.isWall tile) then
                                            Just ( coord, Discovered tile )

                                        else
                                            Nothing

                                    else
                                        Just ( c2, cell2 )
                                )
                            )
                        )
                    )
                    g
            )
            updatedGraph
            neighboringCells
    , entities = updatedEntities
    }


buildGrid : Warrior -> Map -> History -> Grid
buildGrid warrior map history =
    let
        initialGrid =
            { entities = Dict.empty, graph = Dict.empty }
    in
    ( warrior, map )
        :: History.previousStates warrior history
        |> List.foldr
            (\( hWarrior, hMap ) acc ->
                visibleTiles hWarrior hMap
                    |> List.foldl discoverCell acc
            )
            initialGrid


takeTurn : Warrior -> Map -> History -> Warrior.Action
takeTurn warrior map history =
    let
        grid =
            buildGrid warrior map history |> Debug.log "Grid"

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
