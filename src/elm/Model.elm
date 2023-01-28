module Model exposing (Model, buildModel, nextAction)

import Dict exposing (Dict)
import Set exposing (Set)
import Warrior exposing (Action, Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction
import Warrior.History as History exposing (History)
import Warrior.Item
import Warrior.Map exposing (Map)
import Warrior.Map.Tile as Tile



-- DICT UTILS


updateWithDefault : comparable -> (Maybe v -> Maybe v) -> v -> Dict comparable v -> Dict comparable v
updateWithDefault key fn default dict =
    if Dict.member key dict then
        Dict.update key fn dict

    else
        Dict.insert key default dict



-- TILE UTILS


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



-- COMPARABLE COORDINATE
-- Needed to be the keys of the Set and Dict


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



-- CELL
-- Represents a Tile's discovered status


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


type alias Node a =
    ( a, List ( ComparableCoordinate, a ) )


type alias Graph a =
    Dict ComparableCoordinate (Node a)


type Model
    = Model ModelInternals


type alias ModelInternals =
    { entities : Dict String (Set ComparableCoordinate)
    , graph : Graph Cell
    }


buildModel : Warrior -> Map -> History -> Model
buildModel warrior map history =
    let
        initialGrid =
            Model { entities = Dict.empty, graph = Dict.empty }
    in
    ( warrior, map )
        :: History.previousStates warrior history
        |> List.foldr
            (\( hWarrior, hMap ) acc ->
                visibleTiles hWarrior hMap
                    |> List.foldl discoverCell acc
            )
            initialGrid


nextAction : Model -> Action
nextAction (Model model) =
    Warrior.Wait


discoverCell : ( ComparableCoordinate, Tile.Tile ) -> Model -> Model
discoverCell ( coord, tile ) (Model model) =
    let
        updatedEntities =
            tileToString tile
                |> Maybe.map
                    (\entity ->
                        updateWithDefault
                            entity
                            (Maybe.map <| Set.insert coord)
                            (Set.singleton coord)
                            model.entities
                    )
                |> Maybe.withDefault model.entities

        neighboringCells =
            neighboringCoordinates coord
                |> List.map
                    (\c ->
                        Dict.get c model.graph
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
                model.graph
    in
    Model
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
