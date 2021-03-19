module Chain exposing (..)

{- Chain.elm:

   This file outlines the structure and helper function
   for a "chain" of three consecutive tiles in Mahjong.
   Some helpers include functions for finding missing
   tiles in chains or mapping a tile to all its possible chains.

   This file also includes type definition for chain
   sets.
-}

import KeySet exposing (..)
import Set
import Tile exposing (..)


-- This represents a chain of three consecutive tiles
-- denoted by the type of tile and the smallest number of the three
type Chain = Chain NumCategory Int
type alias ChainSet = KeySet Chain

-- total number of possible chains
totChains = 21

-- get the list of potential chains for a tile
potentialChains : Tile -> List (Chain)
potentialChains tile =
    case tile of
        Numeric cat x ->
            let
                minStart = max 1 (x - 2)
                -- max start of a chain is 7 for 7-8-9
                maxStart = min x 7
                range = List.range minStart maxStart
            in
            List.map (Chain cat) range
        -- singletiles can't have chains
        Single _ -> []

-- convert a chain into an int for use in a map
chainKey : Chain -> Int
chainKey chain =
    case chain of
        Chain Number x -> x
        Chain Bamboo x -> 7 + x
        Chain Circle x -> 14 + x

-- convet key back into chain
keyChain : Int -> Chain
keyChain key =
    if key <= 7 then
        Chain Number key
    else if key <= 14 then
        Chain Bamboo (key - 7)
    else
        Chain Circle (key - 14)

chainSetFromList : List Chain -> ChainSet
chainSetFromList cs =
    KeySet.fromList chainKey keyChain cs

{- gets the "chi"-able tiles given
   a set of tiles we have and a chain we are trying to fill.
   Assumed that all tiles are in the chain
-}
chiTiles : TileSet -> Chain -> TileSet
chiTiles ts c =
    let
        (Chain cat x) = c
        tiles = toList ts
        nums = Set.fromList (List.range x (x + 2))
        reduced_nums = List.foldl
            (\t s ->
                case t of
                    Numeric _ y -> Set.remove y s
                    -- single should not be encountered in this function
                    Single _ -> Set.empty
            )
            nums
            tiles
        size = Set.size reduced_nums
        single = reduced_nums |> Set.toList |> List.head
    in
    case single of
        -- this means TileSet contains entire chain, so return all
        Nothing -> ts
        Just y ->
            if size == 1 then
                -- two matches. Return missing third
                singletonTileSet (Numeric cat y)
            else
                -- only one or fewer matches. Not chi-able
                emptyTileSet

-- gets the three tiles (in a list) corresponding to a chain
expandChain : Chain -> List Tile
expandChain (Chain cat x) =
    let
        nums = List.range x (x + 2)
    in
    List.map (Numeric cat) nums

-- given two tiles which are assumed to both be part of a chain,
-- get missing third tile
missingTile : Tile -> Tile -> Chain -> Tile
missingTile t1 t2 (Chain cat x) =
    case (t1, t2) of
        (Numeric _ y, Numeric _ z) ->
            let
                nums = Set.fromList (List.range x (x + 2))
                reduced_nums = nums |> Set.remove y |> Set.remove z
                reduced_list = reduced_nums |> Set.toList
                head = List.head reduced_list
            in
            case head of
                Just a ->
                    Numeric cat a
                -- Missing tile should not encounter a branch with no head
                _ -> t1
        -- any other combination should not appear because cannot be in a chain
        -- this branch should not be hit
        _ -> t1

-- given two tiles, gets all tiles that can form a chain
potentialChain : Tile -> Tile -> List (Tile, Chain)
potentialChain t1 t2 =
    let
        c1 = chainSetFromList (potentialChains t1)
        c2 = chainSetFromList (potentialChains t2)
        chains = intersect c1 c2 |> KeySet.toList
        tiles = List.map (\c -> (missingTile t1 t2 c, c)) chains
    in
    tiles

-- get's the chain starting with the tile
tileToChain : Tile -> Maybe Chain
tileToChain tile =
    case tile of
        Numeric c x ->
            if x > 7 then
                Nothing
            else
                Just (Chain c x)
        -- single tiles can't be part of chains
        Single _ -> Nothing