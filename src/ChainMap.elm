module ChainMap exposing (..)

{- ChainMap.elm:
   
   This file is simply a wrapper for a dictionary mapping
   chains to a tileset (set of tiles which belong to this chain that
   the player has in their hand). It is mainly used for a hand to
   keep track of which chains a hand has or is potentially building up
   with the tiles in hand.
-}

import Chain exposing (Chain, chainKey)
import Dict
import Tile exposing (..)
import KeySet

type ChainMap = Map (Dict.Dict Int TileSet)


empty : ChainMap
empty =
    Map (Dict.empty)

-- gets if a chain is in the map
get : Chain -> ChainMap -> TileSet
get c (Map d) =
    let
        key = chainKey c
    in
    case Dict.get key d of
        Nothing -> emptyTileSet
        Just ts -> ts

-- add tile to chain's tileset
addTileChain : Tile -> Maybe TileSet -> Maybe TileSet
addTileChain tile maybe =
    case maybe of
        Nothing -> Just (singletonTileSet tile)
        Just ts -> Just (KeySet.insert tile ts)

-- remove tile from chain's tileset
removeTileChain : Tile -> Maybe TileSet -> Maybe TileSet
removeTileChain tile maybe =
    case maybe of
        Nothing -> Nothing
        Just ts ->
            let
                new_ts = KeySet.remove tile ts
            in
            if KeySet.isEmpty new_ts then
                Nothing
            else
                Just new_ts

-- insert chain into map and corresponding tileset
-- return the new map and prev ts, and new ts
insert : Tile -> Chain -> ChainMap -> {map: ChainMap, prev: TileSet, new: TileSet}
insert t c map =
    let
        (Map d) = map
        key = chainKey c
        prev_ts = get c map
        new_ts = KeySet.insert t prev_ts
        new_map = Map (Dict.update key (addTileChain t) d)
    in
    {map = new_map, prev = prev_ts, new = new_ts}

-- remove a tile and corresponding chain from the map. Returns
-- the new map and prev tiles and new tiles.
remove : Tile -> Chain -> ChainMap -> {map: ChainMap, prev: TileSet, new: TileSet}
remove t c map =
    let
        (Map d) = map
        key = chainKey c
        prev_ts = get c map
        new_ts = KeySet.remove t prev_ts
        new_map = Map (Dict.update key (removeTileChain t) d)
    in
    {map = new_map, prev = prev_ts, new = new_ts}
