module TileMap exposing (TileMap, empty, get, insert, member)

{- TileMap.elm:

   This file is simply a wrapper for a dictionary mapping
   tiles to a count of combos (A winning hand). Simply used
   to keep track of which tiles can win and what the
   corresponding winning hand is.
-}

import Dict
import Tile exposing (Tile, tileKey, keyTile)
import Combo exposing (ComboCounts)

type TileMap = Map (Dict.Dict Int ComboCounts)

empty : TileMap
empty =
    Map (Dict.empty)

-- returns value
get : Tile -> TileMap -> Maybe ComboCounts
get t (Map d) =
    let
        key = tileKey t
    in
    Dict.get key d

-- checks if a tile is in the map
member : Tile -> TileMap -> Bool
member t (Map d) =
    let
        key = tileKey t
    in
    Dict.member key d

-- inserts a tile into the map
insert : Tile -> ComboCounts -> TileMap -> TileMap
insert tile counts (Map d) =
    let
        key = tileKey tile
        new_d = Dict.insert key counts d
    in
    Map new_d
