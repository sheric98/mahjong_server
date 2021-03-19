module Updates exposing
    (addTileChains, removeTileChains
    , addPungKong, removePungKong)

{- Updates.elm:

   This file is simply a helper file to Mahjong
   which updates the internal states of a hand
   when you add or drop a tile. This file helps
   keep track of which tiles a hand can chi, kong,
   pung, hu, etc. when we add or remove a tile.
-}

import Combo exposing (ComboSet, Combo(..))
import Chain exposing (Chain, chiTiles, potentialChains)
import ChainMap
import ChainMap exposing (ChainMap)
import KeyCounts
import KeySet
import Tile exposing (Tile, TileCounts, TileSet)

-- the fields of Unmatched that matter for this file
type alias PungKongFields a =
    {a
    | canPung: TileSet
    , canKong: TileSet
    , hidKong: ComboSet
    , potential: ComboSet}
type alias PungKongRet =
    {pung: TileSet
    , kong: TileSet
    , potential: ComboSet
    , hidKong: ComboSet
    , changed: Bool}

{- prev and new are from chains NOT chi-able directly
   returns the updated canChi, potential
-}
chiAdd
    : Chain -> TileSet -> TileSet -> TileCounts -> ComboSet
    -> (TileCounts, ComboSet)
chiAdd c prev new canChi potential =
    let
        prevChi = chiTiles prev c
        prevSize = KeySet.size prevChi
        newChi = chiTiles new c
        newSize = KeySet.size newChi
        diff = KeySet.diff newChi prevChi
        tiles = KeySet.toList diff
        newPotential =
            -- new full chain
            if newSize == 3 && prevSize < 3 then
                KeySet.insert (Chain c) potential
            else
                potential
    in
    -- add all new "chi"-able tiles to the canChi counts
    (List.foldl
    (\tile cnts ->
        KeyCounts.insert tile cnts |> .map)
    canChi
    tiles, newPotential)

-- analogous to chiAdd but for when removing a tile
chiRemove
    : Chain -> TileSet -> TileSet -> TileCounts -> ComboSet
    -> (TileCounts, ComboSet)
chiRemove c prev new canChi potential =
    let
        prevChi = chiTiles prev c
        prevSize = KeySet.size prevChi
        newChi = chiTiles new c
        newSize = KeySet.size newChi
        diff = KeySet.diff prevChi newChi
        tiles = KeySet.toList diff
        newPotential =
            -- lost a chain
            if prevSize == 3 && newSize < 3 then
                KeySet.remove (Chain c) potential
            else
                potential
    in
    (List.foldl
    (\tile cnts ->
        KeyCounts.decrease tile 1 cnts |> .map)
    canChi
    tiles, newPotential)

-- update the chiCounts and chain map for a single tile, chain pair
addSingleChain
    : Tile -> Chain -> (TileCounts, ChainMap, ComboSet)
    -> (TileCounts, ChainMap, ComboSet)
addSingleChain tile c (cnts, map, potential) =
    let
        mapRet = ChainMap.insert tile c map
        (updateCnts, updatePot) = chiAdd c mapRet.prev mapRet.new cnts potential
    in
    (updateCnts, mapRet.map, updatePot)

-- update the chiCounts and chain map for removing a tile, chain pair
removeSingleChain
    : Tile -> Chain -> (TileCounts, ChainMap, ComboSet)
    -> (TileCounts, ChainMap, ComboSet)
removeSingleChain tile c (cnts, map, potential) =
    let
        mapRet = ChainMap.remove tile c map
        (updateCnts, updatePot) = chiRemove c mapRet.prev mapRet.new cnts potential
    in
    (updateCnts, mapRet.map, updatePot)

addTileChains
    : Tile -> TileCounts -> ChainMap -> ComboSet
    -> (TileCounts, ChainMap, ComboSet)
addTileChains tile cnts map potential =
    let
        chains = potentialChains tile
        triple = (cnts, map, potential)
    in
    List.foldl (addSingleChain tile) triple chains

removeTileChains
    : Tile -> TileCounts -> ChainMap -> ComboSet
    -> (TileCounts, ChainMap, ComboSet)
removeTileChains tile cnts map potential =
    let
        chains = potentialChains tile
        triple = (cnts, map, potential)
    in
    List.foldl (removeSingleChain tile) triple chains

-- update the canPung canKong given a new tile and its counts
addPungKong
    : Tile -> Int -> Int -> PungKongFields a
    -> PungKongRet
addPungKong tile _ new un =
    if new == 2 then
        -- can pung now
        let
            newPung = KeySet.insert tile un.canPung
            -- also new double
            newDouble = Double tile
            newPot = KeySet.insert newDouble un.potential
        in
        {pung = newPung
        , kong = un.canKong
        , potential = newPot
        , hidKong = un.hidKong
        , changed = False}
    else if new == 3 then
        -- can kong now
        let
            newKong = KeySet.insert tile un.canKong
            newTriple = Triple tile
            newPot = KeySet.insert newTriple un.potential
        in
        {pung = un.canPung
        , kong = newKong
        , hidKong = un.hidKong
        , potential = newPot
        , changed = False}
    else if new == 4 then
        let
            -- new hidden kong
            newQuad = Quad tile
            newHidKong = KeySet.insert newQuad un.hidKong
        in
        {pung = un.canPung
        , kong = un.canKong
        , hidKong = newHidKong
        , potential = un.potential
        , changed = False}
    else
        {pung = un.canPung
        , kong = un.canKong
        , hidKong = un.hidKong
        , potential = un.potential
        , changed = True}

-- update the canPung canKong after removing a tile given its new counts
removePungKong
    : Tile -> Int -> Int -> PungKongFields a
    -> PungKongRet
removePungKong tile prev new un =
    let
        newPung =
            -- previously can pung but now can't
            if prev >= 2 && new < 2 then
                KeySet.remove tile un.canPung
            else
                un.canPung
        newKong =
            -- previously can kong but now can't
            if prev >= 3 && new < 3 then
                KeySet.remove tile un.canKong
            else
                un.canKong
        hidKong = 
            if prev == 4 && new < 4 then
                KeySet.remove (Quad tile) un.hidKong
            else
                un.hidKong
        potential =
            -- if previously started with a triple and more
            if prev >= 3 then
                if new == 3 then
                    un.potential
                else if new == 2 then
                    KeySet.remove (Triple tile) un.potential
                else 
                    un.potential
                        |> KeySet.remove (Triple tile)
                        |> KeySet.remove (Double tile)
            -- only started with double
            else if prev == 2 then
                KeySet.remove (Double tile) un.potential
            -- started with none
            else
                un.potential
        -- new counts is 0, so it is removed
        removed = new == 0
    in
    {pung = newPung
    , kong = newKong
    , hidKong = hidKong
    , potential = potential
    , changed = removed}