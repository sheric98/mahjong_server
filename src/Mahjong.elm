module Mahjong exposing (..)

{- Mahjong.elm

   This file contains most of what is directly called in our
   MVC files. It contains the structure of a hand and functions
   to alter or ascertain the state of the hand, e.g. adding / removing
   tiles to a hand or checking if a hand can make a certain combo.
-}

import Chain exposing (Chain, chiTiles, potentialChains)
import ChainMap
import ChainMap exposing (ChainMap)
import Combo
import Combo exposing (Combo(..), ComboSet, ComboCounts, ComboCountsSet)
import KeyCounts
import KeySet
import Random.List exposing (shuffle)
import Random exposing (Generator)
import Tile exposing (..)
import TileMap exposing (TileMap)
import Updates exposing (..)


-- matched are fixed combos with the number of fixed combos
type alias Matched = (ComboCounts, Int)
type alias Unmatched =
    { tiles: TileCounts
    , tileList: List Tile
    , canPung: TileSet
    , canKong: TileSet
    , hidKong: ComboSet
    , potential: ComboSet
    , canChi: TileCounts
    , canWin: TileMap
    , chainMap: ChainMap
    , usedDouble: Bool
    , numTiles: Int
    , winnable: Maybe ComboCounts
    }

-- a hand consists of an unmatched (hidden) portion
-- and a matched (revealed) portion
type Hand = Hand Unmatched Matched

-- a single change to the hand is a change
type alias Change = Tile -> Hand -> Hand


-- constants -------------------------------------

-- total tiles is 4 of each unique tile
totTiles = uniqueTiles * 4

-- First player essentially draws an extra tile
handSize = 13

--------------------------------------------------


-- Initiallizing Game States ---------------------

-- randomly permutes all possible tiles
tileShuffle : Generator (List Tile)
tileShuffle =
    List.range 0 (totTiles - 1)
        |> shuffle
        |> Random.map (List.map shuffleConvert)

-- initial unmatched
initUnmatched : Unmatched
initUnmatched =
    { tiles = emptyTileCounts
    , tileList = []
    , canPung = emptyTileSet
    , canKong = emptyTileSet
    , hidKong = Combo.emptyComboSet
    , potential = Combo.emptyComboSet
    , canChi = emptyTileCounts
    , canWin = TileMap.empty
    , chainMap = ChainMap.empty
    , usedDouble = False
    , numTiles = 0
    , winnable = Nothing
    }

-- initial matched
initMatched : Matched
initMatched =
    (Combo.emptyComboCounts, 0)

-- initial hand
initHand : Hand
initHand =
    Hand initUnmatched initMatched


-- Functions that make single updates to the Game State ---------------

-- update unmatched and also keep track if the potential combos changed
addTile_aux : Tile -> Unmatched -> Unmatched
addTile_aux tile un =
    let
        newTiles = KeyCounts.insert tile un.tiles
        tileList = newTiles.map
            |> KeyCounts.toList
            |> List.concatMap (\pair -> List.repeat (Tuple.second pair) (Tuple.first pair))
        up = addPungKong tile newTiles.prev newTiles.new un
    in
    if up.changed then
        -- if newly added tile, we should addTileChain
        let
            -- use updated potential set in function call
            (canChi_, chainMap, potential) =
                addTileChains tile un.canChi un.chainMap up.potential
        in
        {un
        | tiles = newTiles.map
        , tileList = tileList
        , canPung = up.pung
        , canKong = up.kong
        , hidKong = up.hidKong
        , potential = potential
        , canChi = canChi_
        , chainMap = chainMap
        , numTiles = un.numTiles + 1
        }
    else
        -- otherwise leave the chi fields the same
        {un
        | tiles = newTiles.map
        , tileList = tileList
        , canPung = up.pung
        , canKong = up.kong
        , hidKong = up.hidKong
        , potential = up.potential
        , numTiles = un.numTiles + 1
        }

-- add tile to a hand
addTile : Tile -> Hand -> Hand
addTile tile (Hand un m) =
    let
        newUn = addTile_aux tile un
    in
    Hand newUn m

-- update unmatched and also keep track if the potential combos changed
removeTiles_aux : Int -> Tile -> Unmatched -> Unmatched
removeTiles_aux n tile un =
    let
        newTiles = KeyCounts.decrease tile n un.tiles
        up = removePungKong tile newTiles.prev newTiles.new un
        tileList = newTiles.map
            |> KeyCounts.toList
            |> List.concatMap (\pair -> List.repeat (Tuple.second pair) (Tuple.first pair))
    in
    if up.changed then
        -- if newly added tile, we should removeTileChain
        let
            -- use updated potential set in funtion call
            (canChi_, chainMap, potential) =
                removeTileChains tile un.canChi un.chainMap up.potential
        in
        {un
        | tiles = newTiles.map
        , tileList = tileList
        , canPung = up.pung
        , canKong = up.kong
        , hidKong = up.hidKong
        , potential = potential
        , canChi = canChi_
        , chainMap = chainMap
        , numTiles = un.numTiles - n
        }
    else
        -- otherwise leave chi fields the same
        {un
        | tiles = newTiles.map
        , tileList = tileList
        , canPung = up.pung
        , canKong = up.kong
        , hidKong = up.hidKong
        , potential = up.potential
        , numTiles = un.numTiles - n
        }

-- remove tile from a hand
removeTiles : Int -> Tile -> Hand -> Hand
removeTiles n tile (Hand un m) =
    let
        newUn = removeTiles_aux n tile un
    in
    Hand newUn m

-- update the unmatched fields when adding a matched combo
unmatchedMatchCombo : Combo -> Unmatched -> Unmatched
unmatchedMatchCombo combo un =
    case combo of
        (Chain c) ->
            let
                tiles = Chain.expandChain c
            in
            List.foldl (removeTiles_aux 1) un tiles
        (Double tile) ->
            let
                new_un = removeTiles_aux 2 tile un
            in
            {new_un | usedDouble = True}
        (Triple tile) ->
            removeTiles_aux 3 tile un
        (Quad tile) ->
            removeTiles_aux 4 tile un

-- update the whole hand to add matched combo
matchCombo : Hand -> Combo -> Hand
matchCombo (Hand un (comboCnt, cnt)) combo =
    let
        new_un = unmatchedMatchCombo combo un
        newComboCnt = KeyCounts.insert combo comboCnt |> .map
        new_match = (newComboCnt, cnt + 1)
    in
    Hand new_un new_match

-- helper for seeing if a hand is winning or one tile off winning
getWinning_tr
    : Hand -> (ComboCountsSet, TileMap, Maybe ComboCounts)
    -> (ComboCountsSet, TileMap, Maybe ComboCounts)
getWinning_tr hand (accSeen, accTiles, accWin) =
    let
        (Hand un (comboCnt, cnt)) = hand
        combos =
            if un.usedDouble then
                KeySet.filter Combo.filterOutDouble un.potential
            else
                un.potential
        -- update the seen combos to include this
        newAccSeen = KeySet.insert comboCnt accSeen
    in
    -- base case when no more potential
    if KeySet.isEmpty combos then
        if cnt == 5 && un.usedDouble && un.numTiles == 0 then
            -- winning hand
            (newAccSeen, accTiles, Just comboCnt)
        -- need 4 combos and one unmatched combo
        else if cnt == 4 then
            if un.numTiles == 1 && not un.usedDouble then
                let
                    head = un.tileList |> List.head
                in
                case head of
                    Just tile ->
                        let
                           winningCombos = comboCnt |> KeyCounts.insert (Double tile) |> .map
                        in
                        (newAccSeen, TileMap.insert tile winningCombos accTiles, accWin)
                    -- we know the list isn't empty since there is one tile left.
                    -- So we have a arbitrary placeholder output here that will never be hit.
                    _ ->
                        (newAccSeen, accTiles, accWin)
            else if un.numTiles == 2 && un.usedDouble then
                let
                    tiles = un.tileList
                in
                case tiles of
                    t1::t2::[] ->
                        if t1 == t2 then
                            -- if two tiles are the same check
                            let
                                winningCombos = comboCnt |> KeyCounts.insert (Triple t1) |> .map
                            in
                            (newAccSeen, TileMap.insert t1 winningCombos accTiles, accWin)
                        else
                            -- check for chain in the other case
                            let
                                winTiles = Chain.potentialChain t1 t2
                                newMap = List.foldl
                                    (\pair map ->
                                        TileMap.insert
                                            (Tuple.first pair)
                                            (comboCnt
                                                |> KeyCounts.insert (Combo.Chain (Tuple.second pair))
                                                |> .map)
                                            map)
                                    accTiles
                                    winTiles
                            in
                            (newAccSeen, newMap, accWin)
                    -- note that there is no other case possible when we know
                    -- we have two tiles left. This branch should never get hit
                    _ ->
                        (newAccSeen, accTiles, accWin)
            else
                (newAccSeen, accTiles, accWin)
        else
            (newAccSeen, accTiles, accWin)
    else
        let
            comboList = KeySet.toList combos
            newHands = List.map (matchCombo hand) comboList
            -- filter out any hands we've already evaluated
            filterHands = List.filter
                (\h ->
                    let
                        (Hand unmatched matched) = h
                        (cc, _) = matched
                    in
                    not (KeySet.member cc accSeen)
                )
                newHands
        in
        List.foldl getWinning_tr (newAccSeen, accTiles, accWin) filterHands

-- given a hand, returns a comboCountsSet, which is not necessary to the user
-- but prevents us repeating recursive checks, a tilemap which contains
-- all tiles that can make a winning hand (mapped to the set of combos making that
-- winning hand), and maybe a combocounts, which is a winning hand if it already exists
getWinning : Hand -> (ComboCountsSet, TileMap, Maybe ComboCounts)
getWinning hand =
    getWinning_tr hand (Combo.emptyComboCountsSet, TileMap.empty, Nothing)

-- update a hand for its winning cards
updateWinning : Hand -> Hand
updateWinning hand =
    let
        (Hand un m) = hand
        (_, canWin, winnable) = getWinning hand
        newUn = {un | canWin = canWin, winnable = winnable}
    in
    (Hand newUn m)

getHand_r : Hand -> List Int -> Hand
getHand_r hand nums =
    case nums of
        [] -> hand
        x::rest ->
            let
                tile = shuffleConvert x
            in
            getHand_r (addTile tile hand) rest

-- makes a hand from a list of shuffled integers
getHand : List Int -> Hand
getHand nums =
    updateWinning (getHand_r initHand nums)

-- drops a tile from a hand
dropTile : Tile -> Hand -> Hand
dropTile tile hand =
    updateWinning (removeTiles 1 tile hand)

-- adds a drawn tile to a hand
drawTile : Tile -> Hand -> Hand
drawTile tile hand =
    updateWinning (addTile tile hand)

-- returns list of sorted tiles
getSortedTiles : Hand -> List Tile
getSortedTiles (Hand un m) =
    List.sortBy tileKey un.tileList

-- gets all the visible combos of a hand
getHandCombos : Hand -> List Combo
getHandCombos (Hand un m) =
    let
        comboCount = Tuple.first m
    in
    comboCount
        |> KeyCounts.toList
        |> List.concatMap (\pair -> List.repeat (Tuple.second pair) (Tuple.first pair))

-- returns -1 for invalid combo. -2 for not containing Chi tile.
-- else return key of valid chain.
confirmChi : Tile -> Tile -> Hand -> Int
confirmChi firstTile chiTile (Hand un _) =
    case Chain.tileToChain firstTile of
        Nothing -> -1
        Just chain ->
            let
                tiles = Chain.expandChain chain
                key = Combo.comboKey (Combo.Chain chain)
            in
            if KeySet.member (Combo.Chain chain) un.potential then
                if List.member chiTile tiles then
                    key
                else
                    -2
            else
                -1

-- return if a given hand can pung a tile
canPung : Tile -> Hand -> Bool
canPung tile (Hand un m) =
    KeySet.member tile un.canPung

-- return if a given hand can kong a tile
canKong : Tile -> Hand -> Bool
canKong tile (Hand un m) =
    KeySet.member tile un.canKong

-- return if a given hand can small kong a tile
-- which is simply checking if they have a revealed pung
canSmallKong : Tile -> Hand -> Bool
canSmallKong tile (Hand _ (comboCounts, _)) =
    let
        checkTriple = Triple tile
    in
    KeyCounts.member checkTriple comboCounts

-- update a hand to reflect a small kong, i.e replace the triple
-- with a quad. Assumes this action is valid
smallKongHand : Tile -> Hand -> Hand
smallKongHand tile (Hand un (combos, cnt)) =
    let
        toRemove = Triple tile
        toAdd = Quad tile
        newCombos = combos
            |> KeyCounts.decrease toRemove 1
            |> .map
            |> KeyCounts.insert toAdd
            |> .map
        newUn = removeTiles_aux 1 tile un
    in
    (Hand newUn (newCombos, cnt))

-- return if a hand can hidden kong (has four of the same tile)
canHidKong : Hand -> Bool
canHidKong (Hand un _) =
    un.hidKong /= Combo.emptyComboSet

-- return if a hand can make a hidden kong using a certain tile
canTileHidKong : Tile -> Hand -> Bool
canTileHidKong tile (Hand un _) =
    let
        check = Quad tile
    in
    KeySet.member check un.hidKong

-- return if a given hand can chi a tile
canChi : Tile -> Hand -> Bool
canChi tile (Hand un m) =
    KeyCounts.member tile un.canChi

-- Returns the Tiles that you can select
-- as the lowest member of a ChiChain
chiLowest : Tile -> Hand -> TileSet
chiLowest tile (Hand un m) =
    let
        chains = potentialChains tile |> List.map Combo.Chain |> Combo.comboSetFromList
        valid = KeySet.intersect chains un.potential |> KeySet.toList
        tiles = List.map
            (\combo ->
                case combo of
                    Combo.Chain c ->
                        let
                            ts = Chain.expandChain c
                            head = List.head ts
                        in
                        case head of
                            Just t -> t
                            -- expand chain will never return an empty list.
                            -- This is an arbitrary placeholder tile that will never be hit.
                            Nothing -> Tile.keyTile 0
                    -- Likewise, the aforementioned intersection should only produce chains.
                    -- So once again, we place an arbitrary output tile here
                    _ -> Tile.keyTile 0
            )
            valid
    in
    Tile.tileSetFromList tiles

-- get all hidden kongs a hand has
getHidKongs : Hand -> TileSet
getHidKongs (Hand un m) =
    let
        combos = un.hidKong |> KeySet.toList
        tiles = List.map
            (\combo ->
                case combo of
                    Quad t -> t
                    -- Hid kong should only have Quads
                    -- We place arbitrary tile here since this will never be hit.
                    _ -> Tile.keyTile 0
            )
            combos
    in
    Tile.tileSetFromList tiles

-- return if a given hand can Hu. If it can, then
-- return the list of combos of the winning hand
canHu : Tile -> Hand -> Maybe ComboCounts
canHu tile (Hand un m) =
    TileMap.get tile un.canWin

-- return if a hand has drawn itself into a winning state
alreadyHu : Hand -> Maybe ComboCounts
alreadyHu (Hand un m) =
    un.winnable

-- adds a combo to a hand
addCombo : Combo -> Hand -> Hand
addCombo combo hand =
    matchCombo hand combo

-- turns a hand into two lists of int keys
-- first is the unmatched tiles, and second is matched tiles
handToLists : Hand -> (List Int, List Int)
handToLists (Hand un (comboCnts, cnt)) =
    let
        unKeys = List.map Tile.tileKey un.tileList
        matchKeys = comboCnts
            |> KeyCounts.toList
            |> List.concatMap (\pair -> List.repeat (Tuple.second pair) (Tuple.first pair))
            |> List.map Combo.comboKey
    in
    (unKeys, matchKeys)

-- using ComboCounts, change hand to this winning set of combos
makeWinningHand : ComboCounts -> Hand
makeWinningHand combos =
    let
        un = initUnmatched
        -- 5 since winning hand always has 5 combos
        matched = (combos, 5)
    in
    Hand un matched