module Combo exposing (..)

{- Combo.elm:
   
   This file outlines the structure and helper functions
   for a "combo" in Mahjong, which is what the player is
   ultimately trying to form to win. These combos include
   chains, doubles, triples, and quadruples.

   This file also implements ComboSet (set of combo),
   ComboCounts (counting number of each combo),
   and ComboCountsSet, set of combocounts to be used
   so we don't repeat ourselves when checking for a win.
-}

import Chain exposing (Chain(..), chainKey, keyChain, totChains)
import KeyCounts
import KeyCounts exposing (KeyCounts)
import KeySet
import KeySet exposing (KeySet)
import Set
import Tile exposing (Tile(..), NumCategory(..), uniqueTiles, tileKey, keyTile)

-- types of combos. (Chain is three consecutive numerics)
type Combo
    = Chain Chain
    | Double Tile  -- only one double can be used in a combo set
    | Triple Tile
    | Quad Tile

-- set of combos
type alias ComboSet = KeySet Combo

-- mapping of combos to counts
type alias ComboCounts = KeyCounts Combo

-- Combos with associated counts
type alias ComboCountsSet = KeySet ComboCounts

-- total number of possible combos (including doubles)
totCombos = totChains + (3 * uniqueTiles)

-- get the key for a combo
comboKey : Combo -> Int
comboKey combo =
    case combo of
        (Chain c) -> chainKey c
        (Double tile) -> tileKey tile + totChains
        (Triple tile) -> tileKey tile + totChains + uniqueTiles
        (Quad tile) -> tileKey tile + totChains + (2 * uniqueTiles)

-- convert key back to combo
keyCombo : Int -> Combo
keyCombo key =
    if key <= totChains then
        Chain (keyChain key)
    else if key <= totChains + uniqueTiles then
        Double (keyTile (key - totChains))
    else if key <= totChains + (2 * uniqueTiles) then
        Triple (keyTile (key - totChains - uniqueTiles))
    else
        Quad (keyTile (key - totChains - (2 * uniqueTiles)))

emptyComboSet : ComboSet
emptyComboSet = KeySet.empty comboKey keyCombo

-- creates a comboset from a list of combos
comboSetFromList : List Combo -> ComboSet
comboSetFromList combos =
    KeySet.fromList comboKey keyCombo combos

emptyComboCounts : ComboCounts
emptyComboCounts = KeyCounts.empty comboKey keyCombo

comboCountsFromList : List Combo -> ComboCounts
comboCountsFromList combos =
    List.foldl (\c set -> KeyCounts.insert c set |> .map) emptyComboCounts combos

-- tail recursive to generate key for list of combos
comboCountsKey_tr : Int -> Int -> List Combo -> Int
comboCountsKey_tr acc n combos =
    case combos of
        [] -> acc
        combo::rest ->
            if n == 0 then
                acc
            else
                let
                    dig = (comboKey combo) * (totCombos ^ (n - 1))
                in
                comboCountsKey_tr (acc + dig) (n - 1) rest


-- key for combocount
comboCountsKey : ComboCounts -> Int
comboCountsKey comboCounts =
    let
        combos = comboCounts
            |> KeyCounts.toList
            |> List.concatMap (\pair -> List.repeat (Tuple.second pair) (Tuple.first pair))
    in
    -- 5 because we know we will have at most 5 combos
    comboCountsKey_tr 0 5 combos

-- note we don't really use this function other
-- than to satisfy type constraints. So put some arbitrary function here
keyComboCounts : Int -> ComboCounts
keyComboCounts key =
    emptyComboCounts

emptyComboCountsSet : ComboCountsSet
emptyComboCountsSet = KeySet.empty comboCountsKey keyComboCounts

-- A function that will eventually be used to filter out doubles
-- from a list of combos. Returns false if found a Double.
filterOutDouble : Combo -> Bool
filterOutDouble combo =
    case combo of
        Double _ -> False
        _ -> True
