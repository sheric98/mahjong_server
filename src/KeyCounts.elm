module KeyCounts exposing (KeyCounts, empty, get, insert, decrease, toList, member)

{- KeyCounts.elm:

   This file serves as a wrapper module that allows any type
   that can be assosciated with a (int) key as a member in
   a Dict serving counts. This requires a function to convert this arbitrary
   type into an int key as well as an inverse function.
   e.g. This allows me to use Tiles in Dict keeping track of how
   many of each tile we have in it.

   The functions in this module are analogous to the normal
   dict functions except inserting increments the counts and
   removing decrements the count.
-}

import Dict
import KeySet exposing (KeyGen, InvKey)

type KeyCounts v = Count (KeyGen v) (InvKey v) (Dict.Dict Int Int)

empty : KeyGen v -> InvKey v -> KeyCounts v
empty keygen invkey =
    Count keygen invkey (Dict.empty)

-- returns count of item
get : v -> KeyCounts v -> Int
get val (Count keygen _ d) =
    let
        key = keygen val
    in
    case Dict.get key d of
        Nothing -> 0
        Just x -> x

-- checks if an item is an element (key) of the dict
member : v -> KeyCounts v -> Bool
member val (Count keygen _ d) =
    let
        key = keygen val
    in
    Dict.member key d

-- function for use by dictionary. increments counter
increment : Maybe Int -> Maybe Int
increment maybe =
    case maybe of
        -- if nonexistent, update to 1
        Nothing -> Just 1
        -- otherwise, simply increment counter to 1
        (Just x) -> Just (x + 1)

-- function to use when removing. Decreases count by n
decrease_n : Int -> Maybe Int -> Maybe Int
decrease_n n maybe =
    case maybe of
        Nothing -> Nothing
        (Just x) ->
            if x <= n then
                Nothing
            else
                Just (x - n)

-- inserts (increments) element in keycount
insert : v -> KeyCounts v -> {map: KeyCounts v, prev: Int, new: Int}
insert val counts =
    let
        (Count keygen invkey d) = counts
        key = keygen val
        prevVal = get val counts
        newVal = prevVal + 1
        newCounts = Count keygen invkey (Dict.update key increment d)
    in
    {map = newCounts, prev = prevVal, new = newVal}

-- decreases (and potentiall removes) item from keycount
decrease : v -> Int -> KeyCounts v -> {map: KeyCounts v, prev: Int, new: Int}
decrease val n counts =
    let
        (Count keygen invkey d) = counts
        key = keygen val
        prevVal = get val counts
        newVal = max 0 (prevVal - n)
        newCounts = Count keygen invkey (Dict.update key (decrease_n n) d)
    in
    {map = newCounts, prev = prevVal, new = newVal}

-- turns keycount into list of pairs of elements and counts
toList : KeyCounts v -> List (v, Int)
toList (Count _ invkey d) =
    let
        counts = Dict.toList d
    in
    List.map (\t -> (t |> Tuple.first |> invkey, Tuple.second t)) counts
