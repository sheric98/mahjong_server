module Tile exposing
    (Tile(..), NumCategory(..), shuffleConvert
    , tileKey, keyTile, totNumerics, uniqueTiles, TileSet
    , emptyTileSet, singletonTileSet, TileCounts
    , emptyTileCounts, tileSetFromList)

{- Tile.elm:

   This file outlines the structure and helper functions
   for a single Tile in mahjong, including the various kinds
   of tiles as well as integer keys for each tile.
   Also defines a set of Tiles called a TileSet.
-}

import KeyCounts
import KeyCounts exposing (KeyCounts)
import KeySet
import KeySet exposing (KeySet)

-- category of numeric tile
type NumCategory
    = Number
    | Bamboo
    | Circle

-- wind tiles
type Wind
    = East
    | South
    | West
    | North

-- dragon tiles
type Dragon
    = Red
    | Green
    | White

-- single tiles are either wind or dragon
type SingleTile
    = W Wind
    | D Dragon

-- tiles are of the form a category with a number
-- number will be from 1-9 inclusive
type Tile
    = Numeric NumCategory Int
    | Single SingleTile

-- tile set are sets of tiles
-- tile counts are a map of tiles to a count
type alias TileSet = KeySet Tile
type alias TileCounts = KeyCounts Tile

-- number of unique numeric tiles
totNumerics = 27
-- number of unique tiles
uniqueTiles = 34

-- convert number (from shuffle) to wind
windConvert : Int -> Wind
windConvert x =
    if x == 0 then
        East
    else if x == 1 then
        South
    else if x == 2 then
        West
    else
        North

-- key of winds
windKey : Wind -> Int
windKey wind =
    case wind of
        East -> 1
        South -> 2
        West -> 3
        North -> 4

-- convert key back into wind
keyWind : Int -> Wind
keyWind x =
    windConvert (x - 1)

-- convert number (from shuffle) to dragon
dragonConvert : Int -> Dragon
dragonConvert x =
    if x == 0 then
        Red
    else if x == 1 then
        Green
    else
        White

-- key of dragons
dragonKey : Dragon -> Int
dragonKey dragon =
    case dragon of
        Red -> 1
        Green -> 2
        White -> 3

-- convert key back into dragon
keyDragon : Int -> Dragon
keyDragon x =
    dragonConvert (x - 1)

-- convert a number from 0..totTiles into a tile
shuffleConvert : Int -> Tile
shuffleConvert x =
    if x < 36 then
        Numeric Number ((x // 4) + 1)
    else if x < 72 then
        Numeric Bamboo (((x - 36) // 4) + 1)
    else if x < 108 then
        Numeric Circle (((x - 72) // 4) + 1)
    else if x < 124 then
        Single (W (windConvert ((x - 108) // 4)))
    else
        Single (D (dragonConvert ((x - 124) // 4)))

-- convert tile to an integer key (for use in dict)
tileKey : Tile -> Int
tileKey tile =
    case tile of
        (Numeric Number x) -> x
        (Numeric Circle x) -> 9 + x
        (Numeric Bamboo x) -> 18 + x
        (Single (W wind)) -> 27 + (windKey wind)
        (Single (D drag)) -> 31 + (dragonKey drag)

-- convert integer key back into tile
keyTile : Int -> Tile
keyTile key =
    if key <= 9 then
        Numeric Number key
    else if key <= 18 then
        Numeric Circle (key - 9)
    else if key <= 27 then
        Numeric Bamboo (key - 18)
    else if key <= 31 then
        Single (W (keyWind (key - 27)))
    else
        Single (D (keyDragon (key - 31)))

emptyTileSet : TileSet
emptyTileSet = KeySet.empty tileKey keyTile

-- make a tileset from a single elemenet
singletonTileSet : Tile -> TileSet
singletonTileSet tile =
    KeySet.singleton tile tileKey keyTile

-- makes a tileset from a list of tiles
tileSetFromList : List Tile -> TileSet
tileSetFromList ts =
    KeySet.fromList tileKey keyTile ts

emptyTileCounts : TileCounts
emptyTileCounts = KeyCounts.empty tileKey keyTile
