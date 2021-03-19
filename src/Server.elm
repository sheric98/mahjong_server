module Server exposing (..)

{- Server.elm

   This file wraps handles special types for specific responses
   from the elm server, as well as holding the information for
   how to store game state and player information (what tiles to hide
   and which tiles to display).
   This file also defines the types for the different game states and
   actions.
-}

import Array exposing (Array)
import Combo exposing (Combo(..))
import Mahjong exposing (Hand, getHand, handSize, totTiles)
import Tile exposing (Tile)

-- infor about other players
type alias PlayerInfo = Array (
    { hidden: Int
    , tiles: List Tile
    , combos: List Combo })

-- the various states the game can be in
type State
    = SelectDrop
    | SelectChi
    | SelectKong
    | Actionable
    | MustDraw
    | Over

-- describing the actions of the previous move
type Action
    = Pung
    | Kong
    | Chi
    | Hu
    | Draw
    | Drop
    | Restart

-- total game state that should be consistent with server
type alias Game =
    { player: Int
    , hand: Hand
    , info: PlayerInfo
    , remaining: Int
    , dropped: Maybe Tile
    , drawn: Maybe Tile
    , turn: Int
    , state: State
    , prevAction: Maybe Action
    , errorMsg: Maybe String
    }

-- return from server of random deal. Returns hand and starting player
type alias DealRet =
    { hands : (Array (List Int))
    , starting : Int
    }

-- return from server of a combo. Returns the key of the combo and the player
-- who issued the combo
type alias ComboRet =
    { key : Int
    , player : Int
    }

-- return from a server for information on a player's hand
type alias HandRet =
    { tiles : List Int
    , combos : List Int
    }

type alias GameLobby =
    { id : Int
    , players : Int
    }

-- the model to use in our MVC model
type alias Model =
    { player: Maybe Int
    , game: Maybe Game
    , lobbies: List GameLobby
    , lobbyStatus: Maybe String
    }

-- different types of messages we can receive from the server (firebase)
type Receive
    = Dropped Int
    | Drew Int
    | Comboed Int Int
    | WillChi
    | WillKong
    | Win Int
    | SmallKong Int
    | Hands (List HandRet)
    | Lobbies (List GameLobby)
    | NoCreate
    | Terminate

-- the Message type we use in our MVC model
type Msg
    = Player Int
    | Deal DealRet
    | Reset Bool -- true for client, false for server
    | TileAction Tile
    | Rcv Receive -- received a response from server
    | Button Action
    | CreateGame
    | JoinGame Int -- id of game

-- initially, player info should be all hand size
-- except starting player has one additional
initPlayerInfo : Int -> PlayerInfo
initPlayerInfo starting =
    let
        initInfo = { hidden = handSize
            , tiles = []
            , combos = []
            }
        initStarting = { initInfo
            | hidden = handSize + 1
            }
        initial = Array.repeat 4 initInfo
    in
    Array.set starting initStarting initial

-- initial game for when a new game starts.
-- requires information on player's hand as well as starting player.
initGame : Int -> List Int -> Int -> Game
initGame player nums starting =
    let
        newHand = getHand nums
        newInfo = initPlayerInfo starting
        remaining = totTiles - (4 * handSize) - 1
    in
    { player = player
    , hand = newHand
    , info = newInfo
    , remaining = remaining
    , dropped = Nothing
    , drawn = Nothing
    , turn = starting
    , state = SelectDrop
    , prevAction = Nothing
    , errorMsg = Nothing
    }

-- update playerinfo for a player after dropping a card
dropTileInfo : Int -> PlayerInfo -> PlayerInfo
dropTileInfo player info =
    let
        maybeCurrInfo = Array.get player info
    in
    case maybeCurrInfo of
        Just currInfo ->
            let
                newInfo = { currInfo
                    | hidden = max 0 (currInfo.hidden - 1)
                    }
            in
            Array.set player newInfo info
        Nothing -> info

-- update playerInfo to add a tile
addTileInfo : Int -> PlayerInfo -> PlayerInfo
addTileInfo player info =
    let
        maybeCurrInfo = Array.get player info
    in
    case maybeCurrInfo of
        Just currInfo ->
            let
                newInfo = { currInfo
                    | hidden = currInfo.hidden + 1
                    }
            in
            Array.set player newInfo info
        Nothing -> info

-- update playerInfo to add a combo and remove hidden tiles
addComboInfo : Int -> Combo -> PlayerInfo -> PlayerInfo
addComboInfo player combo info =
    let
        maybeCurrInfo = Array.get player info
    in
    case maybeCurrInfo of
        Just currInfo ->
            let
                newCombos = combo::(currInfo.combos)
                removing =
                    case combo of
                        Chain _ -> 2
                        Double _ -> 1
                        Triple _ -> 2
                        Quad _ -> 3
                newHidden = max 0 (currInfo.hidden - removing)
                newInfo = { currInfo
                    | hidden = newHidden
                    , combos = newCombos
                    }
            in
            Array.set player newInfo info
        Nothing -> info

-- changes the triple in a list to a quad
upgradeTriple_tr : List Combo -> Tile -> List Combo -> List Combo
upgradeTriple_tr acc tile combos =
    case combos of
        [] -> acc
        combo::rest ->
            if combo == Triple tile then
                -- swap out tile. Rest of list will be the same
                rest ++ ((Quad tile)::acc)
            else
                upgradeTriple_tr (combo::acc) tile rest

-- changes the triple in a list to a quad using the tail recursive helper
upgradeTriple : Tile -> List Combo -> List Combo
upgradeTriple tile combos =
    upgradeTriple_tr [] tile combos

-- update info for when a player small kongs (Changes triple to quad)
smallKongInfo : Int -> Tile -> PlayerInfo -> PlayerInfo
smallKongInfo player tile info =
    let
        maybeCurrInfo = Array.get player info
    in
    case maybeCurrInfo of
        Just currInfo ->
            let
                newCombos = upgradeTriple tile currInfo.combos
                newInfo = { currInfo
                    | hidden = currInfo.hidden - 1
                    , combos = newCombos
                    }
            in
            Array.set player newInfo info
        Nothing -> info

-- update info given list of display information of all players
handsInfo : List HandRet -> PlayerInfo -> PlayerInfo
handsInfo handrets info =
    let
        handsInfo_tr idx hands info_acc =
            if idx == 4 then
                info_acc
            else
                case hands of
                    [] -> info_acc
                    hand::rest ->
                        let
                            newInfo = { hidden = 0
                                , tiles = List.map Tile.keyTile hand.tiles
                                , combos = List.map Combo.keyCombo hand.combos
                                }
                            newArr = Array.set idx newInfo info_acc
                        in
                        handsInfo_tr (idx + 1) rest newArr
    in
    handsInfo_tr 0 handrets info
