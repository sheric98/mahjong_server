port module Main exposing (main)

{- Main.elm:

   This file serves as the connection between client and server logic incoporating
   Elm's MVC as well as the ports to socket. It handles all
   requests that a user makes, including button presses, and responses from servers
   and updates the player's game state accordingly. It also has contains the logic
   for when certain actions available for a player to do, otherwise updating
   the model to contain an error message to display to the user.
-}

import Array exposing (..)
import Browser
import Browser.Events
import Html exposing (Html)

import Mahjong exposing (Hand, getHand, confirmChi, dropTile, addTile, addCombo)
import Combo exposing (Combo(..), keyCombo)
import Display exposing (page)
import Lobby exposing (lobbyPage)
import Server exposing
    ( DealRet, Model, Msg(..), Action(..), Receive(..), State(..)
    , initGame, dropTileInfo, addTileInfo, addComboInfo, ComboRet
    , smallKongInfo, HandRet, handsInfo, GameLobby, ComboSend
    , HuRet )
import Tile exposing (Tile, tileKey, keyTile, shuffleConvert)

-- Ports

port socketJoin : () -> Cmd msg
port socketJoinRes : (Int -> msg) -> Sub msg
port socketStart : (DealRet -> msg) -> Sub msg
port socketDraw : Int -> Cmd msg
port socketDrawRcv : (Int -> msg) -> Sub msg
port socketDrop : Int -> Cmd msg
port socketDropRcv : (Int -> msg) -> Sub msg
port socketChi : Int -> Cmd msg
port socketChiRcv : (() -> msg) -> Sub msg
port socketCombo : ComboSend -> Cmd msg
port socketComboRcv : (ComboRet -> msg) -> Sub msg
port socketHu : (Int, List Int) -> Cmd msg
port socketHuRcv : (HuRet -> msg) -> Sub msg
port socketSendHand : (Int, List Int, List Int) -> Cmd msg
port socketHandRcv : (List HandRet -> msg) -> Sub msg
port socketSmallKong : Int -> Cmd msg
port socketSmallKongRcv : (Int -> msg) -> Sub msg
port socketHidKong : Int -> Cmd msg
port socketHidKongRcv : (() -> msg) -> Sub msg
port socketReset : () -> Cmd msg
port socketResetAll : (Bool -> msg) -> Sub msg
port socketRestart : () -> Cmd msg
port socketUpdateLobbies : () -> Cmd msg
port socketLobbiesRcv : (List GameLobby -> msg) -> Sub msg 
port socketCreateGame : () -> Cmd msg
port socketNoCreate : (() -> msg) -> Sub msg
port socketJoinGame : Int -> Cmd msg
port socketTerminated : (() -> msg) -> Sub msg

-- Init

initModel : Model
initModel =
    { player = Nothing, game = Nothing, lobbies = [], lobbyStatus = Nothing }

init : () -> (Model, Cmd Msg)
init _ =
    ( initModel
    , socketUpdateLobbies ()
    )

-- Update

-- Following are all helper functions for how to update the server -----

-- returns the default return when we want no change
defaultRet : Model -> (Model, Cmd Msg)
defaultRet model =
    ( model, Cmd.none )

-- creates new model and cmd pair for an error status
errorRet : String -> Model -> (Model, Cmd Msg)
errorRet str model =
    case model.game of
        Just game ->
            let
                newGame = { game
                    | errorMsg = Just str
                    }
            in
            ( { model | game = Just newGame }, Cmd.none )
        Nothing -> ( model, Cmd.none )

-- takes the return of a deal from the server and creates a new hand
getDeal : DealRet -> Model -> (Model, Cmd Msg)
getDeal dealRet model =
    case model.player of
        Just x ->
            case get x dealRet.hands of
                Nothing ->
                    ( model, Cmd.none )
                Just nums ->
                    let
                        newGame = initGame x nums dealRet.starting
                    in
                    ( { model | game = Just newGame }, Cmd.none )
        Nothing ->
            defaultRet model
        
-- logic for when a tile is clicked
tileClick : Tile -> Model -> (Model, Cmd Msg)
tileClick tile model =
    case model.game of
        Just game ->
            case game.state of
                SelectDrop ->
                    -- simply send drop message if we are in drop state
                    ( model, socketDrop (tileKey tile) )
                SelectChi ->
                    -- dropped tile is tile we are trying to chi
                    case game.dropped of
                        Just chiTile ->
                            let
                                chiRet = confirmChi tile chiTile game.hand
                            in
                            if chiRet == -1 then
                                errorRet "Select bottom tile of valid Chi chain" model
                            else if chiRet == -2 then
                                errorRet "Chi chain must contain tile you Chi" model
                            else
                                let
                                    send = { key = chiRet
                                        , player = game.player
                                        , is_kong = False
                                        , add_tile = False
                                        }
                                in
                                -- valid chi select
                                ( model, socketCombo send )
                        Nothing -> defaultRet model
                SelectKong ->
                    -- check if we can kong the selected tile
                    if Mahjong.canTileHidKong tile game.hand then
                        let
                            combo = Combo.Quad tile
                            key = Combo.comboKey combo
                            send = { key = key
                                , player = game.player
                                , is_kong = True
                                , add_tile = False
                                }
                        in
                        ( model, socketCombo send )
                    else
                        errorRet "Can't concealed Kong on that tile" model

                -- shouldn't be able to click on tile in any other game state
                _ ->
                    defaultRet model
        Nothing ->
            defaultRet model

-- receives the response from server (int of tile) of a drop
dropRcv : Int -> Model -> (Model, Cmd Msg)
dropRcv num model =
    case model.game of
        Just game ->
            let
                newTurn = modBy 4 (game.turn + 1)
                newState = Actionable
                newInfo = dropTileInfo game.turn game.info
                tile = keyTile num
                -- update for other players
                otherGame = { game
                    | info = newInfo
                    , dropped = Just tile
                    , turn = newTurn
                    , state = newState
                    , prevAction = Just Drop
                    , errorMsg = Nothing
                    }
            in
            if game.turn == game.player then
                let
                    newHand = dropTile tile game.hand
                    newGame = { otherGame
                        | hand = newHand
                        , drawn = Nothing
                        }
                in
                ( { model | game = Just newGame }, Cmd.none )
            else
                ( { model | game = Just otherGame }, Cmd.none )
        Nothing -> defaultRet model

-- receives the response from server (int of tile) of a new tile drawn
drewRcv : Int -> Model -> (Model, Cmd Msg)
drewRcv num model =
    case model.game of
        Just game ->
            if num < 0 then
                -- this means that ther is no more cards left
                -- issue draw to all players
                let 
                    newState = Over
                    newRemaining = 0
                    newGame = { game
                        | remaining = newRemaining
                        , state = newState
                        , prevAction = Just Draw
                        , errorMsg = Nothing
                        }
                    (tiles, combos) = Mahjong.handToLists game.hand
                in
                -- send command to reveal all hands
                ( { model | game = Just newGame }
                , socketSendHand (game.player, tiles, combos) )
            else
                let
                    newState = SelectDrop
                    newInfo = addTileInfo game.turn game.info
                    newRemaining = game.remaining - 1
                    otherGame = { game
                        | info = newInfo
                        , remaining = newRemaining
                        , state = newState
                        , prevAction = Just Draw
                        , errorMsg = Nothing
                        }
                in
                if game.turn == game.player then
                    let
                        tile = shuffleConvert num
                        newHand = Mahjong.drawTile tile game.hand
                        newGame = { otherGame
                            | hand = newHand
                            , drawn = Just tile
                            }
                    in
                    ( { model | game = Just newGame }, Cmd.none )
                else
                    ( { model | game = Just otherGame }, Cmd.none )
        Nothing ->
            defaultRet model

-- receives the response from server a player submitting a combo (key of combo)
comboRcv : Int -> Int -> Bool -> Model -> (Model, Cmd Msg)
comboRcv key player add_tile model =
    case model.game of
        Just game ->
            let
                combo = keyCombo key
                newInfo = addComboInfo player combo game.info
                newTurn = player
                (newState, newAction) =
                    case combo of
                        Triple _ -> (SelectDrop, Just Server.Pung)
                        Quad _ -> (MustDraw, Just Server.Kong)
                        Chain _ -> (SelectDrop, Just Server.Chi)
                        -- This branch should never be hit since player cannot declare a double
                        Double _ -> (SelectDrop, Nothing)
                otherGame = { game
                    | info = newInfo
                    , turn = newTurn
                    , state = newState
                    , prevAction = newAction
                    , errorMsg = Nothing
                    }
            in
            if player == game.player then
                let
                    hand =
                        if add_tile then
                            case game.dropped of
                                Just tile ->
                                    addTile tile game.hand
                                Nothing ->
                                    game.hand
                        else
                            game.hand
                    newHand = addCombo combo hand
                    newGame = { otherGame
                        | hand = newHand
                        }
                in
                ( { model | game = Just newGame }, Cmd.none )
            else
                ( { model | game = Just otherGame }, Cmd.none )
        Nothing ->
            ( model, Cmd.none )

-- receive message that a player is intending to chi
chiRcv : Model -> (Model, Cmd Msg)
chiRcv model =
    case model.game of
        Just game ->
            let
                newState = SelectChi
                prevAction = Just Server.Chi
                newGame = { game
                    | state = newState
                    , prevAction = prevAction
                    , errorMsg = Nothing
                    }
            in
            if game.turn == game.player then
                case game.dropped of
                    Just tile ->
                        let
                            newHand = addTile tile game.hand
                            chiGame = { newGame
                                | hand = newHand
                                }
                        in
                        ( { model | game = Just chiGame }, Cmd.none )
                    Nothing ->
                        ( { model | game = Just newGame }, Cmd.none )
            else
                ( { model | game = Just newGame }, Cmd.none )
        Nothing -> defaultRet model

--receive message that a player is intending to hidden kong
kongRcv : Model -> (Model, Cmd Msg)
kongRcv model =
    case model.game of
        Just game ->
            let
                newState = SelectKong
                prevAction = Just Server.Kong
                newGame = { game
                    | state = newState
                    , prevAction = prevAction
                    , errorMsg = Nothing
                    }
            in
            ( { model | game = Just newGame }, Cmd.none )
        Nothing -> defaultRet model

-- receives message that a player has won
winRcv : Int -> (List Int) -> Model -> (Model, Cmd Msg)
winRcv player comboList model =
    case model.game of
        Just game ->
            let
                newState = Over
                prevAction = Just Server.Hu
                winCombos = List.map keyCombo comboList |> Combo.comboCountsFromList
                hand =
                    if player == game.player then
                        Mahjong.makeWinningHand winCombos
                    else
                        game.hand
                newGame = { game
                    | turn = player
                    , state = newState
                    , prevAction = prevAction
                    , errorMsg = Nothing
                    , hand = hand
                    }
                (tiles, combos) = Mahjong.handToLists hand
            in
            ( { model | game = Just newGame }
            , socketSendHand (game.player, tiles, combos) )
        Nothing -> defaultRet model

-- receive information about all players' hands to reveal
handsRcv : List HandRet -> Model -> (Model, Cmd Msg)
handsRcv hands model =
    case model.game of
        Just game ->
            let
                newInfo = handsInfo hands game.info
                newGame = { game
                    | info = newInfo
                    }
            in
            ( { model | game = Just newGame }, Cmd.none )
        Nothing -> defaultRet model

-- receive tile number that a player is trying to small kong
smallKongRcv : Int -> Model -> (Model, Cmd Msg)
smallKongRcv num model =
    case model.game of
        Just game ->
            let
                tile = keyTile num
                newState = MustDraw
                prevAction = Just Server.Kong
                newInfo = smallKongInfo game.turn tile game.info
                otherGame = { game
                    | info = newInfo
                    , state = newState
                    , prevAction = prevAction
                    , errorMsg = Nothing
                    }
            in
            if game.turn == game.player then
                -- if it's current player, also update hand
                let
                    newHand = Mahjong.smallKongHand tile game.hand
                    newGame = { otherGame
                        | hand = newHand
                        }
                in
                ( { model | game = Just newGame }, Cmd.none )
            else
                ( { model | game = Just otherGame }, Cmd.none )
        Nothing -> defaultRet model

-- logic when someone clicks on button to pung
pungButton : Model -> (Model, Cmd Msg)
pungButton model =
    case model.game of
        Just game ->
            let
                errStr = "Can't Pung right now."
            in
            case game.state of
                Actionable ->
                    case game.dropped of
                        Nothing ->
                            errorRet errStr model
                        Just tile ->
                            if Mahjong.canPung tile game.hand then
                                let
                                    combo = Triple tile
                                    key = Combo.comboKey combo
                                    send = { key = key
                                        , player = game.player
                                        , is_kong = False
                                        , add_tile = True
                                        }
                                in
                                ( model, socketCombo send )
                            else
                                errorRet errStr model
                -- if not actionable, then can't pung
                _ ->
                    errorRet errStr model
        Nothing -> defaultRet model

-- logic for when player clicks on button to chi
chiButton : Model -> (Model, Cmd Msg)
chiButton model =
    case model.game of
        Just game ->
            if game.turn /= game.player then
                errorRet "Can only Chi on your turn." model
            else
                case game.dropped of
                    Nothing ->
                        errorRet "No tile to Chi." model
                    Just tile ->
                        if Mahjong.canChi tile game.hand then
                            ( model, socketChi game.player )
                        else
                            errorRet "Can't Chi." model
        Nothing -> defaultRet model

-- logic for when someone clicks on the button to draw
drawButton : Model -> (Model, Cmd Msg)
drawButton model =
    case model.game of
        Just game ->
            if game.turn /= game.player then
                errorRet "Wait your turn to draw." model
            else
                if (game.state /= Actionable) && (game.state /= MustDraw) then
                    -- can't draw unless in one of these two states
                    errorRet "Can't draw right now" model
                else
                    -- otherwise simply send signal to draw a card
                    ( model, socketDraw game.player )
        Nothing -> defaultRet model

-- logic for when someone clicks on button to kong
kongButton : Model -> (Model, Cmd Msg)
kongButton model =
    case model.game of
        Just game ->
            let
                errorMsg = "Can't Kong right now."
            in
            case game.state of
                Actionable ->
                    -- actionable is normal kong
                    case game.dropped of
                        Nothing ->
                            errorRet errorMsg model
                        Just tile ->
                            if Mahjong.canKong tile game.hand then
                                let
                                    combo = Quad tile
                                    key = Combo.comboKey combo
                                    send = { key = key
                                        , player = game.player
                                        , is_kong = True
                                        , add_tile = True
                                        }
                                in
                                ( model, socketCombo send )
                            else
                                errorRet errorMsg model
                SelectDrop ->
                    -- potentially we can either small kong or hidden kong
                    if game.turn /= game.player then
                        errorRet errorMsg model
                    else
                        -- check if we can small Kong with most recently drawn card
                        case game.drawn of
                            Nothing ->
                                errorRet errorMsg model
                            Just tile ->
                                if Mahjong.canSmallKong tile game.hand then
                                    -- can small kong
                                    let
                                        key = tileKey tile
                                    in
                                    ( model, socketSmallKong key )
                                else if Mahjong.canHidKong game.hand then
                                    -- hidden kong send to server
                                    ( model, socketHidKong game.player )
                                else
                                    errorRet errorMsg model
                -- any other state means player can't kong
                _ ->
                    errorRet errorMsg model
        Nothing -> defaultRet model

-- logic for when a player clicks the Hu Button
huButton : Model -> (Model, Cmd Msg)
huButton model =
    case model.game of
        Just game ->
            let
                errorMsg = "Can't Hu right now."
            in
            case game.state of
                Actionable ->
                    case game.dropped of
                        Just tile ->
                            case Mahjong.canHu tile game.hand of
                                Just combos ->
                                    let
                                        newHand = Mahjong.makeWinningHand combos
                                        (_, comboList) = Mahjong.handToLists newHand
                                    in
                                    -- valid Hu. Send to server.
                                    ( model, socketHu (game.player, comboList) )
                                Nothing ->
                                    errorRet errorMsg model
                        Nothing ->
                            errorRet errorMsg model

                -- can also win when you just drew and you need to drop a card
                -- (Self Hu)
                SelectDrop ->
                    if game.player == game.turn then
                        case Mahjong.alreadyHu game.hand of
                            Just combos ->
                                let
                                    newHand = Mahjong.makeWinningHand combos
                                    (_, comboList) = Mahjong.handToLists newHand
                                in
                                ( model
                                , socketHu (game.player, comboList) )
                            Nothing ->
                                errorRet errorMsg model
                    else
                        errorRet errorMsg model
                -- Can't Hu in any other state
                _ ->
                    errorRet errorMsg model
        Nothing -> defaultRet model

-- update function incorporating all of our helper functions above based
-- on the state of the model
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- get a player number from server
        Player x ->
            case model.player of
                Nothing ->
                    ( { model | player = Just x }
                    , Cmd.none
                    )
                -- only update player number if you haven't been
                -- assigned one yet.
                _ ->
                    defaultRet model
    
        -- dealing with server responses 
        Deal dealRet -> getDeal dealRet model
        Rcv receive ->
            case receive of
                Dropped num -> dropRcv num model
                Drew num -> drewRcv num model 
                Comboed key player add_tile -> comboRcv key player add_tile model
                WillChi -> chiRcv model
                WillKong -> kongRcv model
                Win player comboList -> winRcv player comboList model
                Hands hands -> handsRcv hands model
                SmallKong num -> smallKongRcv num model

                Lobbies lobbies ->
                    ({model | lobbies = lobbies}, Cmd.none)
                NoCreate ->
                    let
                        errMsg = "Empty game already exists."
                    in
                    ({model | lobbyStatus = Just errMsg}, Cmd.none)

                Terminate ->
                    let
                        errMsg = "Player disconnected. Game abandoned."
                        newModel = { model
                            | player = Nothing
                            , game = Nothing
                            , lobbyStatus = Just errMsg
                            }
                    in
                    (newModel, Cmd.none)



        -- dealing with client side clicks
        TileAction tile -> tileClick tile model
        Button Pung -> pungButton model
        Button Chi -> chiButton model
        Button Draw -> drawButton model
        Button Kong -> kongButton model
        Button Hu -> huButton model
            
        -- Restart game with same four players
        Button Restart ->
            ( model, socketRestart () )

        -- no button for drop. Shouldn't reach here
        Button Drop ->
            defaultRet model

        -- create a new game
        CreateGame ->
            ( {model | lobbyStatus = Nothing}, socketCreateGame () )

        -- Join a game
        JoinGame gid ->
            ( model, socketJoinGame gid )

        -- reset the server state
        Reset True ->
            ( model, socketReset () )

        -- response from server to reset. Reset all clients
        Reset False ->
            let
                newModel = { model
                    | lobbyStatus = Nothing
                    , game = Nothing
                    , player = Nothing
                    }
            in
            ( newModel, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ socketJoinRes Player
        , socketStart Deal
        , socketResetAll Reset
        , socketDrawRcv (\x -> Rcv (Drew x))
        , socketDropRcv (\x -> Rcv (Dropped x))
        , socketComboRcv (\comboRet -> Rcv (Comboed comboRet.key comboRet.player comboRet.add_tile))
        , socketChiRcv (\() -> (Rcv WillChi))
        , socketHuRcv (\x -> Rcv (Win x.player x.combos))
        , socketSmallKongRcv (\x -> Rcv (SmallKong x))
        , socketHidKongRcv (\() -> (Rcv WillKong))
        , socketHandRcv (\x -> (Rcv (Hands x)))
        , socketLobbiesRcv (\x -> (Rcv (Lobbies x)))
        , socketNoCreate (\() -> (Rcv NoCreate))
        , socketTerminated (\() -> (Rcv Terminate))
        ]

-- View

view : Model -> Html Msg
view model =
    case model.player of
        Nothing ->
            -- default message if server doesn't respond.
            lobbyPage model.lobbies model.lobbyStatus
        
        Just x ->
            if x < 0 then
                Display.inSession
            else 
                case model.game of
                    Nothing ->
                        Display.waiting
                    Just game ->
                        page game

-- Main

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }