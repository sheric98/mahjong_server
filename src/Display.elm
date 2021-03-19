module Display exposing (page, inSession, waiting)

{- Display.elm:

   This file serves as how to render the page given the state of the game
   from the persepective of a specific player. The display not only includes
   what tiles to display, but also when a tile is clickable, when to show
   certain buttons, and also what status messages to display based on
   the game state.
-}

import Array
import Chain exposing (Chain, expandChain)
import Combo exposing (Combo(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)
import KeySet
import Mahjong exposing (Hand, getSortedTiles, getHandCombos)
import Server exposing (PlayerInfo, Game, Model, Msg(..), Action(..), State(..))
import Tile exposing (Tile, tileKey)

-- which side we are displaying
type Side
    = Top
    | Left
    | Right
    | Bot
    | Center

-- tile could be face up or down
type HidTile
    = Down
    | Up Tile

-- tail recursive helper to find a given tile in a list
find_tr : Int -> Tile -> List Tile -> Int
find_tr acc tile tiles =
    case tiles of
        [] -> -1
        t::rest ->
            if t == tile then
                acc
            else
                find_tr (acc + 1) tile rest

-- finds a given tile in a list of tiles. returns index
-- or -1 if can't find
find : Tile -> List Tile -> Int
find tile tiles =
    find_tr 0 tile tiles

-- display the hand of the given player
displayHand : Game -> List (Html Msg)
displayHand game =
    let
        tiles = getSortedTiles game.hand
        combos = displayCombos Bot (getHandCombos game.hand)  
    in
    case game.state of
        SelectChi ->
            -- only highlight and make selectable the tiles that are lowest of chi chains.
            let
                tile =
                    case game.dropped of
                        Just t -> t
                        -- Note: this branch should never be hit. Just adding a
                        -- random tile so that this compiles.
                        Nothing -> Tile.keyTile 0
                selectable = Mahjong.chiLowest tile game.hand
                valid t =
                    KeySet.member t selectable
                hidden = List.map (\t -> displayTile Bot (Up t) (valid t) (valid t)) tiles
            in
            hidden ++ (divider :: combos)
        SelectKong ->
            -- only highlight and make selectable hidden kongs
            let 
                selectable = Mahjong.getHidKongs game.hand
                valid t =
                    KeySet.member t selectable
                hidden = List.map (\t -> displayTile Bot (Up t) (valid t) (valid t)) tiles
            in
            hidden ++ (divider :: combos)
        SelectDrop ->
            -- Otherwise when selecting drop, only potentially highlight last drawn card
            let
                -- index of drawn tile to highlight
                highlightIdx =
                    case game.drawn of
                        Just tile -> find tile tiles
                        Nothing -> -1
                clickable = game.turn == game.player
                hidden = List.indexedMap
                    (\idx t -> displayTile Bot (Up t) clickable (idx == highlightIdx))
                    tiles
            in
            hidden ++ (divider :: combos)
        _ ->
            -- In any other scenario, don't highlight or let the tiles be selectable
            let
                hidden = List.map (\t -> displayTile Bot (Up t) False False) tiles
            in
            hidden ++ (divider :: combos)

-- produces divider between hidden and visible tiles in our hand
divider : Html msg
divider =
    div
        [id "divider"]
        [ div [] [Html.text <| "← hidden"]
        , div [] [Html.text <| "visible →"]]

-- display info about other player given the side
displayInfo : Int -> Side -> PlayerInfo -> List (Html Msg)
displayInfo player side info =
    let
        inc =
            case side of
                Right -> 1
                Top -> 2
                Left -> 3
                _ -> 0
        idx = modBy 4 (player + inc)
        pair = Array.get idx info
    in
    case pair of
        Just tileInfo ->
            let
                hidden = List.repeat tileInfo.hidden (displayTile side Down False False)
                tiles = List.map (\t -> displayTile side (Up t) False False) tileInfo.tiles
                combos = displayCombos side tileInfo.combos
            in
            hidden ++ tiles ++ combos
        Nothing -> []

-- get the player tag for a player on a different side
getTag : Int -> Side -> Html msg
getTag player side =
    let
        inc =
            case side of
                Right -> 1
                Top -> 2
                Left -> 3
                _ -> 0
        idx = modBy 4 (player + inc)
        txt =
            if side /= Bot then
                "Player " ++ String.fromInt (idx + 1)
            else
                "You are Player " ++ String.fromInt (idx + 1)
    in
    Html.text <| txt

-- returns if a given side has won or not
winningSide : Game -> Maybe Side
winningSide game =
    -- check if the game has been won yet
    if game.state == Over && game.prevAction == Just Hu then
        let
            idx = modBy 4 (game.turn - game.player)
            winSide =
                if idx == 0 then
                    Bot
                else if idx == 1 then
                    Right
                else if idx == 2 then
                    Top
                else
                    Left
        in
        Just winSide
    else
        Nothing

-- display a tile given the side and whether or not the
-- tile is face up or down. Clickable denotes that
-- the tile should be clickable and highlight denotes highlighted
displayTile : Side -> HidTile -> Bool -> Bool -> Html Msg
displayTile side t clickable highlight =
    let
        tileStr =
            case t of
                Down -> "0"
                Up tile -> String.fromInt (tileKey tile)
        baseStr =
            "static/tiles/" ++ tileStr
    in
    case side of
        Top ->
            let
                classes = [class "top", class "tile", class "flip"]
                loc = src (baseStr ++ ".png")
            in
            Html.img (loc::classes) []
        Left ->
            let
                classes = [class "side", class "tile"]
                loc = src (baseStr ++ "r.png")
            in
            Html.img (loc::classes) []
        Right ->
            let
                classes = [class "side", class "tile", class "flip"]
                loc = src (baseStr ++ "r.png")
            in
            Html.img (loc::classes) []
        Bot ->
            let
                classes =
                    if highlight then
                        [class "highlight", class "bot", class "tile"]
                    else
                        [class "bot", class "tile"]
                loc = src (baseStr ++ ".png")
            in
            if clickable then
                let
                    tile = 
                        case t of
                            Up tile_ -> tile_
                            -- Note that this branch is never hit. We always
                            -- call Bot with Up, so placeholder arbitrary tile here.
                            Down -> Tile.keyTile 0
                    click = onClick (TileAction tile)
                    selectable = class "selectable"
                in
                Html.img (loc::click::selectable::classes) []
            else
                Html.img (loc::classes) []
        Center ->
            let
                classes = [class "drop", class "tile"]
                loc = src (baseStr ++ ".png")
            in
            Html.img (loc::classes) []

-- displays the tiles of multiple combos
displayCombos : Side -> List Combo -> List (Html Msg)
displayCombos side combos =
    List.concatMap (displayCombo side) combos

-- displays the tiles of a single combo
displayCombo : Side -> Combo -> List (Html Msg)
displayCombo side combo =
    case combo of
        Chain c ->
            displayChain side c
        Double t ->
            List.repeat 2 (displayTile side (Up t) False False)
        Triple t ->
            List.repeat 3 (displayTile side (Up t) False False)
        Quad t ->
            List.repeat 4 (displayTile side (Up t) False False)

-- displays the tiles in a chain
displayChain : Side -> Chain -> List (Html Msg)
displayChain side chain =
    let
        tiles = expandChain chain
    in
    List.map (\t -> displayTile side (Up t) False False) tiles

-- make a button that has a specific action
makeButton : Maybe (String, Action) -> Html Msg
makeButton maybePair =
    case maybePair of
        Just (txt, action) ->
            let
                button = Html.button [onClick (Button action)] [Html.text txt]
            in
            div [class "button"] [button]
        Nothing ->
            div [class "button"] []

-- renders the reset button for when the game hasn't started yet
returnButton : Html Msg
returnButton =
    let
        button = Html.button [onClick (Reset True)] [Html.text "Home"]
        txt = "Click to return to home page."
    in
    div [] [Html.text <| txt, button]

-- Following are helper functions to construct display for players info

-- produces div of left row
leftRow : Int -> PlayerInfo -> Html Msg
leftRow player info =
    let
        imgs = displayInfo player Left info
    in
    div [class "side_row"] imgs

-- produces tag of left player
leftTag : Game -> Maybe Side -> Html msg
leftTag game wonSide =
    let
        tag = getTag game.player Left
        sideClass = class "side_player"
        classes =
            if wonSide == Just Left then
                [sideClass, class "winner"]
            else
                [sideClass]
    in
    div classes [tag]

-- produces div of right row
rightRow : Int -> PlayerInfo -> Html Msg
rightRow player info =
    let
        imgs = displayInfo player Right info
    in
    div [class "side_row"] imgs

-- produces tag of right player
rightTag : Game -> Maybe Side -> Html msg
rightTag game wonSide =
    let
        tag = getTag game.player Right
        defClasses = [class "side_player", class "flip"]
        classes =
            if wonSide == Just Right then
                (class "winner")::defClasses
            else
                defClasses
    in
    div classes [tag]

-- produces div of top row
topRow : Int -> PlayerInfo -> Html Msg
topRow player info =
    let
        imgs = displayInfo player Top info
    in
    div [id "top_row"] imgs

-- produces tag of top player
topTag : Game -> Maybe Side -> Html msg
topTag game wonSide =
    let
        tag = getTag game.player Top
        sideClass = class "player"
        classes =
            if wonSide == Just Top then
                [sideClass, class "winner"]
            else
                [sideClass]
    in
    div classes [tag]

-- produces div of bot row
botRow : Game -> Html Msg
botRow game =
    div [id "hand"] (displayHand game)

-- produce tag of bot player
botTag : Int -> Html msg
botTag player =
    let
        tag = getTag player Bot
    in
    div [class "player"] [tag]

-- produces items for middle of board

-- produces div for most recently dropped tile / Drawn tile
dropTile : Game -> Html Msg
dropTile game =
    let
        -- center image is either singleton or nothing
        (img, recent) =
            case game.drawn of
                Just drawn ->
                    ( [displayTile Center (Up drawn) False False]
                    , div [id "recent"] [Html.text <| "You Drew:"]
                    )
                Nothing ->
                    case game.dropped of
                        Just dropped ->
                            ( [displayTile Center (Up dropped) False False]
                            , div [id "recent"] [Html.text <| "Recently Dropped:"]
                            )
                        Nothing ->
                            ( []
                            , div [id "recent"] []
                            )
        imageWrap = div [id "drop_inner"] img
        remain =
            div
            [id "remaining"]
            [Html.text <| "Tiles Remaining: " ++ (String.fromInt game.remaining)]
        left = div [id "next_drop"] [recent, remain]
    in
    div [id "drop_wrap"] [left, imageWrap]

-- produces div for all buttons
buttons : Game -> Html Msg
buttons game =
    let
        pairs =
            if game.state == Over then
                -- have only reset button in middle if game is over
                [ Nothing
                , Nothing
                , Just ("Restart", Restart)
                , Nothing
                , Nothing
                ]
            else
                -- otherwise have the normal action buttons
                [ Just ("Chi", Chi)
                , Just ("Pung", Pung)
                , Just ("Draw", Draw)
                , Just ("Kong", Kong)
                , Just ("Hu", Hu)
                ]
        buttonList = List.map makeButton pairs
    in
    div [id "buttons"] buttonList

-- produces div for status message
-- first bool is if the player won
-- second bool is if we are producing an error message
status : Bool -> Bool -> Maybe String -> Html msg
status won err txt =
    let
        inner =
            case txt of
                Just str ->
                    [Html.text str]
                Nothing ->
                    []
        classes = [class "status"]
        classes1 =
            if won then
                (class "winner")::classes
            else
                classes
        classes2 =
            if err then
                (class "error")::classes1
            else
                classes1
    in
    div classes2 inner

-- takes the model and produces the text for certain text boxes
prevTxt : Game -> Maybe String
prevTxt game =
    let
        player =
            if game.turn /= game.player then
                "Player " ++ (String.fromInt (game.turn + 1))
            else
                "You"
    in
    case game.prevAction of
        Nothing ->
            let
                txt = "New Game! "
                end =
                    if game.turn /= game.player then
                        " starts."
                    else
                        " start."
            in
            Just (txt ++ player ++ end)
        Just action ->
            case action of
                Pung ->
                    Just (player ++ " Pung'ed")
                Kong ->
                    Just (player ++ " Kong'ed")
                Chi ->
                    Just (player ++ " Chi'ed")
                Hu ->
                    Just (player ++ " won!")
                Draw ->
                    if game.state == Over then
                        Just "No cards left. Tie."
                    else
                        Just (player ++ " drew a tile.")
                Drop ->
                    let
                        prevIdx = modBy 4 (game.turn - 1)
                        prevPlayer = "Player " ++ (String.fromInt (prevIdx + 1))
                    in
                    Just (prevPlayer ++ " dropped a tile.")
                _ ->
                    Nothing

-- produces the text for the previous action
actionTxt : Game -> Maybe String
actionTxt game =
    let
        player = "Player " ++ (String.fromInt (game.turn + 1))
    in
    case game.state of
        SelectDrop ->
            if game.turn /= game.player then
                Just ("Waiting for " ++ player ++ " to drop a tile.")
            else
                Just "Select a tile to drop."
        SelectChi ->
            if game.turn /= game.player then
                Just ("Waiting for " ++ player ++ " to reveal Chi.")
            else
                Just "Select the lowest tile of your Chi to reveal."
        SelectKong ->
            if game.turn /= game.player then
                Just ("Waiting for " ++ player ++ " to reveal Kong.")
            else
                Just "Select your concealed Kong to reveal."
        Actionable ->
            if game.turn /= game.player then
                Just (player ++ "'s Turn.")
            else
                Just "Your turn."
        MustDraw ->
            if game.turn /= game.player then
                Just ("Waiting for " ++ player ++ " to draw a tile.")
            else
                Just "Now draw another tile."
        Over ->
            Nothing

-- produces the text for any error the player encountered
errorTxt : Game -> Maybe String
errorTxt game =
    game.errorMsg

-- creates the "middle" portion of our display
-- which includes the status message, dropped tile, and buttons
middle : Game -> Maybe Side -> Html Msg
middle game wonSide =
    let
        won = wonSide == Just Bot
        prev = game |> prevTxt |> status won False
        action = game |> actionTxt |> status False False
        errorItem = game |> errorTxt |> status False True
    in
    div [id "middle"] [prev, action, errorItem, dropTile game, buttons game]

-- creates the "center" portion, which is just
-- the middle with the top row
center : Game -> Maybe Side -> Html Msg
center game wonSide =
    let
        topTiles = topRow game.player game.info
        topPlayer = topTag game wonSide
    in
    div [id "center"] [topTiles, topPlayer, middle game wonSide]

-- displays information other than the player him / her self
others : Game -> Maybe Side -> Html Msg
others game wonSide =
    let
        player = game.player
        info = game.info
    in
    div
        [id "others"]
        [ leftRow player info
        , leftTag game wonSide
        , center game wonSide
        , rightTag game wonSide
        , rightRow player info ]

-- Now combine all of these into our final page -----

page : Game -> Html Msg
page game =
    let
        wonSide = winningSide game
    in
    div
        [id "page"]
        [ others game wonSide
        , botTag game.player
        , botRow game
        ]

-- The following are for different non-game (loading) pages

-- for when a user joins and the game is already in session
inSession : Html Msg
inSession =
    let
        txt = Html.text <| "Game already in session."
    in
    div [] [txt, returnButton]

-- for when a user joins and there are not enough players yet.
waiting : Html Msg
waiting =
    let
        txt = Html.text <| "Waiting for more players."
    in
    div [] [txt, returnButton]
