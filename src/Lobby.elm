module Lobby exposing (lobbyPage)

import Html exposing (Html, div, h1, h2)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)
import Server exposing (GameLobby, Msg(..))

pad : Maybe String -> Html Msg
pad lobbyStatus =
    let
        button = Html.button [onClick CreateGame] [Html.text <| "Create Game"]
        new_game = div [id "new_game"] [button]
        msg =
            case lobbyStatus of
                Nothing -> []
                Just str -> [Html.text <| str]
        lobby_status = div [id "lobby_status"] msg
    in
    div [id "pad"] [new_game, lobby_status]

makeLobby : GameLobby -> Html Msg
makeLobby lobby =
    let
        id = "Game " ++ (String.fromInt (lobby.id + 1))
        players = "Players: " ++ String.fromInt (lobby.players) ++ "/4"
        id_div = div [class "lobby_id"] [Html.text <| id]
        players_div = div [class "lobby_players"] [Html.text <| players]
    in
    div [class "lobby", onClick (JoinGame lobby.id)] [id_div, players_div]

makeLobbies : List GameLobby -> Maybe String -> Html Msg
makeLobbies lobbies lobbyStatus =
    let
        lobbyDivs = List.map makeLobby lobbies
    in
    div [id "lobbies"] ((pad lobbyStatus)::lobbyDivs)

lobbyPage : List GameLobby -> Maybe String -> Html Msg
lobbyPage lobbies lobbyStatus =
    let
        title = h1 [] [Html.text <| "Mahjong"]
        caption = h2 [] [Html.text <| "Click to join a game"]
    in
    div [] [title, caption, makeLobbies lobbies lobbyStatus]
