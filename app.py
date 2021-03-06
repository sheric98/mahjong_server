from server import GameInfo
from flask import Flask, render_template, request
from flask_socketio import SocketIO, emit, join_room, close_room
import time

app = Flask(__name__)
socketio = SocketIO(app)


# global variables
games = [[]]
game_map = {}
default_id = [0]
avail_ids = []
player_map = {}

# creates a new game only if there isn't already an empty game
def create_game():
    if not games[0] or not games[0][-1].is_empty():
        if avail_ids:
            game_id = avail_ids.pop(0)
        else:
            game_id = default_id[0]
            default_id[0] += 1
        new_game = GameInfo(game_id)
        games[0].append(new_game)
        game_map[game_id] = new_game
        return game_id
    else:
        return -1

def send_games(broadcast=False):
    ret = [game.game_lobby() for game in games[0]]
    emit('gameInfo', ret, json=True, broadcast=broadcast)

def broadcast_player(sid, event, ret=None):
    if sid in player_map:
        gid = player_map[sid]
        if gid in game_map:
            game = game_map[gid]
            broadcast_game(game, event, ret)

def broadcast_game(game, event, ret=None):
    for sid in game.get_sids():
        if ret is not None:
            emit(event, ret, room=sid)
        else:
            emit(event, room=sid)

def get_player_game(sid):
    if sid in player_map:
        gid = player_map[sid]
        if gid in game_map:
            game = game_map[gid]
            return game
    return None

def game_can_transition(sid, expected, is_kong=False):
    game = get_player_game(sid)
    return game is not None and game.can_transition(expected, is_kong)

@app.route('/')
def index():
    return render_template("index.html")

@socketio.on('createGame')
def handle_create_game():
    ret = create_game()
    if ret == -1:
        emit('noCreate')
        send_games()
    else:
        send_games(broadcast=True)

@socketio.on('joinGame')
def handle_join_game(gid):
    player_id = -1
    game = None
    if gid in game_map:
        game = game_map[gid]
        player_id = game.new_player(request.sid)
        if player_id != -1:
            player_map[request.sid] = gid
    emit('joinRes', player_id)
    send_games(broadcast=True)

    # start game if ready
    if game is not None and player_id != -1 and game.can_start():
        time.sleep(1)
        start = game.start_game()
        broadcast_player(request.sid, 'start', start)

@socketio.on('updateLobbies')
def handle_update_lobbies():
    send_games()

@socketio.on('drop')
def handle_drop(tile_num):
    game = get_player_game(request.sid)
    if game is not None and game.drop() is not None:
        broadcast_player(request.sid, 'dropRes', tile_num)

@socketio.on('draw')
def handle_draw(player):
    game = get_player_game(request.sid)
    if game is not None:
        ret = game.draw(player)
        if ret is not None:
            broadcast_player(request.sid, 'drawRes', ret)

@socketio.on('combo')
def handle_combo(send):
    print(send)
    ret = {'key': send['key'], 'player': send['player'], 'add_tile': send['add_tile']}
    game = get_player_game(request.sid)
    if game is not None and game.combo(send['player'], send['is_kong']) is not None:
        broadcast_player(request.sid, 'comboRes', ret)

@socketio.on('chi')
def handle_chi(player):
    game = get_player_game(request.sid)
    if game is not None and game.chi(player) is not None:
        broadcast_player(request.sid, 'chiRes')

@socketio.on('hidKong')
def handle_hid_kong(player):
    game = get_player_game(request.sid)
    if game is not None and game.hidKong(player) is not None:
        broadcast_player(request.sid, 'hidKongRes')

@socketio.on('smallKong')
def handle_small_kong(tile):
    game = get_player_game(request.sid)
    if game is not None and game.combo(game.turn, True) is not None:
        broadcast_player(request.sid, 'smallKongRes', tile)

@socketio.on('hu')
def handle_hu(pair):
    player, combos = pair
    ret = {'player': player, 'combos': combos}
    game = get_player_game(request.sid)
    if game is not None and game.hu(player) is not None:
        broadcast_player(request.sid, 'huRes', ret)

@socketio.on('sendHand')
def handle_send_hand(hand_info):
    player, tiles, combos = hand_info
    if request.sid in player_map:
        gid = player_map[request.sid]
        if gid in game_map:
            game = game_map[gid]
            ret = game.gather_hands(player, tiles, combos)
            if ret != None:
                broadcast_player(request.sid, 'sendHandRcv', ret)

@socketio.on('reset')
def handle_reset():
    if request.sid in player_map:
        gid = player_map[request.sid]
        if gid in game_map:
            game = game_map[gid]
            game.remove_player(request.sid)
            send_games(broadcast=True)
        del player_map[request.sid]
    emit('resetRcv')

@socketio.on('restart')
def handle_restart():
    game = get_player_game(request.sid)
    if game is not None:
        start = game.start_game()
        broadcast_player(request.sid, 'start', start)

# terminate game
def terminate_game(game):
    for sid in game.get_sids():
        if sid in player_map:
            del player_map[sid]
    if game.game_id in game_map:
        del game_map[game.game_id]
        avail_ids.append(game.game_id)
    games[0] = [g for g in games[0] if g.game_id != game.game_id]

    send_games(broadcast=True)
    for sid in game.get_sids():
        if sid != request.sid:
            emit('terminated', room=sid)

# handle disconnects
@socketio.on('disconnect')
def handle_disconnect():
    if request.sid in player_map:
        gid = player_map[request.sid]
        if gid in game_map:
            game = game_map[gid]
            if game.started:
                print('terminating')
                terminate_game(game)
            else:
                game.remove_player(request.sid)
                send_games(broadcast=True)
                del player_map[request.sid]
        else:
            del player_map[request.sid]



if __name__ == '__main__':
    socketio.run(app, port=9876)
