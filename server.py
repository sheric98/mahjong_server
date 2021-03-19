import numpy as np
import random

tot_tiles = 136
hand_size = 13

class GameInfo:
    def __init__(self, game_id):
        self.game_id = game_id
        self.players = 0
        self.avail = [0, 1, 2, 3]
        self.conns = {}
        self.cards = None
        self.started = False
        self.hand_info = [None] * 4

    def new_player(self, sid):
        if self.players >= 4 or self.started:
            return -1
        else:
            ret = self.avail.pop(0)
            self.players += 1
            self.conns[sid] = ret
            return ret

    def remove_player(self, sid):
        if sid in self.conns:
            remove = self.conns[sid]
            self.players -= 1
            self.avail.append(remove)

    def start_game(self):
        self.started = True
        self.cards = [int(x) for x in np.random.permutation(tot_tiles)]
        hands = []
        for _ in range(self.players):
            hands.append(self.cards[:hand_size])
            self.cards = self.cards[hand_size:]
        starting = random.randint(0, self.players)
        ret = {'hands': hands, 'starting': starting}
        return ret

    def draw(self):
        if len(self.cards) > 0:
            ret = self.cards.pop(0)
        else:
            ret = -1
        return ret

    def gather_hands(self, player, tiles, combos):
        hand = {'tiles': tiles, 'combos': combos}
        self.hand_info[player] = hand
        test = [player_hand != None for player_hand in self.hand_info]
        if all(test):
            return self.hand_info
        else:
            return None

    def is_empty(self):
        return self.players == 0

    def game_lobby(self):
        return {'id': self.game_id, 'players': self.players}

    def get_sids(self):
        return list(self.conns.keys())