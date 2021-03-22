from enum import Enum, auto
import numpy as np
import threading
import random

class State(Enum):
    DROP = auto()
    REVEAL = auto()
    DRAW = auto()
    ACTIONABLE = auto()
    OVER = auto()

    def transition(self, is_kong=False, chi=False, hidKong=False, hu=False):
        if self == State.DROP:
            if hidKong:
                return State.REVEAL
            elif hu:
                return State.OVER
            else:
                return State.ACTIONABLE
        elif self == State.DRAW:
            return State.DROP
        elif self == State.ACTIONABLE:
            if chi:
                return State.REVEAL
            elif is_kong:
                return State.DRAW
            elif hu:
                return State.OVER
            else:
                return State.DROP
        # Reveal
        elif self == State.REVEAL:
            if is_kong:
                return State.DRAW
            else:
                return State.DROP
        # OVER
        else:
            return State.OVER

# constants
tot_tiles = 136
hand_size = 13
DROP_SET = {State.DROP}
CHI_SET = {State.ACTIONABLE}
DRAW_SET = {State.DRAW, State.ACTIONABLE}
COMBO_SET = {State.ACTIONABLE, State.REVEAL}

class GameInfo:
    def __init__(self, game_id):
        self.game_id = game_id
        self.players = 0
        self.avail = [0, 1, 2, 3]
        self.conns = {}
        self.cards = None
        self.started = False
        self.hand_info = [None] * 4
        self.lock = threading.Lock()
        self.state = State.DROP
        self.turn = None

    def new_player(self, sid):
        self.lock.acquire()
        if self.players >= 4 or self.started:
            self.lock.release()
            return -1
        else:
            ret = self.avail.pop(0)
            self.players += 1
            self.conns[sid] = ret
            self.lock.release()
            return ret

    def remove_player(self, sid):
        self.lock.acquire()
        if sid in self.conns:
            remove = self.conns[sid]
            self.players -= 1
            self.avail.append(remove)
        self.lock.release()

    def can_start(self):
        self.lock.acquire()
        ret = self.players == 4 and not self.started
        if ret:
            self.started = True
        self.lock.release()
        return ret

    def start_game(self):
        self.cards = [int(x) for x in np.random.permutation(tot_tiles)]
        hands = []
        for _ in range(self.players):
            hands.append(self.cards[:hand_size])
            self.cards = self.cards[hand_size:]
        starting = random.randint(0, self.players - 1)
        hands[starting].append(self.cards.pop(0))
        self.turn = starting
        self.state = State.DROP
        self.hand_info = [None] * self.players
        ret = {'hands': hands, 'starting': starting}
        return ret

    def draw(self, player):
        if self.lock.acquire(False):
            if self.turn == player and self.can_transition(DRAW_SET):
                if len(self.cards) > 0:
                    ret = self.cards.pop(0)
                else:
                    ret = -1
                self.lock.release()
                return ret
            else:
                self.lock.release()
        return None

    def drop(self):
        if self.lock.acquire(False):
            if self.can_transition(DROP_SET):
                self.turn = (self.turn + 1) % 4
                self.lock.release()
                return 0
            else:
                self.lock.release()
        return None

    def combo(self, player, is_kong):
        if self.lock.acquire(False):
            if self.can_transition(COMBO_SET, is_kong):
                self.turn = player
                self.lock.release()
                return 0
            else:
                self.lock.release()
        return None

    def chi(self, player):
        if self.lock.acquire(False):
            if self.turn == player and self.can_transition(CHI_SET, chi=True):
                self.lock.release()
                return 0
            else:
                self.lock.release()
        return None

    def hidKong(self, player):
        if self.lock.acquire(False):
            if self.turn == player and self.can_transition(DROP_SET, hidKong=True):
                self.lock.release()
                return 0
            else:
                self.lock.release()
        return None

    def hu(self, player):
        if self.lock.acquire(False):
            if (self.turn == player and self.can_transition(DROP_SET, hu=True)) \
                or self.can_transition(CHI_SET, hu=True):
                self.turn = player
                self.lock.release()
                return 0
            else:
                self.lock.release()
        return None

    def gather_hands(self, player, tiles, combos):
        hand = {'tiles': tiles, 'combos': combos}
        self.lock.acquire()
        self.hand_info[player] = hand
        test = [player_hand != None for player_hand in self.hand_info]
        self.lock.release()
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

    # expected is a set of states
    def can_transition(self, expected, is_kong=False, chi=False, hidKong=False, hu=False):
        if self.state in expected:
            self.state = self.state.transition(is_kong, chi, hidKong, hu)
            return True
        else:
            return False