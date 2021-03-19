const totTiles = 136;
const handSize = 13;

// when a new client connects
app.ports.firebaseJoin.subscribe(function() {
  firebase.database().ref("/").once("value").then((snapshot) => {
    var ret;
    var start = false;
    var data = snapshot.val();
    // notify our database that we are starting to look for players
    if (data.started.state == 0) {
      firebase.database().ref("/started/state").set(1);
    }
    if (data.players >= 4) {
      ret = -1;
    }
    else {
      ret = data.players;
      data.players++;
      firebase.database().ref("/players").set(data.players);
      // if incremented to 4, we are ready to start
      if (data.players == 4) {
        start = true;
      }
    }
    app.ports.firebaseJoinRes.send(ret);
    if (start) {
      startGame();
    }
  })
});

/* Got function from
   https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array */
/* Randomize array in-place using Durstenfeld shuffle algorithm */
function shuffleArray(array) {
  for (var i = array.length - 1; i > 0; i--) {
      var j = Math.floor(Math.random() * (i + 1));
      var temp = array[i];
      array[i] = array[j];
      array[j] = temp;
  }
}

// start the game
function startGame() {
  var nums = [...Array(totTiles).keys()];
  var hands = []
  shuffleArray(nums);
  for (let i = 0; i < 4; i++) {
    let hand = nums.slice(i*handSize, (i+1)*handSize);
    hands.push(hand);
  }
  var extra = nums[4 * handSize];
  var startingPlayer = Math.floor(Math.random() * 4);
  hands[startingPlayer].push(extra);
  var start = {
    hands: hands,
    starting: startingPlayer,
  }
  var deck = nums.slice(4*handSize + 1);
  firebase.database().ref("/deck").set(deck);
  firebase.database().ref("/init/start").set(start);
}

// reset the server
function resetServer() {
  firebase.database().ref("/players").set(0);
  firebase.database().ref("/deck").set(null);
  firebase.database().ref("/init/start").set(null);
  firebase.database().ref("game").set(null);
  app.ports.firebaseResetAll.send(false);
}

firebase.database().ref("/started").on("child_changed", function(snapshot) {
  var started = snapshot.val();
  // 1 = players are joining / game started; 0 = reset database
  if (started == 0) {
    // else if started switched to false, reset the server
    resetServer();
  }
});

firebase.database().ref("/init").on("child_added", function(snapshot) {
  var start = snapshot.val();
  app.ports.firebaseStart.send(start);
});

// dropping tile
app.ports.firebaseDrop.subscribe(function(tileNum) {
  firebase.database().ref("/game/drop/num").set(null);
  firebase.database().ref("/game/drop/num").set(tileNum);
});

// drawing a tile
app.ports.firebaseDraw.subscribe(function() {
  firebase.database().ref("/deck").once("value").then((snapshot) => {
    var deck = snapshot.val();
    var tileNum = -1;
    if (deck) {
      // pop and update deck
      tileNum = deck.pop();
      firebase.database().ref("/deck").set(deck);
    }
    firebase.database().ref("/game/draw/num").set(null);
    firebase.database().ref("/game/draw/num").set(tileNum);
  })
});

// player made combo
app.ports.firebaseCombo.subscribe(function(pair) {
  var comboRet = {
    key: pair[0],
    player: pair[1],
  };
  firebase.database().ref("/game/combo/pair").set(null);
  firebase.database().ref("/game/combo/pair").set(comboRet);
});

// player declares valid chi
app.ports.firebaseChi.subscribe(function() {
  firebase.database().ref("/game/chi/val").set(null);
  firebase.database().ref("/game/chi/val").set(0);
});

// player declares valid hidden kong
app.ports.firebaseHidKong.subscribe(function() {
  firebase.database().ref("game/hid_kong/val").set(null);
  firebase.database().ref("game/hid_kong/val").set(0);
});

// player declares valid hu
app.ports.firebaseHu.subscribe(function(player) {
  firebase.database().ref("/game/hu/player").set(null);
  firebase.database().ref("/game/hu/player").set(player);
});

// player declares valid small kong
app.ports.firebaseSmallKong.subscribe(function(tile) {
  firebase.database().ref("game/small_kong/val").set(null);
  firebase.database().ref("game/small_kong/val").set(tile);
});

// player sends their hand for when a game is over
app.ports.firebaseSendHand.subscribe(function(handInfo) {
  var player = handInfo[0];
  var tiles = handInfo[1];
  var combos = handInfo[2];
  var link = "/game/hands/over/" + player.toString();
  console.log(link);
  var hand = {
    tiles: tiles,
    combos: combos,
  };
  console.log(hand);
  firebase.database().ref(link).set(hand);
});

// sends message to clients on what tile is dropped
firebase.database().ref("/game/drop").on("child_added", function(snapshot) {
  var drop = snapshot.val();
  app.ports.firebaseDropRcv.send(drop);
});

// sends message to clients about what tile is drawn
firebase.database().ref("/game/draw").on("child_added", function(snapshot) {
  var draw = snapshot.val();
  app.ports.firebaseDrawRcv.send(draw);
});

// sends message to clients about what combos is revealed
firebase.database().ref("/game/combo").on("child_added", function(snapshot) {
  var comboRet = snapshot.val();
  app.ports.firebaseComboRcv.send(comboRet);
});

// sends message to clients that a player just chi'ed
firebase.database().ref("/game/chi").on("child_added", function(snapshot) {
  app.ports.firebaseChiRcv.send(null);
});

// sends message to clients that a player just hidden kong'ed
firebase.database().ref("/game/hid_kong").on("child_added", function(snapshot) {
  app.ports.firebaseHidKongRcv.send(null);
});

// sends a message to clients that a player won
firebase.database().ref("/game/hu").on("child_added", function(snapshot) {
  var player = snapshot.val();
  app.ports.firebaseHuRcv.send(player);
});

// sends message to clients that a player declared small kong
firebase.database().ref("game/small_kong").on("child_added", function(snapshot) {
  var ret = snapshot.val();
  app.ports.firebaseSmallKongRcv.send(ret);
});

// sends all hands (at end of game) when ready
firebase.database().ref("game/hands").on("child_changed", function(snapshot) {
  var hands = snapshot.val();
  var ready = true;
  var handsArr = [];
  for (let i = 0; i < 4; i++) {
    if (!(i in hands)) {
      ready = false;
      break;
    }
    else {
      var ret = hands[i];
      if (!("tiles" in ret)) {
        ret["tiles"] = [];
      }
      if (!("combos" in ret)) {
        ret["combos"] = [];
      }
      handsArr.push(ret);
    }
  }
  // if received all four hands, send the information back to all players
  if (ready) {
    app.ports.firebaseHandRcv.send(handsArr);
  }
});

// restart the game.
app.ports.firebaseRestart.subscribe(function() {
  // reset database state
  firebase.database().ref("/deck").set(null);
  firebase.database().ref("/init/start").set(null);
  firebase.database().ref("game").set(null);

  // start a new game
  startGame();
});

// receive request to reset. Simply switch started to false.
app.ports.firebaseReset.subscribe(function() {
  firebase.database().ref("/started/state").set(0);
});