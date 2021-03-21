var socket = io();

// Receiving from Elm
app.ports.firebaseCreateGame.subscribe(function() {
    socket.emit('createGame');
});

app.ports.firebaseUpdateLobbies.subscribe(function() {
    socket.emit('updateLobbies');
});

app.ports.firebaseJoinGame.subscribe(function(gid) {
    socket.emit('joinGame', gid);
});

app.ports.firebaseDrop.subscribe(function(tileNum) {
    socket.emit('drop', tileNum);
});

app.ports.firebaseDraw.subscribe(function(player) {
    socket.emit('draw', player);
});

app.ports.firebaseCombo.subscribe(function(send) {
    socket.emit('combo', send);
});

app.ports.firebaseChi.subscribe(function(player) {
    socket.emit('chi', player);
});

app.ports.firebaseHidKong.subscribe(function(player) {
    socket.emit('hidKong', player);
});

app.ports.firebaseSmallKong.subscribe(function(tile) {
    socket.emit('smallKong', tile);
});

app.ports.firebaseHu.subscribe(function(pair) {
    socket.emit('hu', pair);
});

app.ports.firebaseSendHand.subscribe(function(handInfo) {
    socket.emit("sendHand", handInfo);
});

app.ports.firebaseReset.subscribe(function() {
    socket.emit('reset');
});

// Receiving from Python
socket.on('noCreate', function() {
    app.ports.firebaseNoCreate.send(null);
});

socket.on('joinRes', function(player_id) {
    app.ports.firebaseJoinRes.send(player_id);
});

socket.on('start', function(start) {
    app.ports.firebaseStart.send(start);
});

socket.on('dropRes', function(tileNum) {
    app.ports.firebaseDropRcv.send(tileNum);
});

socket.on('drawRes', function(drawNum) {
    app.ports.firebaseDrawRcv.send(drawNum);
});

socket.on('comboRes', function(ret) {
    app.ports.firebaseComboRcv.send(ret);
});

socket.on('chiRes', function() {
    app.ports.firebaseChiRcv.send(null);
});

socket.on('hidKongRes', function() {
    app.ports.firebaseHidKongRcv.send(null);
});

socket.on('smallKongRes', function(tile) {
    app.ports.firebaseSmallKongRcv.send(tile);
});

socket.on('huRes', function(ret) {
    app.ports.firebaseHuRcv.send(ret);
});

socket.on('sendHandRcv', function(ret) {
    app.ports.firebaseHandRcv.send(ret);
});

socket.on('gameInfo', function(ret) {
    app.ports.firebaseLobbiesRcv.send(ret);
});

socket.on('terminated', function() {
    app.ports.firebaseTerminated.send(null);
});

socket.on('resetRcv', function() {
    app.ports.firebaseResetAll.send(false);
});
