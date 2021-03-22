var socket = io();

// Receiving from Elm
app.ports.socketCreateGame.subscribe(function() {
    socket.emit('createGame');
});

app.ports.socketUpdateLobbies.subscribe(function() {
    socket.emit('updateLobbies');
});

app.ports.socketJoinGame.subscribe(function(gid) {
    socket.emit('joinGame', gid);
});

app.ports.socketDrop.subscribe(function(tileNum) {
    socket.emit('drop', tileNum);
});

app.ports.socketDraw.subscribe(function(player) {
    socket.emit('draw', player);
});

app.ports.socketCombo.subscribe(function(send) {
    socket.emit('combo', send);
});

app.ports.socketChi.subscribe(function(player) {
    socket.emit('chi', player);
});

app.ports.socketHidKong.subscribe(function(player) {
    socket.emit('hidKong', player);
});

app.ports.socketSmallKong.subscribe(function(tile) {
    socket.emit('smallKong', tile);
});

app.ports.socketHu.subscribe(function(pair) {
    socket.emit('hu', pair);
});

app.ports.socketSendHand.subscribe(function(handInfo) {
    socket.emit("sendHand", handInfo);
});

app.ports.socketReset.subscribe(function() {
    socket.emit('reset');
});

app.ports.socketRestart.subscribe(function() {
    socket.emit('restart');
});

// Receiving from Python
socket.on('noCreate', function() {
    app.ports.socketNoCreate.send(null);
});

socket.on('joinRes', function(player_id) {
    app.ports.socketJoinRes.send(player_id);
});

socket.on('start', function(start) {
    app.ports.socketStart.send(start);
});

socket.on('dropRes', function(tileNum) {
    app.ports.socketDropRcv.send(tileNum);
});

socket.on('drawRes', function(drawNum) {
    app.ports.socketDrawRcv.send(drawNum);
});

socket.on('comboRes', function(ret) {
    app.ports.socketComboRcv.send(ret);
});

socket.on('chiRes', function() {
    app.ports.socketChiRcv.send(null);
});

socket.on('hidKongRes', function() {
    app.ports.socketHidKongRcv.send(null);
});

socket.on('smallKongRes', function(tile) {
    app.ports.socketSmallKongRcv.send(tile);
});

socket.on('huRes', function(ret) {
    app.ports.socketHuRcv.send(ret);
});

socket.on('sendHandRcv', function(ret) {
    app.ports.socketHandRcv.send(ret);
});

socket.on('gameInfo', function(ret) {
    app.ports.socketLobbiesRcv.send(ret);
});

socket.on('terminated', function() {
    app.ports.socketTerminated.send(null);
});

socket.on('resetRcv', function() {
    app.ports.socketResetAll.send(false);
});
