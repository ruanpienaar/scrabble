s = Cookies.get('scrabble_player_id');
if(s == undefined){
    window.location.href = 'index.html';
}

$(document).ready(function(){
    //console.log('Document Ready...');

    function connect_websocket(){
        var url = window.location.href;
        var arr = url.split("/");
        var ws_url = "ws://"+arr[2]+"/swp"+window.location.search;
        return new WebSocket(ws_url);
    }

    // JQuery Websocket:
    var webSocket = connect_websocket();

    webSocket.onerror = function(event) {
        onError(event);
    }

    webSocket.onopen = function(event) {
        onOpen(event);
    }

    webSocket.onmessage = function(event) {
        onMessage(event);
    }

    webSocket.onclose = function(){
        console.log('Socket Status: '+webSocket.readyState+' (Closed)');
    }

    // Websocket keepalive
    function send_ping() {
        //console.log(webSocket);
        webSocket.send(JSON.stringify({'request':'ping'}));
    }
    setInterval(send_ping, 30000);

    // websocket handler functions
    function onError(event) {
        console.log(event);
    }

    function onOpen(event) {
        console.log(event);
    }

    function onMessage(event) {
        console.log(event.data);
        // if json == {"redirect":"index.html"}
        // window.location = "index.html"
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('response')) {
            if(json_data.response == 'ping_reply'){
                //
            } else {
                console.log('Unhandled response '+json_data.response);
            }
        } else if(json_data.hasOwnProperty('redirect')) {
            $('#status').html('Going to redirect to '+json_data.redirect+'...');
            setTimeout(function(){
                window.location.href = json_data.redirect;
            }, 1000);
        } else if(json_data.hasOwnProperty('awaiting_game')) {
            gid = json_data.awaiting_game.number;
            Cookies.set('scrabble_game_id', gid);
            $('#game_number').empty();
            $('#game_players').empty();
            $('#game_state').empty();
            $('#game_number').html(gid);
            for(var i=json_data.awaiting_game.players.length-1; i >= 0; i--){
                var playerspid = json_data.awaiting_game.players[i].spid;
                var playerstate = json_data.awaiting_game.players[i].game_state;
                $('#game_players').append(
                    '<p>'+playerspid+' - '+playerstate+' </p>'
                );
            }
            var gs = json_data.awaiting_game.state;
            // if(gs == 'starting'){

            // } else {

            // }
            $('#game_state').html(gs);
        } else if(json_data.hasOwnProperty('player_ready')){
            console.log(json_data);
        }
    }

    $('#leave').click(function(){
        var spid = Cookies.get('scrabble_player_id');
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(JSON.stringify(
            {
                'player_leave': spid,
                'gid': gid
            }
        ));
    });

    $('#ready').click(function(){
        var spid = Cookies.get('scrabble_player_id');
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(JSON.stringify(
            {
                'player_ready': spid,
                'gid': gid
            }
        ));
    });

    $('#start').click(function(){
        var spid = Cookies.get('scrabble_player_id');
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(JSON.stringify(
            {
                'game_start': spid,
                'gid': gid
            }
        ));
    });

});
