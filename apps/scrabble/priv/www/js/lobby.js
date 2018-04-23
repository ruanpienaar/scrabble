s = Cookies.get('scrabble_player_id');
if(s == undefined){
    console.log('Anonymous user started...');
    // TODO: Set field 'anon' to true
    // TODO: generate anon cookie value...
} else {
    console.log('Scrabble player id '+s+' started...');
}

$(document).ready(function(){


    function connect_websocket(){
        var url = window.location.href;
        var arr = url.split("/");
        var ws_url = "ws://"+arr[2]+"/sws";
        return new WebSocket(ws_url);
    }

    // JQuery Websocket:
    var webSocket = connect_websocket();

    webSocket.onerror = function(event) {
        onError(event)
    }

    webSocket.onopen = function(event) {
        onOpen(event)
    }

    webSocket.onmessage = function(event) {
        onMessage(event)
    }

    webSocket.onclose = function(){
        console.log('Socket Status: '+webSocket.readyState+' (Closed)');
    }

    // Websocket keepalive
    function send_echo() {
        //console.log(webSocket);
        webSocket.send(JSON.stringify({'request':'echo'}));
    }
    setInterval(send_echo, 30000);

    // websocket handler functions
    function onError(event) {
        console.log(event);
    }

    function onOpen(event) {
        console.log(event);
        get_lobby_players();
        get_lobby_games();
    }

    function onMessage(event) {
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('response')) {
            if(json_data.response == 'echo_reply'){
                //
            } else {
                console.log('Unhandled response '+json_data.response);
            }
        } else if(json_data.hasOwnProperty('lobby_players')) {
            spid = Cookies.get('scrabble_player_id');
            $('#lobby_players').empty();
            // if spid not in lobby_players ( value was sent from back-end )
            // then register the spid, which will send over the list of lobby players again
            //console.log(json_data.lobby_players);
            //console.log('cookie '+spid);
            if( json_data.lobby_players.includes(spid) || spid == undefined) {
                //console.log('do NOT REregister player - player cookie undefiend, or in list of players');
                for (var i = json_data.lobby_players.length - 1; i >= 0; i--) {
                    $('#lobby_players').append('<li>'+json_data.lobby_players[i]+'</li>');
                }
            } else {
                //console.log('do REregister player - player cookie is not in list of players');
                register_player(spid);
            }
        } else if(json_data.hasOwnProperty('lobby_games')) {
            spid = Cookies.get('scrabble_player_id');
            $('#lobby_games').empty();
            for (var i = json_data.lobby_games.length - 1; i >= 0; i--) {
                var game_num = json_data.lobby_games[i].number;
                //TODO: urlencode the URI's
                var join_html = '<a class="btn btn-success" '
                               +'href="awaiting_players.html'
                               +'?a=j&gid='+game_num+'&spid='+spid+'"'
                               +' role="button" >Join</button>';
                // var spectate_html = '<a class="btn btn-success"'
                //                   +' href="awaiting_players.html'
                //                   +'?a=s&gid='+game_num+'&spid='+spid+'"'
                //                   + ' role="button">Spectate</a>';
                var spectate_html = '';
                var num_html = 'game # '+game_num;
                var state_html = json_data.lobby_games[i].state;
                //var players_html = json_data.lobby_games[i].players;
                var players_html = '';
                for (var j = json_data.lobby_games[i].players.length - 1; j >= 0; j--) {
                    players_html += '<p>'+json_data.lobby_games[i].players[j].spid+'</p>';
                }
                $('#lobby_games').append('<tr>'
                    +'<td>'+join_html+spectate_html+'</td>'
                    +'<td>'+num_html+'</td>'
                    +'<td>'+state_html+'</td>'
                    +'<td>'+players_html+'</td>'
                    +'</tr>');
            };
        } else {
            console.log(event.data);
        }
    }

    function register_player(spid) {
        Cookies.set('scrabble_player_id', spid);
        guid = make_guid();
        webSocket.send(JSON.stringify(
            {
                'register_lobby_player': spid,
                'guid': guid
            }
        ));
        window.location.href = 'index.html';
    }

    function get_lobby_players() {
        webSocket.send(JSON.stringify(
            {'request':'lobby_players'}
        ));
    }

    function get_lobby_games(){
        webSocket.send(JSON.stringify(
            {'request': 'lobby_games'}
        ));
    }

    // make guid to identify browser
    function make_guid() {
        var nav = window.navigator;
        var screen = window.screen;
        var guid = nav.mimeTypes.length;
        guid += nav.userAgent.replace(/\D+/g, '');
        guid += nav.plugins.length;
        guid += screen.height || '';
        guid += screen.width || '';
        guid += screen.pixelDepth || '';
        return guid;
    }

    // Join Loby
    $('#join_lobby').click(function() {
        if(Cookies.get('scrabble_player_id') == undefined){
            $('#msg').dialog({
                autoOpen:true,
                modal:true,
                title: 'Enter your player name:',
                buttons: {
                    Go: function() {
                        $(this).dialog('close');
                    }
                },
                close: function() {
                    var s = $('#sumpin').val();
                    if (s.length<3){
                        alert('Please type more than 3 characters');
                    } else {
                        //alert('Welcome : ' + s);
                        register_player(s);
                    }
                }
            });
        }

    });

    // Signout:
    $('#signout').click(function(){
        spid = Cookies.get('scrabble_player_id');
        if(spid == undefined){
            console.log('Not in Lobby!');
        } else {
            guid = make_guid();
            webSocket.send(JSON.stringify(
                {'deregister_lobby_player': spid, 'guid': guid}
            ));
            console.log('deleting cookie');
            Cookies.remove('scrabble_player_id');
            // if(Cookies.remove('scrabble_player_id') == undefined){
                 console.log('Cookie deleted')
            // } else {
            //     //console.log('Cookie NOT deleted')
            // }
        }
    });

    // Create new game:
    $('#create_new_game').click(function(){
        webSocket.send(
            JSON.stringify({'request': 'create_new_game'})
        );
    });

});