$(document).ready(function(){
    s = Cookies.get('scrabble_player_id');
    if(s == undefined){
        console.log('Anonymous user started...');
    } else {
        console.log('Scrabble player id '+s+' started...');    
    }
    
    // // get the player id
    // var spid = Cookies.get('scrabble_player_id');
    // if (spid == undefined) {
    //     //spid = prompt('Enter your username...');
    //     // TODO: check if user is already present in backend...
    //     //if(spid != null || spid != ""){
    //         //Cookies.set('scrabble_player_id', spid);
    //     //}
    // }

    // JQuery Websocket:
    var url = window.location.href;
    var arr = url.split("/");
    var ws_url = "ws://"+arr[2]+"/sws";
    var webSocket = new WebSocket(ws_url);

    webSocket.onerror = function(event) {
        onError(event)
    };

    webSocket.onopen = function(event) {
        onOpen(event)
    };

    webSocket.onmessage = function(event) {
        onMessage(event)
    };

    // websocket handler functions
    function onError(event) {
        console.log(event.data);
    }

    function onOpen(event) {
        get_lobby_players();
    }

    function onMessage(event) {
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('lobby_players')) {
            $('#lobby_players').empty();
            for (var i = json_data.lobby_players.length - 1; i >= 0; i--) {
                //alert('Lobby player : '+json_data.lobby_players[i]);
                $('#lobby_players').append('<li>'+json_data.lobby_players[i]+'</li>');
            }
        } else {
            console.log(event.data);
        }
    }

    function register_player(spid) {
        if(Cookies.get('scrabble_player_id') == undefined){
            Cookies.set('scrabble_player_id', spid);
            guid = make_guid();
            webSocket.send(JSON.stringify(
                {
                    'register_lobby_player': spid,
                    'guid': guid
                }
            ));
        } else {
            console.log('Already registered ...');
        }
    }

    function get_lobby_players() {
        webSocket.send(JSON.stringify(
            {'request':'lobby_players'}
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
    });

    // Signout:
    $('#signout').click(function(){
        spid = Cookies.get('scrabble_player_id');
        guid = make_guid();
        webSocket.send(JSON.stringify(
            {
                'deregister_lobby_player': spid,
                'guid': guid
            }
        ));
        Cookies.remove('scrabble_player_id');
        window.location.href = 'index.html';
    });

});