$(document).ready(function(){
    // get the player id
    var spid = Cookies.get('scrabble_player_id');
    if (spid == undefined) {
        spid = prompt('Enter your username...');
        // TODO: check if user is already present in backend...
        // TODO: redementry user input validation
        Cookies.set('scrabble_player_id', spid);
    }
    console.log('Scrabble player id '+spid+' started...');

    $('#signout').click(function(){
        // TODO: do websocket call, to remove back-end info too...
        Cookies.remove('scrabble_player_id');
    });

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
        register_player();
    }

    function onMessage(event) {

        // if event.data ( register done )
        // call
        // get_lobby_players();

        console.log(event.data);
    }

    function register_player() {
        spid = Cookies.get('scrabble_player_id');
        guid = guid();
        webSocket.send(JSON.stringify(
            {
                'register_lobby_player': spid,
                'guid': guid
            }
        ));
    }

    function get_lobby_players() {
        webSocket.send(JSON.stringify(
            {'request':'lobby_players'}
        ));
    }

    function guid() {
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

});