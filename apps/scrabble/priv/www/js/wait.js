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
    }

    function onMessage(event) {
        console.log(event.data);
        // if json == {"redirect":"index.html"}
        // window.location = "index.html"
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('redirect')) {
            $('#status').html('Going to redirect back to lobby...');
            setTimeout(function(){
                window.location.href = json_data.redirect;
            }, 5000);

        } else if(json_data.hasOwnProperty('awaiting_game')) {
            $('#game').empty();
            $('#game_number').html('GAME');
            $('#game_players').html('GAME');
            $('#game_state').html('GAME');
        }
    }

});