

$(document).ready(function(){
    var spid = Cookies.get('scrabble_player_id');
    if (spid == undefined) {
        alert('Unknown User, going back to lobby...');
        window.location.href = 'index.html';
    }

    $('#signout').click(function(){
        // TODO: do websocket call, to remove back-end info too...
        Cookies.remove('scrabble_player_id');
        window.location.href = 'index.html';
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
        //webSocket.send('Hello, World!');
        request_player_hand();
    }

    function onMessage(event) {
        // console.log('websocket opened');
        // get hand...
        //request_player_hand();

        // if(){
        // }
        console.log(event.data);
    }

    function request_player_hand() {
        webSocket.send(JSON.stringify([
            {'request':'player_hand'},
            {'player_id':spid}
        ]));
    }

    function create_player_hand() {
        // Query backend for tile state.
        // Query for player hand/tiles
        // This array should come from a websocket/ajax BackEnd call.
        // player_tiles = [
        //     '<div class="player_tiles" id="player_tile_1">a</div>',
        //     '<div class="player_tiles" id="player_tile_2">b</div>',
        //     '<div class="player_tiles" id="player_tile_3">c</div>',
        //     '<div class="player_tiles" id="player_tile_4">d</div>',
        //     '<div class="player_tiles" id="player_tile_5">e</div>',
        //     '<div class="player_tiles" id="player_tile_6">f</div>',
        //     '<div class="player_tiles" id="player_tile_7">g</div>'
        // ];

        var squares = '';
        for (var t = 1; t <= 7; t++) {
            squares +=
                '<td id="scrabble_hand_square_tile_'+t+'" class="scrabble_hand_square" >'
                +player_tiles[t-1]
                +'</td>';
        }
        $('#scrabble_player_tiles').append('<tr>'+squares+'</tr>');
    }

    $('.player_tiles').draggable({
        containment: 'document'
    });

    $('.scrabble_hand_square').droppable({
        drop: function( event, ui ) {
            var dropped = ui.draggable;
            var droppedOn = $(this);
            // console.log(droppedOn[0].childNodes.length);
            alert(droppedOn[0].childNodes.length);
            if(droppedOn[0].childNodes.length <= 1 ){
                // TODO: mark this tile as ready to be submitted.
                $(dropped).attr('on_board', 'false');
                $(dropped).detach().css({top: 0,left: 0}).appendTo(droppedOn);
            } else {
                // TODO: Snapp tile back into hand...
                console.log(droppedOn[0].childNodes);
                //alert('Too Many...');
            }
        }

    });

    $('.scrabble_board_square').droppable({
      drop: function( event, ui ) {
        var dropped = ui.draggable;
        var droppedOn = $(this);
        // console.log(droppedOn[0].childNodes.length);
        if(droppedOn[0].childNodes.length == 1){
            // TODO: mark this tile as ready to be submitted.
            $(dropped).attr('on_board', 'true');
            $(dropped).detach().css({top: 0,left: 0}).appendTo(droppedOn);
        } else {
            // TODO: Snapp tile back into hand...
            alert('Too Many...');
        }
      }
    });

    function get_tiles_on_board(){
        var player_tiles_on_board = [];
        for (var i = 1; i <= 7; i++) {
            var id = $('#player_tile_'+i).parent().attr("id");
            if( id == 'scrabble_hand_square_tile_'+i){ // still in hand
                //console.log('tile '+i+' in hand ');
            } else { // on board
                //console.log('tile '+i+' at '+id);
                var tile = 'player_tile_'+i;
                player_tiles_on_board.push([
                    {tile: tile},
                    {position: id}
                ]);
            }
        }
        return player_tiles_on_board;
    }

    $('#submit_tiles').click(function(){
        webSocket.send(
            JSON.stringify(
                // {'player_tiles': [
                //     {'player_tile_1': get_tile_x_location(1)},
                //     {'player_tile_2': get_tile_x_location(2)},
                //     {'player_tile_3': get_tile_x_location(3)},
                //     {'player_tile_4': get_tile_x_location(4)},
                //     {'player_tile_5': get_tile_x_location(5)},
                //     {'player_tile_6': get_tile_x_location(6)},
                //     {'player_tile_7': get_tile_x_location(7)}
                // ]}
                {'player_tiles': [
                    get_tiles_on_board()
                ]}
            )
        );
    });

});