var spid = Cookies.get('scrabble_player_id');
if (spid == undefined) {
    alert('Unknown User, going back to lobby...');
    window.location.href = 'index.html';
}

$(document).ready(function(){

    // $('#signout').click(function(){
    //     // TODO: do websocket call, to remove back-end info too...
    //     Cookies.remove('scrabble_player_id');
    //     window.location.href = 'index.html';
    // });

    // JQuery Websocket:
    var url = window.location.href;
    var arr = url.split("/");
    var ws_url = "ws://"+arr[2]+"/swg";
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
        console.log(event.data);
    }

    function onOpen(event) {
        request_player_hand();
    }

    function onMessage(event) {
        //console.log(event.data);
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('redirect')) {
            $('#status').html('Going to redirect back to lobby...');
            setTimeout(function(){
                window.location.href = json_data.redirect;
            }, 500);
        } else if(json_data.hasOwnProperty('player_hand')) {
            create_player_hand(json_data.player_hand);
        }
    }

    function request_player_hand() {
        webSocket.send(JSON.stringify([
            {'request':'player_hand'},
            {'player_id':spid},
            {'guid': guid()}
        ]));
    }

    function create_player_hand(player_tiles) {
        // var squares = '';
        // for (var t = 1; t <= 7; t++) {
        //     squares +=
        //         '<td id="scrabble_hand_square_tile_'+t+'" class="scrabble_hand_square" >'
        //         +'<div class="player_tiles" id="player_tile_'+t+'">'+player_tiles[t-1]+'</div>'
        //         +'</td>';
        // }
        // $('#scrabble_player_tiles').append('<tr>'+squares+'</tr>');
    }

    $('.player_tiles').draggable({
        containment: 'document',
        stack: '.game_square',
        snap: '.game_square',
        cursor: 'move',
        // start: function(){
        //     alert('what a drag...');
        // },
        stop: function(){
            //alert('dropped');
        }
    });

    $('.scrabble_hand_square').droppable({
        drop: function( event, ui ) {
            // console.log(event);
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
        console.log(event);
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

    function get_tiles_on_board() {
        var player_tiles_on_board = [];
        for (var i = 1; i <= 7; i++) {
            var id = $('#player_tile_'+i).parent().attr("id");
            if( id == 'scrabble_hand_square_tile_'+i){ // still in hand
                console.log('tile '+i+' in hand ');
            } else { // on board
                console.log('tile '+i+' at '+id);
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
                {'player_tiles': [
                    get_tiles_on_board()
                ]}
            )
        );
    });

    $('#leave_game').click(function(){
        var spid = Cookies.get('scrabble_player_id');
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(
            JSON.stringify(
                {
                    'player_leave': spid,
                    'gid': gid
                }
            )
        );
    })

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

    // Creating the scrabble board
    var double_letter_score = [
        4,
        12,
        37,
        39,
        46,
        53,
        60,
        93,
        97,
        99,
        103,
        109,
        117,
        123,
        127,
        129,
        133,
        166,
        173,
        180,
        187,
        189,
        214,
        222
    ];
    double_word_score = [
        17,
        29,
        33,
        43,
        49,
        57,
        65,
        71,
        155,
        161,
        169,
        177,
        183,
        193,
        197,
        209
    ];
    var tripple_letter_score = [
        21,
        25,
        77,
        81,
        85,
        89,
        137,
        141,
        145,
        149,
        201,
        205
    ];
    var tripple_word_score = [
        1,
        8,
        15,
        106,
        120,
        211,
        218,
        225
    ];

    // 113 Center

    // create the scrabble board...
    //for (var y = -4; y <= 4; y++) {
    var count = 0;
    for (var y = 1; y <= 15; y++) {
        var row='<tr>';
        //for (var x = -4; x <= 4; x++) {
        for (var x = 1; x <= 15; x++) {
            count += 1;
            var css_class = 'scrabble_board_square ';
            var square_text = '&nbsp';
            var id = 'Y'+y+'_X'+x;
            if( count == 113 ){
                css_class = 'middle_square';
            } else if ( double_letter_score.includes(count) ){
                css_class += ' double_letter_score';
                square_text = 'DLS';
            } else if( double_word_score.includes(count) ){
                css_class += ' double_word_score';
                square_text = 'DWS';
            } else if( tripple_letter_score.includes(count) ){
                css_class += ' tripple_letter_score';
                square_text = 'TLS';
            } else if( tripple_word_score.includes(count) ){
                css_class += ' tripple_word_score';
                square_text = 'TWS';
            }
            row += '<td id="id" class="'+css_class+' game_square">';
            //row += count;
            row += square_text;
            row += '</td>';
        }
        $('#scrabble_board').append(row+'</tr>');
    }

    // Once Tile state is resolved, allow play/interaction.

});

// 2 blank tiles (scoring 0 points)
// 1 point: E ×12, A ×9, I ×9, O ×8, N ×6, R ×6, T ×6, L ×4, S ×4, U ×4
// 2 points: D ×4, G ×3
// 3 points: B ×2, C ×2, M ×2, P ×2
// 4 points: F ×2, H ×2, V ×2, W ×2, Y ×2
// 5 points: K ×1
// 8 points: J ×1, X ×1
// 10 points: Q ×1, Z ×1
