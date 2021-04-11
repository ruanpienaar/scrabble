var spid = Cookies.get('scrabble_player_id');
if (spid == undefined) {
    alert('Unknown User, going back to lobby...');
    window.location.href = 'index.html';
}
var gid = Cookies.get('scrabble_game_id');
if (gid == undefined) {
    alert('Unknown Game, going back to lobby...');
    window.location.href = 'index.html';
}

$(document).ready(function(){

    // $('#signout').click(function(){
    //     // TODO: do websocket call, to remove back-end info too...
    //     Cookies.remove('scrabble_player_id');
    //     window.location.href = 'index.html';
    // });

    // JQuery Web socket:
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

    // Web-socket keep alive
    function send_ping() {
        //console.log(webSocket);
        webSocket.send(JSON.stringify({'request':'ping'}));
    }
    setInterval(send_ping, 30000);

    // web-socket handler functions
    function onError(event) {
        console.log(event.data);
    }

    function onOpen(event) {
        request_player_hand();
        request_game_board();
    }

    function onMessage(event) {
        //console.log(event.data);
        var json_data = JSON.parse(event.data);
        if(json_data.hasOwnProperty('response')) {
            if(json_data.response == 'ping_reply'){
                //
            } else if (json_data.response == 'refresh_board') {
                request_player_hand();
                request_game_board();
            } else if (json_data.response.hasOwnProperty('game_board')) {
                //alert(json_data.response.game_board);
                create_game_board(json_data.response.game_board)

            } else {
                console.log('Unhanded response '+json_data.response);
            }
        } else if(json_data.hasOwnProperty('redirect')) {
            $('#status').html('Going to redirect back to lobby...');
            setTimeout(function(){
                window.location.href = json_data.redirect;
            }, 500);
        } else if(json_data.hasOwnProperty('player_hand')) {
            create_player_hand(json_data.player_hand);
        }
    }

    function request_player_hand() {
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(JSON.stringify([
            {'request': 'player_hand'},
            {'player_id': spid},
            {'gid': gid},
            {'guid': guid()}
        ]));
    }

    function request_game_board(){
        webSocket.send(JSON.stringify([
            {'request': 'game_board'},
            {'player_id': spid},
            {'gid': gid},
            {'guid': guid()}
        ]));
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
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(
            JSON.stringify(
                {
                    'player_leave': spid,
                    'gid': gid
                }
            )
        );
    });

    $('#place_word').click(function(){
        var gid = Cookies.get('scrabble_game_id');
        webSocket.send(
            JSON.stringify(
                {
                    'place_word': spid,
                    'gid': gid,
                    'tiles': get_word_placement_tiles()
                }
            )
        );
    });

    function create_player_hand(player_tiles) {

        //alert(player_tiles);
        for (var t = 1; t <= 7; t++) {
            // "player_hand_tile_" ++ t
            var tt = player_tiles[t-1];
            var ttt = "#player_hand_tile_" + t
            $(ttt).val(tt);
        }

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
                //tiles = get_word_placement_tiles();
            }
        }
        return player_tiles_on_board;
    }

    function get_word_placement_tiles(){
        // maybe don't send the entire board....
        var board_struct = new Array ();
        for (var board_x = 1; board_x <= 15; board_x++){
            var y_array = new Array ();
            for (var board_y = 1; board_y <= 15; board_y++){
                cell = '#board_' + board_x + '_' + board_y;
                val = $(cell).val();
                if ( val != "" ) {
                    console.log(cell + ' - ' + val);
                }
                //y_array[board_y-1] = val;
                y_array.push({
                    y: board_y,
                    v: val
                });
             }
             console.log(y_array);
             // board_struct[board_x-1] = y_array;
             board_struct.push({ 
                 x: board_x,
                 v: y_array
             })
        }
        //console.log(board_struct);
        return board_struct;
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


// 2 blank tiles (scoring 0 points)
// 1 point: E ×12, A ×9, I ×9, O ×8, N ×6, R ×6, T ×6, L ×4, S ×4, U ×4
// 2 points: D ×4, G ×3
// 3 points: B ×2, C ×2, M ×2, P ×2
// 4 points: F ×2, H ×2, V ×2, W ×2, Y ×2
// 5 points: K ×1
// 8 points: J ×1, X ×1
// 10 points: Q ×1, Z ×1

    // Once Tile state is resolved, allow play/interaction.

    function create_game_board(backend_matrix){
        // simpler board creation
        var matrix = '';
        var input_name = '';
        var backend_matrix_tile = '';

        // 'board_X_Y'

        for (var board_x = 1; board_x <= 15; board_x++){
            matrix += '<tr>';
            for (var board_y = 1; board_y <= 15; board_y++){
                //backend_matrix_tile = 'board_'+ board_x + '_' + board_y;
                var cell_val = '';
                if(backend_matrix[board_x][board_y] != ""){
                    //console.log(board_x + '_' + board_y + backend_matrix[board_x][board_y]);
                    cell_val = 'value=\"' + backend_matrix[board_x][board_y] + '\" disabled=\"disabled\" ';
                }
                matrix += '<td>';
                input_name = 'board_'+board_x+'_'+board_y;
                if ( board_x == 8 && board_y == 8) {
                    matrix += '<input '+cell_val+' style="background: red;" type"text" id="'+input_name+'" size="1" maxlength="1" onchange="check(this.id);" onkeydown="back_into_hand(this.id, this.value);" onkeyup="check_matrix_input(this.id, this.value);" />';
                } else {
                    matrix += '<input '+cell_val+' type"text" id="'+input_name+'" size="1" maxlength="1" onchange="check(this.id);" onkeydown="back_into_hand(this.id, this.value);" onkeyup="check_matrix_input(this.id, this.value);" />';

                }
                matrix += '</td>';
            }
            matrix += '</tr>';
        }
        $('#simplified_board').empty();
        $('#simplified_board').append(matrix);
    }

});

// TODO: complete front end validation

function check(input_id){
    // alert(input_id);
}

function back_into_hand(input_id, input_value){
    // alert(input_value);
    // if(input_value == ''){
    //     alert('nothing to restore');
    // } else {
    //     alert('restore to hand');
    // }
    // $('#player_hand_tile_1').focus();
}

function check_matrix_input(input_id, input_value){
    // if (input_value == ''){ // check if in hand
    //     var hand_tile = '';
    //     for (var i = 1; i < 8; i++) {
    //         hand_tile = '#player_hand_tile_'+i;
    //         if($(hand_tile).val() == input_value){
    //             $(hand_tile).val('');
    //             return true;
    //         }
    //     }
    //     $('#'+input_id).val('');
    // } else { // place back in hand

    // }
    // $('#player_hand_tile_1').focus();
}
