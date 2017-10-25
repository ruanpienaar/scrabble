$(document).ready(function(){

    // Query backend for tile state.

    // Query for player hand/tiles
    // This array should come from a websocket/ajax BackEnd call.
    player_tiles = [
        '<div class="player_tiles">a</div>',
        '<div class="player_tiles">b</div>',
        '<div class="player_tiles">c</div>',
        '<div class="player_tiles">d</div>',
        '<div class="player_tiles">e</div>',
        '<div class="player_tiles">f</div>',
        '<div class="player_tiles">g</div>'
    ];

    // Query for board tiles

    var squares = '';
    for (var t = 0; t <= 6; t++) {
        squares +=
        '<tr>'
        +'<td class="scrabble_square" id="player_tile_'+t+'">'
        +player_tiles[t]
        +'</td>'
        +'</tr>';
    }
    $('#scrabble_player_tiles').append(squares);

    // Make the player tiles Draggable:
    // $('.player_tiles').each(function(){
    //    this.draggable();
    // });
    $('.player_tiles').draggable({
        containment: 'document'
    });

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
            var css_class = 'scrabble_square ';
            var square_text = '&nbsp';
            var id = 'Y'+y+'X'+x;
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
            row += '<td id="id" class="'+css_class+'">';
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