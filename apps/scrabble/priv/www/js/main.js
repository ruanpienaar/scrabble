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

    var squares = '';
    for (var t = 0; t <= 6; t++) {
        squares +=
            '<td class="scrabble_hand_square" id="player_tile_'+t+'">'
            +player_tiles[t]
            +'</td>';
    }
    $('#scrabble_player_tiles').append('<tr>'+squares+'</tr>');

    $('.player_tiles').draggable({
        containment: 'document'
    });

    $('.scrabble_board_square').droppable({
      drop: function( event, ui ) {
        var dropped = ui.draggable;
        var droppedOn = $(this);
        $(dropped).detach().css({top: 0,left: 0}).appendTo(droppedOn);
      }
    });
});