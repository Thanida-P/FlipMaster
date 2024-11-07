% reversi rules

% board size
size(8).

% check if a move is valid
valid_move(Board, Player, X, Y) :-
    size(Size),
    MaxIndex is Size - 1,
    between(0, MaxIndex, X),
    between(0, MaxIndex, Y),
    get(Board, X, Y, 0),
    can_flip_any_direction(Board, Player, X, Y).

% check if a move can flip pieces in any direction
can_flip_any_direction(Board, Player, X, Y) :-
    (check_direction(Board, Player, X, Y, 1, 0);
     check_direction(Board, Player, X, Y, 1, 1);
     check_direction(Board, Player, X, Y, 0, 1);
     check_direction(Board, Player, X, Y, -1, 1);
     check_direction(Board, Player, X, Y, -1, 0);
     check_direction(Board, Player, X, Y, -1, -1);
     check_direction(Board, Player, X, Y, 0, -1);
     check_direction(Board, Player, X, Y, 1, -1)).

% check if a move can flip pieces in a specific direction
check_direction(Board, Player, X, Y, DX, DY) :-
    X1 is X + DX,
    Y1 is Y + DY,
    size(Size),
    X1 >= 0, X1 < Size,
    Y1 >= 0, Y1 < Size,
    opposite(Player, Opponent),
    get(Board, X1, Y1, Opponent),
    find_player_piece(Board, Player, X1, Y1, DX, DY).

% find a player piece in a specific direction
find_player_piece(Board, Player, X, Y, DX, DY) :-
    X1 is X + DX,
    Y1 is Y + DY,
    size(Size),
    X1 >= 0, X1 < Size,
    Y1 >= 0, Y1 < Size,
    get(Board, X1, Y1, Value),
    (Value = Player;
        (opposite(Player, Value),
        find_player_piece(Board, Player, X1, Y1, DX, DY))).

% Make move and flip pieces
make_move(Board, Player, X, Y, NewBoard) :-
    set(Board, X, Y, Player, TempBoard),
    flip_all_directions(TempBoard, Player, X, Y, NewBoard).

% flip pieces in all directions
flip_all_directions(Board, Player, X, Y, NewBoard) :-
    flip_direction(Board, Player, X, Y, 1, 0, B1),
    flip_direction(B1, Player, X, Y, 1, 1, B2),
    flip_direction(B2, Player, X, Y, 0, 1, B3),
    flip_direction(B3, Player, X, Y, -1, 1, B4),
    flip_direction(B4, Player, X, Y, -1, 0, B5),
    flip_direction(B5, Player, X, Y, -1, -1, B6),
    flip_direction(B6, Player, X, Y, 0, -1, B7),
    flip_direction(B7, Player, X, Y, 1, -1, NewBoard).

% flip pieces in a specific direction
flip_direction(Board, Player, X, Y, DX, DY, NewBoard) :-
    collect_flips(Board, Player, X, Y, DX, DY, [], Flips),
    make_flips(Board, Player, Flips, NewBoard).

% collect pieces to flip in a specific direction
collect_flips(Board, Player, X, Y, DX, DY, Acc, Flips) :-
    X1 is X + DX,
    Y1 is Y + DY,
    size(Size),
    X1 >= 0, X1 < Size,
    Y1 >= 0, Y1 < Size,
    get(Board, X1, Y1, Value),
    opposite(Player, Opponent),
    (Value = Opponent ->
        collect_flips(Board, Player, X1, Y1, DX, DY, [(X1,Y1)|Acc], Flips)
    ; Value = Player ->
        Flips = Acc
    ;
        Flips = []
    ).
collect_flips(_, _, _, _, _, _, _, []).

% flip pieces
make_flips(Board, _, [], Board).
make_flips(Board, Player, [(X,Y)|Rest], NewBoard) :-
    set(Board, X, Y, Player, TempBoard),
    make_flips(TempBoard, Player, Rest, NewBoard).

% check if a player has valid moves
has_valid_moves(Board, Player) :-
    size(Size),
    MaxIndex is Size - 1,
    between(0, MaxIndex, X),
    between(0, MaxIndex, Y),
    valid_move(Board, Player, X, Y),
    !.

% check if the game is over
game_over(Board, Player) :-
    \+ has_valid_moves(Board, Player),
    opposite(Player, Opponent),
    \+ has_valid_moves(Board, Opponent).
