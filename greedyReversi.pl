:- ensure_loaded('./game/rules.pl').
:- ensure_loaded('./game/state.pl').
:- ensure_loaded('./ai/greedy.pl').
:- ensure_loaded('./ai/utility.pl').

% start game (main loop)
start_game :-
  reversiInitialGrid(InitialBoard),
  play_game(InitialBoard, w).

% Input handling
get_valid_move(Board, Player, ValidMoves, X, Y) :-
    repeat,
    format('Player ~w, enter row (0-7): ', [Player]),
    read_line_to_codes(user_input, RowCodes),
    number_codes(X, RowCodes),
    format('Player ~w, enter column (0-7): ', [Player]),
    read_line_to_codes(user_input, ColCodes),
    number_codes(Y, ColCodes),
    (member((X,Y), ValidMoves) ->
        !
    ;
        writeln('Invalid move. Try again.'),
        fail
    ).

% Console display
display_board(Board) :-
    nl,
    write('  0 1 2 3 4 5 6 7'), nl,
    display_rows(Board, 0).

display_rows([], _).
display_rows([Row|Rest], N) :-
    format('~w ', [N]),
    display_row(Row),
    nl,
    N1 is N + 1,
    display_rows(Rest, N1).

display_row([]).
display_row([Cell|Rest]) :-
    (Cell = 0 -> write('. ')
    ; write(Cell), write(' ')),
    display_row(Rest).

% Game end and scoring
count_pieces(Board, White, Black) :-
    flatten(Board, FlatBoard),
    count(w, FlatBoard, White),
    count(b, FlatBoard, Black).

count(_, [], 0).
count(Player, [Player|Rest], Count) :-
    count(Player, Rest, RestCount),
    Count is RestCount + 1.
count(Player, [Other|Rest], Count) :-
    Other \= Player,
    count(Player, Rest, Count).

announce_winner(Board) :-
    count_pieces(Board, White, Black),
    format('Game Over!~nWhite: ~w~nBlack: ~w~n', [White, Black]),
    (White > Black -> write('White wins!')
    ; Black > White -> write('Black wins!')
    ; write('It\'s a tie!')).

% Implement play game for player
play_game(Board, w) :-
    display_board(Board),
    (game_over(Board, w) ->
        announce_winner(Board)
    ;
        handle_turn(Board, w)
    ).

% Implement play game for AI
play_game(Board, b) :-
    display_board(Board),
    (game_over(Board, b) ->
        announce_winner(Board)
    ;   
        findall_moves_with_priority(Board, b, Moves),
        (Moves = [] ->
            writeln('No valid moves for Black (AI). Switching to White.'),
            play_game(Board, w)
        ;   
            greedy_move(Board, b, (X, Y)),
            format('AI plays at: (~w, ~w)~n', [X, Y]),
            make_move(Board, b, X, Y, NewBoard),
            play_game(NewBoard, w)
        )
    ).

% Human player's turn
handle_turn(Board, w) :-
    findall((X, Y), valid_move(Board, w, X, Y), Moves),
    (Moves = [] ->
        writeln('No valid moves for White. Switching players.'),
        play_game(Board, b)
    ;
        writeln('Your turn, White. Valid moves: '),
        writeln(Moves),
        get_valid_move(Board, w, Moves, X, Y),
        make_move(Board, w, X, Y, NewBoard),
        play_game(NewBoard, b)
    ).
