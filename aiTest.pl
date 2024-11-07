:- ensure_loaded('./game/rules.pl').
:- ensure_loaded('./game/state.pl').
:- ensure_loaded('./ai/alphabeta.pl').
:- ensure_loaded('./ai/utility.pl').
:- ensure_loaded('./ai/minimax.pl').
:- ensure_loaded('./ai/greedy.pl').

% start game (main loop)
start_game :-
    set_prolog_flag(stack_limit, 5_000_000_000), % Increase to 5GB, or as needed
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

% play_game for black (AI) to handle endgame properly
play_game(Board, w) :-
    display_board(Board),
    (game_over(Board, w) ->
        announce_winner(Board)
    ;   
        findall_moves_with_priority(Board, w, Moves),
        (Moves = [] ->
            writeln('No valid moves for Black (AI). Switching to White.'),
            play_game(Board, b)
        ;   
            minimax(Board, w, 3, _BestScore, (X, Y)),
            format('AI plays at: (~w, ~w)~n', [X, Y]),
            make_move(Board, w, X, Y, NewBoard),
            play_game(NewBoard, b)
        )
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
            init_cache,
            alpha_beta(Board, b, 5, -10000, 10000, _BestScore, (X, Y)),
            format('AI plays at: (~w, ~w)~n', [X, Y]),
            make_move(Board, b, X, Y, NewBoard),
            play_game(NewBoard, w)
        )
    ).

% Implement play game for AI
% play_game(Board, w) :-
%     display_board(Board),
%     (game_over(Board, w) ->
%         announce_winner(Board)
%     ;   
%         findall_moves_with_priority(Board, w, Moves),
%         (Moves = [] ->
%             writeln('No valid moves for Black (AI). Switching to White.'),
%             play_game(Board, b)
%         ;   
%             greedy_move(Board, w, (X, Y)),
%             format('AI plays at: (~w, ~w)~n', [X, Y]),
%             make_move(Board, w, X, Y, NewBoard),
%             play_game(NewBoard, b)
%         )
%     ).

% % Human player's turn
% handle_turn(Board, w) :-
%     findall((X, Y), valid_move(Board, w, X, Y), Moves),
%     sort(Moves, UniqueMoves),
%     (UniqueMoves = [] ->
%         writeln('No valid moves for White. Switching players.'),
%         play_game(Board, b)
%     ;
%         writeln('Your turn, White. Valid moves: '),
%         writeln(UniqueMoves),
%         get_valid_move(Board, w, UniqueMoves, X, Y),
%         make_move(Board, w, X, Y, NewBoard),
%         play_game(NewBoard, b)
%     ).
