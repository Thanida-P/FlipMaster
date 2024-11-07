:- ensure_loaded('./game/rules.pl').
:- ensure_loaded('./game/state.pl').
:- ensure_loaded('./ai/alphabeta.pl').
:- ensure_loaded('./ai/utility.pl').


% Start game (main loop)
start_game(Board, UniqueMoves) :-
	reversiInitialGrid(InitialBoard), Board = InitialBoard, 
	findall(
		(X, Y), 
		valid_move(Board, w, X, Y), Moves), 
	sort(Moves, UniqueMoves).

% Game end and scoring
count_pieces(Board, White, Black) :-
	flatten(Board, FlatBoard), 
	count(w, FlatBoard, White), 
	count(b, FlatBoard, Black).

count(_, [], 0).
count(Player, [Player|Rest], Count) :-
	count(Player, Rest, RestCount), Count is RestCount + 1.

count(Player, [Other|Rest], Count) :-
	Other \= Player, 
	count(Player, Rest, Count).

% Player move
play_game(Board, w, X, Y, NewBoard, GameOver, White, Black) :-  
	(game_over(NewBoard, w) ->
		GameOver = true
	;
		(valid_move(Board, w, X, Y) ->
			make_move(Board, w, X, Y, NewBoard)
		;
			NewBoard = Board
		), GameOver = false
), count_pieces(NewBoard, White, Black), !.
   

% AI move
play_game(Board, b, NewBoard, UniqueMoves, GameOver, White, Black) :-
    (game_over(Board, b) ->
        GameOver = true,
        NewBoard = Board,
        UniqueMoves = []
    ;
        findall((X, Y), valid_move(Board, b, X, Y), Moves),
        (Moves = [] ->
            NewBoard = Board
        ;
            alpha_beta(Board, b, 4, -10000, 10000, _BestScore, (X, Y)),
            make_move(Board, b, X, Y, NewBoard)
        ), GameOver = false, 
			findall((X1, Y1), valid_move(NewBoard, w, X1, Y1), NextMoves),
			sort(NextMoves, UniqueMoves)
    ), count_pieces(NewBoard, White, Black), !.

