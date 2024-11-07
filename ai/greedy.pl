% Greedy algorithm - Easy

greedy_move(Board, Player, BestMove) :-
    findall_moves_with_priority(Board, Player, Moves),
    (Moves = [] -> BestMove = none
    ; 
        find_best_move(Board, Player, Moves, -1000, none, BestMove)
    ).

% Find best move - base case
find_best_move(_, _, [], BestScore, BestMove, BestMove).

% Find best move - evaluate for max score
find_best_move(Board, Player, [(X, Y) | Moves], CurrentBest, CurrentMove, BestMove) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    evaluate_board(NewBoard, Score),
    (Score > CurrentBest ->
        NewBest = Score,
        NewMove = (X, Y)
    ;   
        NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    find_best_move(Board, Player, Moves, NewBest, NewMove, BestMove).