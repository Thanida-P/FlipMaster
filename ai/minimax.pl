% Minimax algorithm - Medium

minimax(Board, Player, Depth, BestScore, BestMove) :-
    (Depth = 0 -> 
        evaluate_board(Board, BestScore),
        BestMove = none
    ;   
        findall_moves_with_priority(Board, Player, Moves),
        (Moves = [] -> 
            opposite(Player, Opponent),
            findall_moves_with_priority(Board, Opponent, OpponentMoves),
            (OpponentMoves = [] ->
                % Game over
                count_pieces(Board, White, Black),
                BestScore is Black - White,
                BestMove = none
            ;   
                % Player has no moves, skip turn
                minimax(Board, Opponent, Depth, BestScore, BestMove)
            )
        ;   
            (Player = b ->
                maximize_minimax(Board, Player, Moves, Depth, -1000, none, BestScore, BestMove)
            ;
                minimize_minimax(Board, Player, Moves, Depth, 1000, none, BestScore, BestMove)
            )
        )
    ).

% maximize_minimax function - base case
maximize_minimax(_, _, [], _, BestScore, BestMove, BestScore, BestMove).

% maximize_minimax function - evaluate for max score
maximize_minimax(Board, Player, [(X, Y)|Moves], Depth, CurrentBest, CurrentMove, BestScore, BestMove) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    minimax(NewBoard, Opponent, NextDepth, Score, _),
    (Score > CurrentBest ->
        NewBest = Score,
        NewMove = (X, Y)
    ;   
        NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    maximize_minimax(Board, Player, Moves, Depth, NewBest, NewMove, BestScore, BestMove).

% minimize_minimax function - base case
minimize_minimax(_, _, [], _, BestScore, BestMove, BestScore, BestMove).

% minimize_minimax function - evaluate for min score
minimize_minimax(Board, Player, [(X, Y)|Moves], Depth, CurrentBest, CurrentMove, BestScore, BestMove) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    minimax(NewBoard, Opponent, NextDepth, Score, _),
    (Score < CurrentBest ->
        NewBest = Score,
        NewMove = (X, Y)
    ;   
        NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    minimize_minimax(Board, Player, Moves, Depth, NewBest, NewMove, BestScore, BestMove).