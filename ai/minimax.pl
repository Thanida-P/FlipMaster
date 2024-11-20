% Minimax algorithm

minimax(Board, Player, Depth, BestScore, BestMove, Scores) :-
    (Depth = 0 -> 
        evaluate_board(Board, BestScore),
        BestMove = none,
        Scores = []
    ;   
        findall_moves_with_priority(Board, Player, Moves),
        (Moves = [] -> 
            opposite(Player, Opponent),
            findall_moves_with_priority(Board, Opponent, OpponentMoves),
            (OpponentMoves = [] ->
                % Game over
                count_pieces(Board, White, Black),
                BestScore is Black - White,
                BestMove = none,
                Scores = []
            ;   
                % Player has no moves, skip turn
                minimax(Board, Opponent, Depth, BestScore, BestMove, Scores)
            )
        ;   
            (Player = b ->
                maximize_minimax(Board, Player, Moves, Depth, -1000, none, BestScore, BestMove, Scores)
            ;
                minimize_minimax(Board, Player, Moves, Depth, 1000, none, BestScore, BestMove, Scores)
            )
        )
    ).

% maximize - base case
maximize_minimax(_, _, [], _, BestScore, BestMove, BestScore, BestMove, []).

% maximize - evaluate for max score
maximize_minimax(Board, Player, [(X, Y)|Moves], Depth, CurrentBest, CurrentMove, BestScore, BestMove, [(X, Y, Score)|RestScores]) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    minimax(NewBoard, Opponent, NextDepth, Score, _, _),
    (Score > CurrentBest ->
        NewBest = Score,
        NewMove = (X, Y)
    ;   
        NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    maximize_minimax(Board, Player, Moves, Depth, NewBest, NewMove, BestScore, BestMove, RestScores).

% minimize function - base case
minimize_minimax(_, _, [], _, BestScore, BestMove, BestScore, BestMove, []).

% minimize function - evaluate for min score
minimize_minimax(Board, Player, [(X, Y)|Moves], Depth, CurrentBest, CurrentMove, BestScore, BestMove, [(X, Y, Score)|RestScores]) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    minimax(NewBoard, Opponent, NextDepth, Score, _, _),
    (Score < CurrentBest ->
        NewBest = Score,
        NewMove = (X, Y)
    ;   
        NewBest = CurrentBest,
        NewMove = CurrentMove
    ),
    minimize_minimax(Board, Player, Moves, Depth, NewBest, NewMove, BestScore, BestMove, RestScores).