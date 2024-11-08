% Alpha-Beta Algorithm

% Cache management
:- dynamic position_cache/4.  % Cache for board positions - Optimize performance for depth > 3
:- dynamic game_id/1.  % Game ID tracking

% Initialize game ID
:- assertz(game_id(0)).

% Initialize cache with game tracking
init_cache :-
    retractall(position_cache(_, _, _, _)),
    retract(game_id(OldID)),
    NewID is OldID + 1,
    assertz(game_id(NewID)).

% Cache lookup to include game ID
lookup_cache(Board, Depth, Player, Score) :-
    game_id(ID),
    position_cache(Board, Depth, Player, Score),
    position_cache(ID, Board, Depth, Player, Score).

% Cache storage to include game ID
store_cache(Board, Depth, Player, Score) :-
    game_id(ID),
    aggregate_all(count, position_cache(_, _, _, _), Count),
    (Count > 10000 -> 
        cleanup_cache
    ;
        true
    ),
    assertz(position_cache(ID, Board, Depth, Player, Score)).

% Cleanup to handle game IDs
cleanup_cache :-
    game_id(ID),
    retract(position_cache(ID, _, _, _, _)).

% Initialize cache
init_cache :-
    retractall(position_cache(_, _, _, _)).

% Find cached score for a board position
lookup_cache(Board, Depth, Player, Score) :-
    position_cache(Board, Depth, Player, Score).

% Store score in cache
store_cache(Board, Depth, Player, Score) :-
    aggregate_all(count, position_cache(_, _, _, _), Count),
   % Limit cache size to avoid memory issues (Remove oldest entry)
    (Count > 10000 -> 
        cleanup_cache
    ;
        true
    ),
    assertz(position_cache(Board, Depth, Player, Score)).

% Remove oldest cache entry
cleanup_cache :-
    retract(position_cache(_, _, _, _)).

% Alpha-beta algorithm - handles caching
alpha_beta(Board, Player, Depth, Alpha, Beta, BestScore, BestMove) :-
    % Check cache for existing score
    (lookup_cache(Board, Depth, Player, CachedScore) ->
        BestScore = CachedScore,
        BestMove = cached
    ;
        alpha_beta_search(Board, Player, Depth, Alpha, Beta, BestScore, BestMove),
        % Store result in cache
        (Depth > 0 ->  
            store_cache(Board, Depth, Player, BestScore)
        ;   
            true
        )
    ).

% Main alpha-beta search logic
alpha_beta_search(Board, Player, Depth, Alpha, Beta, BestScore, BestMove) :-
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
                alpha_beta(Board, Opponent, Depth, Alpha, Beta, BestScore, BestMove)
            )
        ;   
            Player == b -> 
                maximize_alphabeta(Board, Player, Moves, Depth, Alpha, Beta, BestScore, BestMove)
            ;   
                minimize_alphabeta(Board, Player, Moves, Depth, Alpha, Beta, BestScore, BestMove)
        )
    ).

% maximize_alphabeta - base case
maximize_alphabeta(_, _, [], _, Alpha, _, Alpha, none).

% maximize_alphabeta - evaluate for max score (alpha)
maximize_alphabeta(Board, Player, [(X, Y) | Moves], Depth, Alpha, Beta, BestScore, BestMove) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    alpha_beta(NewBoard, Opponent, NextDepth, Alpha, Beta, Score, _),
    % New alpha
    (Score > Alpha -> 
        NewAlpha = Score,
        NewBestMove = (X, Y)
    ;   
        NewAlpha = Alpha,
        NewBestMove = BestMove
    ),
    % Pruning check
    (NewAlpha >= Beta -> 
        BestScore = NewAlpha,
        BestMove = NewBestMove
    ;   
        maximize_alphabeta(Board, Player, Moves, Depth, NewAlpha, Beta, BestScore, TempMove),
        (TempMove = none -> 
            BestMove = NewBestMove
        ;   
            BestMove = TempMove
        )
    ).

% minimize_alphabeta - base case
minimize_alphabeta(_, _, [], _, _, Beta, Beta, none).

% minimize_alphabeta - evaluate for min score (beta)
minimize_alphabeta(Board, Player, [(X, Y) | Moves], Depth, Alpha, Beta, BestScore, BestMove) :-
    make_move(Board, Player, X, Y, NewBoard),
    opposite(Player, Opponent),
    NextDepth is Depth - 1,
    alpha_beta(NewBoard, Opponent, NextDepth, Alpha, Beta, Score, _),
    % New beta
    (Score < Beta -> 
        NewBeta = Score,
        NewBestMove = (X, Y)
    ;   
        NewBeta = Beta,
        NewBestMove = BestMove
    ),
    % Pruning check
    (Alpha >= NewBeta -> 
        BestScore = NewBeta,
        BestMove = NewBestMove
    ;   
        minimize_alphabeta(Board, Player, Moves, Depth, Alpha, NewBeta, BestScore, TempMove),
        (TempMove = none -> 
            BestMove = NewBestMove
        ;   
            BestMove = TempMove
        )
    ).