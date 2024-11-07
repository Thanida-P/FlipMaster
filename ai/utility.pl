% Move priority function
move_priority((X, Y), Priority) :-
    % High (3) priority for corners
    ((X = 0, Y = 0) ; (X = 0, Y = 7) ; (X = 7, Y = 0) ; (X = 7, Y = 7)), !,
    Priority = 3.
move_priority((X, Y), Priority) :-
    % Very low (-1) priority for risky positions i.e. Positions adjacent to corners
    ((X = 1, Y = 1) ; (X = 1, Y = 0) ; (X = 0, Y = 1) ;
     (X = 6, Y = 0) ; (X = 6, Y = 1) ; (X = 7, Y = 1) ;
     (X = 0, Y = 6) ; (X = 1, Y = 6) ; (X = 1, Y = 7) ;
     (X = 6, Y = 6) ; (X = 6, Y = 7) ; (X = 7, Y = 6)), !,
    Priority = -1.  
move_priority((X, Y), Priority) :-
    % Medium-high (2.5) priority for positions two steps away from corners
    ((X = 2, Y = 0) ; (X = 0, Y = 2) ; (X = 2, Y = 7) ; (X = 7, Y = 2) ;
     (X = 5, Y = 0) ; (X = 0, Y = 5) ; (X = 5, Y = 7) ; (X = 7, Y = 5)), !,
    Priority = 2.5.
move_priority((X, Y), Priority) :-
    % Medium (2) priority for edges
    (X = 0 ; X = 7 ; Y = 0 ; Y = 7), !,
    Priority = 2.
move_priority((X, Y), Priority) :-
    % Medium-low (1.5) priority for positions adjacent to edges
    ((X = 1, Y = 2) ; (X = 2, Y = 1) ; (X = 1, Y = 5) ; (X = 5, Y = 1) ;
     (X = 6, Y = 2) ; (X = 2, Y = 6) ; (X = 6, Y = 5) ; (X = 5, Y = 6)), !,
    Priority = 1.5.
move_priority(_, 1).  % Default priority (1) for all other positions

% Find all possible moves with priority and sort them by priority
findall_moves_with_priority(Board, Player, SortedMoves) :-
    findall((X, Y, Priority), 
            (valid_move(Board, Player, X, Y), move_priority((X, Y), Priority)), 
            MovesWithPriority),
    sort(2, @>=, MovesWithPriority, SortedMovesWithPriority),
    findall((X, Y), member((X, Y, _), SortedMovesWithPriority), SortedMoves).

% Enhanced evaluation function considering piece stability and mobility
evaluate_board(Board, Score) :-
    count_pieces(Board, White, Black),
    count_stable_pieces(Board, WhiteStable, BlackStable),
    findall_moves_with_priority(Board, white, WhiteMoves),
    findall_moves_with_priority(Board, black, BlackMoves),
    length(WhiteMoves, WhiteMobility),
    length(BlackMoves, BlackMobility),
    Score is (Black - White) + 2 * (BlackStable - WhiteStable) + (BlackMobility - WhiteMobility).

% Count stable pieces (pieces that cannot be flipped)
count_stable_pieces(Board, WhiteStable, BlackStable) :-
    WhiteStable = 0,
    BlackStable = 0.