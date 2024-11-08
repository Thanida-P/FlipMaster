% initial Grid for Reversi Game
reversiInitialGrid([
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, w, b, 0, 0, 0],
    [0, 0, 0, b, w, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0]
]).

% player opposite
opposite(w, b).
opposite(b, w).

% get the value of a cell
get(Board, X, Y, Value) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Value).

% set the value of a cell
set(Board, X, Y, Value, NewBoard) :-
    nth0(X, Board, Row),
    replace(Row, Y, Value, NewRow),
    replace(Board, X, NewRow, NewBoard).

% replace an element in a list
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

% next row and column
nextRow(1, 2).
nextRow(2, 3).
nextRow(3, 4).
nextRow(4, 5).
nextRow(5, 6).
nextRow(6, 7).
nextRow(7, 8).

nextCol(1, 2).
nextCol(2, 3).
nextCol(3, 4).
nextCol(4, 5).
nextCol(5, 6).
nextCol(6, 7).
nextCol(7, 8).