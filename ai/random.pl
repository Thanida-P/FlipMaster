% Random AI - Easy

random_move(Board, Player, (X, Y)) :-
    findall((X, Y), valid_move(Board, b, X, Y), Moves),
    random_member((X, Y), Moves).