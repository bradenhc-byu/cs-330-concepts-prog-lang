% Prolog code for solving TIC TAC TOE

aline([1,2,3]).
aline([4,5,6]).
aline([7,8,9]).
aline([1,4,7]).
aline([2,5,8]).
aline([3,6,9]).
aline([3,5,7]).
aline([1,5,9]).

% check for a forced move
forced_move(Board, SQ) :-
    aline(Squares),
    threatening(Squares, Board, Sq),
    !.

% 
threatening([X,Y,Z], B, X) :-
    empty(X,B),
    cross(Y,B),
    cross(Z,B).

%
threatening([X,Y,Z], B, Z) :-
    empty(Z,B),
    cross(X,B),
    cross(Y,B).

%
threatening([X,Y,Z], B, Y) :-
    empty(Y,B),
    cross(X,B),
    cross(Z,B).

% arg will extract the variable and set it to value
% var will determine whether it is unbound
empty(Square, Board) :-
    arg(Square, Board, Value),
    var(Value).


%
cross(Square, Board) :-
    arg(Square, Board, Value),
    nonvar(Value),
    Value = x.

%
nought(Square, Board) :-
    arg(Square, Board, Value),
    nonvar(Value),
    Value = 0.




