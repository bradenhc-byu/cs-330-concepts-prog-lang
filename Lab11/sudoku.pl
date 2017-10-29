% A Sudoku solver.  The basic idea is for each position,
% check that it is a digit with `digit`.  Then verify that the digit
% chosen doesn't violate any constraints (row, column, and cube).
% If no constraints were violated, proceed further.  If a constraint
% was violated, then backtrack to the last digit choice and move from
% there (the Prolog engine should handle this for you automatically).
% If we reach the end of the board with this scheme, it means that
% the whole thing is solved.
 
% YOU SHOULD FILL IN THE SOLVE PROCEDURE, DOWN BELOW.
 
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).
 
numBetween(Num, Lower, Upper) :-
        Num >= Lower,
        Num =< Upper.
 
% cubeBounds: (RowLow, RowHigh, ColLow, ColHigh, CubeNumber)
cubeBounds(0, 2, 0, 2, 0).
cubeBounds(0, 2, 3, 5, 1).
cubeBounds(0, 2, 6, 8, 2).
cubeBounds(3, 5, 0, 2, 3).
cubeBounds(3, 5, 3, 5, 4).
cubeBounds(3, 5, 6, 8, 5).
cubeBounds(6, 8, 0, 2, 6).
cubeBounds(6, 8, 3, 5, 7).
cubeBounds(6, 8, 6, 8, 8).
 
% Given a board and the index of a column of interest (0-indexed),
% returns the contents of the column as a list.
% columnAsList: (Board, ColumnNumber, AsRow)
columnAsList([], _, []).
columnAsList([Head|Tail], ColumnNum, [Item|Rest]) :-
        nth0(ColumnNum, Head, Item),
        columnAsList(Tail, ColumnNum, Rest).
 
% given which row and column we are in, gets which cube
% is relevant.  A helper ultimately for `getCube`.
% cubeNum: (RowNum, ColNum, WhichCube)
cubeNum(RowNum, ColNum, WhichCube) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, WhichCube),
        numBetween(RowNum, RowLow, RowHigh),
        numBetween(ColNum, ColLow, ColHigh).
 
% Drops the first N elements from a list.  A helper ultimately
% for `getCube`.
% drop: (InputList, NumToDrop, ResultList)
drop([], _, []):-!.
drop(List, 0, List):-!.
drop([_|Tail], Num, Rest) :-
        Num > 0,
        NewNum is Num - 1,
        drop(Tail, NewNum, Rest).
 
% Takes the first N elements from a list.  A helper ultimately
% for `getCube`.
% take: (InputList, NumToTake, ResultList)
take([], _, []):-!.
take(_, 0, []):-!.
take([Head|Tail], Num, [Head|Rest]) :-
        Num > 0,
        NewNum is Num - 1,
        take(Tail, NewNum, Rest).
 
% Gets a sublist of a list in the same order, inclusive.
% A helper for `getCube`.
% sublist: (List, Start, End, NewList)
sublist(List, Start, End, NewList) :-
        drop(List, Start, TempList),
        NewEnd is End - Start + 1,
        take(TempList, NewEnd, NewList).
 
% Given a board and cube number, gets the corresponding cube as a list.
% Cubes are 3x3 portions, numbered from the top left to the bottom right,
% starting from 0.  For example, they would be numbered like so:
%
% 0  1  2
% 3  4  5
% 6  7  8
%
% getCube: (Board, CubeNumber, ContentsOfCube)
getCube(Board, Number, AsList) :-
        cubeBounds(RowLow, RowHigh, ColLow, ColHigh, Number),
        sublist(Board, RowLow, RowHigh, [Row1, Row2, Row3]),
        sublist(Row1, ColLow, ColHigh, Row1Nums),
        sublist(Row2, ColLow, ColHigh, Row2Nums),
        sublist(Row3, ColLow, ColHigh, Row3Nums),
        append(Row1Nums, Row2Nums, TempRow),
        append(TempRow, Row3Nums, AsList).
 
% Given a board, solve it in-place.
% After calling `solve` on a board, the board should be fully
% instantiated with a satisfying Sudoku solution.
 
% ---- PUT CODE HERE ---
% ---- PUT CODE HERE ---
 
solve(Board) :- 
        
        % import some helpful functions like is_set
        use_module(library(lists)),
        
        % initialize variables
        Cols = [Col0,Col1,Col2,Col3,Col4,Col5,Col6,Col7,Col8],

        Cubes = [Cube0,Cube1,Cube2,Cube3,Cube4,Cube5,Cube6,Cube7,Cube8],

        % break up the board into columns and sub grids, storing them as variables
        getCube(Board,0,Cube0),
        getCube(Board,1,Cube1),
        getCube(Board,2,Cube2),
        getCube(Board,3,Cube3),
        getCube(Board,4,Cube4),
        getCube(Board,5,Cube5),
        getCube(Board,6,Cube6),
        getCube(Board,7,Cube7),
        getCube(Board,8,Cube8),

        columnAsList(Board,0,Col0),
        columnAsList(Board,1,Col1),
        columnAsList(Board,2,Col2),
        columnAsList(Board,3,Col3),
        columnAsList(Board,4,Col4),
        columnAsList(Board,5,Col5),
        columnAsList(Board,6,Col6),
        columnAsList(Board,7,Col7),
        columnAsList(Board,8,Col8),
        
        % recurse through the Board, left to right, top to bottom
        recurseRow(Board,Cols,Cubes,0,0).       

% solves the Board recursively, left to right, top to bottom
recurseRow(Board,Cols,Cubes,Row,Col) :-

        nth0(Row,Board,RowList),
        recurseCol(Board, RowList, Cols, Cubes, Row, Col),
        NextRow is Row + 1,
        (NextRow > 8;
         recurseRow(Board, Cols, Cubes, NextRow, 0)). 

recurseCol(Board,RowList,Cols,Cubes,Row,Col) :-

        nth0(Col,RowList,Space),
        cubeNum(Row,Col,CubeNum),
        nth0(Col,Cols,ColList),
        nth0(CubeNum,Cubes,CubeList),
        solveSpace(Space, RowList, ColList, CubeList),
        NextCol is Col + 1,
        (NextCol > 8;
        recurseCol(Board,RowList,Cols,Cubes,Row,NextCol)).

solveSpace(Var, RowList, ColList, CubeList) :-
 
        (nonvar(Var);
        var(Var),
        digit(Var),
        is_set(RowList),
        is_set(ColList),
        is_set(CubeList)).

% ---- PUT CODE HERE ---
% ---- PUT CODE HERE ---
 
% Prints out the given board.
printBoard([]).
printBoard([Head|Tail]) :-
        write(Head), nl,
        printBoard(Tail).
 
test1(Board) :-
        Board = [[2, _, _, _, 8, 7, _, 5, _],
                 [_, _, _, _, 3, 4, 9, _, 2],
                 [_, _, 5, _, _, _, _, _, 8],
                 [_, 6, 4, 2, 1, _, _, 7, _],
                 [7, _, 2, _, 6, _, 1, _, 9],
                 [_, 8, _, _, 7, 3, 2, 4, _],
                 [8, _, _, _, _, _, 4, _, _],
                 [3, _, 9, 7, 4, _, _, _, _],
                 [_, 1, _, 8, 2, _, _, _, 5]],
        solve(Board),
        printBoard(Board).
 
test2(Board) :-
        Board = [[_, _, _, 7, 9, _, 8, _, _],
                 [_, _, _, _, _, 4, 3, _, 7],
                 [_, _, _, 3, _, _, _, 2, 9],
                 [7, _, _, _, 2, _, _, _, _],
                 [5, 1, _, _, _, _, _, 4, 8],
                 [_, _, _, _, 5, _, _, _, 1],
                 [1, 2, _, _, _, 8, _, _, _],
                 [6, _, 4, 1, _, _, _, _, _],
                 [_, _, 3, _, 6, 2, _, _, _]],
        solve(Board),
        printBoard(Board).
