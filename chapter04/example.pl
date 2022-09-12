% 4.1 generating multiple solutions

is_integer(0).
is_integer(X) :- is_integer(Y), X is Y + 1.

% [trace]  ?- is_integer(X).
%    Call: (10) is_integer(_144566) ? creep
%    Exit: (10) is_integer(0) ? creep
% X = 0 ;
%    Redo: (10) is_integer(_144566) ? creep
%    Call: (11) is_integer(_148582) ? creep
%    Exit: (11) is_integer(0) ? creep
%    Call: (11) _144566 is 0+1 ? creep
%    Exit: (11) 1 is 0+1 ? creep
%    Exit: (10) is_integer(1) ? creep
% X = 1 ;
%    Redo: (11) is_integer(_148582) ? creep
%    Call: (12) is_integer(_154442) ? creep
%    Exit: (12) is_integer(0) ? creep
%    Call: (12) _148582 is 0+1 ? creep
%    Exit: (12) 1 is 0+1 ? creep
%    Exit: (11) is_integer(1) ? creep
%    Call: (11) _144566 is 1+1 ? creep
%    Exit: (11) 2 is 1+1 ? creep
%    Exit: (10) is_integer(2) ? creep
% X = 2 .

is_member(X, [X|_]).
is_member(X, [_|Y]) :- is_member(X, Y).

% [trace]  ?- is_member(1, [3,2,1,0,-1]).
%    Call: (10) is_member(1, [3, 2, 1, 0, -1]) ? creep
%    Call: (11) is_member(1, [2, 1, 0, -1]) ? creep
%    Call: (12) is_member(1, [1, 0, -1]) ? creep
%    Exit: (12) is_member(1, [1, 0, -1]) ? creep
%    Exit: (11) is_member(1, [2, 1, 0, -1]) ? creep
%    Exit: (10) is_member(1, [3, 2, 1, 0, -1]) ? creep
% true .

% [trace]  ?- is_member(7, [3,2,1,0,-1]).
%    Call: (10) is_member(7, [3, 2, 1, 0, -1]) ? creep
%    Call: (11) is_member(7, [2, 1, 0, -1]) ? creep
%    Call: (12) is_member(7, [1, 0, -1]) ? creep
%    Call: (13) is_member(7, [0, -1]) ? creep
%    Call: (14) is_member(7, [-1]) ? creep
%    Call: (15) is_member(7, []) ? creep
%    Fail: (15) is_member(7, []) ? creep
%    Fail: (14) is_member(7, [-1]) ? creep
%    Fail: (13) is_member(7, [0, -1]) ? creep
%    Fail: (12) is_member(7, [1, 0, -1]) ? creep
%    Fail: (11) is_member(7, [2, 1, 0, -1]) ? creep
%    Fail: (10) is_member(7, [3, 2, 1, 0, -1]) ? creep
% false.

% [trace]  ?- is_member(1, [3,1,2,1,1,0,1,-1]).
%    Call: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
%    Call: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% true .

% [trace]  ?- is_member(1, [3,1,2,1,1,0,1,-1]).
%    Call: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
%    Call: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% true ;
%    Redo: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Call: (12) is_member(1, [2, 1, 1, 0, 1, -1]) ? creep
%    Call: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Exit: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Exit: (12) is_member(1, [2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% true ;
%    Redo: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Call: (14) is_member(1, [1, 0, 1, -1]) ? creep
%    Exit: (14) is_member(1, [1, 0, 1, -1]) ? creep
%    Exit: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Exit: (12) is_member(1, [2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% true ;
%    Redo: (14) is_member(1, [1, 0, 1, -1]) ? creep
%    Call: (15) is_member(1, [0, 1, -1]) ? creep
%    Call: (16) is_member(1, [1, -1]) ? creep
%    Exit: (16) is_member(1, [1, -1]) ? creep
%    Exit: (15) is_member(1, [0, 1, -1]) ? creep
%    Exit: (14) is_member(1, [1, 0, 1, -1]) ? creep
%    Exit: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Exit: (12) is_member(1, [2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Exit: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% true ;
%    Redo: (16) is_member(1, [1, -1]) ? creep
%    Call: (17) is_member(1, [-1]) ? creep
%    Call: (18) is_member(1, []) ? creep
%    Fail: (18) is_member(1, []) ? creep
%    Fail: (17) is_member(1, [-1]) ? creep
%    Fail: (16) is_member(1, [1, -1]) ? creep
%    Fail: (15) is_member(1, [0, 1, -1]) ? creep
%    Fail: (14) is_member(1, [1, 0, 1, -1]) ? creep
%    Fail: (13) is_member(1, [1, 1, 0, 1, -1]) ? creep
%    Fail: (12) is_member(1, [2, 1, 1, 0, 1, -1]) ? creep
%    Fail: (11) is_member(1, [1, 2, 1, 1, 0, 1, -1]) ? creep
%    Fail: (10) is_member(1, [3, 1, 2, 1, 1, 0, 1, -1]) ? creep
% false.

% 4.2 the "cut"

cutted(X, even) :- divisible_by(X, 2), !.
cutted(_, odd).

uncutted(X, even) :- divisible_by(X, 2).
uncutted(_, odd).

divisible_by(X, N) :- 0 =:= X mod N.

% ?- cutted(2, R).
% R = even.

% ?- uncutted(2, R).
% R = even ;
% R = odd.

% ?- cutted(3, R).
% R = odd.

% ?- uncutted(3, R).
% R = odd.
