% 10.7 the relation of prolog to logic

append1([], X, X).
append1([H|T], L1, [H|L2]) :- append1(T, L1, L2).

% ?- append1([], [1], R).
% R = [1].

% ?- append1([a, b, c], [1], R).
% R = [a, b, c, 1].

% ?- append1([a, b, c], [], R).
% R = [a, b, c].

% ?- append1([a, b, c], L, [a, b, c, 1, 2, 3]).
% L = [1, 2, 3].

member1(_, L) :- var(L), !, fail. % ! removes from linear input resolution strategy
member1(X, [X|_]).
member1(X, [_|L]) :- member1(X, L).

% ?- member1(1, []).
% false.

% ?- member1(1, [a]).
% false.

% ?- member1(1, [a, 1]).
% true .

% ?- member1(2, [a, 1]).
% false.

count(0) :- !. % ! removes from linear input resolution strategy
count(N) :- write(N), succ(N1, N), count(N1).

% ?- count(0).
% true.

% ?- count(1).
% 1
% true.

% ?- count(5).
% 54321
% true.

count1(N) :- N > 0, write(N), succ(N1, N), count(N1).

% ?- count1(0).
% false.

% ?- count1(1).
% 1
% true.

% ?- count1(5).
% 54321
% true.