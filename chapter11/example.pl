flatten1(L, R) :-
  flatten1(L, R, []).
flatten1([], R, R) :- !.
flatten1([H|T], [H|R1], R) :-
  \+is_list(H), !,
  flatten1(T, R1, R).
flatten1([H|T], R1, R) :-
  flatten1(H, R1, R2),
  flatten1(T, R2, R).

% ?- flatten1([a, [b, c], [[d], e]], [a, b, c, d, e]).
% true.

flatten2(L, R) :- phrase(flatten2(L), R).
flatten2([]) --> [], !.
flatten2([H|T]) --> {\+is_list(H)}, [H], !, flatten2(T).
flatten2([H|T]) --> flatten2(H), flatten2(T).

% ?- flatten2([a, [b, c], [[d], e]], [a, b, c, d, e]).
% true.