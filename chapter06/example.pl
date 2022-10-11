foo :-
  true.

bar :-
  Y = 7,
  plus(X, 5, Y),
  !, fail.