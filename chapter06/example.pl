% 6.2 sucess and failure

foo :-
  true.

bar :-
  Y = 7,
  plus(X, 5, Y),
  print(X),nl,
  !, fail.

% 6.3 classifying terms

baz :-
  var(X).

qux :-
  nonvar(X).

checker1(X) :-
  atom(X).

checker2(X) :-
  number(X).

checker3(X) :-
  atomic(X).

% ?- checker1(23).
% false.

% ?- checker1(x).
% true.

% ?- checker1('hello').
% true.

% ?- checker2(23).
% true.

% ?- checker2(x).
% false.

% ?- checker2('hello').
% false.

% ?- checker3(23).
% true.

% ?- checker3(x).
% true.

% ?- checker3('hello').
% true.