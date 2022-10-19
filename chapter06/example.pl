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
  var(_X).

qux :-
  nonvar(_X).

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

% 6.4 treating clauses as terms

':-'(add5(X, R), plus(5, X, R)).

:- dynamic foo/3, bar/2.

:- asserta(foo(X, Y, R) :- plus(X, Y, R)).
:- asserta(bar(X, R) :- foo(X, 8, R)).

% 6.5 constructing and accessing component of structures

ex65_1 :-
  functor([a, b, c], F1, 2),
  write('functor([a, b, c], F1, 2)'),nl,
  write('F1='),write(F1),nl,
  nl,
  functor(A2, '[|]', 2),
  write('functor(A2, ''[|]'', 2)'),nl,
  write('A2='),write(A2),nl.

copy(Old, New) :-
  functor(Old, F, N),
  functor(New, F, N).

% ?- copy(plus(1, 2, X), R).
% R = plus(_, _, _).

% ?- copy(plus(1, 2, 4), R).
% R = plus(_, _, _).

% ?- copy(R0, plus(1, 2, 3)).
% ERROR: Arguments are not sufficiently instantiated

% ?- copy(plus(1, 2, 3), plus(_, _, _)).
% true.

% ?- copy(plus(1, 2, 3), plus(_, _, 3)).
% true.

ex65_2 :-
  arg(2, f(1, 2, 3), X1),
  write('arg(2, f(1, 2, 3), X1)'),nl,
  write('X1='),write(X1),nl,
  nl,
  arg(N2, f(1, 2, 3), 3),
  write('arg(N2, f(1, 2, 3), 3)'),nl,
  write('N2='),write(N2),nl.

ex65_3 :-
  f(1, 2, 3) =.. X1,
  write('f(1, 2, 3) =.. X1'),nl,
  write('X1='),write(X1),nl,
  nl,
  [a, b, c] =.. L2,
  write('[a, b, c] =.. L2'),nl,
  write('L2'),write(L2),nl,
  nl,
  (a+b) =.. [+, X3, Y3],
  write('(a+b) =.. [+, X3, Y3]'),nl,
  write('X3='),write(X3),write(' Y3='),write(Y3),nl,
  nl,
  [1, 2, 3] =.. [X4|Y4],
  write('[1, 2, 3] =.. [X4|Y4]'),nl,
  write('X4='),write(X4),write(' Y4='),write(Y4),nl.

% 6.6 affecting backtracking

new_get(X) :- repeat, get_char(X).

get_non_space(X) :- new_get(X), \+ X = ' ', !.

% 6.7 constructing compund goals

search_and_print(X, L) :-
  member(X, L), write(X).

double_negative_search_and_print(X, L) :-
  \+ \+ member(X, L), write(X).

% ?- search_and_print(1, [2, 3, 4, 1]).
% 1
% true.

% ?- search_and_print(1, [2, 3, 4, 5]).
% false.

% ?- double_negative_search_and_print(1, [2, 3, 4, 1]).
% 1
% true.

% ?- double_negative_search_and_print(1, [2, 3, 4, 5]).
% false.

% ?- double_negative_search_and_print(X, [2, 3, 4, 5]).
% _9428
% true.

% ?- search_and_print(X, [2, 3, 4, 5]).
% 2
% X = 2 ;
% 3
% X = 3 ;
% 4
% X = 4 ;
% 5
% X = 5.

% 6.8 equality

% ?- X = Y.
% X = Y.

% ?- X == Y.
% false.

% ?- X = Y, X == Y.
% X = Y.

% ?- append([A|B], C) = append(X, Y).
% C = Y,
% X = [A|B].

% ?- append([A|B], C) == append(X, Y).
% false.

% ?- append([A|B], C) = append([A|B], C).
% true.

% ?- append([A|B], C) == append([A|B], C).
% true.

% 6.9 input and output

ex69_1 :-
  write('write('),write(2 + 2),write(')'),nl,
  write_canonical('write_canonical('),write_canonical(2 + 2),write_canonical(')'),nl.

% 6.10 handling files

create_hello(FileName) :-
  current_output(Output),
  write('creating '),write(FileName),write('....'),nl,
  open(FileName, write, File),
  set_output(File),
  write('hello'),nl,
  close(File),
  set_output(Output),
  write('....done'),nl.