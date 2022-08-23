likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).

ex1_1 :-
  likes(mary, X), likes(john, X),
  write(X).

male(albert).
male(edward).
female(alice).
female(victoria).
parents(edward, victoria, albert).
parents(alice, victoria, albert).

different(X, Y) :- X \= Y.

sibling(X, Y) :-
  parents(X, M, F),
  parents(Y, M, F),
  different(X, Y). % has to be placed after we know what Y is see https://www.swi-prolog.org/pldoc/doc_for?object=(%3D)/2

sister_of(X, Y) :-
  female(X),
  sibling(X, Y).

ex1_2 :-
  sister_of(alice, X),
  write(X).

ex1_3 :-
  sibling(X, Y),
  write('X='),write(X),write(' Y='),write(Y).

ex1_4 :-
  female(alice),
  parents(alice, M, F),
  parents(X, M, F),
  X \= edward,
  write(X). % X is alice, needs different to avoid self reference
