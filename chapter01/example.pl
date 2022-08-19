likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).

ex1 :-
  likes(mary, X), likes(john, X),
  write(X).