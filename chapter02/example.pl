ex2_1a :-
  % false
  pilots(A, london) = pilots(london, paris),
  write(A).

ex2_1b :-
  % true
  point(X, Y, Z) = point(X1, Y1, Z1),
  write('X='),write(X),write(' X1='),write(X1),nl,
  write('Y='),write(Y),write(' Y1='),write(Y1),nl,
  write('Z='),write(Z),write(' Z1='),write(Z1),nl.

ex2_1c :-
  % false
  letter(C) = word(letter),
  write(C).

ex2_1d :-
  % false
  noun(alpha) = alpha.

ex2_1e :-
  % true
  'student' = student.

ex2_1f :-
  % false
  f(X, X) = f(a, b),
  write(X).

ex2_1g :-
  % true
  f(X, a(b, c)) = f(Z, a(Z, c)),
  write('X='),write(X),write(' Z='),write(Z),nl.