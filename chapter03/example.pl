% 3.4 mapping
change(you, i).
change(are, [am, not]).
change(american, british).
change(do, no).
change(X, X). % catch all

alter([], []). % done
alter([H|T], [H1|T1]) :- change(H, H1), alter(T, T1).

exp34_1 :-
  S = [you, are, a, computer],
  write(S),write('->'),
  alter(S, Z),
  write(Z),nl,!.

exp34_2 :-
  S = [are, you, british],
  write(S),write('->'),
  alter(S, Z),
  write(Z),nl,!.