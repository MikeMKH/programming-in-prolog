% 7.1 sorted tree dictionary

lookup(H, winning(H, G, _, _), G1) :-
  !, G = G1.
lookup(H, winning(H1, _, L, _), G) :-
  H @< H1,
  lookup(H, L, G).
lookup(H, winning(H1, _, _, R), G) :-
  H @> H1,
  lookup(H, R, G).

% ?- lookup(mike, T, 100), lookup(kelsey, T, 200), lookup(lily, T, 300).
% T = winning(mike, 100, winning(kelsey, 200, _, winning(lily, 300, _, _)), _) .

% ?- lookup(lily, T, 300), lookup(kelsey, T, 200), lookup(mike, T, 100).
% T = winning(lily, 300, winning(kelsey, 200, _, _), winning(mike, 100, _, _)) .

% ?- lookup(kelsey, T, 200), lookup(lily, T, 300), lookup(mike, T, 100).
% T = winning(kelsey, 200, _, winning(lily, 300, _, winning(mike, 100, _, _))).

% ?- lookup(a, T, 1), lookup(b, T, 2), lookup(c, T, 3), lookup(d, T, 4), lookup(e, T, 5).
% T = winning(a, 1, _, winning(b, 2, _, winning(c, 3, _, winning(d, 4, _, winning(e, 5, _, _))))).

% ?- lookup(c, T, 3), lookup(d, T, 4), lookup(a, T, 1), lookup(b, T, 2), lookup(e, T, 5).
% T = winning(c, 3, winning(a, 1, _, winning(b, 2, _, _)), winning(d, 4, _, winning(e, 5, _, _))) .

% ?- lookup(e, T, 5), lookup(d, T, 4), lookup(c, T, 3), lookup(b, T, 2), lookup(a, T, 1).
% T = winning(e, 5, winning(d, 4, winning(c, 3, winning(b, 2, winning(a, 1, _, _), _), _), _), _) .

% 7.2 searching a maze

d(a, b). d(b, a).
d(b, e). d(e, b).
d(b, c). d(c, b).
d(d, e). d(e, d).
d(c, d). d(d, c).
d(e, f). d(f, e).
d(g, e). d(e, g).

has_treasure(g).
% has_treasure(g) :-
%   write('found treasure in g'), nl. % ex 7.2

go(X, X, P, R) :-
  reverse(P, R), !.
go(X, Y, P, R) :-
  d(X, Z),
  \+ member(Z, P),
  % write('enter room '),write(Z),nl, % ex 7.2
  go(Z, Y, [Z|P], R).
  % go(Z, Y, [Z|P], R), !. % ex 7.3 just one path

% ?- go(a, f, [], P).
% P = [b, e, f] ;
% P = [b, c, d, e, f] ;
% false.

% ?- has_treasure(T), go(a, T, [], P).
% T = g,
% P = [b, e, g] ;
% T = g,
% P = [b, c, d, e, g] ;
% false.