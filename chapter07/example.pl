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

% 7.3 towers of hanoi

hanoi(N) :-
  move(N, left, center, right).

move(0, _, _, _) :- !.
move(N, Source, Destination, Extra) :-
  N1 is N - 1,
  move(N1, Source, Extra, Destination),
  inform(Source, Destination),
  move(N1, Extra, Destination, Source).

inform(From, To) :-
  write(From),write(' => '),write(To),nl.

% ?- hanoi(3).
% left => center
% left => right
% center => right
% left => center
% right => left
% right => center
% left => center
% true.

% [1,2,3] [] []
% [2,3] [1] []
% [3] [1] [2]
% [3] [] [1,2]
% [] [3] [1,2]
% [1] [3] [2]
% [1] [2,3] []
% [] [1,2,3] []

% [trace]  ?- hanoi(2).
%    Call: (10) hanoi(2) ? creep
%    Call: (11) move(2, left, center, right) ? creep
%    Call: (12) _38838 is 2+ -1 ? creep
%    Exit: (12) 1 is 2+ -1 ? creep
%    Call: (12) move(1, left, right, center) ? creep
%    Call: (13) _41116 is 1+ -1 ? creep
%    Exit: (13) 0 is 1+ -1 ? creep
%    Call: (13) move(0, left, center, right) ? creep
%    Exit: (13) move(0, left, center, right) ? creep
%    Call: (13) inform(left, right) ? creep
%    Call: (14) write(left) ? creep
% left
%    Exit: (14) write(left) ? creep
%    Call: (14) write(' => ') ? creep
%  => 
%    Exit: (14) write(' => ') ? creep
%    Call: (14) write(right) ? creep
% right
%    Exit: (14) write(right) ? creep
%    Call: (14) nl ? creep

%    Exit: (14) nl ? creep
%    Exit: (13) inform(left, right) ? creep
%    Call: (13) move(0, center, right, left) ? creep
%    Exit: (13) move(0, center, right, left) ? creep
%    Exit: (12) move(1, left, right, center) ? creep
%    Call: (12) inform(left, center) ? creep
%    Call: (13) write(left) ? creep
% left
%    Exit: (13) write(left) ? creep
%    Call: (13) write(' => ') ? creep
%  => 
%    Exit: (13) write(' => ') ? creep
%    Call: (13) write(center) ? creep
% center
%    Exit: (13) write(center) ? creep
%    Call: (13) nl ? creep

%    Exit: (13) nl ? creep
%    Exit: (12) inform(left, center) ? creep
%    Call: (12) move(1, right, center, left) ? creep
%    Call: (13) _62188 is 1+ -1 ? creep
%    Exit: (13) 0 is 1+ -1 ? creep
%    Call: (13) move(0, right, left, center) ? creep
%    Exit: (13) move(0, right, left, center) ? creep
%    Call: (13) inform(right, center) ? creep
%    Call: (14) write(right) ? creep
% right
%    Exit: (14) write(right) ? creep
%    Call: (14) write(' => ') ? creep
%  => 
%    Exit: (14) write(' => ') ? creep
%    Call: (14) write(center) ? creep
% center
%    Exit: (14) write(center) ? creep
%    Call: (14) nl ? creep

%    Exit: (14) nl ? creep
%    Exit: (13) inform(right, center) ? creep
%    Call: (13) move(0, left, center, right) ? creep
%    Exit: (13) move(0, left, center, right) ? creep
%    Exit: (12) move(1, right, center, left) ? creep
%    Exit: (11) move(2, left, center, right) ? creep
%    Exit: (10) hanoi(2) ? creep
% true.

% 7.4 parts inventory

partlist(P) :-
  collect(P, Q),
  printpartlist(Q).

collect([], []).
collect([quant(X,N)|L1], [quant(X,Ntotal)|L2]) :-
  collectrest(X, N, L1, Others, Ntotal),
  collect(Others, L2).

collectrest(_, N, [], [], N).
collectrest(X, N, [quant(X,Num)|Rest], Others, Ntotal) :-
  !,
  M is N + Num,
  collectrest(X, M, Rest, Others, Ntotal).
collectrest(X, N, [Other|Rest], [Other|Others], Ntotal) :-
  collectrest(X, N, Rest, Others, Ntotal).

printpartlist([]).
printpartlist([quant(X,N)|R]) :-
  write(N),put_char('\t'),write(X),nl,
  printpartlist(R).

% ?- partlist([quant(a,1),quant(b,3),quant(c,2),quant(a,2)]).
% 3       a
% 3       b
% 2       c