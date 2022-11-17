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

% 7.5 list processing

last0(X, [X]).
last0(X, [_|Y]) :- last0(X, Y).

% ?- last0(X, [1, 2, 3]).
% X = 3 ;
% false.

% ?- last0(3, [1, 2, 3]).
% true .

% ?- last0(3, L).
% L = [3] ;
% L = [_, 3] ;
% L = [_, _, 3] ;
% L = [_, _, _, 3] .

nextto0(X, Y, [X,Y|_]).
nextto0(X, Y, [_|Z]) :- nextto0(X, Y, Z).

% ?- nextto0(1, 2, [0, 1, 2, 3]).
% true .

% ?- nextto0(1, Y, [0, 1, 2, 3]).
% Y = 2 .

% ?- nextto0(X, 3, [0, 1, 2, 3]).
% X = 2 .

% ?- nextto0(3, Y, [0, 1, 2, 3]).
% false.

% ?- nextto0(X, Y, [0, 1, 2, 3]).
% X = 0,
% Y = 1 ;
% X = 1,
% Y = 2 .

% ?- nextto0(1, 2, L).
% L = [1, 2|_] ;
% L = [_, 1, 2|_] ;
% L = [_, _, 1, 2|_] .

append0([], L, L).
append0([X|L1], L2, [X|L3]) :- append0(L1, L2, L3).

% ?- append0([a, b], [1, 2], L).
% L = [a, b, 1, 2].

% ?- append0([a, b], Y, [a, b, 3, 4]).
% Y = [3, 4].

% ?- append0(X, [3, 4], [a, b, 3, 4]).
% X = [a, b] .

% ?- append0([a, b], [3, 4], [a, b, 3, 4]).
% true.

% ?- append0([a, b], [no], [a, b, 3, 4]).
% false.

% ?- append0(X, Y, [a, b, 3, 4]).
% X = [],
% Y = [a, b, 3, 4] ;
% X = [a],
% Y = [b, 3, 4] ;
% X = [a, b],
% Y = [3, 4] ;
% X = [a, b, 3],
% Y = [4] .

last1(X, L) :- append0(_, [X], L).
nextto1(X, Y, L) :- append0(_, [X,Y|_], L).
member0(X, L) :- append0(_, [X|_], L).

% ?- last1(X, [1, 2, 3]).
% X = 3 ;
% false.

% ?- nextto1(X, Y, [1, 2, 3]).
% X = 1,
% Y = 2 ;
% X = 2,
% Y = 3 ;
% false.

% ?- member0(X, [1, 2, 3]).
% X = 1 ;
% X = 2 ;
% X = 3 ;
% false.

% ?- member0(2, [1, 2, 3]).
% true .

reverse0([], []).
reverse0([H|T], L) :-
  reverse0(T, L1), append0(L1, [H], L).

reverse1(L, R) :- revacc1(L, [], R).
revacc1([], L, L).
revacc1([X|L1], L2, L3) :-
  revacc1(L1, [X|L2], L3).

% ?- reverse1([1, 2, 3], L).
% L = [3, 2, 1].

% ?- reverse1(L, [4, 5, 6]).
% L = [6, 5, 4] .

% ?- reverse1([1, 2, 3], [X|1]).
% false.

% ?- reverse1([1, 2, 3], [3|X]).
% X = [2, 1].

remove1(_, [], []).
remove1(X, [X|L], L) :- !.
remove1(X, [Y|L], [Y|M]) :- remove1(X, L, M).

% ?- remove1(1, [1, 2, 3, 1, 2, 3, 4], L).
% L = [2, 3, 1, 2, 3, 4].

% ?- remove1(a, [1, 2, 3, 1, 2, 3, 4], L).
% L = [1, 2, 3, 1, 2, 3, 4] .

% ?- remove1(X, [1, 2, 3], [1, 2, 3, 4]).
% false.

% ?- remove1(1, L, [1, 2, 3, 4]).
% L = [1, 1, 2, 3, 4].

removeall(_, [], []).
removeall(X, [X|L], M) :- !, removeall(X, L, M).
removeall(X, [Y|L], [Y|M]) :- removeall(X, L, M).

% ?- removeall(a, [1, 2, 3, 1, 2, 3, 4], L).
% L = [1, 2, 3, 1, 2, 3, 4] .

% 7.6 representing and manipulating sets

element_of(X, [X|_]).
element_of(X, [_|Y]) :- element_of(X, Y).

% ?- element_of(2, [1, 2, 3]).
% true .

% ?- element_of(2, []).
% false.

% ?- element_of(2, [4, 5, 6]).

subset([], _).
subset([X|R], Y) :- element_of(X, Y), subset(R, Y).

% ?- subset([], []).
% true.

% ?- subset([], [1, 2, 3]).
% true.

% ?- subset([1], [1, 2, 3]).
% true .

% ?- subset([1, 3], [1, 2, 3]).
% true .

% ?- subset([1, 3], [4, 5, 6]).
% false.

% ?- subset([1, 4], [4, 5, 6]).
% false.

intersection([], _, []).
intersection([X|R], Y, [X|Z]) :-
  element_of(X, Y),
  !,
  intersection(R, Y, Z).
intersection([_|R], Y, Z) :- intersection(R, Y, Z).

% ?- intersection([], [], R).
% R = [].

% ?- intersection([], [1, 2, 3], R).
% R = [].

% ?- intersection([1, 3], [], R).
% R = [].

% ?- intersection([1], [1, 2, 3], R).
% R = [1].

% ?- intersection([1, 3], [1, 2, 3], R).
% R = [1, 3].

% ?- intersection([1, 3], [4, 5, 6], R).
% R = [].

% ?- intersection([1, 3, 4], [4, 5, 6], R).
% R = [4].

union([], S, S).
union([X|R], Y, Z) :-
  element_of(X, Y),
  !,
  union(R, Y, Z).
union([X|R], Y, [X|Z]) :- union(R, Y, Z).

% ?- union([], [1], R).
% R = [1].

% ?- union([1], [2, 3], R).
% R = [1, 2, 3].

% ?- union([1, 2], [3, 4, 5], R).
% R = [1, 2, 3, 4, 5].

% ?- union([1, 2], [1, 2, 3, 4, 5], R).
% R = [1, 2, 3, 4, 5].

% ?- union([1, 2], [], R).
% R = [1, 2].

% ?- union([1, 2], Y, [1, 2, 3]).
% Y = [1, 2, 3].

% ?- union(X, Y, [1, 2, 3]).
% X = [],
% Y = [1, 2, 3] ;
% X = [1],
% Y = [1, 2, 3] ;
% X = [1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1, 1, 1, 1, 1],
% Y = [1, 2, 3] ;
% X = [1, 1, 1, 1, 1, 1, 1, 1, 1|...],
% Y = [1, 2, 3] .

% 7.7 sorting

naive_sort(O, L, R) :- permutation(L, R), sorted(O, R), !.
sorted(_, []).
sorted(_, [_]).
sorted(O, [X,Y|R]) :-
  P =.. [O, X, Y],
  call(P),
  sorted(O, [Y|R]).

% ?- naive_sort(<, [1, 2, 3], R).
% R = [1, 2, 3].

% ?- naive_sort(<, [2, 1, 3], R).
% R = [1, 2, 3].

% ?- naive_sort(<, [3, 2, 1], R).
% R = [1, 2, 3].

% ?- naive_sort(@<, [a, b, c, aaa, d, aa, e, aaaa, f], R).
% R = [a, aa, aaa, aaaa, b, c, d, e, f].

% ex 7.5

% ?- permutation([1, 2, 3], R).
% R = [1, 2, 3] ;
% R = [1, 3, 2] ;
% R = [2, 1, 3] ;
% R = [2, 3, 1] ;
% R = [3, 1, 2] ;
% R = [3, 2, 1] ;
% false.

% ex 7.6

my_sort(O, L, R) :-
  length(L, Len),
  (Len < 4
  -> naive_sort(O, L, R)
  ; sort(0, O, L, R)).

% ?- my_sort(>, [1, 2, 3], R).
% R = [3, 2, 1].

% ?- my_sort(>, [1, 2, 3, 4], R).
% R = [4, 3, 2, 1].

% 7.8 using the database

% ex 7.7

random_pick(L, E) :-
  length(L, Length),
  random_between(1, Length, Index),
  nth1(Index, L, E).

% ?- random_pick([1, 2, 3], E).
% E = 3.

% ?- random_pick([1, 2, 3], E).
% E = 1.

% ?- random_pick([1, 2, 3], E).
% E = 2.

% ?- random_pick([1, 2, 3], E).
% E = 1.

% ?- random_pick([1, 2, 3], E).
% E = 3.

% ex 7.8

% ?- findall(X, member(X, [1, 2, 3]), R).
% R = [1, 2, 3].

% ?- findall(X, member(Y, [1, 2, 3]), R).
% R = [_, _, _].

% ?- findall(X, member(X, []), R).
% R = [].

% ?- findall(X, member(Y, []), R).
% R = [].

% ?- findall(hello, member(X, [1, 2, 3]), R).
% R = [hello, hello, hello].

% ?- findall(foo(X,Y), (member(X,[1,2]),member(Y,[a,b])), R).
% R = [foo(1, a), foo(1, b), foo(2, a), foo(2, b)].

% ?- findall(foo(X,Y), (member(X,[1,2]),member(Y,[a,b,c])), R).
% R = [foo(1, a), foo(1, b), foo(1, c), foo(2, a), foo(2, b), foo(2, c)].

% ?- findall(foo(X,Y,hi,bye), (member(X,[1,2]),member(Y,[a,b,c])), R).
% R = [foo(1, a, hi, bye), foo(1, b, hi, bye), foo(1, c, hi, bye), foo(2, a, hi, bye), foo(2, b, hi, bye), foo(2, c, hi, bye)].

% 7.9 searching graphs

a(g,h).
a(g,d).
a(e,d).
a(h,f).
a(e,f).
a(a,e).
a(a,b).
a(b,f).
a(b,c).
a(f,c).

go0(X,X).
go0(X,Y) :- a(X,Z),go0(Z,Y).

% ?- go0(a,h).
% false.

% ?- go0(a,c).
% true ;
% true ;
% true ;
% false.

go(Start, End, Route) :-
  go1(Start, End, [], R),
  reverse(R, Route).

go1(X, X, T, [X|T]).
go1(X, Y, T, R) :-
  a(X, Z),
  legal(Z, T),
  go1(Z, Y, [X|T], R).
legal(_, []).
legal(X, [H|T]) :-
  \+ X = H,
  legal(X, T).

% ?- go(a,c,Path).
% Path = [a, e, f, c] ;
% Path = [a, b, f, c] ;
% Path = [a, b, c] ;
% false.

% ?- go(a,h,Path).
% false.

% 7.10 sift the two's and sift the three's

% ex 7.9

pythag(X, Y, Z, N) :-
  length(_, N), % https://stackoverflow.com/a/10517031/2370606
  between(1, N, X),
  between(1, N, Y),
  between(1, N, Z),
  Z * Z =:= X * X + Y * Y.

% ?- pythag(X, Y, Z, N).
% X = 3,
% Y = 4,
% Z = N, N = 5 .

% ?- pythag(X, Y, Z, 10).
% X = 3,
% Y = 4,
% Z = 5 ;
% X = 4,
% Y = 3,
% Z = 5 ;
% X = 6,
% Y = 8,
% Z = 10 ;
% X = 8,
% Y = 6,
% Z = 10 ;
% false.

% 7.11 symbolic differentiation

?- op(300, yfx, ^).
d(X, X, 1) :- !.
d(C, _, 0) :- atomic(C).
d(-U, X, -A) :- d(U, X, A).
d(U+V, X, A+B) :- d(U, X, A), d(V, X, B).
d(U-V, X, A-B) :- d(U, X, A), d(V, X, B).
d(C*U, X, C*A) :- atomic(C), \+ C = X, d(U, X, A), !.
d(U*V, X, B*U+A*V) :- d(U, X, A), d(V, X, B).
d(U/V, X, A) :- d(U*V^(-1), X, A).
d(U^C, X, C*U^(C-1)*W) :- atomic(C), \+ C = X, d(U, X, W).
d(log(U), X, A*U^(-1)) :- d(U, X, A).

% ?- d(x+1, x, R).
% R = 1+0.

% ?- d(x*x-2, x, R).
% R = 1*x+1*x-0.

% ?- d(x*y, x, R).
% R = 0*x+1*y.

% ?- d(x^2, x, R).
% R = 2*x^(2-1)*1.

% ?- d(5*x+x^2, x, R).
% R = 5*1+2*x^(2-1)*1.

% 7.12 mapping structures and transforming trees

simpl(E, E) :- atomic(E), !.
simpl(E, F) :-
  E =.. [Op, L, R],
  simpl(L, X),
  simpl(R, Y),
  s(Op, X, Y, F).

s(*, 1, X, X).
s(*, X, 1, X).
s(*, 0, _, 0).
s(*, _, 0, 0).
s(*, X, Y, Z) :-
  number(X), number(Y),
  Z is X * Y.
s(*, X, Y, X*Y). % catch all

s(+, 0, X, X).
s(+, X, 0, X).
s(+, X, Y, Z) :-
  number(X), number(Y),
  Z is X + Y.
s(+, X, Y, X+Y). % catch all

s(^, X, 1, X).
s(^, _, 0, 0).
s(^, X, Y, Z) :-
  number(X), number(Y),
  pow(X, Y, Z).
s(^, X, Y, X^Y). % catch all

s(-, X, 0, X).
s(-, X, Y, Z) :-
  number(X), number(Y),
  Z is X - Y.
s(-, X, Y, X-Y). % catch all

% ?- simpl(0 + x * 1 + 0 * y * 1 + 0, R).
% R = x .

% ?- simpl(0 + x * 1 + y * 1 + 0, R).
% R = x+y .

% ?- simpl(0 + x * 1 + y * 1 + 0 + 2 * 3 + 1 + 0 * 1 - 2 * 1, R).
% R = x+y+6+1-2 .

% 7.13 manipulating programs

fact(lily, good).
fact(lily, dog).

l(Head) :-
  clause(Head, Body),
  output_clause(Head, Body),write('.'),nl,
  fail.
l(_).

output_clause(Head, true) :- !,write(Head).
output_clause(Head, Body) :- write((Head :- Body)).

% ?- clause(fact(lily, X),B).
% X = good,
% B = true ;
% X = dog,
% B = true.

% ?- l(fact).
% true.

% ?- l(fact(Subject,Attribute)).
% fact(lily,good).
% fact(lily,dog).
% true.

% ?- l(fact(lily,Attribute)).
% fact(lily,good).
% fact(lily,dog).
% true.

% ?- l(fact(Subject,good)).
% fact(lily,good).
% true.