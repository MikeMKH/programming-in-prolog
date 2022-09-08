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

% 3.6 joining structures together
partsof(X, [X]) :- basicpart(X).
partsof(X, P) :-
  assembly(X, Subparts),
  partsoflist(Subparts, P).

partsoflist([], []).
partsoflist([P|Tail], All) :-
  partsof(P, Heahparts),
  partsoflist(Tail, Tailparts),
  append(Heahparts, Tailparts, All).

assembly(sentence, [noun_phrase, verb_phrase]).
assembly(noun_phrase, [determiner, noun]).
assembly(verb_phrase, [verb, noun]).

assembly(determiner, [the]).
assembly(noun, [apple]).
assembly(noun, [fruit]).
assembly(verb, [is]).

basicpart(apple).
basicpart(fruit).

% not sure how to use this

% 3.7 accumulators

len1([], 0).
len1([_|T], N) :-
  len1(T, N1),
  N is N1 + 1.

len2(List, N) :- acclen2(List, 0, N).
acclen2([], A, A).
acclen2([_|T], A, N) :-
  A1 is A + 1,
  acclen2(T, A1, N).

exp37_1 :-
  List = [1, 2, 3],
  write(List),write(' length='),
  len1(List, N1),
  len2(List, N2),
  N1 =:= N2,
  write(N1),nl.

accumulator([], _, Seed, Seed).
accumulator([H|T], Func, Seed, R) :-
  apply(Func, [Seed, H, Next]),
  accumulator(T, Func, Next, R).

exp37_2 :-
  List = [1, 2, 3],
  write('fold(+,'),write(List),write(',0)='),
  accumulator(List, plus, 0, R),
  write(R),nl.

% 15-819K Logic Programming: Lecture 11
% https://www.cs.cmu.edu/~fp/courses/lp/lectures/11-diff.pdf

rev(List, Rev) :-
  rev_(List, Rev,[]).
rev_([H|T], Rs,Rev) :-
  rev_(T, Rs,[H|Rev]).
rev_([], Rev,Rev).

exp38_1 :-
  List = [4, 5, 6],
  write('reverse('),write(List),write(')='),
  rev(List, Rev),
  write(Rev),nl.

% https://www.cl.cam.ac.uk/teaching/0809/Prolog/Prolog08ML5R2.pdf

map(List, Func, Mapped) :-
  map_(List, Func, Mapped,[]).
map_([], _, S,S).
map_([H|T], Func, [R|S],S1) :-
  apply(Func, [H, R]),
  map_(T, Func, S,S1).

exp38_2 :-
  List = [7, 8, 9],
  write('map(+1,'),write(List),write(')='),
  map(List, plus(1), Plus1),
  write(Plus1),nl.

% [trace]  ?- map([7,8,9],plus(1),R).
%    Call: (10) map([7, 8, 9], plus(1), _44280) ? creep
%    Call: (11) map_([7, 8, 9], plus(1), _44280, []) ? creep
% ^  Call: (12) apply(plus(1), [7, _46312]) ? creep
% ^  Exit: (12) apply(user:plus(1), [7, 8]) ? creep
%    Call: (12) map_([8, 9], plus(1), _46314, []) ? creep
% ^  Call: (13) apply(plus(1), [8, _48664]) ? creep
% ^  Exit: (13) apply(user:plus(1), [8, 9]) ? creep
%    Call: (13) map_([9], plus(1), _48666, []) ? creep
% ^  Call: (14) apply(plus(1), [9, _51016]) ? creep
% ^  Exit: (14) apply(user:plus(1), [9, 10]) ? creep
%    Call: (14) map_([], plus(1), _51018, []) ? creep
%    Exit: (14) map_([], plus(1), [], []) ? creep
%    Exit: (13) map_([9], plus(1), [10], []) ? creep
%    Exit: (12) map_([8, 9], plus(1), [9, 10], []) ? creep
%    Exit: (11) map_([7, 8, 9], plus(1), [8, 9, 10], []) ? creep
%    Exit: (10) map([7, 8, 9], plus(1), [8, 9, 10]) ? creep
% R = [8, 9, 10].

m([], _, S).
m([H|T], Func, [R|S]) :-
  apply(Func, [H, R]),
  m(T, Func, S).

% ?- m([1,2,3],plus(1),R).
% R = [2, 3, 4|_].