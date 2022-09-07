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