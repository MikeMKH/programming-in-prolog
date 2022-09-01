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