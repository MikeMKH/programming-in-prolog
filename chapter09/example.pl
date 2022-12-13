% 9.2 representing the parsing problem in prolog

sentence(S) :-
  append(N, V, S),
  noun_pharse(N),
  verb_pharse(V).

noun_pharse(S) :-
  append(D, N, S),
  determiner(D),
  noun(N).

verb_pharse(S) :-
  append(V, N, S),
  verb(V),
  noun_pharse(N).
verb_pharse(V) :-
  verb(V).

determiner([the]).

noun([apple]).
noun([man]).
noun([dog]).
noun([treat]).

verb([eats]).
verb([sings]).

% ?- sentence([the,man,eats,the,apple]).
% true .

% ?- sentence([the,dog,eats,the,treat]).
% true .

% ?- sentence([man,eats,the]).
% false.

% ?- sentence([the,apple,eats,the,man]).
% true .

sentence(S0, S) :-
  noun_pharse(S0, S1),
  verb_pharse(S1, S).

noun_pharse(S0, S) :-
  determiner(S0, S1),
  noun(S1, S).

verb_pharse(S0, S) :-
  verb(S0, S1),
  noun_pharse(S1, S).
verb_pharse(S0, S) :-
  verb(S0, S).

determiner([the|S], S).

noun([apple|S], S).
noun([man|S], S).
noun([dog|S], S).
noun([treat|S], S).

verb([eats|S], S).
verb([sings|S], S).

% ?- sentence([the,dog,eats,the,treat]).
% true .

% ?- sentence([the,dog,eats,the,treat], []).
% true .

% ?- sentence([man,eats,the], []).
% false.

% ?- sentence([the,apple,eats,the,man], []).
% true .

% ?- sentence([the,apple,eats,the,man], [the,man]).
% true.

% ?- sentence([the,apple,eats,the,man], [eats,the,man]).
% false.

% 9.3 the grammar rule notation

sentence1 --> noun_pharse1, verb_pharse1.

noun_pharse1 --> determiner1, noun1.

verb_pharse1 --> verb1, noun_pharse1.
verb_pharse1 --> verb1.

determiner1 --> [the].

noun1 --> [apple].
noun1 --> [man].
noun1 --> [dog].
noun1 --> [treat].

verb1 --> [eats].
verb1 --> [sings].

% ?- phrase(sentence1,[the,man,eats,the,apple]).
% true .

% ?- phrase(sentence1,[the,dog,eats,the,treat]).
% true .

% ?- phrase(sentence1,[the,apple,eats,the,man]).
% true .

% ?- phrase(sentence1,[the,dog,eats]).
% true.

% ?- phrase(sentence1,[eats,the,sing]).
% false.

% 9.4 adding extra arguments

sentence2 --> sentence2(_).

sentence2(X) --> noun_pharse2(X), verb_pharse2(X).

noun_pharse2(X) --> determiner2(X), noun2(X).

verb_pharse2(X) --> verb2(X), noun_pharse2(X).
verb_pharse2(X) --> verb2(X).

determiner2(_) --> [the].

noun2(singular) --> [apple].
noun2(singular) --> [man].
noun2(singular) --> [dog].
noun2(singular) --> [treat].
noun2(plural) --> [apples].
noun2(plural) --> [men].
noun2(plural) --> [dogs].
noun2(plural) --> [treats].

verb2(singular) --> [eats].
verb2(singular) --> [sings].
verb2(plural) --> [eat].
verb2(plural) --> [sing].

% ?- phrase(sentence2,[the,dog,eats,the,treat]).
% true .

% ?- phrase(sentence2,[the,dogs,eat,the,treats]).
% true .

% ?- phrase(sentence2,[the,dogs,eats,the,treats]).
% false.

% ?- phrase(sentence2,[the,dog,eat,the,treat]).
% false.

% ex 9.1
translate(X, Y) :-
  R =..[X, Y],
  call(R). % I think this is what is being asked for

% ?- translate(noun, [dog]).
% true.

% ?- translate(verb, [eats]).
% true.

% ?- translate(verb, [dog]).
% false.

% ex 9.2
phrase1(P, L) :-
  P =..[Pred|Type],
  append(Type, [L,[]], Args),
  G =..[Pred|Args],
  call(G).

% ?- phrase(sentence2(X), [the,dog,eats,the,treat]).
% X = singular .

% ?- phrase1(sentence2(X), [the,dog,eats,the,treat]).
% X = singular .

% 9.5 adding extra tests

sentence3 --> sentence3(_).

sentence3(X) --> noun_pharse3(X), verb_pharse3(X).

noun_pharse3(X) --> determiner3(X), noun3(X).

verb_pharse3(X) --> verb3(X), noun_pharse3(X).
verb_pharse3(X) --> verb3(X).

determiner3(_) --> [the].

noun3(S) --> [N], {is_noun(S, N)}.
noun3(plural) -->
  [N],
  {atom_chars(N, PluralN),
   append(RootN, [s], PluralN),
   atom_chars(SingularN, RootN),
   is_noun(singular, SingularN)}. % text example has RootN by mistake

is_noun(singular, apple).
is_noun(singular, man).
is_noun(singular, dog).
is_noun(singular, treat).

verb3(singular) --> [eats].
verb3(singular) --> [sings].
verb3(plural) --> [eat].
verb3(plural) --> [sing].

% ?- phrase(sentence3(X), [the,dogs,eat,the,treats]).
% X = plural .

% ?- phrase(sentence3(X), [the,dog,eats,the,treat]).
% X = singular .

% 9.6 summary

expression --> operation, expression, expression.
expression --> [N], {number(N)}.

operation --> [+].
operation --> [-].
operation --> [*].
operation --> [/].

% ?- phrase(expression, [/,-2,70000000000]).
% true .

% ?- phrase(expression, [+,2,*,3.4,-,56,7.89]).
% true .

% ?- phrase(expression, [+,2]).
% false.