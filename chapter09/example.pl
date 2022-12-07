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