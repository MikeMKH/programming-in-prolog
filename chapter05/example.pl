% 5.1 reading and writing terms

tease(Name) :-
  random_between(1, 3, Tease),
  tease_message(Tease, Name, Message),
  write(Message),nl.
tease_message(1, Name, Message) :-
  string_concat(Name, ' has not read a book since their school days.', Message).
tease_message(2, Name, Message) :-
  string_concat(Name, ' does not even know what a matrix is.', Message).
tease_message(3, Name, Message) :-
  string_concat(Name, ' watches TV all night.', Message).

% ?- tease('Mike').
% Mike does not even know what a matrix is.
% true.

% ?- tease('Mike').
% Mike watches TV all night.
% true.

% ?- tease('Mike').
% Mike has not read a book since their school days.
% true.

% ?- write_canonical(a+b*c/d-e+1.23*45.6).
% +(-(+(a,/(*(b,c),d)),e),*(1.23,45.6))
% true.

% ?- write_canonical(tease('Lily')).
% tease('Lily')
% true.