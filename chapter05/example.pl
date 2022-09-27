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

% 5.2 reading and writing characters

check_line(OK) :-
  get_char(X),
  rest_line('\n', X, OK).
rest_line(_, '\n', true).
rest_line(Last, Current, false) :-
  typing_error(Last, Current), !,
  get_char(Next),
  rest_line(Current, Next, _).
rest_line(_, Current, OK) :-
  get_char(Next),
  rest_line(Current, Next, OK).
typing_error('q', 'w').
typing_error('c', 'v').