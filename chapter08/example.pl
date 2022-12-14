% 8.3 the tracing model

foo(a, aa).
foo(a, ab).
foo(ab, ba).
foo(ab, bb).
foo(ba, ca).
foo(ca, da).

bar(X, Y) :- foo(X, Y).
bar(X, Z) :- foo(X, Y), bar(Y, Z).

dar(X, Z) :- foo(X, Y), dar(Y, Z).
dar(X, Y) :- foo(X, Y).

baz(X, Y) :- foo(X, Y), !.
baz(X, Z) :- foo(X, Y), baz(Y, Z).

caz(X, Y) :- foo(X, Y).
caz(X, Z) :- foo(X, Y), !, caz(Y, Z).

% ?- bar(a, X).
% X = aa ;
% X = ab ;
% X = ba ;
% X = bb ;
% X = ca ;
% X = da ;
% false.

% ?- bar(a, da).
% true ;
% false.

% ?- dar(a, X).
% X = da ;
% X = ca ;
% X = ba ;
% X = bb ;
% X = aa ;
% X = ab.

% ?- dar(a, da).
% true ;
% false.

% ?- baz(a, X).
% X = aa.

% ?- baz(a, da).
% true ;
% false.

% ?- caz(a, X).
% X = aa ;
% X = ab ;
% false.

% ?- caz(a, da).
% false.

% [trace]  ?- bar(a, X).
%    Call: (10) bar(a, _47572) ? creep
%    Call: (11) foo(a, _47572) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Exit: (10) bar(a, aa) ? creep
% X = aa ;
%    Redo: (11) foo(a, _47572) ? creep
%    Exit: (11) foo(a, ab) ? creep
%    Exit: (10) bar(a, ab) ? creep
% X = ab ;
%    Redo: (10) bar(a, _47572) ? creep
%    Call: (11) foo(a, _56740) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Call: (11) bar(aa, _47572) ? creep
%    Call: (12) foo(aa, _47572) ? creep
%    Fail: (12) foo(aa, _47572) ? creep
%    Redo: (11) bar(aa, _47572) ? creep
%    Call: (12) foo(aa, _61266) ? creep
%    Fail: (12) foo(aa, _61266) ? creep
%    Fail: (11) bar(aa, _47572) ? creep
%    Redo: (11) foo(a, _56740) ? creep
%    Exit: (11) foo(a, ab) ? creep
%    Call: (11) bar(ab, _47572) ? creep
%    Call: (12) foo(ab, _47572) ? creep
%    Exit: (12) foo(ab, ba) ? creep
%    Exit: (11) bar(ab, ba) ? creep
%    Exit: (10) bar(a, ba) ? creep
% X = ba .

% [trace]  ?- dar(a, X).
%    Call: (10) dar(a, _88028) ? creep
%    Call: (11) foo(a, _89230) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Call: (11) dar(aa, _88028) ? creep
%    Call: (12) foo(aa, _91494) ? creep
%    Fail: (12) foo(aa, _91494) ? creep
%    Redo: (11) dar(aa, _88028) ? creep
%    Call: (12) foo(aa, _88028) ? creep
%    Fail: (12) foo(aa, _88028) ? creep
%    Fail: (11) dar(aa, _88028) ? creep
%    Redo: (11) foo(a, _89230) ? creep
%    Exit: (11) foo(a, ab) ? creep
%    Call: (11) dar(ab, _88028) ? creep
%    Call: (12) foo(ab, _98282) ? creep
%    Exit: (12) foo(ab, ba) ? creep
%    Call: (12) dar(ba, _88028) ? creep
%    Call: (13) foo(ba, _100546) ? creep
%    Exit: (13) foo(ba, ca) ? creep
%    Call: (13) dar(ca, _88028) ? creep
%    Call: (14) foo(ca, _102810) ? creep
%    Exit: (14) foo(ca, da) ? creep
%    Call: (14) dar(da, _88028) ? creep
%    Call: (15) foo(da, _105074) ? creep
%    Fail: (15) foo(da, _105074) ? creep
%    Redo: (14) dar(da, _88028) ? creep
%    Call: (15) foo(da, _88028) ? creep
%    Fail: (15) foo(da, _88028) ? creep
%    Fail: (14) dar(da, _88028) ? creep
%    Redo: (13) dar(ca, _88028) ? creep
%    Call: (14) foo(ca, _88028) ? creep
%    Exit: (14) foo(ca, da) ? creep
%    Exit: (13) dar(ca, da) ? creep
%    Exit: (12) dar(ba, da) ? creep
%    Exit: (11) dar(ab, da) ? creep
%    Exit: (10) dar(a, da) ? creep
% X = da ;
%    Redo: (12) dar(ba, _88028) ? creep
%    Call: (13) foo(ba, _88028) ? creep
%    Exit: (13) foo(ba, ca) ? creep
%    Exit: (12) dar(ba, ca) ? creep
%    Exit: (11) dar(ab, ca) ? creep
%    Exit: (10) dar(a, ca) ? creep
% X = ca ;
%    Redo: (12) foo(ab, _98282) ? creep
%    Exit: (12) foo(ab, bb) ? creep
%    Call: (12) dar(bb, _88028) ? creep
%    Call: (13) foo(bb, _124352) ? creep
%    Fail: (13) foo(bb, _124352) ? creep
%    Redo: (12) dar(bb, _88028) ? creep
%    Call: (13) foo(bb, _88028) ? creep
%    Fail: (13) foo(bb, _88028) ? creep
%    Fail: (12) dar(bb, _88028) ? creep
%    Redo: (11) dar(ab, _88028) ? creep
%    Call: (12) foo(ab, _88028) ? creep
%    Exit: (12) foo(ab, ba) ? creep
%    Exit: (11) dar(ab, ba) ? creep
%    Exit: (10) dar(a, ba) ? creep
% X = ba .

% [trace]  ?- baz(a, X).
%    Call: (10) baz(a, _42524) ? creep
%    Call: (11) foo(a, _42524) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Exit: (10) baz(a, aa) ? creep
% X = aa.

% [trace]  ?- caz(a, X).
%    Call: (10) caz(a, _24448) ? creep
%    Call: (11) foo(a, _24448) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Exit: (10) caz(a, aa) ? creep
% X = aa ;
%    Redo: (11) foo(a, _24448) ? creep
%    Exit: (11) foo(a, ab) ? creep
%    Exit: (10) caz(a, ab) ? creep
% X = ab ;
%    Redo: (10) caz(a, _24448) ? creep
%    Call: (11) foo(a, _33594) ? creep
%    Exit: (11) foo(a, aa) ? creep
%    Call: (11) caz(aa, _24448) ? creep
%    Call: (12) foo(aa, _24448) ? creep
%    Fail: (12) foo(aa, _24448) ? creep
%    Redo: (11) caz(aa, _24448) ? creep
%    Call: (12) foo(aa, _38120) ? creep
%    Fail: (12) foo(aa, _38120) ? creep
%    Fail: (11) caz(aa, _24448) ? creep
%    Fail: (10) caz(a, _24448) ? creep
% false.

% 8.4 tracing and spy points

s(X, Y) :- permutation(X, Y), sorted(Y), !.
sorted([]).
sorted([_]).
sorted([X,Y|R]) :-
  X =< Y,
  sorted([Y|R]).

% ?- s([1, 2, 3], R).
% R = [1, 2, 3].

% ?- s([3, 2, 1], R).
% R = [1, 2, 3].

% ?- s([3, 1, 1], R).
% R = [1, 1, 3].

% ?- s([3, 1], R).
% R = [1, 3].

% ?- s([1], R).
% R = [1].

% ?- s([], R).
% R = [].

% ?- s(X, [1, 2, 3]).
% X = [1, 2, 3].

% ?- s([2, X, 3], [1, 2, 3]).

% [trace]  ?- s([3, 2, 1], R).
%    Call: (10) s([3, 2, 1], _21432) ? creep
%    Call: (11) lists:permutation([3, 2, 1], _21432) ? skip
%    Exit: (11) lists:permutation([3, 2, 1], [3, 2, 1]) ? creep
%    Call: (11) sorted([3, 2, 1]) ? leap
% R = [1, 2, 3].

% [trace]  ?- s([3, 2, 1], R).
%    Call: (10) s([3, 2, 1], _30602) ? creep
%    Call: (11) lists:permutation([3, 2, 1], _30602) ? creep
%    Exit: (11) lists:permutation([3, 2, 1], [3, 2, 1]) ? creep
%    Call: (11) sorted([3, 2, 1]) ? creep
%    Call: (12) 3=<2 ? creep
%    Fail: (12) 3=<2 ? creep
%    Fail: (11) sorted([3, 2, 1]) ? creep
%    Redo: (11) lists:permutation([3, 2, 1], [3, 2, 1]) ? creep
%    Exit: (11) lists:permutation([3, 2, 1], [3, 1, 2]) ? creep
%    Call: (11) sorted([3, 1, 2]) ? creep
%    Call: (12) 3=<1 ? creep
%    Fail: (12) 3=<1 ? creep
%    Fail: (11) sorted([3, 1, 2]) ? creep
%    Redo: (11) lists:permutation([3, 2, 1], [3, 1, 2]) ? creep
%    Exit: (11) lists:permutation([3, 2, 1], [2, 3, 1]) ? creep
%    Call: (11) sorted([2, 3, 1]) ? skip
%    Fail: (11) sorted([2, 3, 1]) ? creep
%    Redo: (11) lists:permutation([3, 2, 1], [2, 3, 1]) ? fail
%    Exit: (11) lists:permutation([3, 2, 1], [2, 1, 3]) ? fail
%    Fail: (10) s([3, 2, 1], _30602) ? retry
% retry s/2 at level 10
%    Call: (10) s([3, 2, 1], _30602) ? creep
%    Call: (11) lists:permutation([3, 2, 1], _30602) ? creep
%    Exit: (11) lists:permutation([3, 2, 1], [3, 2, 1]) ? creep
%    Call: (11) sorted([3, 2, 1]) ? fail
%    Redo: (11) lists:permutation([3, 2, 1], [3, 2, 1]) ? creep
%    Exit: (11) lists:permutation([3, 2, 1], [3, 1, 2]) ? fail
%    Fail: (10) s([3, 2, 1], _30602) ? creep
% false.

% 8.5 fixing bugs

ap([], X, X).
ap([A|B], C, [A|D]) :- ap(B, C, D).

rv([], []).
rv([A|B], C) :- rv(B, D), ap(D, [A], C).

% ?- ap([a,b,c], [1,2,3], X).
% X = [a, b, c, 1, 2, 3].

% ?- rv([a,b,c],  X).
% X = [c, b, a].

% ?- ap([], [1,2,3], X).
% X = [1, 2, 3].

% ?- rv([],  X).
% X = [].