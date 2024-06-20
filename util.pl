:- use_module(library(clpfd)).

count(Val, List, Count) :- count(Val, List, #=, Count).

% counts(OvertimeShifts, Worker_Shift, DailyOvertimeShifts)
counts(Vals, List, Count) :-
    ( foreach(Val, Vals),
      foreach(ThisCount, Counts),
      param(List) do
        count(Val, List, ThisCount)
    ),
    sum(Counts, Count).

sum(List, Sum) :- sum(List, #=, Sum).

sums(List, Sum) :-
    maplist(sum, List, Sums),
    sum(Sums, Sum).

eq_list(A, B) :- maplist(eq, A, B).

eq(A, B) :- A #= B.

dist(A, B, Dist) :- Dist #= abs(A - B).
