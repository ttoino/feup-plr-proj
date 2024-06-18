:- use_module(library(clpfd)).

counts([], _, Count) :- Count #= 0.
counts([Val | Vals], List, Count) :-
    count(Val, List, #=, ThisCount),
    counts(Vals, List, PartialCount),
    Count #= ThisCount + PartialCount.

sums([], Sum) :- Sum #= 0.
sums([List | Lists], Sum) :-
    sum(List, #=, ThisSum),
    sums(Lists, PartialSum),
    Sum #= ThisSum + PartialSum.

eq_list(A, B) :- maplist(eq, A, B).

eq(A, B) :- A #= B.
