:- use_module(library(clpfd)).

counts([], _, Count) :- Count #= 0.
counts([Val | Vals], List, Count) :-
    count(Val, List, #=, ThisCount),
    counts(Vals, List, PartialCount),
    Count #= ThisCount + PartialCount.

eq(A, B) :- A #= B.
