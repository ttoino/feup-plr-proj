:- use_module(library(clpfd)).

counts([], _, Count) :- Count #= 0.
counts([Val | Vals], List, Count) :-
    count(Val, List, #=, ThisCount),
    counts(Vals, List, PartialCount),
    Count #= ThisCount + PartialCount.

eq_list(A, B) :- maplist(eq, A, B).

eq(A, B) :- A #= B.
