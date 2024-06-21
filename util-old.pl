:- use_module(library(clpfd)).

% Same as `count/4`, but counts the occurences of each element in `Vals`.
counts([], _, Count) :- Count #= 0.
counts([Val | Vals], List, Count) :-
    count(Val, List, #=, ThisCount),
    counts(Vals, List, PartialCount),
    Count #= ThisCount + PartialCount.

% Same as `sum/3`, but accepts a list of lists.
sums([], Sum) :- Sum #= 0.
sums([List | Lists], Sum) :-
    sum(List, #=, ThisSum),
    sums(Lists, PartialSum),
    Sum #= ThisSum + PartialSum.

% Constraints the each element in `A` to equal the corresponding element in `B`.
eq_list(A, B) :- maplist(eq, A, B).

% Constraints `A` to equal `B`.
eq(A, B) :- A #= B.
