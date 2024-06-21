:- use_module(library(clpfd)).

% Same as `count/4`, but with `RelOp` fixed to `#=`.
count(Val, List, Count) :- count(Val, List, #=, Count).

% Same as `count/4`, but counts the occurences of each element in `Vals`.
counts(Vals, List, Count) :-
    ( foreach(Val, Vals),
      foreach(ThisCount, Counts),
      param(List) do
        count(Val, List, ThisCount)
    ),
    sum(Counts, Count).

% Same as `sum/3`, but with `RelOp` fixed to `#=`.
sum(List, Sum) :- sum(List, #=, Sum).

% Same as `sum/3`, but accepts a list of lists.
sums(List, Sum) :-
    maplist(sum, List, Sums),
    sum(Sums, Sum).

% Constraints the each element in `A` to equal the corresponding element in `B`.
eq_list(A, B) :- maplist(eq, A, B).

% Constraints `A` to equal `B`.
eq(A, B) :- A #= B.

% Constraints `Y` to 1 if `X` is not zero, otherwise 0.
not_zero(X, Y) :- Y #= (X #\= 0).

% Constraints `Dist` to be the absolute difference between `A` and `B`.
dist(A, B, Dist) :- Dist #= abs(A - B).
