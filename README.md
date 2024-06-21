# CLP Project

## Installation

In order to run the project, you need to have SICStus Prolog 4.9.0 installed.

## Usage

To run the project, open SICStus Prolog and consult the file `main.pl`.

Then, if you want to use the "empty" input, consult the file `input.pl`.
Otherwise, if you want to use the "known" input, consult the file `input-known.pl`.

If you want to use the model that didn't have some of its constraints replaced with global constraints, consult the files `schedule-old.pl` and `util-old.pl`.
Othewise, consult the files `schedule.pl` and `util.pl`.

At this point, you can run the predicate `main` to test the model.

`benchmark(Flags)` can be used to benchmark the model with the given labeling options (flags).
Otherwise, `benchmark` will run the model with the default flags.

You can change the files `input.pl` and `input-known.pl` (depends on which one you're using) to test the model with different inputs.
