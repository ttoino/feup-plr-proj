# CLP Project

## Installation

In order to run the project, you need to have SICStus Prolog 4.9.0 installed (earlier versions may also work).

## Usage

To run the project, open SICStus Prolog and consult the file `main.pl`.

By default, the "empty" input will be consulted, contained in the file `input.pl`.
If you want to use the "known" input, consult the file `input-known.pl`.

If you want to use the model that didn't have some of its constraints replaced with global constraints, consult the file `schedule-old.pl`.
Otherwise, you can do nothing as the most recent model (in the file `schedule.pl`) has already been loaded.

At this point, you can run the predicate `main` to test the model.

`benchmark(+Flags)` can be used to benchmark the model with the given labeling options (flags).
Otherwise, `benchmark` will benchmark the model with the default flags.

You can change the files `input.pl` and `input-known.pl` (depending on which one you're using) to test the model with different inputs.
