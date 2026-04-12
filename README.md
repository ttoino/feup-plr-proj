# FEUP-PLR-PROJ

Group project for the PLR course unit at FEUP.

The goal of this project was to develop a constraint logic programming solution for a scheduling problem using Prolog and CLP(FD).

## Problem Description

The project addresses a scheduling problem where the objective is to assign tasks to time slots subject to various constraints such as:
- Task durations and dependencies
- Resource availability
- Time windows and deadlines
- Optimization criteria (minimizing makespan, resource usage, etc.)

## Implementation

The solution uses:
- **SICStus Prolog 4.9.0** (or compatible version)
- **CLP(FD)** (Constraint Logic Programming over Finite Domains)
- Global constraints for efficient propagation

## Running

To run the project:

1. Open SICStus Prolog
2. Consult the main file:
```prolog
?- consult('main.pl').
```

3. By default, the "empty" input from `input.pl` is used. To use the "known" input:
```prolog
?- consult('input-known.pl').
```

4. Run the main predicate:
```prolog
?- main.
```

### Benchmarking

To benchmark the model:
```prolog
?- benchmark(Flags).  % With specific labeling options
?- benchmark.         % With default flags
```

## Files

- `main.pl` - Entry point and top-level predicates
- `schedule.pl` - Main scheduling model with global constraints
- `schedule-old.pl` - Alternative model without some global constraints
- `input.pl` - Default (empty) problem instance
- `input-known.pl` - Known/test problem instance
- `util.pl` - Utility predicates

## Unit info

- **Name**: Programação em Lógica (Logic Programming)
- **Date**: Year 1, Semester 2, 2023/24
- [**More info**](https://sigarra.up.pt/feup/ucurr_geral.ficha_uc_view?pv_ocorrencia_id=518824)

## Disclaimer

This repository (and all others with the name format `feup-*`) are for archival and educational purposes only.

If you don't understand some part of the code or anything else in this repo, feel free to ask (although I may not understand it myself anymore).

Keep in mind that this repo is public. If you copy any code and use it in your school projects you may be flagged for plagiarism by automated tools.
