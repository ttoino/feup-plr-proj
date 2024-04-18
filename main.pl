:- consult('schedule.pl').
:- consult('input.pl').

:- use_module(library(codesio)).

schedule(Assignments, NightShiftAssignments) :-
    workers(Workers),
    shifts(Shifts),
    night_shifts(NightShifts),
    days(Days),
    overtime_shifts(OvertimeShifts),
    daily_overtime_shifts(DailyOvertimeShifts),
    late_shifts(LateShifts),
    weekly_late_shifts(WeeklyLateShifts),
    alternative_shifts(AlternativeShifts),
    incompatible_shifts(IncompatibleShifts),
    preferred_shifts(PreferredShifts),

    schedule(
        Assignments,
        NightShiftAssignments,
        Workers,
        Shifts,
        NightShifts,
        Days,
        OvertimeShifts,
        DailyOvertimeShifts,
        LateShifts,
        WeeklyLateShifts,
        AlternativeShifts,
        IncompatibleShifts,
        PreferredShifts
    ).

print_schedule(Assignments, NightShiftAssignments) :-
    workers(Workers),
    shifts(Shifts),
    night_shifts(NightShifts),
    days(Days),

    length(Workers, NumberOfWorkers),

    append([Shifts, NightShifts, Days, Workers], Columns),

    maplist(atom_length, Columns, ColumnLengths),
    max_member(ColumnSize, ColumnLengths),

    format_to_codes('~~|~~a~~~d+ ', [ColumnSize], ColumnFormat),

    format(ColumnFormat, ['']),
    ( foreach(Day, Days), param(ColumnFormat) do
        format(ColumnFormat, [Day])
    ), nl,

    ( for(Index, 1, NumberOfWorkers), param(Workers), param(ColumnFormat), param(Shifts), param(Assignments) do
        nth1(Index, Workers, Worker),
        format(ColumnFormat, [Worker]),
        ( foreach(Assignment, Assignments), param(Index), param(Shifts), param(ColumnFormat) do
            nth1(Index, Assignment, ShiftIndex),
            print_shift(ShiftIndex, Shifts, ColumnFormat)
        ), nl
    ).

print_shift(0, _, ColumnFormat) :-
    format(ColumnFormat, ['N/A']).
print_shift(ShiftIndex, Shifts, ColumnFormat) :-
    nth1(ShiftIndex, Shifts, Shift),
    format(ColumnFormat, [Shift]).
