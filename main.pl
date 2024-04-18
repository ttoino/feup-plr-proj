:- consult('schedule.pl').
:- consult('input.pl').

:- use_module(library(codesio)).

schedule(
    Day_Worker_Shift,
    Day_Shift_Worker,
    Worker_Day_Shift,
    Shift_Day_Worker,
    Day_Worker_NightShift,
    Day_NightShift_Worker,
    Worker_Day_NightShift,
    NightShift_Day_Worker
) :-
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
    rotated_shifts(RotatedShifts),

    schedule(
        Day_Worker_Shift,
        Day_Shift_Worker,
        Worker_Day_Shift,
        Shift_Day_Worker,
        Day_Worker_NightShift,
        Day_NightShift_Worker,
        Worker_Day_NightShift,
        NightShift_Day_Worker,
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
        PreferredShifts,
        RotatedShifts
    ).

print_schedule(
    Shift_Day_Worker,
    NightShift_Day_Worker
) :-
    workers(Workers),
    shifts(Shifts),
    night_shifts(NightShifts),
    days(Days),

    append([Shifts, NightShifts, Days, Workers], Columns),

    maplist(atom_length, Columns, ColumnLengths),
    max_member(ColumnSize, ColumnLengths),

    format_to_codes('~~|~~a~~~d+ ', [ColumnSize], ColumnFormat),

    format(ColumnFormat, ['']),
    ( foreach(Day, Days), param(ColumnFormat) do
        format(ColumnFormat, [Day])
    ), nl,

    ( foreach(Day_Worker, Shift_Day_Worker), foreach(Shift, Shifts), param(Workers), param(ColumnFormat) do
        format(ColumnFormat, [Shift]),
        ( foreach(Worker, Day_Worker), param(Workers), param(ColumnFormat) do
            print_worker(Worker, Workers, ColumnFormat)
        ),
        nl
    ),
    
    ( foreach(Day_Worker, NightShift_Day_Worker), foreach(NightShift, NightShifts), param(Workers), param(ColumnFormat) do
        format(ColumnFormat, [NightShift]),
        ( foreach(Worker, Day_Worker), param(Workers), param(ColumnFormat) do
            print_worker(Worker, Workers, ColumnFormat)
        ),
        nl
    ).

print_worker(0, _, ColumnFormat) :-
    format(ColumnFormat, ['']).
print_worker(WorkerIndex, Workers, ColumnFormat) :-
    nth1(WorkerIndex, Workers, Worker),
    format(ColumnFormat, [Worker]).

main :-
    schedule(_, _, _, Shift_Day_Worker, _, _, _, NightShift_Day_Worker), !,
    print_schedule(Shift_Day_Worker, NightShift_Day_Worker).
