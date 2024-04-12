:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

schedule(
    Assignments,
    Workers,
    Shifts,
    NightShifts,
    Days,
    OvertimeShifts,
    DailyOvertimeShifts,
    LateShifts,
    WeeklyLateShifts,
    IncompatibleWithNightShifts,
    AlternativeShifts,
    IncompatibleShifts,
    PreferredShifts
) :-
    length(Workers, NumberOfWorkers),
    length(Shifts, NumberOfShifts),
    length(NightShifts, NumberOfNightShifts),
    length(Days, NumberOfDays),

    length(Assignments, NumberOfDays),
    length(AssignmentsInverse, NumberOfDays),
    
