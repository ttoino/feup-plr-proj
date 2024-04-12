:- consult('schedule.pl').
:- consult('input.pl').

schedule(Assignments) :-
    workers(Workers),
    shifts(Shifts),
    night_shifts(NightShifts),
    days(Days),
    overtime_shifts(OvertimeShifts),
    daily_overtime_shifts(DailyOvertimeShifts),
    late_shifts(LateShifts),
    weekly_late_shifts(WeeklyLateShifts),
    incompatible_with_night_shifts(IncompatibleWithNightShifts),
    alternative_shifts(AlternativeShifts),
    incompatible_shifts(IncompatibleShifts),
    preferred_shifts(PreferredShifts),

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
        PreferredShifts.
    ).

print_schedule(Assignments) :-
    true.
