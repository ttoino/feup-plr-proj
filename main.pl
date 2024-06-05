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
    NightShift_Day_Worker,
    AvailableAbsencesOut,
    RotatedShiftScoresOut,
    OvertimeShiftScoresOut
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
    known_shifts(KnownShifts),
    known_night_shifts(KnownNightShifts),
    available_absences(AvailableAbsences),
    rotated_shift_scores(RotatedShiftScores),
    overtime_shift_scores(OvertimeShiftScores),

    schedule(
        Day_Worker_Shift,
        Day_Shift_Worker,
        Worker_Day_Shift,
        Shift_Day_Worker,
        Day_Worker_NightShift,
        Day_NightShift_Worker,
        Worker_Day_NightShift,
        NightShift_Day_Worker,
        AvailableAbsencesOut,
        RotatedShiftScoresOut,
        OvertimeShiftScoresOut,
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
        RotatedShifts,
        KnownShifts,
        KnownNightShifts,
        AvailableAbsences,
        RotatedShiftScores,
        OvertimeShiftScores
    ).

print_outputs(
    AvailableAbsences,
    RotatedShiftScores,
    OvertimeShiftScores
) :-
    write('Available absences: '), 
    print_list(AvailableAbsences), nl,

    write('Rotated shift scores: '), 
    print_list(RotatedShiftScores), nl,

    write('Overtime shift scores: '),
    print_list(OvertimeShiftScores), nl.

print_list_element(Element) :-
    is_list(Element), !,
    write('[ '),
    length(Element, Length),
    ( count(I, 1, Length),
      foreach(El, Element),
      param(Length) do
        print_list_element(El),
        ( I \= Length, write(', ') ; true )
    ),
    write(' ]').
print_list_element(Element) :-
    write(Element).

print_list(List) :-
    write('['), nl,
    length(List, Length),
    ( count(I, 1, Length),
      foreach(El, List),
      param(Length) do
        write('    '),
        print_list_element(El),
        ( I \= Length, write(',') ; true ),
        nl
    ),
    write(']'), nl.

print_schedule(
    Day_Worker_Shift,
    Shift_Day_Worker,
    NightShift_Day_Worker
) :-
    workers(Workers),
    shifts(Shifts),
    night_shifts(NightShifts),
    days(Days),

    same_length(Days, Absences),
    ( foreach(Worker_Shift, Day_Worker_Shift), 
      foreach(Absence, Absences) do
        ( count(Worker, 1, _), 
          foreach(Shift, Worker_Shift), 
          fromto([], In, Out, Absence) do
            Shift = 0,
            last(In, Worker, Out);
            In = Out
        )
    ),
    transpose(Absences, RealAbsences),

    append([Shifts, NightShifts, Days, Workers, ['Absence']], Columns),

    maplist(atom_length, Columns, ColumnLengths),
    max_member(ColumnSize, ColumnLengths),

    format_to_codes('~~|~~a~~~d+ ', [ColumnSize], ColumnFormat),

    format(ColumnFormat, ['']),
    ( foreach(Day, Days), 
      param(ColumnFormat) do
        format(ColumnFormat, [Day])
    ), nl, nl,

    ( foreach(Day_Worker, Shift_Day_Worker), 
      foreach(Shift, Shifts), 
      param(Workers), 
      param(ColumnFormat) do
        format(ColumnFormat, [Shift]),
        ( foreach(Worker, Day_Worker), 
          param(Workers), 
          param(ColumnFormat) do
            print_worker(Worker, Workers, ColumnFormat)
        ),
        nl
    ), nl,
    
    ( foreach(Day_Worker, NightShift_Day_Worker), 
      foreach(NightShift, NightShifts), 
      param(Workers), 
      param(ColumnFormat) do
        format(ColumnFormat, [NightShift]),
        ( foreach(Worker, Day_Worker), 
          param(Workers), 
          param(ColumnFormat) do
            print_worker(Worker, Workers, ColumnFormat)
        ),
        nl
    ), nl,
    
    ( foreach(Absence, RealAbsences), 
      param(Workers), 
      param(ColumnFormat) do
        format(ColumnFormat, ['Absence']),
        ( foreach(Worker, Absence), 
          param(Workers), 
          param(ColumnFormat) do
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
    schedule(
        Day_Worker_Shift, 
        _, _, 
        Shift_Day_Worker, 
        _, _, _, 
        NightShift_Day_Worker, 
        AvailableAbsences, 
        RotatedShiftScores, 
        OvertimeShiftScores
    ), !,
    nl,
    fd_statistics, nl,
    statistics, nl,
    print_outputs(AvailableAbsences, RotatedShiftScores, OvertimeShiftScores),
    print_schedule(Day_Worker_Shift, Shift_Day_Worker, NightShift_Day_Worker).
main :- write('Could not find solution in the given time'), nl.
