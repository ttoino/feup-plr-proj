:- consult('schedule.pl').
:- consult('input.pl').

:- use_module(library(codesio)).

% schedule(
%     +Flags,
%     -Day_Worker_Shift,
%     -Day_Shift_Worker,
%     -Worker_Day_Shift,
%     -Shift_Day_Worker,
%     -Day_Worker_NightShift,
%     -Day_NightShift_Worker,
%     -Worker_Day_NightShift,
%     -NightShift_Day_Worker,
%     -AvailableAbsencesOut,
%     -RotatedShiftScoresOut,
%     -OvertimeShiftScoresOut,
%     -NightShiftScoresOut,
%     -Score
% )
%
% Schedules the workers to the shifts, maximizing the score.
% 
% The following parameters customize the search:
%
% `Flags` is a list of options for the labeling predicate.
%
% The following parameters are the outputed schedule:
%
% `Day_Worker_Shift` is a matrix where each line is a day and each column is a 
%   worker, containing the shift assigned to said worker in said day.
%   0 means no shift is assigned.
% `Day_Shift_Worker` is a matrix where each line is a day and each column is a
%   shift, containing the worker assigned to said shift in said day.
%   0 means no worker is assigned.
% `Worker_Day_Shift` is the transpose of `Day_Shift_Worker`.
% `Shift_Day_Worker` is the transpose of `Day_Worker_Shift`.
% `Day_Worker_NightShift` is the same as `Day_Worker_Shift`, but for night 
%   shifts.
% `Day_NightShift_Worker` is the same as `Day_Shift_Worker`, but for night
%   shifts.
% `Worker_Day_NightShift` is the transpose of `Day_NightShift_Worker`.
% `NightShift_Day_Worker` is the transpose of `Day_Worker_NightShift`.
% `AvailableAbsencesOut` is a list where each element is the number of
%   available absences for the corresponding worker.
% `RotatedShiftScoresOut` is a matrix where each line is a worker and each
%   column is a rotated shift, containing the number of times the worker has
%   been assigned to said shift.
% `OvertimeShiftScoresOut` is a list where each element is the number of times
%   the corresponding worker has been assigned to an overtime shift.
% `NightShiftScoresOut` is a list where each element is the number of night
%   shifts assigned to the corresponding worker.
% `Score` is the score of the schedule.
%
% The parameters to customize the scheduling are defined in the `input.pl` file.
schedule(
    Flags,
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
    NightShiftScoresOut,
    Score
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
    preferred_night_shifts(PreferredNightShifts),
    rotated_shifts(RotatedShifts),
    known_shifts(KnownShifts),
    known_night_shifts(KnownNightShifts),
    available_absences(AvailableAbsences),
    rotated_shift_scores(RotatedShiftScores),
    overtime_shift_scores(OvertimeShiftScores),
    night_shift_scores(NightShiftScores),

    schedule(
        Flags,
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
        NightShiftScoresOut,
        Score,
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
        PreferredNightShifts,
        RotatedShifts,
        KnownShifts,
        KnownNightShifts,
        AvailableAbsences,
        RotatedShiftScores,
        OvertimeShiftScores,
        NightShiftScores
    ).

% print_outputs(
%     +AvailableAbsences,
%     +RotatedShiftScores,
%     +OvertimeShiftScores,
%     +NightShiftScores
% )
%
% Prints the outputs of the scheduling, but not the schedule itself.
print_outputs(
    AvailableAbsences,
    RotatedShiftScores,
    OvertimeShiftScores,
    NightShiftScores
) :-
    write('Available absences: '), 
    print_list(AvailableAbsences), nl,

    write('Rotated shift scores: '), 
    print_list(RotatedShiftScores), nl,

    write('Overtime shift scores: '),
    print_list(OvertimeShiftScores), nl,
    
    write('Night shift scores: '),
    print_list(NightShiftScores), nl.

% print_list_element(+Element)
%
% Prints an element of a list in a human-readable format.
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

% print_list(+List)
%
% Prints a list in a human-readable format.
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

% print_schedule(
%     +Day_Worker_Shift,
%     +Shift_Day_Worker,
%     +NightShift_Day_Worker
% )
%
% Prints the schedule itself in a human-readable format.
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

% print_worker(+WorkerIndex, +Workers, +ColumnFormat)
%
% Prints a worker (a cell of the schedule) in a human-readable format.
print_worker(0, _, ColumnFormat) :-
    format(ColumnFormat, ['']).
print_worker(WorkerIndex, Workers, ColumnFormat) :-
    nth1(WorkerIndex, Workers, Worker),
    format(ColumnFormat, [Worker]).

% benchmark
% benchmark(+Flags)
%
% Runs the scheduling algorithm with the given flags (or the ones defined in the
% `input.pl` file) and prints the time it took and the score it achieved for
% each solution found, in CSV format.
benchmark :-
    flags(Flags),
    benchmark(Flags).
benchmark(Flags) :-
    write('time,score'), nl,
    append(Flags, [all], AllFlags),
    schedule(AllFlags, _, _, _, _, _, _, _, _, _, _, _, _, S),
    statistics(runtime, [Time, _]),
    write(Time), write(','), write(S), nl, flush_output,
    false.

% main
% 
% Runs the scheduling algorithm with the flags and timeout defined in the
% `input.pl` file and prints the outputs and the schedule itself.
main :-
    flags(Flags),
    timeout(Timeout),
    append(Flags, [time_out(Timeout, Flag)], AllFlags),
    schedule(
        AllFlags,
        Day_Worker_Shift, 
        _, _, 
        Shift_Day_Worker, 
        _, _, _, 
        NightShift_Day_Worker, 
        AvailableAbsences, 
        RotatedShiftScores, 
        OvertimeShiftScores,
        NightShiftScores,
        Score
    ), !,
    nl,
    fd_statistics, nl,
    statistics, nl,
    print_outputs(AvailableAbsences, RotatedShiftScores, OvertimeShiftScores, NightShiftScores),
    print_schedule(Day_Worker_Shift, Shift_Day_Worker, NightShift_Day_Worker),
    write('Score: '), write(Score), nl.
main :- write('Could not find solution in the given time'), nl.
