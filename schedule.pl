:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

:- consult('util.pl').

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
    RotatedShifts,
    KnownShifts,
    KnownNightShifts
) :-
    length(Workers, NumberOfWorkers),
    length(Shifts, NumberOfShifts),
    length(NightShifts, NumberOfNightShifts),
    length(Days, NumberOfDays),

    setup_domain_and_channeling(Day_Worker_Shift, Day_Shift_Worker, NumberOfDays, NumberOfWorkers, NumberOfShifts),
    setup_domain_and_channeling(Day_Worker_NightShift, Day_NightShift_Worker, NumberOfDays, NumberOfWorkers, NumberOfNightShifts),

    transpose(Day_Worker_Shift, Worker_Day_Shift),
    transpose(Day_Shift_Worker, Shift_Day_Worker),
    transpose(Day_Worker_NightShift, Worker_Day_NightShift),
    transpose(Day_NightShift_Worker, NightShift_Day_Worker),

    % Setup known shifts
    maplist(eq_list, Worker_Day_Shift, KnownShifts),

    % Setup known night shifts
    maplist(eq_list, NightShift_Day_Worker, KnownNightShifts),

    setup_overtime_shifts(Day_Worker_Shift, OvertimeShifts, DailyOvertimeShifts),

    setup_incompatible_shifts(Day_Worker_Shift, IncompatibleShifts),

    setup_late_shifts(Worker_Day_Shift, LateShifts, WeeklyLateShifts),

    setup_alternative_shifts(Day_Shift_Worker, AlternativeShifts),

    setup_rotated_shifts(Shift_Day_Worker, RotatedShifts),

    setup_night_shifts(Day_Worker_Shift, Day_Worker_NightShift),

    setup_late_night_shifts(Day_Shift_Worker, Day_Worker_NightShift, LateShifts),

    ( foreach(NightShift_Worker, Day_NightShift_Worker) do
        count(0, NightShift_Worker, #=, 0)
    ),

    calculate_preference_scores(Worker_Day_Shift, PreferredShifts, PreferenceScores),
    sum(PreferenceScores, #=, Score),

    % All variables to be flattened
    append([
        Day_Worker_Shift, 
        Day_Worker_NightShift,
        Day_Shift_Worker,
        Day_NightShift_Worker
    ], AllAssignments),
    append(AllAssignments, Variables),

    write('Searching...'), nl,
    labeling([
        time_out(10000, Flag),
        maximize(Score),
        % ffc,
        % bisect,
        middle
    ], Variables),
    Flag \= time_out. 

% Sets the domain of the decision variables, and initializes alternative
% matrices using channeling constraints.
setup_domain_and_channeling(Day_Worker_Shift, Day_Shift_Worker, NumberOfDays, NumberOfWorkers, NumberOfShifts) :-
    length(Day_Worker_Shift, NumberOfDays),
    length(Day_Shift_Worker, NumberOfDays),

    ( foreach(Worker_Shift, Day_Worker_Shift), foreach(Shift_Worker, Day_Shift_Worker), param(NumberOfWorkers), param(NumberOfShifts) do
        length(Worker_Shift, NumberOfWorkers),
        domain(Worker_Shift, 0, NumberOfShifts),
        all_distinct_except_0(Worker_Shift),

        length(Shift_Worker, NumberOfShifts),
        domain(Shift_Worker, 0, NumberOfWorkers),
        all_distinct_except_0(Shift_Worker),

        ( for(Worker, 1, NumberOfWorkers), param(Worker_Shift), param(Shift_Worker) do
            nth1(Worker, Worker_Shift, Shift),
            element(Shift, Shift_Worker, Worker) #<=> (Shift #\= 0)
        ),

        ( for(Shift, 1, NumberOfShifts), param(Worker_Shift), param(Shift_Worker) do
            nth1(Shift, Shift_Worker, Worker),
            element(Worker, Worker_Shift, Shift) #<=> (Worker #\= 0)
        )
    ).

% Constraints the maximum number of overtime shifts for each worker
setup_overtime_shifts(Day_Worker_Shift, OvertimeShifts, DailyOvertimeShifts) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), param(OvertimeShifts), param(DailyOvertimeShifts) do
        counts(OvertimeShifts, Worker_Shift, DailyOvertimeShifts)
    ).

% Prevents alternative shifts from being chosen in the same day
setup_alternative_shifts(Day_Shift_Worker, AlternativeShifts) :-
    length(AlternativeShifts, NumberOfAlternativeShifts),
    ( foreach(Shift_Worker, Day_Shift_Worker), param(AlternativeShifts), param(NumberOfAlternativeShifts) do
        count(0, Shift_Worker, #=, NumberOfAlternativeShifts),
        ( foreach(Shift1-Shift2, AlternativeShifts), param(Shift_Worker) do
            nth1(Shift1, Shift_Worker, Worker1),
            nth1(Shift2, Shift_Worker, Worker2),
            Worker1 #= 0 #\/ Worker2 #= 0,
            Worker1 #\= Worker2
        )
    ).

% Prevents incompatible shifts from being chosen by each worker
setup_incompatible_shifts(Day_Worker_Shift, IncompatibleShifts) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), param(IncompatibleShifts) do
        ( foreach(Shift, Worker_Shift), foreach(IncompatibleShiftsForWorker, IncompatibleShifts) do 
            ( foreach(IncompatibleShift, IncompatibleShiftsForWorker), param(Shift) do
                Shift #\= IncompatibleShift
            )
        )
    ).

% Constrains the maximum number of late shifts for each worker
setup_late_shifts(Worker_Day_Shift, LateShifts, WeeklyLateShifts) :-
    ( foreach(Day_Shift, Worker_Day_Shift), param(LateShifts), param(WeeklyLateShifts) do
        counts(LateShifts, Day_Shift, WeeklyLateShiftsForWorker),
        WeeklyLateShiftsForWorker #=< WeeklyLateShifts
    ).

% Rotates shifts between all workers
setup_rotated_shifts(Shift_Day_Worker, RotatedShifts) :-
    ( foreach(Shift, RotatedShifts), param(Shift_Day_Worker) do
        nth1(Shift, Shift_Day_Worker, Day_Worker),
        all_distinct_except_0(Day_Worker)
    ).

% Forces workers that have a night shift assigned to also have a normal shift
% assigned in each day
setup_night_shifts(Day_Worker_Shift, Day_Worker_NightShift) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), foreach(Worker_NightShift, Day_Worker_NightShift) do
        ( foreach(Shift, Worker_Shift), foreach(NightShift, Worker_NightShift) do
            (NightShift #\= 0) #=> (Shift #\= 0)
        )
    ).

% Forces workers that have a late shift assigned to not have a night shift
% assigned
setup_late_night_shifts(Day_Shift_Worker, Day_Worker_NightShift, LateShifts) :-
    ( foreach(Shift_Worker, Day_Shift_Worker), foreach(Worker_NightShift, Day_Worker_NightShift), param(LateShifts) do
        ( foreach(LateShift, LateShifts), param(Shift_Worker), param(Worker_NightShift) do
            nth1(LateShift, Shift_Worker, Worker),
            (Worker #\= 0) #=> (element(Worker, Worker_NightShift, 0))
        )
    ).

calculate_preference_scores(Worker_Day_Shift, PreferredShifts, PreferenceScores) :-
    ( foreach(Day_Shift, Worker_Day_Shift), foreach(Shifts, PreferredShifts), foreach(Score, PreferenceScores) do
        ( foreach(Shift, Shifts), fromto(0, In, Out, Score), param(Day_Shift) do
            count(Shift, Day_Shift, #=, Count),
            Out #= In + Count
        )
    ).
