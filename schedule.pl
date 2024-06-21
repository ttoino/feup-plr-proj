:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

:- consult('util.pl').

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

    % MISSING IN REPORT
    setup_rotated_shifts(Shift_Day_Worker, RotatedShifts),

    setup_night_shifts(Day_Worker_Shift, Day_Worker_NightShift),

    %setup_late_night_shifts(Day_Shift_Worker, Day_Worker_NightShift, LateShifts),
    setup_late_night_shifts(Day_Worker_Shift, Day_Worker_NightShift, LateShifts),

    % MISSING IN REPORT
    ( foreach(NightShift_Worker, Day_NightShift_Worker) do
        count(0, NightShift_Worker, 0)
    ),

    calculate_preference_scores(Worker_Day_Shift, PreferredShifts, PreferenceScores),
    calculate_preference_scores(Worker_Day_NightShift, PreferredNightShifts, NightPreferenceScores),

    calculate_priority_preference_scores(Worker_Day_Shift, Worker_Day_NightShift, PreferredShifts, PriorityPreferenceScores),

    calculate_absences(Worker_Day_Shift, AvailableAbsences, AvailableAbsencesOut),
    score_distances(AvailableAbsencesOut, AvailableAbsencesScore),

    calculate_rotated_shift_scores(Worker_Day_Shift, RotatedShifts, RotatedShiftScores, RotatedShiftScoresOut),
    transpose(RotatedShiftScoresOut, RotatedShiftScoresOutTransposed),
    maplist(score_distances, RotatedShiftScoresOutTransposed, RotatedShiftScoresDistances),
    sum(RotatedShiftScoresDistances, RotatedShiftScore),

    calculate_overtime_shift_scores(Worker_Day_Shift, OvertimeShifts, OvertimeShiftScores, OvertimeShiftScoresOut),
    score_distances(OvertimeShiftScoresOut, OvertimeShiftScore),

    calculate_night_shift_scores(Worker_Day_NightShift, NightShiftScores, NightShiftScoresOut),
    score_distances(NightShiftScores, NightShiftScore),

    sums([
        PreferenceScores,
        NightPreferenceScores,
        PriorityPreferenceScores
    ], MaxiScore),
    sum([
        AvailableAbsencesScore,
        RotatedShiftScore,
        OvertimeShiftScore,
        NightShiftScore
    ], MiniScore),
    Score #= MaxiScore - MiniScore,

    % All variables to be flattened
    append([
        Day_Worker_Shift, 
        Day_Worker_NightShift,
        Day_Shift_Worker,
        Day_NightShift_Worker
    ], AllAssignments),
    append(AllAssignments, Variables),
    append([Score], Variables, AllVariables),

    write('Searching...'), nl,
    append(Flags, [maximize(Score)], AllFlags),
    labeling(AllFlags, AllVariables).

% Sets the domain of the decision variables, and initializes alternative
% matrices using channeling constraints.
setup_domain_and_channeling(Day_Worker_Shift, Day_Shift_Worker, NumberOfDays, NumberOfWorkers, NumberOfShifts) :-
    length(Day_Worker_Shift, NumberOfDays),
    length(Day_Shift_Worker, NumberOfDays),

    ( foreach(Worker_Shift, Day_Worker_Shift), 
      foreach(Shift_Worker, Day_Shift_Worker), 
      param(NumberOfWorkers), 
      param(NumberOfShifts) do
        length(Worker_Shift, NumberOfWorkers),
        domain(Worker_Shift, 0, NumberOfShifts),
        all_distinct_except_0(Worker_Shift),

        length(Shift_Worker, NumberOfShifts),
        domain(Shift_Worker, 0, NumberOfWorkers),
        all_distinct_except_0(Shift_Worker),

        ( for(Worker, 1, NumberOfWorkers), 
          param(Worker_Shift), 
          param(Shift_Worker) do
            nth1(Worker, Worker_Shift, Shift),
            element(Shift, Shift_Worker, Worker) #<=> (Shift #\= 0)
        ),

        ( for(Shift, 1, NumberOfShifts), 
          param(Worker_Shift), 
          param(Shift_Worker) do
            nth1(Shift, Shift_Worker, Worker),
            element(Worker, Worker_Shift, Shift) #<=> (Worker #\= 0)
        )
    ).

% Constraints the maximum number of overtime shifts for each worker
setup_overtime_shifts(Day_Worker_Shift, OvertimeShifts, DailyOvertimeShifts) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), 
      param(OvertimeShifts), 
      param(DailyOvertimeShifts) do
        counts(OvertimeShifts, Worker_Shift, DailyOvertimeShifts)
    ).

% Prevents alternative shifts from being chosen in the same day
setup_alternative_shifts(Day_Shift_Worker, AlternativeShifts) :-
    length(AlternativeShifts, NumberOfAlternativeShifts),
    ( foreach(Shift_Worker, Day_Shift_Worker), 
      param(AlternativeShifts), 
      param(NumberOfAlternativeShifts) do
        count(0, Shift_Worker, NumberOfAlternativeShifts),
        ( foreach(Shift1-Shift2, AlternativeShifts), 
          param(Shift_Worker) do
            nth1(Shift1, Shift_Worker, Worker1),
            nth1(Shift2, Shift_Worker, Worker2),
            Worker1 #= 0 #<=> Worker2 #\= 0
        )
    ).

% Prevents incompatible shifts from being chosen by each worker
setup_incompatible_shifts(Day_Worker_Shift, IncompatibleShifts) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), 
      param(IncompatibleShifts) do
        ( foreach(Shift, Worker_Shift), 
          foreach(IncompatibleShiftsForWorker, IncompatibleShifts) do 
            ( foreach(IncompatibleShift, IncompatibleShiftsForWorker), 
              param(Shift) do
                Shift #\= IncompatibleShift
            )
        )
    ).

% Constrains the maximum number of late shifts for each worker
setup_late_shifts(Worker_Day_Shift, LateShifts, WeeklyLateShifts) :-
    ( foreach(Day_Shift, Worker_Day_Shift), 
      param(LateShifts), 
      param(WeeklyLateShifts) do
        counts(LateShifts, Day_Shift, WeeklyLateShiftsForWorker),
        WeeklyLateShiftsForWorker #=< WeeklyLateShifts
    ).

% Rotates shifts between all workers
setup_rotated_shifts(Shift_Day_Worker, RotatedShifts) :-
    ( foreach(Shift, RotatedShifts), 
      param(Shift_Day_Worker) do
        nth1(Shift, Shift_Day_Worker, Day_Worker),
        all_distinct_except_0(Day_Worker)
    ).

% Forces workers that have a night shift assigned to also have a normal shift
% assigned in each day
setup_night_shifts(Day_Worker_Shift, Day_Worker_NightShift) :-
    ( foreach(Worker_Shift, Day_Worker_Shift), 
      foreach(Worker_NightShift, Day_Worker_NightShift) do
        ( foreach(Shift, Worker_Shift), 
          foreach(NightShift, Worker_NightShift) do
            (NightShift #\= 0) #=> (Shift #\= 0)
        )
    ).

% Forces workers that have a late shift assigned to not have a night shift
% assigned
%setup_late_night_shifts(Day_Shift_Worker, Day_Worker_NightShift, LateShifts) :-
setup_late_night_shifts(Day_Worker_Shift, Day_Worker_NightShift, LateShifts) :-
    list_to_fdset(LateShifts, LateShiftsSet),
    ( foreach(Worker_Shift, Day_Worker_Shift), 
      foreach(Worker_NightShift, Day_Worker_NightShift), 
      param(LateShiftsSet) do
        ( foreach(Shift, Worker_Shift), 
          foreach(NightShift, Worker_NightShift), 
          param(LateShiftsSet) do
            (NightShift #\= 0) #=> (#\ (Shift in_set LateShiftsSet))
        )
    ).
    % ( foreach(Shift_Worker, Day_Shift_Worker), 
    %   foreach(Worker_NightShift, Day_Worker_NightShift), 
    %   param(LateShifts) do
    %     ( foreach(LateShift, LateShifts), 
    %       param(Shift_Worker), 
    %       param(Worker_NightShift) do
    %         nth1(LateShift, Shift_Worker, Worker),
    %         (Worker #\= 0) #=> (element(Worker, Worker_NightShift, 0))
    %     )
    % ).

calculate_preference_scores(Worker_Day_Shift, PreferredShifts, PreferenceScores) :-
    maplist(counts, PreferredShifts, Worker_Day_Shift, PreferenceScores).

calculate_priority_preference_scores(Worker_Day_Shift, Worker_Day_NightShift, PreferredShifts, PriorityPreferenceScores) :-
    ( foreach(Day_Shift, Worker_Day_Shift), 
      foreach(Day_NightShift, Worker_Day_NightShift), 
      foreach(PreferredShift, PreferredShifts), 
      foreach(PriorityPreferenceScore, PriorityPreferenceScores) do
        ( foreach(Shift, Day_Shift), 
          foreach(NightShift, Day_NightShift), 
          param(PreferredShift),
          fromto(0, In, Out, PriorityPreferenceScore) do
            Out #= In + ((NightShift #\= 0) #/\ element(_, PreferredShift, Shift))
        )
    ).

calculate_absences(Worker_Day_Shift, AvailableAbsences, AvailableAbsencesOut) :-
    ( foreach(Day_Shift, Worker_Day_Shift), 
      foreach(AvailableAbsence, AvailableAbsences), 
      foreach(AvailableAbsenceOut, AvailableAbsencesOut) do
        count(0, Day_Shift, UsedAbsences),
        AvailableAbsenceOut #>= 0,
        AvailableAbsenceOut #= AvailableAbsence - UsedAbsences
    ).

calculate_rotated_shift_scores(Worker_Day_Shift, RotatedShifts, RotatedShiftScores, RotatedShiftScoresOut) :-
    ( foreach(Day_Shift, Worker_Day_Shift), 
      foreach(RotatedShiftScore, RotatedShiftScores), 
      foreach(RotatedShiftScoreOut, RotatedShiftScoresOut), 
      param(RotatedShifts) do
        ( foreach(RotatedShift, RotatedShifts), 
          foreach(RotatedShiftScoreEl, RotatedShiftScore), 
          foreach(RotatedShiftScoreOutEl, RotatedShiftScoreOut), 
          param(Day_Shift) do
            count(RotatedShift, Day_Shift, RotatedShiftScoreTemp),
            RotatedShiftScoreOutEl #= RotatedShiftScoreTemp + RotatedShiftScoreEl
        )
    ).

calculate_overtime_shift_scores(Worker_Day_Shift, OvertimeShifts, OvertimeShiftScores, OvertimeShiftScoresOut) :-
    ( foreach(Day_Shift, Worker_Day_Shift), 
      foreach(OvertimeShiftScore, OvertimeShiftScores), 
      foreach(OvertimeShiftScoreOut, OvertimeShiftScoresOut), 
      param(OvertimeShifts) do
        counts(OvertimeShifts, Day_Shift, OvertimeShiftScoreTemp),
        OvertimeShiftScoreOut #= OvertimeShiftScoreTemp + OvertimeShiftScore
    ).

calculate_night_shift_scores(Worker_Day_NightShift, NightShiftScores, NightShiftScoresOut) :-
    ( foreach(Day_NightShift, Worker_Day_NightShift), 
      foreach(NightShiftScore, NightShiftScores),
      foreach(NightShiftScoreOut, NightShiftScoresOut) do
        ( foreach(NightShift, Day_NightShift), 
          fromto(NightShiftScore, In, Out, NightShiftScoreOut) do
            Out #= In + (NightShift #\= 0)
        )
    ).

score_distances(Scores, DistancesSum) :-
    map_product(dist, Scores, Scores, Distances),
    sum(Distances, DistancesSum).
