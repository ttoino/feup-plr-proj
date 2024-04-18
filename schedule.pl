:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

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
) :-
    length(Workers, NumberOfWorkers),
    length(Shifts, NumberOfShifts),
    length(NightShifts, NumberOfNightShifts),
    length(Days, NumberOfDays),

    setup_domain_and_channeling(Assignments, AssignmentsInverse, NumberOfDays, NumberOfWorkers, NumberOfShifts),
    setup_domain_and_channeling(NightShiftAssignments, NightShiftAssignmentsInverse, NumberOfDays, NumberOfWorkers, NumberOfNightShifts),

    setup_incompatible_shifts(Assignments, IncompatibleShifts, NumberOfWorkers),

    % All variables to be flattened
    append(Assignments, NightShiftAssignments, AllAssignments),
    append(AllAssignments, Variables),

    labeling([ffc, bisect, down, time_out(10000, Flag)], Variables).

% Assignments[Day][Worker] = Shift
% AssignmentsInverse[Day][Shift] = Worker
setup_domain_and_channeling(Assignments, AssignmentsInverse, NumberOfDays, NumberOfWorkers, NumberOfShifts) :-
    length(Assignments, NumberOfDays),
    ( foreach(Assignment, Assignments), param(NumberOfWorkers), param(NumberOfShifts) do 
        length(Assignment, NumberOfWorkers),
        domain(Assignment, 0, NumberOfShifts),
        all_distinct_except_0(Assignment)
    ),
    length(AssignmentsInverse, NumberOfDays),
    ( foreach(AssignmentInverse, AssignmentsInverse), param(NumberOfShifts), param(NumberOfWorkers) do 
        length(AssignmentInverse, NumberOfShifts),
        domain(AssignmentInverse, 0, NumberOfWorkers),
        all_distinct_except_0(AssignmentInverse)
    ),
    % Channel
    ( foreach(Assignment, Assignments), foreach(AssignmentInverse, AssignmentsInverse), param(NumberOfWorkers), param(NumberOfShifts) do
        ( for(Worker, 1, NumberOfWorkers), param(Assignment), param(AssignmentInverse) do
            nth1(Worker, Assignment, Shift),
            element(Shift, AssignmentInverse, Worker) #<=> (Shift #\= 0)
        ),
        ( for(Shift, 1, NumberOfShifts), param(Assignment), param(AssignmentInverse) do
            nth1(Shift, AssignmentInverse, Worker),
            element(Worker, Assignment, Shift) #<=> (Worker #\= 0)
        )
    ).

setup_incompatible_shifts(Assignments, IncompatibleShifts, NumberOfWorkers) :-
    ( foreach(Assignment, Assignments), param(IncompatibleShifts) do
        ( foreach(Shift, Assignment), foreach(IncompatibleShiftsForWorker, IncompatibleShifts) do 
            ( foreach(IncompatibleShift, IncompatibleShiftsForWorker), param(Shift) do
                Shift #\= IncompatibleShift
            )
        )
    ).
