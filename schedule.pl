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

    % All variables to be flattened
    append(Assignments, NightShiftAssignments, AllAssignments),
    append(AllAssignments, Variables),

    count(0, Variables, #=, ZeroCount),

    labeling([ffc, bisect, down, minimize(ZeroCount), time_out(60000, Flag)], Variables).

setup_domain_and_channeling(Assignments, AssignmentsInverse, NumberOfDays, NumberOfWorkers, NumberOfShifts) :-
    % Assignments[Day][Worker] = Shift
    % AssignmentsInverse[Day][Shift] = Worker
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
    ( for(Day, 1, NumberOfDays), param(Assignments), param(AssignmentsInverse), param(NumberOfWorkers), param(NumberOfShifts) do
        nth1(Day, Assignments, Assignment),
        nth1(Day, AssignmentsInverse, AssignmentInverse),

        ( for(Worker, 1, NumberOfWorkers), param(Assignment), param(AssignmentInverse) do
            nth1(Worker, Assignment, Shift),
            element(Shift, AssignmentInverse, Worker) #<=> (Shift #\= 0)
        ),
        ( for(Shift, 1, NumberOfShifts), param(Assignment), param(AssignmentInverse) do
            nth1(Shift, AssignmentInverse, Worker),
            element(Worker, Assignment, Shift) #<=> (Worker #\= 0)
        )
    ).