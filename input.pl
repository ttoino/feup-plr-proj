% The options to use in the labeling/2 predicate.
flags([middle]).

% The maximum number of seconds the solver is allowed to run.
% Must be a positive integer.
timeout(900000).

% Which workers are available.
% Must be a list of worker names.
workers([
    'Alice',
    'Bob',
    'Carol',
    'David',
    'Erin',
    'Frank',
    'Grace',
    'Heidi',
    'Ivan',
    'Judy',
    'Michael',
    'Olivia',
    'Peggy'
]).

% Which shifts are available.
% Must be a list of shift names.
shifts([
    '08:15 17:45',      % 1
    '08:45 18:45',      % 2
    '09:00 19:00',      % 3
    '09:00 20:00',      % 4
    '09:00 19:30',      % 5
    '09:15 19:00',      % 6
    '09:15 20:00',      % 7
    '09:30 19:00',      % 8
    '09:30 20:00',      % 9
    '10:00 20:00',      % 10
    '08:45 18:45 Luso', % 11
    '09:30 19:30 Luso'  % 12
]).

% Which night shifts are available.
% Must be a list of night shift names.
night_shifts([
    'Night'
]).

% Which days are available.
% Must be a list of day names.
days([
    'Monday',
    'Tuesday',
    'Wednesday',
    'Thursday',
    'Friday'
]).

% Which shifts grant overtime.
% Must be a list of shift indices (1-based).
overtime_shifts([ 4, 5, 7, 9, 10 ]).

% How many overtime shifts are allowed per day, for all workers.
% Must be a positive integer.
daily_overtime_shifts( 2 ).

% Which shifts are considered late shifts (end after 19:00).
% Must be a list of shift indices (1-based).
late_shifts([ 4, 5, 7, 9, 10, 12 ]).

% How many late shifts are allowed per week, for each worker.
% Must be a positive integer
weekly_late_shifts( 2 ).

% Which shifts cannot have a worker assigned on the same day.
% Must be a list of shift index pairs (1-based).
alternative_shifts([ 3-4, 6-7, 8-9 ]).

% Which shifts cannot be assigned to specific workers.
% Must be a list of lists of shift indices (1-based), where each list
% corresponds to the worker at the same index in the workers list.
incompatible_shifts([
    [ 4 ],                             % Alice
    [ 4 ],                             % Bob
    [ 4, 11, 12 ],                     % Carol
    [ 4, 11, 12 ],                     % David
    [ 11, 12 ],                        % Erin
    [ 4 ],                             % Frank
    [ 4 ],                             % Grace
    [ 4, 11, 12 ],                     % Heidi
    [ 4, 11, 12 ],                     % Ivan
    [ 11, 12 ],                        % Judy
    [ 4 ],                             % Michael
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], % Olivia
    [ 4 ]                              % Peggy
]).

% Which shifts each worker prefers.
% Must be a list of lists of shift indices (1-based), where each list
% corresponds to the worker at the same index in the workers list.
preferred_shifts([
    [ ],               % Alice
    [ ],               % Bob
    [ ],               % Carol
    [ 8, 9, 10 ],      % David
    [ 3, 4 ],          % Erin
    [ ],               % Frank
    [ ],               % Grace
    [ ],               % Heidi
    [ ],               % Ivan
    [ 2, 3, 4, 10 ],   % Judy
    [ ],               % Michael
    [ ],               % Olivia
    [ 6, 7, 8, 9, 10 ] % Peggy
]).

% Which night shifts each worker prefers.
% Must be a list of lists of night shift indices (1-based), where each list
% corresponds to the worker at the same index in the workers list.
preferred_night_shifts([
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ],
    [ ]
]).

% Which shifts must be assigned to a different worker each day.
% Must be a list of shift indices (1-based).
rotated_shifts([ 10 ]).

% Shifts that have been assigned ahead of time.
% Must be a matrix where each line is a worker, each column is a day, and
% each cell is the index of the shift assigned to that worker on that day.
% _ means no shift has been assigned yet, and 0 means the worker is absent.
known_shifts([
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _]
]).

% Night shifts that have been assigned ahead of time.
% Must be a matrix where each line is a night shift, each column is a day, and
% each cell is the index of the worker assigned to that night shift on that day.
% _ means no night shift has been assigned yet.
known_night_shifts([
    [_, _, _, _, _]
]).

% How many absences each worker has available to enjoy.
% Must be a list of integers, where each integer corresponds to the worker
% at the same index in the workers list.
available_absences([
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10,
    10
]).

% How many times each worker has been assigned to each rotated shift.
% Must be a matrix where each line is a worker, each column is a rotated shift,
% and each cell is the number of times that worker has been assigned to that
% rotated shift.
rotated_shift_scores([
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ],
    [ 0 ]
]).

% How many times each worker has been assigned to an overtime shift.
% Must be a list of integers, where each integer corresponds to the worker
% at the same index in the workers list.
overtime_shift_scores([
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
]).

% How many times each worker has been assigned to a night shift.
% Must be a list of integers, where each integer corresponds to the worker
% at the same index in the workers list.
night_shift_scores([
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
]).
