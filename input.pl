flags([ffc, middle]).

timeout(90000).

% Which workers are available
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

% Which shifts are available
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

% Which night shifts are available
night_shifts([
    'Night'
]).

% Which days are available
days([
    'Monday',
    'Tuesday',
    'Wednesday',
    'Thursday',
    'Friday'
]).

% Which shifts grant overtime
% Values must be shifts
overtime_shifts([ 4, 5, 7, 9, 10 ]).

% How many daily overtime shifts are allowed
daily_overtime_shifts( 2 ).

% Which shifts are considered late shifts
% Values must be shifts
late_shifts([ 4, 5, 7, 9, 10, 12 ]).

% How many weekly late shifts are allowed
weekly_late_shifts( 2 ).

% Which shifts are incompatible with each other
% Values must be pairs of shifts
alternative_shifts([ 3-4, 6-7, 8-9 ]).

% Which shifts are incompatible with each worker
% Each line is a worker, values must be shifts
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

% Which shifts are preferred by each worker
% Each line is a worker, values must be shifts
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

% Which night shifts are preferred by each worker
% Each line is a worker, values must be night shifts
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

% Which shifts must be rotated by each worker
% Values must be shifts
rotated_shifts([ 10 ]).

% Shifts that are known ahead of time
% Each line is a worker, each column is a day
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

% Night shifts that are known ahead of time
% Each line is a shift, each column is a day
known_night_shifts([
    [_, _, _, _, _]
]).

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
