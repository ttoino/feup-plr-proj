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
overtime_shifts([ 4, 5, 7, 9 ]).

% How many daily overtime shifts are allowed
daily_overtime_shifts( 2 ).

% Which shifts are considered late shifts
late_shifts([ 4, 5, 7, 9, 10, 12 ]).

% How many weekly late shifts are allowed
weekly_late_shifts( 2 ).

% Which shifts are incompatible with each other
alternative_shifts([ 3-4, 6-7, 8-9 ]).

% Which shifts are incompatible with each worker
incompatible_shifts([
    [ ],                               % Alice
    [ ],                               % Bob
    [ 11, 12 ],                        % Carol
    [ 11, 12 ],                        % David
    [ 11, 12 ],                        % Erin
    [ ],                               % Frank
    [ ],                               % Grace
    [ 11, 12 ],                        % Heidi
    [ 11, 12 ],                        % Ivan
    [ 11, 12 ],                        % Judy
    [ ],                               % Michael
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], % Olivia
    [ ]                                % Peggy
]).

% Which shifts are preferred by each worker
preferred_shifts([
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
