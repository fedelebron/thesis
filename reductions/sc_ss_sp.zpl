set P := {"fede", "javi"};
set R := {"ay2", "ay1", "jtp", "prof"};
set D := { 1 .. 10 };
set C := { 1 .. 3 };

param d[C * D] :=   | 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |
                  |1| 0, 1, 0, 0, 0, 0, 0, 0, 0, 0  |
                  |2| 0, 0, 0, 0, 1, 0, 0, 0, 0, 0  |
                  |3| 0, 0, 0, 0, 0, 0, 0, 0, 1, 0  |;
param a[P * D] :=        | 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |
                  |"fede"| 1, 1, 1, 0, 1, 0, 1, 0, 1, 0  |
                  |"javi"| 0, 1, 0, 0, 1, 1, 1, 0, 1, 1  |;
param r[P * R] :=        | "ay2", "ay1", "jtp", "prof" |
                  |"fede"| 1,     1,     0,     0      |
                  |"javi"| 0,     1,     1,     1      |;
param q[P] := <"javi"> 10, <"fede"> 6;
param n[C * R] :=   | "ay2", "ay1", "jtp", "prof" |
                  |1| 1,     0,     1,     0      |
                  |2| 0,     1,     0,     1      |
                  |3| 1,     0,     0,     1      |;

var x[P * C * R] binary;

maximize quality: sum<i, j, k> in P cross C cross R: x[i, j, k] * q[i];

subto day_availability: forall <i, j, ro, k> in P cross C cross R cross D:
                                x[i, j, ro] + d[j, k] <= 1 + a[i, k];

subto available_roles: forall <i, j, k> in P cross C cross R:
                                x[i, j, k] <= r[i, k];

subto filled_roles: forall <j, k> in C cross R:
                        sum<i> in P: x[i, j, k] >= n[j, k];

subto no_double_roles: forall <i, j> in P * C:
                           sum<k> in R: x[i, j, k] <= 1;
