set P := {"fede", "javi"};
set R := {"ay2", "ay1", "jtp", "prof"};
set D := { 1 .. 10 };
set C := { 1 .. 3 };
set S := { 1 .. 4 };

param L := 10;
set L_ := { 1 .. L };

#param d[C * L_  * D] := 1;
param a[P * D] := 1;
param r[P * R] := 1;
param q[P * C] := 1;
param m[C * L_ * R] := 1;
param pp[C * S] := 1;

#ds[i, j, k] = 1 means that the ith weekly plan
#                has the jth class on the kth
#                day of the year.
param ds[S * D * L_] := 1;

#cp[i, j] = 1 means that the ith course
#             has chosen the jth weekly plan
var cp[C * S] binary;

var x[C * L_ * P * R] binary;

#class_date[c, l, d] = 1 means the cth course
#                        will have its lth class
#                        on the dth day
var class_date[C * L_ * D] binary;

#busy[p, c, d] = 1 means the pth professor
#                  will teach the cth
#                  course on the dth day
var busy[P * C * D] binary;

var busy_l[P * C * L_] binary;

maximize quality:
  sum<c, l, p, k> in C * L_ * P * R:
    x[c, l, p, k] * q[p, c];

subto class_date_coherency:
  forall <d, c, j, l> in D * C * S * L_ with l <= n[c]
    class_date[c, l, d] >= cp[c, j] * ds[j, l, d];

subto busy_l_coherency:
  forall <c, l, p> in C * L_ * P:
    sum<k> in K: x[c, l, p, k] = busy_l[p, c, l];

subto busy_coherency:
  forall <p, c, l, d> in P * C * L_ * D:
    busy[p, c, d] >= busy_l[p, c, l] + class_date[c, l, d] - 1;
  
subto professor_availability:
  forall <p, d> in P * D:
    sum<c> in C: busy[p, c, d] <= a[p, d];

subto one_class_per_day_max:
  forall <p, d> in P * D:
    sum<c> in C: busy[p, c, d] <= 1;

subto valid_role:
    forall <c, l, p, k> in C * {1 .. L} * P * R:
      x[c, l, p, k] <= r[p, k];

subto satisfied_roles:
    forall <c, l, k> in C * L_ * R:
      sum<p> in P:
        x[c, l, p, k] >= m[c, l, k];

subto no_duplicate_profs:
    forall <c, l, p> in C * {1 .. L} * P:
      sum<k> in R: x[c, l, p, k] <= 1;

subto legal_weekly_pattern:
  forall <c, s> in C * S:
    cp[c, s] <= pp[c, s];
