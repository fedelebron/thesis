set P := {"fede", "javi"};
set R := {"ay2", "ay1", "jtp", "prof"};
set D := { 1 .. 10 };
set C := { 1 .. 3 };

param L := 10;
set L_ := { 1 .. L };

param d[C * L_  * D] := 1;
param a[P * D] := 1;
param r[P * R] := 1;
param q[C * P] := 1;
param m[C * L_ * R] := 1;

var x[C * L_ * P * R] binary;

maximize quality:
  sum<c, l, p, k> in C * L_ * P * R:
    x[c, l, p, k] * q[c, p];

subto professor_availability:
    forall <c, l, p, w> in C * {1 .. L} * P * D:
      sum<k> in R: x[c, l, p, k] <= a[p, w];
subto one_class_per_day_max:
    forall <p, w> in P * D:
      sum<i, j, k> in C * L_ * R:
        d[i, j, w] * x[i, j, p, k] <= 1;
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
