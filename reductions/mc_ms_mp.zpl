set P := {"fede", "javi"};
set R := {"ay2", "ay1", "jtp", "prof"};
set D := { 1 .. 10 };
set C := { 1 .. 3 };
set S := { 1 .. 4 };
set SD := { 1 .. 6 };
param L := 10;
set L_ := { 1 .. L };

param a[P * D] := 1;
param r[P * R] := 1;
param q[P * C] := 1;
param n[C] := 1;
param m[C * L_ * R] := 1;
param M[C * L_ * R] := 1;
param pp[C * S] := 1;
param psd[C * SD] := 1;
param ds[S * SD *  L_ * D] := 1;
param maxp[P] := 1;

var cp[C * S] binary;
var csd[C * SD] binary;
var x[C * L_ * P * R] binary;

var class_date[C * L_ * D] binary;
var busy[P * C * D] binary;
var busy_l[P * C * L_] binary;

maximize quality:
  sum<c, l, p, k> in C * L_ * P * R:
    x[c, l, p, k] * q[p, c];

subto class_date_coherency:
  forall<c, sd, p, l, d> in C * SD * S * L_ * D with ds[p, sd, l, d] == 1:
    csd[c, sd] + cp[c, p] - 1 <= class_date[c, l, d];

subto class_date_coherency_2:
  forall<c, l> in C * L_ with l <= n[c]:
    sum<d> in D: class_date[c, l, d] == 1;

subto class_date_coherency_3:
  forall<c, l, d> in C * L_ * D with l > n[c] and l <= L:
    class_date[c, l, d] == 0;

subto busy_l_coherency:
  forall <c, l, p> in C * L_ * P:
    sum<k> in R: x[c, l, p, k] == busy_l[p, c, l];

subto busy_l_coherency_2:
  forall <c, l, p> in C * L_ * P with l > n[c]:
    busy_l[p, c, l] == 0;

subto busy_coherency:
  forall <p, c, l, d> in P * C * L_ * D:
    busy[p, c, d] >= busy_l[p, c, l] + class_date[c, l, d] - 1;

subto busy_coherency_2:
  forall<p, c> in P * C:
    sum<d> in D: busy[p, c, d] == sum<l> in L_ with l <= n[c]: busy_l[p, c, l];

subto professor_availability:
  forall <p, d> in P*D:
    sum<c> in C: busy[p, c, d] <= a[p, d];

subto professor_classes_limit:
  forall <p> in P:
    sum<c, d> in C * D: busy[p, c, d] <= maxp[p];

subto valid_role:
    forall <c, l, p, k> in C * L_ * P * R:
      x[c, l, p, k] <= r[p, k];

subto satisfied_roles_1:
    forall <c, l, k> in C * L_ * R:
      sum<p> in P:
        x[c, l, p, k] >= m[c, l, k];

subto satisfied_roles_2:
    forall <c, l, k> in C * L_ * R:
      sum<p> in P:
        x[c, l, p, k] <= M[c, l, k];

subto no_duplicate_profs:
    forall <c, l, p> in C * L_ * P:
      sum<k> in R: x[c, l, p, k] <= 1;

subto legal_weekly_pattern:
  forall <c, s> in C * S:
    cp[c, s] <= pp[c, s];

subto some_weekly_pattern:
  forall <c> in C:
    sum<s> in S: cp[c, s] == 1;

subto legal_start_date:
  forall <c, sd> in C * SD:
    csd[c, sd] <= psd[c, sd];

subto some_start_date:
  forall <c> in C:
    sum<sd> in SD: csd[c, sd] == 1;
