set P := {"fede", "javi"};
set R := {"ay2", "ay1", "jtp", "prof"};
set D := { 1 .. 10 };
set S := { 1 .. 4 };
param L := 10;
set L_ := { 1 .. L };

param r[P * R] := 1;
param a[P * D] := 1;
param ds[S * L_ * D] := 1;
param m[L_ * R] = := 1;
param q[P] := 8;

var x[L_ * P * R] binary;
var cp[S] binary;
var class_date[L_ * D] binary;
var busy[P * D] binary;
var busy_l[P * L_] binary;

maximize quality:
  sum<l, p, k> in L_ * P * R:
    x[l, p, k] * q[p];

subto class_date_coherency:
  forall <d, j, l in D * S * L_:
    class_date[l, d] >= cp[j] * ds[j, l, d];

subto busy_l_coherency:
  forall <l, p> in L_ * P:
    sum<k> in R: x[l, p, k] = busy_l[p, l];

subto busy_coherency:
  forall <p, l, d> in P * L_ * D:
    busy[p, d] >= busy_l[p, l] + class_date[l, d] - 1;

subto professor_availability:
  forall <p, d> in P * D:
    busy[p, d] <= a[p, d];

subto valid_role:
  forall <l, p, k> in L_ * P * R:
    x[l, p, k] <= r[p, k];

subto satisfied_roles:
  forall <l, k> in L_ * R:
    sum<p> in P:
      x[l, p, k] >= m[l, k];

subto no_duplicate_profs:
  forall <l, p> in L_ * P:
    sum<k> in R: x[l, p, k] <= 1;
