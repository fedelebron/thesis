SINGLE COURSE, SINGLE START DATE, MULTIPLY WEEKLY PATTERN
=========================================================

Variables:
* bool x[j, k, l] = jth class has kth professor with the lth role
* bool cp[j] = the jth plan was chosen
* bool class_date[i, j] = the ith class is on the jth day
* bool busy[i, j] = the ith professor is busy on the jth day
* bool busy_l[i, j] = the ith professor is busy on the jth class

Constants:
* We have a set P of professors.
* We have a set D of days in the year.
* We have a set R of roles.
* We have a set S of weekly patterns.
* We have a number L, the number of classes in the course.

Parameters:
* bool r[i, j] = ith prof can take on jth role
* bool a[i, j] = ith prof is available on the jth day
* bool ds[i, j, k] = ith weekly plan has the jth class on the kth day
* int m[i, j] = ith class needs this many profs in role j
* int q[i] = ith prof has this quality

Restriction
* If a professor teaches on day i, the professor needs to be available on day i.
* If a professor teaches a class with role k, then the professor needs to be able to act in the role k.
* For every class, the number of professors for each role are satisfied.
* No professor teaches the same class with two roles.

Objective function (max):
  \sum_{i, j, k} x[i, j, k] * q[j]
