MULTIPLE COURSE, SINGLE START DATE, MULTIPLE WEEKLY PATTERN
===========================================================

Variables:
*  bool x[i, j, k, l] = ith course, jth class, kth professor, lth role.
*  bool cp[i, j] = ith course has chosen the jth weekly plan
*  bool class_date[i, j, k] = ith course has its jth class on the kth day
*  bool busy[i, j, k] = ith prof is busy teaching the jth course on the kth day
*  bool busy_l[i, k, k] = ith prof is busy teaching the jth course on its kth class

Constants:
* We have a set P of professors.
* We have a set D of days in the year.
* We have a set C of courses.
* We have a set R of roles.
* We have a set S of weekly patterns.
* We have a number L, the maximum number of classes any course has.

Parameters:
*  bool r[i, j] = ith prof can take on jth role
*  bool a[i, j] = ith prof is available on the jth day
*  bool ds[i, j, k] = ith weekly plan has the jth class on the kth day
*  bool pp[i, j] = ith course can choose the jth weekly pattern
*  int m[i, j, k] = ith course, jth class needs this many profs in role k
*  int n[i] = ith course will have this many classes
*  int q[i, j] = ith prof has this quality when teaching jth course

Restrictions:
* A professor teaches at most 1 class per day.
* If a professor teaches a class on day i, the professor needs to be available on day i.
* If a professor teaches a class with role k, then the processor needs to be able to act in the role k.
* For every class, the number of professors for each role are satisfied.
* No professor teaches the same class with two roles.
* A course can only choose among its available weekly patterns.

Objective function (max):
  \sum_{i, j, k, l} x[i, j, k, l] * q[k, i]
