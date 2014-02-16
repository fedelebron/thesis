MULTIPLE COURSE, SINGLE START DATE, SINGLE WEEKLY PATTERN
=========================================================

Variables:
  x[i, j, k, l] = ith course, jth class, kth professor, lth role.

Constants:
* We have a set P of professors.
* We have a set D of days in the year.
* We have a set C of courses.
* We have a set R of roles.
* We have a number L, the maximum number of classes any course has.

Parameters:
*  bool d[i, j, k] = ith course, jth class will be taught on the kth day
*  bool r[i, j] = ith prof can take on jth role
*  bool a[i, j] = ith prof is available on the jth day
*  int m[i, j, k] = ith course, jth class needs this many profs in role k
*  int n[i] = ith course will have this many classes
*  int q[i, j] = ith prof has this quality when teaching jth course

Restrictions:
* A professor teaches at most 1 class per day.
* If a professor teaches a class on day i, the professor needs to be available on day i.
* If a professor teaches a class with role k, then the processor needs to be able to act in the role k.
* For every class, the number of professors for each role are satisfied. 
* No professor teaches the same class with two roles.
