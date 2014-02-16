SINGLE COURSE, SINGLE START DATE, SINGLE WEEKLY PATTERN
-------------------------------------------------------

Constants:
* We have a number R of roles.
* We have a number P of professors.
* We have a number D of days.
* We have a number C of classes for this course.

* We have C * D booleans d_{i, j}, which is true if the ith class is on the jth day.
* We have P * D booleans a_{p, i}, which is true if the professor p can give classes on day i.
* We have P * R booleans r_{p, i}, which is true if the professor p can act in the role i.
* We have P integers 10 >= q_p >= 0, the quality of professor p in this course.
* We have C * R integers n_{i, r}, how many professors of role r are needed in the ith class.

Variables:
* We have booleans x_{p, i, r}, which is true if the professor p teaches the ith class with role r.

Restrictions:
* If a professor i teaches the jth class, and the jth class is on day k, then i needs to be able to give classes on day k.
  => If x_{i, j, r} and d_{j, k}, then a_{i, k}.

* For every professor i, class j, and every role k, if professor i teaches the jth class with role k, then i can act in the role k.
  => If x_{i, j, r}, then r_{i, k}.

* For every class j, and every role k, the number of professors of role k is at least the required number for class j.
  => \sum_{i = 1}^P x_{i, j, k} >= n_{j, k}

* For every class i, and every professor j, professor j only teaches class i with at most one role.
  => \sum_{k = 1}^R x_{i, j, k} <= 1

Objective function (maximize):
  \sum_{i = 1}^P \sum_{j = 1}^C \sum_{k = 1}^R x_{i, j, k} * q_i

Linear constraints:

* forall 1 <= i <= P, 1 <= j <= C, 1 <= r <= R, 1 <= k <= D: x_{i, j, r} + d_{j, k} <= 1 + a_{i, k}
* forall 1 <= i <= P, 1 <= j <= C, 1 <= k <= R: x_{i, j, r} <= r_{i, k}
* forall 1 <= j <= C, 1 <= k <= R: \sum_{i = 1}^P x_{i, j, k} >= n_{j, k}
* forall 1 <= i <= C, 1 <= j <= P: \sum_{k = 1}^R x_{i, j, k} <= 1


