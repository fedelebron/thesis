#include <stddef.h>
#include <iosfwd>
#include <tuple>
#include <unordered_map>
#include <vector>
using std::vector;
using std::unordered_map;
using std::tuple;
using std::istream;
using std::ostream;

enum class ConstraintType {LE, GE, EQ};
typedef vector<int> Coefficients;
typedef tuple<Coefficients, ConstraintType, int> Constraint;

struct Polytope {
  size_t original_dimension;
  size_t dimension;
  unordered_map<unsigned int, int> determined;
  // translated[i] == j iff the current i is the original jth variable
  vector<int> translated;
  vector<Constraint> constraints;
  void clean();
  // translate from original indices to new indices
  // an exception is thrown if the variable no longer exists
  int untranslate(int idx) const;
  // translate from new indices to original indices
  int translate(int idx) const;
  void print_constraint(ostream&, const Constraint& c, bool human_readable) const;
  void print_ieq(ostream&) const;
  void print_poi(ostream&) const;
  void print_ine(ostream&) const;
  private:
  void remove_variable(int, int);
  void fix_translation_table(int idx);
  void remove_empty_constraints();
  bool clean_zero_equalities();
  bool clean_trivial_constraints();
  void compute_inverse_translation();
  bool necessarily_violated_constraint(const Constraint&) const;
  bool necessarily_invalid_recursive_state() const;
  void print_vertices_recursive(ostream&, unsigned int) const;
  mutable vector<int> recursive_state;
  vector<int> inverse_translation;
};

istream& operator>>(istream& i, Polytope& p);
ostream& operator<<(ostream& o, const Polytope& p);
