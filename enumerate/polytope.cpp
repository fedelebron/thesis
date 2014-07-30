#include "polytope.h"

#include <ctype.h>
#include <algorithm>
#include <boost/optional/optional.hpp>
#include <iostream>
#include <iterator>
#include <numeric>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>
using std::istream;
using std::string;
using boost::optional;
using std::isspace;
using std::get;
using std::to_string;

Polytope::Polytope() : original_dimension(0), dimension(0) {}

bool is_trivial(const Coefficients& c) {
  return std::count(begin(c), end(c), 0) == c.size() - 1;
}

bool is_tautological(const Constraint& c) {
  const Coefficients& coeffs = get<0>(c);
  const ConstraintType& cmp = get<1>(c);
  const int& value = get<2>(c);
  if (cmp == ConstraintType::EQ) return false;
  int pos = std::count(begin(coeffs), end(coeffs), 1);
  int neg = std::count(begin(coeffs), end(coeffs), -1);
  if (cmp == ConstraintType::LE
      && pos <= value) return true;
  if (cmp == ConstraintType::GE
      && -neg >= value) return true;
  return false;
}

bool is_nonnegative(const Coefficients& c) {
  return std::count_if(begin(c), end(c), [](int x) { return x >= 0; }) == c.size();
}

bool is_equality(const Constraint& c) {
  return get<1>(c) == ConstraintType::EQ;
}

bool is_inequality(const Constraint& c) {
  return !is_equality(c);
}

int get_trivial_variable(const Coefficients& c) {
  // assumes is_trivial(c)
  auto it = std::find_if(begin(c), end(c), [](int x) { return x != 0; });
  return std::distance(begin(c), it);
}

void Polytope::compute_inverse_translation() {
  inverse_translation.resize(original_dimension + 1);
  std::fill(begin(inverse_translation), end(inverse_translation), -1);
  for (size_t i = 1; i < translated.size(); ++i) {
    inverse_translation[translated[i]] = i;
  }
}

bool Polytope::necessarily_violated_constraint(const Constraint& c) const {
  const Coefficients& coeff = get<0>(c);
  int sum = 0;
  for (int j = 1; j < coeff.size(); ++j) {
    int ours = recursive_state[translate(j)];
    if (ours == -1) return false;
    sum += coeff[j] * ours;
  }
  int rhs = get<2>(c);
  switch (get<1>(c)) {
    case ConstraintType::EQ:
      if (sum != rhs) return true;
      break;
    case ConstraintType::LE:
      if (sum > rhs) return true;
      break;
    case ConstraintType::GE:
      if (sum < rhs) return true;
      break;
  }
  return false;
}

bool Polytope::necessarily_invalid_recursive_state() const {
  // must use a lambda since the predicate is a member function
  return std::any_of(begin(constraints), end(constraints), [this](auto x) {
    return this->necessarily_violated_constraint(x);
  });
}

void Polytope::print_poi(ostream& o) const {
  o << "DIM = " << original_dimension << std::endl;
  o << "CONV_SECTION" << std::endl;
  recursive_state.resize(original_dimension + 1);
  std::fill(begin(recursive_state), end(recursive_state), -1);
  print_vertices_recursive(o, 1);
  o << "END" << std::endl;
}

void Polytope::print_vertices_recursive(ostream& o, unsigned int k) const {
  if (k == original_dimension + 1) {
    for (int i = 1; i <= original_dimension; ++i) {
      o << recursive_state[i] << ' ';
    }
    o << std::endl;
    std::cerr << '*';
    return;
  }
  if (determined.count(k) == 1) {
    recursive_state[k] = determined.at(k);
    if (necessarily_invalid_recursive_state()) {
      recursive_state[k] = -1;
      return;
    }
    print_vertices_recursive(o, k + 1);
    recursive_state[k] = -1;
    return;
  }

  for (int chosen = 0; chosen <= 1; ++chosen) {
    recursive_state[k] = chosen;
    if (necessarily_invalid_recursive_state()) continue;
    print_vertices_recursive(o, k + 1);
  }

  recursive_state[k] = -1;
  return;
}

void Polytope::clean() {
  original_dimension = dimension;
  while(clean_trivial_constraints()
        || clean_zero_equalities()
        || clean_tautological_inequalities());
  compute_inverse_translation();
}

int Polytope::translate(int idx) const {
  return translated[idx];
}

void Polytope::fix_translation_table(int idx) {
  for (size_t i = idx; i < dimension; ++i) {
    translated[i] = translated[i + 1];
  }
}

int Polytope::untranslate(int idx) const {
  auto it = std::find(begin(translated), end(translated), idx);
  if (it == end(translated))
    throw std::invalid_argument("Index " + std::to_string(idx) + " not found.");
  return std::distance(begin(translated), it);
}

void Polytope::remove_empty_constraints() {
  auto is_empty = [](const Constraint& c) {
    const Coefficients& coeffs = get<0>(c);
    const ConstraintType& ct = get<1>(c);
    const int& val = get<2>(c);
    if (std::count(begin(coeffs), end(coeffs), 0) < coeffs.size())
      return false;
    // the constraint is empty on the left hand side,
    // but if the operator and right hand side are
    // not feasible, then the set of constraints is
    // inconsistent.
    if ((ct == ConstraintType::EQ && val != 0)
        || (ct == ConstraintType::LE && val < 0)
        || (ct == ConstraintType::GE && val > 0)) {
        throw std::logic_error("Inconsistent constraints.");
    }

    return true;
  };
  constraints.erase(std::remove_if(begin(constraints),
                                   end(constraints),
                                   is_empty),
                    end(constraints));
}

void Polytope::remove_variable(int idx, int value) {
  for (auto& c : constraints) {
    Coefficients& coeffs = get<0>(c);
    int coeff = coeffs[idx];
    coeffs.erase(begin(coeffs) + idx);
    get<2>(c) -= coeff * value;
  }
  determined[translate(idx)] = value;
  fix_translation_table(idx);
  --dimension;
  remove_empty_constraints();
}


bool Polytope::clean_trivial_constraints() {
  bool should_clean_again = false;
  for (size_t i = 0; i < constraints.size(); ++i) {
    const Constraint& c = constraints[i];
    const Coefficients& coeffs = get<0>(c);

    if (!is_trivial(coeffs)) continue;

    int idx = get_trivial_variable(coeffs);
    int rhs = get<2>(c);
    int value = (coeffs[idx] == 1) ? rhs : -rhs;
    switch (get<1>(c)) {
      case ConstraintType::EQ:
        remove_variable(idx, value);
        return true;
      case ConstraintType::LE:
        if (rhs == 0) {
          if (coeffs[idx] == 1) {
            // xi <= 0
            remove_variable(idx, 0);
            return true;
          } else {
            // -xi <= 0
            constraints.erase(begin(constraints) + i);
            should_clean_again = true;
          }
        } else if (rhs == 1) {
          // xi <= 1
          // -xi <= 1
          constraints.erase(begin(constraints) + i);
          should_clean_again = true;
        } else if (rhs == -1) {
          if (coeffs[idx] == -1) {
            // -xi <= -1
            remove_variable(idx, 1);
            return true;
          }
          // xi <= -1
          throw std::logic_error("Variable " + std::to_string(translate(idx)) + " cannot be negative.");
        } else if (rhs < -1) {
          throw std::logic_error("Variable " + std::to_string(translate(idx)) + " cannot be negative or greater than 1.");
          // -xi <= -4
          // xi <= -4
        } else {
          // xi <= 4
          // -xi <= 4
          constraints.erase(begin(constraints) + i);
          should_clean_again = true;
        }
        break;
      case ConstraintType::GE:
        if (rhs == 0) {
          if (coeffs[idx] == 1) {
            // xi >= 0
            constraints.erase(begin(constraints) + i);
            should_clean_again = true;
          } else {
            // -xi >= 0
            remove_variable(idx, 0);
            return true;
          }
        } else if (rhs == 1) {
          if (coeffs[idx] == 1) {
            // xi >= 1
            remove_variable(idx, 1);
            return true;
          } else {
            // -xi >= 1
            throw std::logic_error("Variable " + std::to_string(translate(idx)) + " cannot be negative.");
          }
        } else if (rhs == -1) {
          // xi >= -1
          // -xi >= -1
          constraints.erase(begin(constraints) + i);
          should_clean_again = true;
        } else if (rhs < -1) {
          // xi >= -4
          // -xi >= -4
          constraints.erase(begin(constraints) + i);
          should_clean_again = true;
        } else {
          if (coeffs[idx] == 1) {
            // xi >= 4
            remove_variable(idx, 1);
            return true;
          } else {
            // xi >= -4
            constraints.erase(begin(constraints) + i);
            should_clean_again = true;
          }
        }
    }
  }
  return should_clean_again;
}

bool Polytope::clean_zero_equalities() {
  for (size_t i = 0; i < constraints.size(); ++i) {
    const Constraint& c = constraints[i];
    const Coefficients& coeffs = get<0>(c);
    if (get<2>(c) == 0
        && (is_equality(c) || get<1>(c) == ConstraintType::LE)
        && is_nonnegative(coeffs)) {
      for (size_t j = 1; j < coeffs.size(); ++j) {
        if (!coeffs[j]) continue;
        remove_variable(j, 0);
        // we return here since remove_variable
        // has just invalidated all our iterators
        return true;
      }
    }
  }
  return false;
}

bool Polytope::clean_tautological_inequalities() {
  int length = constraints.size();
  constraints.erase(std::remove_if(begin(constraints),
                                   end(constraints),
                                   is_tautological),
                    end(constraints));
  return constraints.size() != length;
}

optional<Constraint> read_constraint(size_t dim, istream& i) {
  while(isspace(i.peek())) i.get();
  if (i.peek() == 'E') return optional<Constraint>();

  Coefficients coeffs(dim + 1);
  char c;
  while ((c = i.peek()) && c != '+' && c != '-') i.get();
  while ((c = i.peek()) && c != '=' && c != '<' && c != '>') {
    bool sign = c == '+';
    while (!isdigit(i.peek())) i.get();
    int coeff;
    i >> coeff;
    coeffs[coeff] = sign ? 1 : -1;
    while (isspace(i.peek())) i.get();
  }

  ConstraintType type;
  switch (i.peek()) {
    case '<': type = ConstraintType::LE; break;
    case '>': type = ConstraintType::GE; break;
    default: type = ConstraintType::EQ;
  }

  i.ignore(2);

  int bound;
  i >> bound;

  return Constraint{coeffs, type, bound};
}

istream& operator>>(istream& i, Polytope& p) {
  i.ignore(6);
  i >> p.dimension;
  p.original_dimension = p.dimension;
  string s;
  while (i >> s && s != "INEQUALITIES_SECTION");
  i.ignore(1);
  /*i.ignore(sizeof("\nLOWER_BOUNDS\n") - 1
           + 2 * p.dimension
           + sizeof("\nUPPER_BOUNDS\n") - 1
           + 2 * p.dimension
           + sizeof("\nINEQUALITIES_SECTION\n") - 1);*/
  optional<Constraint> c;
  while ((c = read_constraint(p.dimension, i))) {
    p.constraints.push_back(c.get());
  }

  p.translated.resize(p.dimension + 1);
  for (int i = 1; i <= p.dimension; ++i) {
    p.inverse_sequential_translation[i] = 'x' + to_string(i);
    p.sequential_translation['x' + to_string(i)] = i;
  }

  std::iota(begin(p.translated), end(p.translated), 0);

  return i;
}

void Polytope::read_lp(istream& i) {
  char c;
  string s;
  vector<vector<int>> constraint_indices;
  auto& h = sequential_translation;
  auto& ih = inverse_sequential_translation;
  h.clear();
  while(getline(i, s) && s != "Subject to");
  while(getline(i, s) && s != "Bounds") {
    vector<int> current_constraint_indices;
    while (isspace(i.peek())) i.get();
    while (i.peek() == '+' || i.peek() == '-') {
      i >> c;
      i.ignore(1); // ' '
      i >> s;
      if (h.find(s) == end(h)) {
        h[s] = h.size() + 1;
        ih[h.size()] = s;
      }
      current_constraint_indices.push_back((c == '+' ? 1 : -1) * h[s]);
      while (isspace(i.peek())) i.get();
    }

    constraint_indices.push_back(current_constraint_indices);

    ConstraintType c;
    switch (i.peek()) {
      case '<':
        c = ConstraintType::LE;
        break;
      case '=':
        c = ConstraintType::EQ;
        break;
      case '>':
        c = ConstraintType::GE;
        break;
    }

    i.ignore(2); // '==', '<=', or '>='
    int v;
    i >> v;
    while (isspace(i.peek())) i.get();
    constraints.push_back(Constraint{{}, c, v});
  }

  dimension = h.size();
  original_dimension = h.size();

  for (int i = 0; i < constraints.size(); ++i) {
    auto& v = get<0>(constraints[i]);
    v.resize(dimension + 1);
    for (const auto& idx : constraint_indices[i]) {
      v[abs(idx)] = idx > 0 ? 1 : -1;
    }
  }
  translated.resize(dimension + 1);
  std::iota(begin(translated), end(translated), 0);
}


ostream& operator<<(ostream& o, const Polytope& p) {
  o << "Constraints: " << std::endl;
  for (auto& constraint : p.constraints) {
    p.print_constraint(o, constraint, true, true);
  }

  o << "Determined: " << std::endl;
  for (const auto& kv : p.determined) {
    o << 'x' << kv.first << " = " << kv.second << std::endl;
  }

  o << "Names: " << std::endl;
  for (const auto& kv : p.sequential_translation) {
    o << kv.first << " -> " << kv.second << std::endl;
  }

  return o;
}


void Polytope::print_constraint(ostream& o,
                                const Constraint& c,
                                bool human_readable,
                                bool numbered) const {
  bool output = false;
  for (size_t k = 1; k <= dimension; ++k) {
    int val = std::get<0>(c)[k];
    if (!val) continue;
    if (val == -1) o << '-';
    else if (output || !human_readable) o << '+';
    if (!numbered) {
      o << ' ' << inverse_sequential_translation.at(translate(k)) << ' ';
    } else {
      o << " x" << (human_readable ? translate(k) : k) << ' ';
    }
    output = true;
  }

  switch(get<1>(c)) {
    case ConstraintType::LE: o << "<="; break;
    case ConstraintType::GE: o << ">="; break;
    case ConstraintType::EQ:
      o << '=';
      if (human_readable) o << '=';
      break;
  }

  o  << ' ' << get<2>(c) << std::endl;
}

void Polytope::print_ieq(ostream& o) const {
  o << "DIM = " << dimension << std::endl;
  o << "LOWER_BOUNDS" << std::endl;
  for (size_t i = 0; i < dimension; ++i) o << "0 ";
  o << std::endl << "UPPER_BOUNDS" << std::endl;
  for (size_t i = 0; i < dimension; ++i) o << "1 ";
  o << std::endl << "INEQUALITIES_SECTION" << std::endl;
  for (auto& constraint : constraints) {
    print_constraint(o, constraint, false, true);
  }
  o << std::endl << "END" << std::endl;
}

void Polytope::print_ine(ostream& o) const {
  o << "H-representation\n";
  int linearities = std::count_if(begin(constraints), end(constraints), [](const Constraint& c) {
      return get<1>(c) == ConstraintType::EQ;
  });
  if (linearities) {
    o << "linearity " << linearities << ' ';
    for (int i = 0; i < constraints.size(); ++i) {
      if (get<1>(constraints[i]) == ConstraintType::EQ) {
        o << i + 1 << ' ';
        //std::cout << "The " << i << "th constraint was an equality." << std::endl;
      }
    }
    o << '\n';
  }
  o << "begin\n";
  o << constraints.size() + 2 * dimension << ' ' << dimension + 1 << " integer\n";
  for (size_t i = 0; i < constraints.size(); ++i) {
    //std::cout << "The " << i << "th constraint: ";
    //print_constraint(std::cout, i, false);
    int sign = (get<1>(constraints[i]) == ConstraintType::GE) ? -1 : 1;
    const Coefficients& c = get<0>(constraints[i]);
    o << get<2>(constraints[i]) * sign << ' ';
    for (size_t j = 1; j < c.size(); ++j) {
      o << -c[j] * sign << ' ';
    }
    o << '\n';
  }
  // add 0-1 constraints
  for (size_t i = 1; i <= dimension; ++i) {
    o << 0 << ' ';
    for (size_t j = 1; j < i; ++j) o << "0 ";
    o << "1 ";
    for (size_t j = i + 1; j <= dimension; ++j) o << "0 ";
    o << "\n1 ";
    for (size_t j = 1; j < i; ++j) o << "0 ";
    o << "-1 ";
    for (size_t j = i + 1; j <= dimension; ++j) o << "0 ";
    o << '\n';
  }

  o << "end\nmaxcutoff";
}

void Polytope::print_lp(ostream& o, bool numbered) const {
  o << "Maximize potato: 1\n";
  o << "Subject to\n";

  for (const auto& c : constraints) {
    print_constraint(o, c, false, numbered);
  }

  o << "Bounds\n";
  for (unsigned int i = 1; i <= original_dimension; ++i) {
    if (determined.count(i)) continue;
    if (numbered) o << "0 <= x" << i << " <= 1\n";
    else o << "0 <= " << inverse_sequential_translation.at(i)
                      << " <= 1\n";
  }

  o << "General\n";

  for (unsigned int i = 1; i <= original_dimension; ++i) {
    if (determined.count(i)) continue;
    if (numbered) o << " x" << i << '\n';
    else o << inverse_sequential_translation.at(i)
           << '\n';
  }

  o << "End\n";
}

void Polytope::print_tbl(ostream& o) const {
  o << sequential_translation.size() << ' '
    << determined.size() << '\n';
  for (const auto& kv : sequential_translation) {
    o << kv.first << " x" << kv.second << '\n';
  }
  for (const auto& kv : determined) {
    o << 'x' << kv.first << " = " << kv.second << '\n';
  }
}

void Polytope::read_tbl(istream& i) {
  int nvars, ndetermined;
  i >> nvars >> ndetermined;
  int k, v;
  string s;
  for (int j = 0; j < nvars; ++j) {
    i >> s;
    i.ignore(2); // " x"
    i >> k;
    sequential_translation[s] = k;
    inverse_sequential_translation[k] = s;
  }

  for (int j = 0; j < ndetermined; ++j) {
    while (isspace(i.peek())) i.get();
    i.ignore(1); // 'x'
    i >> k;
    i.ignore(3); // " = "
    i >> v;
    determined[k] = v;
  }
}
