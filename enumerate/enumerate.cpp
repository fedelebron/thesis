#include <iostream>
#include <string>
#include <fstream>
#include <vector>

#include "polytope.h"
using namespace std;

int main(int argc, char** argv) {
  if (argc <= 1) {
    cout << "Usage: " << argv[0] << " filename.ieq" << endl;
    return 1;
  }

  vector<string> args(argv, argv+argc);
  string input_file = args[1];

  ifstream f(input_file);
  Polytope poly;
  f >> poly;

  if (find(begin(args), end(args), "--clean") != end(args)) {
    poly.clean();
    std::cout << "Cleaned input down to " << poly.dimension << " variables, " << poly.constraints.size() << " inequalities." << std::endl;
    for (const auto& c : poly.constraints) {
      poly.print_constraint(std::cout, c, true);
    }
  }

  if (find(begin(args), end(args), "--human") != end(args)) {
    cout << poly << endl;
    poly.print_ieq(cout);
  } else if (find(begin(args), end(args), "--poi") != end(args)) {
    ofstream o(input_file + ".poi");
    poly.print_poi(o);
  } else if (find(begin(args), end(args), "--ine") != end(args)) {
    ofstream of(input_file + ".ine");
    poly.print_ine(of);
  }
}
