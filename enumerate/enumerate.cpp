#include "polytope.h"

#include <iostream>
#include <fstream>
#include <string>
using namespace std;

int main(int argc, char** argv) {
  if (argc <= 1) {
    cout << "Usage: " << argv[0] << " filename.ieq" << endl;
    return 1;
  }
  string input_file = argv[1];

  ifstream f(input_file);
  Polytope poly;
  f >> poly;
#ifdef CLEAN
  poly.clean();
  std::cout << "Cleaned input down to " << poly.nvars << " variables, " << poly.constraints.size() << " inequalities." << std::endl;
  for (size_t i = 0; i < poly.constraints.size(); ++i) {
    poly.print_constraint(std::cout, i, true);
  }
#endif
#ifdef HUMAN
  cout << poly << endl;
  poly.print_ieq(cout);
#endif
#ifdef POI
  string output_file = input_file + ".poi";
  ofstream o(output_file);
  poly.print_poi(o);
#endif
#ifdef INE
  ofstream of(input_file + ".ine");
  poly.print_ine(of);
#endif
}
