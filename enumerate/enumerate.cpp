#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <tuple>
#include "polytope.h"
using namespace std;

enum class Format {NUMBERED_LP, LP, IEQ, INE, POI, Human};
struct Config {
  string input_file;
  string tbl_file;
  Format input_format;
  Format output_format;
  bool clean;
};

string in_fmt_error = "Expected one of [\"ieq\", \"lp\"] after --in.";
string out_fmt_error = "Expected one of [\"ieq\", \"lp\", \"numbered_lp\", \"ine\", \"poi\", \"human\"] after --out.";

Config parse_command_line(vector<string> args) {
  Config c{"default", "", Format::IEQ, Format::IEQ, false};
  auto it = args.begin();
  while (++it != args.end()) {
    if (*it == "--in") {
      ++it;
      if (it == end(args)) {
        cerr << in_fmt_error << endl;
        exit(-1);
      }
      if (*it == "ieq") c.input_format = Format::IEQ;
      else if (*it == "lp") c.input_format = Format::LP;
      else {
        cerr << in_fmt_error << endl;
        exit(-1);
      }
    }
    else if (*it == "--out") {
      ++it;
      if (it == end(args)) {
        cerr << out_fmt_error << endl;
        exit(-1);
      }
      if (*it == "ieq") c.output_format = Format::IEQ;
      else if (*it == "lp") c.output_format = Format::LP;
      else if (*it == "numbered_lp") c.output_format = Format::NUMBERED_LP;
      else if (*it == "ine") c.output_format = Format::INE;
      else if (*it == "poi") c.output_format = Format::POI;
      else if (*it == "human") c.output_format = Format::Human;
      else {
        cerr << out_fmt_error << endl;
        exit(-1);
      }
    } else if (*it == "--clean") {
      c.clean = true;
    } else if (*it == "--tbl-file") {
      ++it;
      c.tbl_file = *it;
    } else c.input_file = *it;
  }

  return c;
}

int main(int argc, char** argv) {
  if (argc <= 1) {
    cout << "Usage: " << argv[0] << " filename.ieq" << endl;
    return 1;
  }

  Config c = parse_command_line(vector<string>(argv, argv+argc));

  ifstream f(c.input_file);
  Polytope poly;

  switch (c.input_format) {
    case Format::LP:
      poly.read_lp(f);
      break;
    case Format::IEQ:
      f >> poly;
      break;
    default:
      cerr << in_fmt_error << endl;
      exit(-1);
  }

  if (c.clean) {
    poly.clean();
    if (!c.tbl_file.empty()
        && c.output_format == Format::LP) {
      ofstream o(c.tbl_file);
      poly.print_tbl(o);
    }
  }

  if (!c.tbl_file.empty() && c.input_format == Format::IEQ) {
    ifstream i(c.tbl_file);
    poly.read_tbl(i);
  }

  switch (c.output_format) {
    case Format::Human:
      cout << poly << endl;
      break;
    case Format::IEQ:
      poly.print_ieq(cout);
      break;
    case Format::LP:
      poly.print_lp(cout, false);
      break;
    case Format::NUMBERED_LP:
      poly.print_lp(cout, true);
      break;
    case Format::INE:
      poly.print_ine(cout);
      break;
    case Format::POI:
      poly.print_poi(cout);
      break;
    default:
      cerr << out_fmt_error << endl;
      exit(-1);
  }
}

