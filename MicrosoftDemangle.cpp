//===- ItaniumDemangle.cpp ------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

class string {
public:
  string() = default;
  string(const string &other) = default;
  string(const char *p, const char *end) : p(p), len(end - p) {}

  std::string str() const { return {p, p + len}; }

  bool startswith(const char *s) const {
    size_t slen = strlen(s);
    return slen <= len && strncmp(p, s, slen) == 0;
  }

  bool startswith(const std::string &s) const {
    size_t slen = s.size();
    return slen <= len && strncmp(p, s.data(), slen) == 0;
  }

  ssize_t find(char c) const {
    for (ssize_t i = 0; i < len; i++)
      if (p[i] == c)
        return i;
    return -1;
  }

  ssize_t find(const char *s) const {
    size_t slen = strlen(s);
    if (slen > len)
      return -1;

    for (size_t i = 0; i < len - slen; i++)
      if (strncmp(p + i, s, slen) == 0)
        return i;
    return -1;
  }

  string substr(size_t start, ssize_t end = -1) const {
    return {p + start, p + (end == -1 ? len : end)};
  }

  const char *p = nullptr;
  size_t len = 0;
};

enum Error { OK, BAD };

enum {
  Near = 1 << 0,
  Const = 1 << 1,
  Volatile = 1 << 2,
  Far = 1 << 3,
  Huge = 1 << 4,
  Unaligned = 1 << 5,
  Restrict = 1 << 6,
};

enum PrimTy : uint8_t {
  Unknown,
  Void,
  Bool,
  Char,
  Schar,
  Uchar,
  Short,
  Ushort,
  Int,
  Uint,
  Long,
  Ulong,
  Llong,
  Ullong,
  Wchar,
  Float,
  Double,
  Ldouble,
  M64,
  M128,
  M128d,
  M128i,
  M256,
  M256d,
  M256i,
  M512,
  M512d,
  M512i,
  Varargs,
};

struct Type {
  PrimTy prim;
  uint8_t sclass = 0;
  bool is_function = false;
  string name;

  // if is_function == true
  std::vector<struct Type *> params{6};
};

namespace {
class Demangler {
public:
  Demangler(const char *s, size_t len) : input(s, s + len) {}
  void parse();
  std::string str();

  Error error = OK;

private:
  PrimTy read_type();

  Type *alloc() { return type_buffer + type_index++; }

  string input;

  string symbol;
  Type type;

  Type type_buffer[100];
  size_t type_index = 0;
};
} // namespace

void Demangler::parse() {
  if (!input.startswith("?")) {
    symbol = input;
    type.prim = Unknown;
  }

  ssize_t name_len = input.find("@@");
  if (name_len < 0) {
    error = BAD;
    return;
  }
  symbol = input.substr(1, name_len);
  input = input.substr(name_len + 2);

  if (input.startswith("3")) {
    input = input.substr(1);
    type.prim = read_type();
  }
}

PrimTy Demangler::read_type() {
  typedef struct {
    std::string code;
    PrimTy prim;
  } Pattern;

  Pattern patterns[] = {{"X", Void},           {"_N", Bool},
                        {"D", Char},           {"C", Schar},
                        {"E", Uchar},          {"F", Short},
                        {"int", Ushort},       {"H", Int},
                        {"I", Uint},           {"J", Long},
                        {"int", Ulong},        {"_J", Llong},
                        {"_K", Ullong},        {"_W", Wchar},
                        {"M", Float},          {"N", Double},
                        {"ldouble", Ldouble},  {"T__m64@@", M64},
                        {"T__m128@@", M128},   {"U__m128d@@", M128d},
                        {"T__m128i@@", M128i}, {"T__m256@@", M256},
                        {"U__m256d@@", M256d}, {"T__m256i@@", M256i},
                        {"T__m512@@", M512},   {"U__m512d@@", M512d},
                        {"T__m512i@@", M512i}, {"Z", Varargs}};

  for (Pattern &p : patterns) {
    if (!input.startswith(p.code))
      continue;
    input = input.substr(p.code.size());
    return p.prim;
  }

  error = BAD;
  return Void;
}

std::string Demangler::str() {
  assert(error == OK);
  if (type.is_function)
    return "<function>";

  switch (type.prim) {
  case Unknown:
    return symbol.str();
  case Void:
    return "void " + symbol.str();
  case Bool:
    return "bool " + symbol.str();
  case Char:
    return "char " + symbol.str();
  case Schar:
    return "schar " + symbol.str();
  case Uchar:
    return "uchar " + symbol.str();
  case Short:
    return "short " + symbol.str();
  case Ushort:
    return "ushort " + symbol.str();
  case Int:
    return "int " + symbol.str();
  case Uint:
    return "uint " + symbol.str();
  case Long:
    return "long " + symbol.str();
  case Ulong:
    return "ulong " + symbol.str();
  case Llong:
    return "llong " + symbol.str();
  case Ullong:
    return "ullong " + symbol.str();
  case Wchar:
    return "wchar " + symbol.str();
  case Float:
    return "float " + symbol.str();
  case Double:
    return "double " + symbol.str();
  case Ldouble:
    return "ldouble " + symbol.str();
  case M64:
    return "m64 " + symbol.str();
  case M128:
    return "m128 " + symbol.str();
  case M128d:
    return "m128d " + symbol.str();
  case M128i:
    return "m128i " + symbol.str();
  case M256:
    return "m256 " + symbol.str();
  case M256d:
    return "m256d " + symbol.str();
  case M256i:
    return "m256i " + symbol.str();
  case M512:
    return "m512 " + symbol.str();
  case M512d:
    return "m512d " + symbol.str();
  case M512i:
    return "m512i " + symbol.str();
  case Varargs:
    return "... " + symbol.str();
  }
  return "";
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << argv[0] << " <symbol>\n";
    exit(1);
  }

  Demangler d(argv[1], strlen(argv[1]));
  d.parse();
  if (d.error != OK) {
    std::cerr << "BAD\n";
    return 1;
  }

  std::cout << d.str() << '\n';
  return 0;
}
