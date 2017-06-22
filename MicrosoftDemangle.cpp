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

enum class error { ok, bad };

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

enum {
  s_near = 1 << 0,
  s_const = 1 << 1,
  s_volatile = 1 << 2,
  s_far = 1 << 3,
  s_huge = 1 << 4,
  s_unaligned = 1 << 5,
  s_restrict = 1 << 6,
};

typedef enum : uint8_t {
  t_unknown,
  t_void,
  t_bool,
  t_char,
  t_schar,
  t_uchar,
  t_short,
  t_ushort,
  t_int,
  t_uint,
  t_long,
  t_ulong,
  t_llong,
  t_ullong,
  t_wchar,
  t_float,
  t_double,
  t_ldouble,
  t_m64,
  t_m128,
  t_m128d,
  t_m128i,
  t_m256,
  t_m256d,
  t_m256i,
  t_m512,
  t_m512d,
  t_m512i,
  t_varargs,
} PrimTy;

struct type {
  PrimTy prim;
  uint8_t sclass = 0;
  bool is_function = false;
  string name;

  // if is_function == true
  std::vector<struct type *> paramty{6};
};

static std::string to_string(error &err) {
  if (err == error::ok)
    return "OK";
  return "BAD";
}

namespace {
class Demangler {
public:
  Demangler(const char *s, size_t len) : input(s, s + len) {}
  void parse();
  std::string str();

  error err = error::ok;

private:
  PrimTy read_type();

  type *alloc() { return type_buffer + type_index++; }

  string input;

  string symbol;
  type ty;

  type type_buffer[100];
  size_t type_index = 0;
};
} // namespace

void Demangler::parse() {
  if (!input.startswith("?")) {
    symbol = input;
    ty.prim = t_unknown;
  }

  ssize_t name_len = input.find("@@");
  if (name_len < 0) {
    err = error::bad;
    return;
  }
  symbol = input.substr(1, name_len);
  input = input.substr(name_len + 2);

  if (input.startswith("3")) {
    input = input.substr(1);
    ty.prim = read_type();
  }
}

PrimTy Demangler::read_type() {
  typedef struct {
    std::string code;
    PrimTy prim;
  } Pattern;

  Pattern patterns[] = {{"X", t_void},           {"_N", t_bool},
                        {"D", t_char},           {"C", t_schar},
                        {"E", t_uchar},          {"F", t_short},
                        {"int", t_ushort},       {"H", t_int},
                        {"I", t_uint},           {"J", t_long},
                        {"int", t_ulong},        {"_J", t_llong},
                        {"_K", t_ullong},        {"_W", t_wchar},
                        {"M", t_float},          {"N", t_double},
                        {"ldouble", t_ldouble},  {"T__m64@@", t_m64},
                        {"T__m128@@", t_m128},   {"U__m128d@@", t_m128d},
                        {"T__m128i@@", t_m128i}, {"T__m256@@", t_m256},
                        {"U__m256d@@", t_m256d}, {"T__m256i@@", t_m256i},
                        {"T__m512@@", t_m512},   {"U__m512d@@", t_m512d},
                        {"T__m512i@@", t_m512i}, {"Z", t_varargs}};

  for (Pattern &p : patterns) {
    if (!input.startswith(p.code))
      continue;
    input = input.substr(p.code.size());
    return p.prim;
  }

  err = error::bad;
  return t_void;
}

std::string Demangler::str() {
  assert(err == error::ok);
  if (ty.is_function)
    return "<function>";

  switch (ty.prim) {
  case t_unknown:
    return symbol.str();
  case t_void:
    return "void " + symbol.str();
  case t_bool:
    return "bool " + symbol.str();
  case t_char:
    return "char " + symbol.str();
  case t_schar:
    return "schar " + symbol.str();
  case t_uchar:
    return "uchar " + symbol.str();
  case t_short:
    return "short " + symbol.str();
  case t_ushort:
    return "ushort " + symbol.str();
  case t_int:
    return "int " + symbol.str();
  case t_uint:
    return "uint " + symbol.str();
  case t_long:
    return "long " + symbol.str();
  case t_ulong:
    return "ulong " + symbol.str();
  case t_llong:
    return "llong " + symbol.str();
  case t_ullong:
    return "ullong " + symbol.str();
  case t_wchar:
    return "wchar " + symbol.str();
  case t_float:
    return "float " + symbol.str();
  case t_double:
    return "double " + symbol.str();
  case t_ldouble:
    return "ldouble " + symbol.str();
  case t_m64:
    return "m64 " + symbol.str();
  case t_m128:
    return "m128 " + symbol.str();
  case t_m128d:
    return "m128d " + symbol.str();
  case t_m128i:
    return "m128i " + symbol.str();
  case t_m256:
    return "m256 " + symbol.str();
  case t_m256d:
    return "m256d " + symbol.str();
  case t_m256i:
    return "m256i " + symbol.str();
  case t_m512:
    return "m512 " + symbol.str();
  case t_m512d:
    return "m512d " + symbol.str();
  case t_m512i:
    return "m512i " + symbol.str();
  case t_varargs:
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
  if (d.err != error::ok) {
    std::cerr << to_string(d.err) << "\n";
    return 0;
  }

  std::cout << d.str() << '\n';
  return 0;
}
