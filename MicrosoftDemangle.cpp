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

  bool consume(const char *s, ssize_t slen = -1) {
    slen = (slen == -1) ? strlen(s) : slen;
    if (slen > len || strncmp(p, s, slen) != 0)
      return false;
    p += slen;
    len -= slen;
    return true;
  }

  bool consume(const std::string &s) {
    return consume(s.data(), s.size());
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
  Ptr,
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
  Type *ptr = nullptr;

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
  void read_type(Type &ty);
  void read_prim_type(Type &ty);

  Type *alloc() { return type_buffer + type_index++; }

  string input;

  string symbol;
  Type type;

  Type type_buffer[100];
  size_t type_index = 0;
};
} // namespace

void Demangler::parse() {
  if (!input.consume("?")) {
    symbol = input;
    type.prim = Unknown;
  }

  ssize_t name_len = input.find("@@");
  if (name_len < 0) {
    error = BAD;
    return;
  }
  symbol = input.substr(0, name_len);
  input = input.substr(name_len + 2);

  if (input.consume("3"))
    read_type(type);
}

void Demangler::read_type(Type &ty) {
  if (input.consume("PEA")) {
    ty.prim = Ptr;
    ty.ptr = alloc();
    read_type(*ty.ptr);
    return;
  }

  read_prim_type(ty);
}

void Demangler::read_prim_type(Type &ty) {
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
    if (input.consume(p.code)) {
      input = input.substr(p.code.size());
      ty.prim = p.prim;
      return;
    }
  }

  error = BAD;
}

static std::string type2str(Type &type, const std::string &partial) {
  switch (type.prim) {
  case Unknown:
    return partial;
  case Ptr:
    return type2str(*type.ptr, "*" + partial);
  case Void:
    return "void " + partial;
  case Bool:
    return "bool " + partial;
  case Char:
    return "char " + partial;
  case Schar:
    return "schar " + partial;
  case Uchar:
    return "uchar " + partial;
  case Short:
    return "short " + partial;
  case Ushort:
    return "ushort " + partial;
  case Int:
    return "int " + partial;
  case Uint:
    return "uint " + partial;
  case Long:
    return "long " + partial;
  case Ulong:
    return "ulong " + partial;
  case Llong:
    return "llong " + partial;
  case Ullong:
    return "ullong " + partial;
  case Wchar:
    return "wchar " + partial;
  case Float:
    return "float " + partial;
  case Double:
    return "double " + partial;
  case Ldouble:
    return "ldouble " + partial;
  case M64:
    return "m64 " + partial;
  case M128:
    return "m128 " + partial;
  case M128d:
    return "m128d " + partial;
  case M128i:
    return "m128i " + partial;
  case M256:
    return "m256 " + partial;
  case M256d:
    return "m256d " + partial;
  case M256i:
    return "m256i " + partial;
  case M512:
    return "m512 " + partial;
  case M512d:
    return "m512d " + partial;
  case M512i:
    return "m512i " + partial;
  case Varargs:
    return "... " + partial;
  }
}

std::string Demangler::str() {
  assert(error == OK);
  if (type.is_function)
    return "<function>";

  return type2str(type, symbol.str());
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << argv[0] << " <symbol>\n";
    exit(1);
  }

  Demangler demangler(argv[1], strlen(argv[1]));
  demangler.parse();
  if (demangler.error != OK) {
    std::cerr << "BAD\n";
    return 1;
  }

  std::cout << demangler.str() << '\n';
  return 0;
}
