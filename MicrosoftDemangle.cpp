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
  string(const std::string &s) : p(s.data()), len(s.size()) {}
  string(const char *p) : p(p), len(strlen(p)) {}
  string(const char *p, const char *end) : p(p), len(end - p) {}
  template <size_t N> string(const char (&p)[N]) : p(p), len(N - 1) {}

  std::string str() const { return {p, p + len}; }

  bool startswith(char c) { return len > 0 && *p == c; }
  bool empty() const { return len == 0; }

  bool consume(const char *s, ssize_t slen = -1) {
    slen = (slen == -1) ? strlen(s) : slen;
    if (slen > len || strncmp(p, s, slen) != 0)
      return false;
    p += slen;
    len -= slen;
    return true;
  }

  bool consume(const std::string &s) { return consume(s.data(), s.size()); }

  ssize_t find(const char *s) const {
    size_t slen = strlen(s);
    if (slen > len)
      return -1;

    for (size_t i = 0; i < len - slen; ++i)
      if (strncmp(p + i, s, slen) == 0)
        return i;
    return -1;
  }

  void trim(size_t n) {
    assert(n <= len);
    p += n;
    len -= n;
  }

  string substr(size_t start, ssize_t end = -1) const {
    return {p + start, p + (end == -1 ? len : end)};
  }

  const char *p = nullptr;
  size_t len = 0;
};

enum Error { OK, BAD, BAD_NUMBER, BAD_CALLING_CONV };

// Storage classes
enum {
  Const = 1 << 0,
  Volatile = 1 << 1,
  Far = 1 << 2,
  Huge = 1 << 3,
  Unaligned = 1 << 4,
  Restrict = 1 << 5,
};

// Calling conventions
enum {
  Cdecl,
  Pascal,
  Thiscall,
  Stdcall,
  Fastcall,
  Regcall,
};

// Types
enum PrimTy : uint8_t {
  Unknown,
  Ptr,
  Array,

  Struct,
  Union,
  Class,
  Enum,

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
  int32_t len;

  // if prim is one of (Struct, Union, Class, Enum).
  string name;

  // if is_function == true
  std::vector<struct Type *> params;
  uint8_t calling_conv;
};

namespace {
class Demangler {
public:
  Demangler(const char *s, size_t len) : input(s, s + len) {}
  void parse();
  std::string str();

  Error error = OK;

private:
  void read_var_type(Type &ty);
  void read_func_type(Type &ty);

  int read_number();
  string read_string();
  void read_prim_type(Type &ty);
  void read_calling_conv(Type &ty);
  int8_t read_storage_class();

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

  symbol = read_string();

  if (input.consume("3"))
    read_var_type(type);
  else if (input.consume("Y"))
    read_func_type(type);
}

int Demangler::read_number() {
  bool neg = input.consume("?");

  if (0 < input.len && '0' <= *input.p && *input.p <= '9') {
    int32_t ret = *input.p - '0' + 1;
    input.trim(1);
    return neg ? -ret : ret;
  }

  size_t i = 0;
  int32_t ret = 0;
  for (; i < input.len; ++i) {
    char c = input.p[i];
    if (c == '@') {
      input.trim(i + 1);
      return neg ? -ret : ret;
    }
    if ('A' <= c && c <= 'P') {
      ret = (ret << 4) + (c - 'A');
      continue;
    }
    break;
  }
  error = BAD_NUMBER;
  return 0;
}

string Demangler::read_string() {
  ssize_t len = input.find("@@");
  if (len < 0) {
    error = BAD;
    return "";
  }
  string ret = input.substr(0, len);
  input.trim(len + 2);
  return ret;
}

void Demangler::read_func_type(Type &ty) {
  ty.is_function = true;

  read_calling_conv(ty);
  int8_t sclass = read_storage_class();
  ty.ptr = alloc();
  read_var_type(*ty.ptr);
  ty.ptr->sclass = sclass;

  while (error == OK && !input.empty() && !input.startswith('@')) {
    Type *tp = alloc();
    read_var_type(*tp);
    ty.params.push_back(tp);
  }
}

void Demangler::read_calling_conv(Type &ty) {
  if (input.consume("A"))
    ty.calling_conv = Cdecl;
  else if (input.consume("C"))
    ty.calling_conv = Pascal;
  else if (input.consume("E"))
    ty.calling_conv = Thiscall;
  else if (input.consume("G"))
    ty.calling_conv = Stdcall;
  else if (input.consume("I"))
    ty.calling_conv = Fastcall;
  else if (input.consume("E"))
    ty.calling_conv = Regcall;
  else
    error = BAD_CALLING_CONV;
};

int8_t Demangler::read_storage_class() {
  if (input.consume("A"))
    return 0;
  if (input.consume("B"))
    return Const;
  if (input.consume("C"))
    return Volatile;
  if (input.consume("D"))
    return Const | Volatile;
  if (input.consume("E"))
    return Far;
  if (input.consume("F"))
    return Const | Far;
  if (input.consume("G"))
    return Volatile | Far;
  if (input.consume("H"))
    return Const | Volatile | Far;
  if (input.consume("I"))
    return Huge;
  if (input.consume("F"))
    return Unaligned;
  if (input.consume("I"))
    return Restrict;
  return 0;
}

void Demangler::read_var_type(Type &ty) {
  if (input.consume("T")) {
    ty.prim = Union;
    ty.name = read_string();
    return;
  }

  if (input.consume("U")) {
    ty.prim = Struct;
    ty.name = read_string();
    return;
  }

  if (input.consume("V")) {
    ty.prim = Class;
    ty.name = read_string();
    return;
  }

  if (input.consume("W4")) {
    ty.prim = Enum;
    ty.name = read_string();
    return;
  }

  if (input.consume("PEA")) {
    ty.prim = Ptr;
    ty.ptr = alloc();
    read_var_type(*ty.ptr);
    return;
  }

  if (input.consume("Y")) {
    int dimension = read_number();
    if (dimension <= 0) {
      error = BAD;
      return;
    }

    Type *tp = &ty;
    for (int i = 0; i < dimension; ++i) {
      tp->prim = Array;
      tp->len = read_number();
      tp->ptr = alloc();
      tp = tp->ptr;
    }
    read_var_type(*tp);
    return;
  }

  if (input.consume("P6A")) {
    ty.prim = Ptr;
    ty.ptr = alloc();

    Type &fn = *ty.ptr;
    fn.is_function = true;
    fn.ptr = alloc();
    read_var_type(*fn.ptr);

    while (error == OK && !input.consume("@Z")) {
      Type *tp = alloc();
      read_var_type(*tp);
      fn.params.push_back(tp);
    }
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
      ty.prim = p.prim;
      return;
    }
  }

  error = BAD;
}

static void push_front(std::vector<string> &vec, const string &s) {
  vec.insert(vec.begin(), s);
}

static std::vector<string> atsign_to_colons(string s) {
  std::vector<string> vec;
  for (;;) {
    if (!vec.empty())
      vec.push_back("::");
    ssize_t pos = s.find("@");
    if (pos == -1)
      break;
    vec.push_back(s.substr(0, pos));
    s.trim(pos + 1);
  }
  vec.push_back(s);
  std::reverse(vec.begin(), vec.end());
  return vec;
}

static void type2str(Type &type, std::vector<string> &partial,
                     std::vector<std::string> &buf) {
  if (type.is_function) {
    if (partial[0].startswith('*')) {
      push_front(partial, "(");
      partial.push_back(")");
    }

    std::vector<string> retty = {""};
    type2str(*type.ptr, retty, buf);
    partial.insert(partial.begin(), retty.begin(), retty.end());

    partial.push_back("(");
    for (size_t i = 0; i < type.params.size(); ++i) {
      if (i != 0)
        partial.push_back(",");

      std::vector<string> paramty = {""};
      type2str(*type.params[i], paramty, buf);
      partial.insert(partial.end(), paramty.begin(), paramty.end());
    }
    partial.push_back(")");
    return;
  }

  switch (type.prim) {
  case Unknown:
    return;
  case Ptr: {
    push_front(partial, "*");
    type2str(*type.ptr, partial, buf);
    return;
  }
  case Array: {
    if (partial[0].startswith('*')) {
      push_front(partial, "(");
      partial.push_back(")");
    }

    partial.push_back("[");
    buf.push_back(std::to_string(type.len));
    partial.push_back(buf.back());
    partial.push_back("]");

    type2str(*type.ptr, partial, buf);
    return;
  }
  case Struct:
    push_front(partial, type.name);
    push_front(partial, "struct");
    return;
  case Union:
    push_front(partial, type.name);
    push_front(partial, "union");
    return;
  case Class:
    push_front(partial, type.name);
    push_front(partial, "class");
    return;
  case Enum: {
    std::vector<string> name = atsign_to_colons(type.name);
    partial.insert(partial.begin(), name.begin(), name.end());
    push_front(partial, "enum");
    return;
  }
  case Void:
    push_front(partial, "void");
    return;
  case Bool:
    push_front(partial, "bool");
    return;
  case Char:
    push_front(partial, "char");
    return;
  case Schar:
    push_front(partial, "signed char");
    return;
  case Uchar:
    push_front(partial, "unsigned char");
    return;
  case Short:
    push_front(partial, "short");
    return;
  case Ushort:
    push_front(partial, "unsigned short");
    return;
  case Int:
    push_front(partial, "int");
    return;
  case Uint:
    push_front(partial, "unsigned int");
    return;
  case Long:
    push_front(partial, "long");
    return;
  case Ulong:
    push_front(partial, "unsigned long");
    return;
  case Llong:
    push_front(partial, "long long");
    return;
  case Ullong:
    push_front(partial, "unsigned long long");
    return;
  case Wchar:
    push_front(partial, "wchar_t");
    return;
  case Float:
    push_front(partial, "float");
    return;
  case Double:
    push_front(partial, "double");
    return;
  case Ldouble:
    push_front(partial, "long double");
    return;
  case M64:
    push_front(partial, "__m64");
    return;
  case M128:
    push_front(partial, "__m128");
    return;
  case M128d:
    push_front(partial, "__m128d");
    return;
  case M128i:
    push_front(partial, "__m128i");
    return;
  case M256:
    push_front(partial, "__m256");
    return;
  case M256d:
    push_front(partial, "__m256d");
    return;
  case M256i:
    push_front(partial, "__m256i");
    return;
  case M512:
    push_front(partial, "__m512");
    return;
  case M512d:
    push_front(partial, "__m512d");
    return;
  case M512i:
    push_front(partial, "__m512i");
    return;
  case Varargs:
    push_front(partial, "...");
    return;
  }
}

std::string Demangler::str() {
  assert(error == OK);

  std::vector<string> partial = atsign_to_colons(symbol);
  std::vector<std::string> buf;
  type2str(type, partial, buf);

  partial.erase(std::remove_if(partial.begin(), partial.end(),
                               [](string &s) { return s.empty(); }),
                partial.end());

  std::string ret;
  for (size_t i = 0; i < partial.size(); ++i) {
    if (i > 0 && isalpha(*partial[i - 1].p) && isalpha(*partial[i].p))
      ret += " ";
    ret += partial[i].str();
  }
  return ret;
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
