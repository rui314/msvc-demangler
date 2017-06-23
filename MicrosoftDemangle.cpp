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
#include <sstream>
#include <string>
#include <utility>
#include <vector>

class String {
public:
  String() = default;
  String(const String &other) = default;
  String(const std::string &s) : p(s.data()), len(s.size()) {}
  String(const char *p) : p(p), len(strlen(p)) {}
  String(const char *p, const char *end) : p(p), len(end - p) {}
  template <size_t N> String(const char (&p)[N]) : p(p), len(N - 1) {}

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

  ssize_t find(const std::string &s) const {
    if (s.size() > len)
      return -1;

    for (size_t i = 0; i < len - s.size(); ++i)
      if (strncmp(p + i, s.data(), s.size()) == 0)
        return i;
    return -1;
  }

  void trim(size_t n) {
    assert(n <= len);
    p += n;
    len -= n;
  }

  String substr(size_t start, ssize_t end = -1) const {
    return {p + start, p + (end == -1 ? len : end)};
  }

  const char *p = nullptr;
  size_t len = 0;
};

std::ostream &operator<<(std::ostream &os, const String s) {
  os.write(s.p, s.len);
  return os;
}

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
  uint8_t calling_conv;

  Type *ptr = nullptr;
  int32_t len;

  // if prim is one of (Struct, Union, Class, Enum).
  String name;

  // function or template parameters
  std::vector<struct Type *> params;
};

namespace {
class Demangler {
public:
  Demangler(const char *s, size_t len) : input(s, s + len) {}
  void parse();
  std::string str();

  Error status = OK;

private:
  // Parser
  void read_var_type(Type &ty);
  void read_func_type(Type &ty);

  int read_number();
  String read_string();
  String read_until(const std::string &s);
  void read_prim_type(Type &ty);
  void read_calling_conv(Type &ty);
  int8_t read_storage_class();

  Type *alloc() { return type_buffer + type_index++; }

  String input;
  Type type;
  String symbol;
  Type type_buffer[100];
  size_t type_index = 0;

  // Writer
  void write_pre(Type &type);
  void write_post(Type &type);
  void write_params(Type &type);
  void write_name(String s);
  void write_space();

  std::stringstream os;
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
  status = BAD_NUMBER;
  return 0;
}

String Demangler::read_string() {
  return read_until("@@");
}

String Demangler::read_until(const std::string &delim) {
  ssize_t len = input.find(delim);
  if (len < 0) {
    status = BAD;
    return "";
  }
  String ret = input.substr(0, len);
  input.trim(len + delim.size());
  return ret;
}

void Demangler::read_func_type(Type &ty) {
  ty.is_function = true;

  read_calling_conv(ty);
  int8_t sclass = read_storage_class();
  ty.ptr = alloc();
  read_var_type(*ty.ptr);
  ty.ptr->sclass = sclass;

  while (status == OK && !input.empty() && !input.startswith('@')) {
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
    status = BAD_CALLING_CONV;
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

    if (input.consume("?$")) {
      ty.name = read_until("@");
      while (status == OK && !input.consume("@")) {
        Type *tp = alloc();
        read_var_type(*tp);
        ty.params.push_back(tp);
      }
    } else {
      ty.name = read_string();
    }
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

  if (input.consume("PEB")) {
    ty.prim = Ptr;
    ty.ptr = alloc();
    read_var_type(*ty.ptr);
    ty.ptr->sclass = Const;
    return;
  }

  if (input.consume("QEB")) {
    ty.prim = Ptr;
    ty.sclass = Const;
    ty.ptr = alloc();
    read_var_type(*ty.ptr);
    ty.ptr->sclass = Const;
    return;
  }

  if (input.consume("Y")) {
    int dimension = read_number();
    if (dimension <= 0) {
      status = BAD;
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

    while (status == OK && !input.consume("@Z")) {
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

  status = BAD;
}

std::string Demangler::str() {
  write_pre(type);
  write_space();
  write_name(symbol);
  write_post(type);
  return os.str();
}

void Demangler::write_pre(Type &type) {
  if (type.is_function) {
    write_pre(*type.ptr);
    return;
  }

  switch (type.prim) {
  case Unknown:
    break;
  case Ptr:
    write_pre(*type.ptr);
    if (type.ptr->is_function || type.ptr->prim == Array)
      os << "(";
    os << "*";
    break;
  case Array:
    write_pre(*type.ptr);
    break;
  case Struct:
    os << "struct " << type.name;
    break;
  case Union:
    os << "union " << type.name;
    break;
  case Class:
    os << "class " << type.name;
    if (!type.params.empty()) {
      os << "<";
      write_params(type);
      os << ">";
    }
    break;
  case Enum: {
    os << "enum ";
    write_name(type.name);
    break;
  }
  case Void:
    os << "void";
    break;
  case Bool:
    os << "bool";
    break;
  case Char:
    os << "char";
    break;
  case Schar:
    os << "signed char";
    break;
  case Uchar:
    os << "unsigned char";
    break;
  case Short:
    os << "short";
    break;
  case Ushort:
    os << "unsigned short";
    break;
  case Int:
    os << "int";
    break;
  case Uint:
    os << "unsigned int";
    break;
  case Long:
    os << "long";
    break;
  case Ulong:
    os << "unsigned long";
    break;
  case Llong:
    os << "long long";
    break;
  case Ullong:
    os << "unsigned long long";
    break;
  case Wchar:
    os << "wchar_t";
    break;
  case Float:
    os << "float";
    break;
  case Double:
    os << "double";
    break;
  case Ldouble:
    os << "long double";
    break;
  case M64:
    os << "__m64";
    break;
  case M128:
    os << "__m128";
    break;
  case M128d:
    os << "__m128d";
    break;
  case M128i:
    os << "__m128i";
    break;
  case M256:
    os << "__m256";
    break;
  case M256d:
    os << "__m256d";
    break;
  case M256i:
    os << "__m256i";
    break;
  case M512:
    os << "__m512";
    break;
  case M512d:
    os << "__m512d";
    break;
  case M512i:
    os << "__m512i";
    break;
  case Varargs:
    os << "...";
    break;
  }

  if (type.sclass & Const) {
    write_space();
    os << "const";
  }
}

void Demangler::write_post(Type &type) {
  if (type.is_function) {
    os << "(";
    write_params(type);
    os << ")";
    return;
  }

  if (type.prim == Ptr) {
    if (type.ptr->is_function || type.ptr->prim == Array)
      os << ")";
    write_post(*type.ptr);
    return;
  }

  if (type.prim == Array) {
    os << "[" << type.len << "]";
    write_post(*type.ptr);
  }
}

void Demangler::write_params(Type &type) {
  for (size_t i = 0; i < type.params.size(); ++i) {
    if (i != 0)
      os << ",";
    write_pre(*type.params[i]);
    write_post(*type.params[i]);
  }
}

void Demangler::write_name(String s) {
  size_t pos = s.len;
  bool sep = false;

  for (ssize_t i = pos - 1; i >= 0; --i) {
    if (s.p[i] != '@')
      continue;

    if (sep)
      os << "::";
    sep = true;
    os.write(s.p + i + 1, pos - i - 1);
    pos = i;
  }

  if (sep)
    os << "::";
  os.write(s.p, pos);
}

void Demangler::write_space() {
  std::string s = os.str();
  if (!s.empty() && isalpha(s.back()))
    os << " ";
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << argv[0] << " <symbol>\n";
    exit(1);
  }

  Demangler demangler(argv[1], strlen(argv[1]));
  demangler.parse();
  if (demangler.status != OK) {
    std::cerr << "BAD\n";
    return 1;
  }

  std::cout << demangler.str() << '\n';
  return 0;
}
