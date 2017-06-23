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

static std::vector<String> atsign_to_colons(String s) {
  std::vector<String> vec;
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

namespace {
class Demangler {
public:
  Demangler(const char *s, size_t len) : input(s, s + len) {}
  void parse();
  std::string str();

  Error status = OK;
  Type type;
  String symbol;

private:
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

  Type type_buffer[100];
  size_t type_index = 0;
};

class Stringer {
public:
  Stringer(Demangler &d) : demangler(d) {}
  std::string str();

private:
  void type2str(Type &type, std::vector<String> &v);

  Demangler &demangler;
  std::vector<std::string> buffer;
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

std::string Stringer::str() {
  std::vector<String> v = atsign_to_colons(demangler.symbol);
  type2str(demangler.type, v);
  v.erase(
      std::remove_if(v.begin(), v.end(), [](String &s) { return s.empty(); }),
      v.end());

  std::stringstream ss;
  for (size_t i = 0; i < v.size(); ++i) {
    if (i > 0 && isalpha(*v[i - 1].p) && isalpha(*v[i].p))
      ss << " ";
    ss << v[i];
  }
  return ss.str();
}

void Stringer::type2str(Type &type, std::vector<String> &v) {
  if (type.is_function) {
    if (v[0].startswith('*')) {
      v.insert(v.begin(), "(");
      v.push_back(")");
    }

    std::vector<String> retty = {""};
    type2str(*type.ptr, retty);
    v.insert(v.begin(), retty.begin(), retty.end());

    v.push_back("(");
    for (size_t i = 0; i < type.params.size(); ++i) {
      if (i != 0)
        v.push_back(",");

      std::vector<String> paramty = {""};
      type2str(*type.params[i], paramty);
      v.insert(v.end(), paramty.begin(), paramty.end());
    }
    v.push_back(")");
    return;
  }

  switch (type.prim) {
  case Unknown:
    break;
  case Ptr: {
    if (type.sclass & Const)
      v.insert(v.begin(), "const");
    v.insert(v.begin(), "*");
    type2str(*type.ptr, v);
    return;
  }
  case Array: {
    if (v[0].startswith('*')) {
      v.insert(v.begin(), "(");
      v.push_back(")");
    }

    v.push_back("[");
    buffer.push_back(std::to_string(type.len));
    v.push_back(buffer.back());
    v.push_back("]");

    type2str(*type.ptr, v);
    break;
  }
  case Struct:
    v.insert(v.begin(), type.name);
    v.insert(v.begin(), "struct");
    break;
  case Union:
    v.insert(v.begin(), type.name);
    v.insert(v.begin(), "union");
    break;
  case Class: {
    std::vector<String> vec = {"class", type.name};
    if (!type.params.empty()) {
      vec.push_back("<");
      for (size_t i = 0; i < type.params.size(); ++i) {
        if (i != 0)
          vec.push_back(",");
        std::vector<String> tmp = {""};
        type2str(*type.params[i], tmp);
        vec.insert(vec.end(), tmp.begin(), tmp.end());
      }
      vec.push_back(">");
    }
    v.insert(v.begin(), vec.begin(), vec.end());
    break;
  }
  case Enum: {
    std::vector<String> name = atsign_to_colons(type.name);
    v.insert(v.begin(), name.begin(), name.end());
    v.insert(v.begin(), "enum");
    break;
  }
  case Void:
    v.insert(v.begin(), "void");
    break;
  case Bool:
    v.insert(v.begin(), "bool");
    break;
  case Char:
    v.insert(v.begin(), "char");
    break;
  case Schar:
    v.insert(v.begin(), "signed char");
    break;
  case Uchar:
    v.insert(v.begin(), "unsigned char");
    break;
  case Short:
    v.insert(v.begin(), "short");
    break;
  case Ushort:
    v.insert(v.begin(), "unsigned short");
    break;
  case Int:
    v.insert(v.begin(), "int");
    break;
  case Uint:
    v.insert(v.begin(), "unsigned int");
    break;
  case Long:
    v.insert(v.begin(), "long");
    break;
  case Ulong:
    v.insert(v.begin(), "unsigned long");
    break;
  case Llong:
    v.insert(v.begin(), "long long");
    break;
  case Ullong:
    v.insert(v.begin(), "unsigned long long");
    break;
  case Wchar:
    v.insert(v.begin(), "wchar_t");
    break;
  case Float:
    v.insert(v.begin(), "float");
    break;
  case Double:
    v.insert(v.begin(), "double");
    break;
  case Ldouble:
    v.insert(v.begin(), "long double");
    break;
  case M64:
    v.insert(v.begin(), "__m64");
    break;
  case M128:
    v.insert(v.begin(), "__m128");
    break;
  case M128d:
    v.insert(v.begin(), "__m128d");
    break;
  case M128i:
    v.insert(v.begin(), "__m128i");
    break;
  case M256:
    v.insert(v.begin(), "__m256");
    break;
  case M256d:
    v.insert(v.begin(), "__m256d");
    break;
  case M256i:
    v.insert(v.begin(), "__m256i");
    break;
  case M512:
    v.insert(v.begin(), "__m512");
    break;
  case M512d:
    v.insert(v.begin(), "__m512d");
    break;
  case M512i:
    v.insert(v.begin(), "__m512i");
    break;
  case Varargs:
    v.insert(v.begin(), "...");
    break;
  }

  if (type.sclass & Const)
    v.insert(v.begin(), "const");
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

  std::cout << Stringer(demangler).str() << '\n';
  return 0;
}
