//===- MicrosoftDemangle.cpp ----------------------------------------------===//
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
#include <memory>
#include <numeric>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

// A string class that does not own its contents.
// This class provides a few utility functions for string manipulations.
class String {
public:
  String() = default;
  String(const String &) = default;
  String(const std::string &s) : p(s.data()), len(s.size()) {}
  String(const char *p) : p(p), len(strlen(p)) {}
  String(const char *p, size_t len) : p(p), len(len) {}
  template <size_t N> String(const char (&p)[N]) : p(p), len(N - 1) {}

  std::string str() const { return {p, p + len}; }

  bool empty() const { return len == 0; }

  bool startswith(char c) const { return len > 0 && *p == c; }

  bool startswith(const std::string &s) const {
    return s.size() <= len && strncmp(p, s.data(), s.size()) == 0;
  }

  bool startswith_digit() const {
    return 0 < len && '0' <= p[0] && p[0] <= '9';
  }

  String substr(size_t off) const { return {p + off, len - off}; }
  String substr(size_t off, size_t length) const { return {p + off, length}; }

  bool operator==(const String s) const {
    return len == s.len && memcmp(p, s.p, len) == 0;
  }

  void trim(size_t n) {
    assert(n <= len);
    p += n;
    len -= n;
  }

  int get() {
    if (len == 0)
      return -1;
    len--;
    return *p++;
  }

  void unget(int c) {
    if (c == -1)
      return;
    p--;
    len++;
  }

  const char *p = nullptr;
  size_t len = 0;
};

std::ostream &operator<<(std::ostream &os, const String s) {
  os.write(s.p, s.len);
  return os;
}

// This memory allocator is extremely fast, but it doesn't call dtors
// for allocated objects. That means you can't use STL containers
// (such as std::vector) if you use this allocator. But it pays off --
// the demangler is 3x faster with this allocator compared to one with
// STL containers.
namespace {
class Arena {
public:
  void *alloc(size_t size) {
    assert(size < unit);

    uint8_t *p = buf + nused;
    nused += size;
    if (nused < unit)
      return p;

    buf = new uint8_t[Arena::unit];
    buf2.emplace_back(buf);
    nused = 0;
    return buf;
  }

private:
  static constexpr size_t unit = 4096;

  uint8_t *buf = init_buf;
  alignas(sizeof(void *)) uint8_t init_buf[unit];
  size_t nused = 0;
  std::vector<std::unique_ptr<uint8_t[]>> buf2;
};
}

void *operator new(size_t size, Arena &a) { return a.alloc(size); }
void operator delete(void *, Arena &) {}

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
enum CallingConv : uint8_t {
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
  None,
  Function,
  Ptr,
  Ref,
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
};

// Function classes
enum FuncClass : uint8_t {
  Public = 1 << 0,
  Protected = 1 << 1,
  Private = 1 << 2,
  Global = 1 << 3,
  Static = 1 << 4,
  Virtual = 1 << 5,
  FFar = 1 << 6,
};

namespace {
struct Type;

// Represents an identifier which may be a template.
struct Name {
  String str;
  Type *params = nullptr;
  Name *next = nullptr;
};

// The type class. Mangled symbols are first parsed and converted to
// this type and then converted to string.
struct Type {
  PrimTy prim;

  // Represents a type X in "a pointer to X", "a reference to X",
  // "an array of X", or "a function returning X".
  Type *ptr = nullptr;

  uint8_t sclass = 0;  // storage class
  CallingConv calling_conv;
  FuncClass func_class;

  uint32_t len; // valid if prim == Array

  // Valid if prim is one of (Struct, Union, Class, Enum).
  Name *name = nullptr;

  // Function parameters.
  Type *params = nullptr;

  Type *next = nullptr;
};

// Demangler class takes the main role in demangling symbols.
// It has a set of functions to parse mangled symbols into Type instnaces.
// It also has a set of functions to cnovert Type instances to strings.
class Demangler {
public:
  Demangler(String s) : input(s) {}

  // You are supposed to call parse() first and then check if Error is
  // still empty. After that, call str() to get a result.
  void parse();
  std::string str();

  // Error string. Empty if there's no error.
  std::string error;

private:
  // Parser functions. This is a recursive-descendent parser.
  void read_var_type(Type &ty);
  void read_member_func_type(Type &ty);

  int read_number();
  String read_string(bool memorize);
  void memorize_string(String s);
  Name *read_name();
  String read_until(const std::string &s);
  PrimTy read_prim_type();
  int read_func_class();
  int8_t read_func_access_class();
  CallingConv read_calling_conv();
  void read_func_return_type(Type &ty);
  int8_t read_storage_class();
  int8_t read_storage_class_for_return();

  void read_class(Type &ty, PrimTy prim);
  void read_pointee(Type &ty, PrimTy prim);
  void read_array(Type &ty);
  Type *read_params();

  bool consume(const std::string &s) {
    if (!input.startswith(s))
      return false;
    input.trim(s.size());
    return true;
  }

  void expect(const std::string &s) {
    if (!consume(s))
      error = s + " expected, but got " + input.str();
  }

  // Mangled symbol. read_* functions shorten this string
  // as they parse it.
  String input;

  // A parsed mangled symbol.
  Type type;

  // The main symbol name. (e.g. "ns::foo" in "int ns::foo()".)
  Name *symbol = nullptr;

  Arena arena;

  // The first 10 names in a mangled name can be back-referenced by
  // special name @[0-9]. This is a storage for the first 10 names.
  std::vector<String> repeated_names;

  // Functions to convert Type to String.
  void write_pre(Type &ty);
  void write_post(Type &ty);
  void write_params(Type *ty);
  void write_name(Name *name);
  void write_tmpl_params(Name *name);
  void write_space();

  // The result is written to this stream.
  std::stringstream os;
};
} // namespace

// Parser entry point.
void Demangler::parse() {
  // MSVC-style mangled symbols must start with '?'.
  if (!consume("?")) {
    symbol = new (arena) Name;
    symbol->str = input;
    type.prim = Unknown;
  }

  // What follows is a main symbol name. This may include
  // namespaces or class names.
  symbol = read_name();

  // Read a variable.
  if (consume("3")) {
    read_var_type(type);
    return;
  }

  // Read a non-member function.
  if (consume("Y")) {
    type.prim = Function;
    type.calling_conv = read_calling_conv();
    type.ptr = new (arena) Type;
    type.ptr->sclass = read_storage_class_for_return();
    read_var_type(*type.ptr);
    type.params = read_params();
    return;
  }

  // Read a member function.
  type.prim = Function;
  type.func_class = (FuncClass)read_func_class();
  expect("E"); // if 64 bit
  type.sclass = read_func_access_class();
  type.calling_conv = read_calling_conv();

  type.ptr = new (arena) Type;
  type.ptr->sclass = read_storage_class();
  read_func_return_type(*type.ptr);
  type.params = read_params();
}

// Sometimes numbers are encoded in mangled symbols. For example,
// "int (*x)[20]" is a valid C type (x is a pointer to an array of
// length 20), so we need some way to embed numbers as part of symbols.
// This function parses it.
//
// <number>               ::= [?] <non-negative integer>
//
// <non-negative integer> ::= <decimal digit> # when 1 <= Number <= 10
//                        ::= <hex digit>+ @  # when Numbrer == 0 or >= 10
//
// <hex-digit>            ::= [A-P]           # A = 0, B = 1, ...
int Demangler::read_number() {
  bool neg = consume("?");

  if (input.startswith_digit()) {
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
  error = "bad number";
  return 0;
}

// Read until the next '@'.
String Demangler::read_string(bool memorize) {
  for (size_t i = 0; i < input.len; ++i) {
    if (input.p[i] != '@')
      continue;
    String ret = input.substr(0, i);
    input.trim(i + 1);

    if (memorize)
      memorize_string(ret);
    return ret;
  }
  error = "read_string: missing '@': " + input.str();
  return "";
}

void Demangler::memorize_string(String s) {
  if (repeated_names.size() >= 10)
    return;
  for (String t : repeated_names)
    if (s == t)
      return;
  repeated_names.push_back(s);
}

// Parses a name in the form of A@B@C@@ which represents C::B::A.
Name *Demangler::read_name() {
  Name *head = nullptr;

  while (!consume("@")) {
    if (input.startswith_digit()) {
      int i = input.p[0] - '0';
      if (i >= repeated_names.size()) {
        error = "name reference too large: " + input.str();
        return {};
      }
      input.trim(1);

      Name *elem = new (arena) Name();
      elem->str = repeated_names[i];
      elem->next = head;
      head = elem;
      continue;
    }

    // Class template.
    if (consume("?$")) {
      Name *elem = new (arena) Name;
      elem->str = read_string(false);
      elem->params = read_params();
      elem->next = head;
      head = elem;
      expect("@");
      continue;
    }

    // Non-template functions or classes.
    Name *elem = new (arena) Name;
    elem->str = read_string(true);
    elem->next = head;
    head = elem;
  }

  return head;
}

int Demangler::read_func_class() {
  switch (int c = input.get()) {
  case 'A':
    return Private;
  case 'B':
    return Private | FFar;
  case 'C':
    return Private | Static;
  case 'D':
    return Private | Static;
  case 'E':
    return Private | Virtual;
  case 'F':
    return Private | Virtual;
  case 'I':
    return Protected;
  case 'J':
    return Protected | FFar;
  case 'K':
    return Protected | Static;
  case 'L':
    return Protected | Static | FFar;
  case 'M':
    return Protected | Virtual;
  case 'N':
    return Protected | Virtual | FFar;
  case 'Q':
    return Public;
  case 'R':
    return Public | FFar;
  case 'S':
    return Public | Static;
  case 'T':
    return Public | Static | FFar;
  case 'U':
    return Public | Virtual;
  case 'V':
    return Public | Virtual | FFar;
  case 'Y':
    return Global;
  case 'Z':
    return Global | FFar;
  default:
    input.unget(c);
    error = "unknown func class: " + input.str();
    return 0;
  }
}

int8_t Demangler::read_func_access_class() {
  if (consume("A"))
    return 0;
  if (consume("B"))
    return Const;
  if (consume("C"))
    return Volatile;
  if (consume("D"))
    return Const | Volatile;
  return 0;
}

CallingConv Demangler::read_calling_conv() {
  switch (int c = input.get()) {
  case 'A':
  case 'B':
    return Cdecl;
  case 'C':
    return Pascal;
  case 'E':
    return Thiscall;
  case 'G':
    return Stdcall;
  case 'I':
    return Fastcall;
  default:
    input.unget(c);
    error = "unknown calling convention: " + input.str();
    return Cdecl;
  }
};

// <return-type> ::= <type>
//               ::= @ # structors (they have no declared return type)
void Demangler::read_func_return_type(Type &ty) {
  if (consume("@"))
    ty.prim = None;
  else
    read_var_type(ty);
}

int8_t Demangler::read_storage_class() {
  switch (int c = input.get()) {
  case 'A':
    return 0;
  case 'B':
    return Const;
  case 'C':
    return Volatile;
  case 'D':
    return Const | Volatile;
  case 'E':
    return Far;
  case 'F':
    return Const | Far;
  case 'G':
    return Volatile | Far;
  case 'H':
    return Const | Volatile | Far;
  default:
    input.unget(c);
    return 0;
  }
}

int8_t Demangler::read_storage_class_for_return() {
  if (consume("?A"))
    return 0;
  if (consume("?B"))
    return Const;
  if (consume("?C"))
    return Volatile;
  if (consume("?D"))
    return Const | Volatile;
  return 0;
}

// Reads a variable type.
void Demangler::read_var_type(Type &ty) {
  if (consume("W4")) {
    ty.prim = Enum;
    ty.name = read_name();
    return;
  }

  if (consume("P6A")) {
    ty.prim = Ptr;
    ty.ptr = new (arena) Type;

    Type &fn = *ty.ptr;
    fn.prim = Function;
    fn.ptr = new (arena) Type;
    read_var_type(*fn.ptr);
    fn.params = read_params();

    if (input.startswith("@Z"))
      input.trim(2);
    else if (input.startswith("Z"))
      input.trim(1);
    return;
  }

  switch (int c = input.get()) {
  case 'T':
    read_class(ty, Union);
    return;
  case 'U':
    read_class(ty, Struct);
    return;
  case 'V':
    read_class(ty, Class);
    return;
  case 'A':
    read_pointee(ty, Ref);
    return;
  case 'P':
    read_pointee(ty, Ptr);
    return;
  case 'Q':
    read_pointee(ty, Ptr);
    ty.sclass = Const;
    return;
  case 'Y':
    read_array(ty);
    return;
  default:
    input.unget(c);
    ty.prim = read_prim_type();
    return;
  }
}

// Reads a primitive type.
PrimTy Demangler::read_prim_type() {
  String orig = input;
  switch (input.get()) {
  case 'X':
    return Void;
  case 'D':
    return Char;
  case 'C':
    return Schar;
  case 'E':
    return Uchar;
  case 'F':
    return Short;
  case 'G':
    return Ushort;
  case 'H':
    return Int;
  case 'I':
    return Uint;
  case 'J':
    return Long;
  case 'K':
    return Ulong;
  case 'M':
    return Float;
  case 'N':
    return Double;
  case 'O':
    return Ldouble;
  case '_':
    switch (input.get()) {
    case 'N':
      return Bool;
    case 'J':
      return Llong;
    case 'K':
      return Ullong;
    case 'W':
      return Wchar;
    }
    // fallthrough
  default:
    error = "unknown primitive type: " + orig.str();
    return Unknown;
  }
}

void Demangler::read_class(Type &ty, PrimTy prim) {
  ty.prim = prim;
  ty.name = read_name();
}

void Demangler::read_pointee(Type &ty, PrimTy prim) {
  ty.prim = prim;
  expect("E"); // if 64 bit
  ty.ptr = new (arena) Type;
  ty.ptr->sclass = read_storage_class();
  read_var_type(*ty.ptr);
}

void Demangler::read_array(Type &ty) {
  int dimension = read_number();
  if (dimension <= 0) {
    error = "invalid array dimension: " + std::to_string(dimension);
    return;
  }

  Type *tp = &ty;
  for (int i = 0; i < dimension; ++i) {
    tp->prim = Array;
    tp->len = read_number();
    tp->ptr = new (arena) Type;
    tp = tp->ptr;
  }

  if (consume("$$C")) {
    if (consume("B"))
      ty.sclass = Const;
    else if (consume("C") || consume("D"))
      ty.sclass = Const | Volatile;
    else if (!consume("A"))
      error = "unkonwn storage class: " + input.str();
  }

  read_var_type(*tp);
}

Type * Demangler::read_params() {
  Type *backref[10];
  size_t idx = 0;

  Type *head = nullptr;
  Type **tp = &head;
  while (error.empty() && !input.startswith('@') && !input.startswith('Z')) {
    if (input.startswith_digit()) {
      int n = input.p[0] - '0';
      if (n >= idx) {
        error = "invalid backreference: " + input.str();
        return nullptr;
      }
      input.trim(1);

      *tp = new (arena) Type(*backref[n]);
      tp = &(*tp)->next;
      (*tp)->next = nullptr;
      continue;
    }

    size_t len = input.len;

    *tp = new (arena) Type;
    read_var_type(**tp);
    if (idx <= 9 && input.len - len > 1)
      backref[idx++] = *tp;
    tp = &(*tp)->next;
  }
  return head;
}

// Converts an AST to a string.
//
// Converting an AST representing a C++ type to a string is tricky due
// to the bad grammar of the C++ declaration inherited from C. You have
// to construct a string from inside to outside. For example, if a type
// X is a pointer to a function returning int, the order you create a
// string becomes something like this:
//
//   (1) X is a pointer: *X
//   (2) (1) is a function returning int: int (*X)()
//
// So you cannot construct a result just by appending strings to a result.
//
// To deal with this, we split the function into two. write_pre() writes
// the "first half" of type declaration, and write_post() writes the
// "second half". For example, write_pre() writes a return type for a
// function and write_post() writes an parameter list.
std::string Demangler::str() {
  write_pre(type);
  write_name(symbol);
  write_post(type);
  return os.str();
}

// Write the "first half" of a given type.
void Demangler::write_pre(Type &ty) {
  switch (ty.prim) {
  case Unknown:
  case None:
    break;
  case Function:
    write_pre(*ty.ptr);
    return;
  case Ptr:
  case Ref:
    write_pre(*ty.ptr);

    // "[]" and "()" (for function parameters) take precedence over "*",
    // so "int *x(int)" means "x is a function returning int *". We need
    // parentheses to supercede the default precedence. (e.g. we want to
    // emit something like "int (*x)(int)".)
    if (ty.ptr->prim == Function || ty.ptr->prim == Array)
      os << "(";

    if (ty.prim == Ptr)
      os << "*";
    else
      os << "&";
    break;
  case Array:
    write_pre(*ty.ptr);
    break;
  case Struct:
    os << "struct ";
    write_name(ty.name);
    break;
  case Union:
    os << "union ";
    write_name(ty.name);
    break;
  case Class:
    os << "class ";
    write_name(ty.name);
    break;
  case Enum:
    os << "enum ";
    write_name(ty.name);
    break;
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
  }

  if (ty.sclass & Const) {
    write_space();
    os << "const";
  }
}

// Write the "second half" of a given type.
void Demangler::write_post(Type &ty) {
  if (ty.prim == Function) {
    os << "(";
    write_params(ty.params);
    os << ")";
    if (ty.sclass & Const)
      os << "const";
    return;
  }

  if (ty.prim == Ptr || ty.prim == Ref) {
    if (ty.ptr->prim == Function || ty.ptr->prim == Array)
      os << ")";
    write_post(*ty.ptr);
    return;
  }

  if (ty.prim == Array) {
    os << "[" << ty.len << "]";
    write_post(*ty.ptr);
  }
}

// Write a function or template parameter list.
void Demangler::write_params(Type *params) {
  for (Type *tp = params; tp; tp = tp->next) {
    if (tp != params)
      os << ",";
    write_pre(*tp);
    write_post(*tp);
  }
}

// Write a name read by read_name().
void Demangler::write_name(Name *name) {
  if (!name)
    return;
  write_space();

  for (; name->next; name = name->next) {
    os << name->str;
    write_tmpl_params(name);
    os << "::";
  }

  // ?0 and ?1 are special names for ctors and dtors.
  if (name->str.startswith("?0")) {
    String s = name->str.substr(2);
    os << s;
    write_params(name->params);
    os << "::" << s;
  } else if (name->str.startswith("?1")) {
    String s = name->str.substr(2);
    os << s;
    write_params(name->params);
    os << "::~" << s;
  } else {
    os << name->str;
    write_tmpl_params(name);
  }
}

void Demangler::write_tmpl_params(Name *name) {
  if (!name->params)
    return;
  os << "<";
  write_params(name->params);
  os << ">";
}

// Writes a space if the last token does not end with a punctuation.
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

  Demangler demangler({argv[1], strlen(argv[1])});
  demangler.parse();
  if (!demangler.error.empty()) {
    std::cerr << demangler.error << "\n";
    return 1;
  }

  std::cout << demangler.str() << '\n';
  return 0;
}
