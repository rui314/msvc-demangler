//===- ItaniumDemangle.cpp ------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <numeric>
#include <string>
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
  t_void, t_bool, t_char, t_schar, t_uchar, t_short, t_ushort,
  t_int, t_uint, t_long, t_ulong, t_llong, t_ullong, t_wchar,
      t_float, t_double, t_ldouble, t_m64, t_m128, t_m128d, t_m128i,
  t_m256, t_m256d, t_m256i, t_m512, t_m512d, t_m512i, t_varargs,
} prim_type;

struct type {
  prim_type ty;
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

static std::string to_string(type &t) {
  if (t.is_function)
    return "<function>";

  switch (t.ty) {
    case t_void: return "void " + t.name.str();
    case t_bool: return "bool " + t.name.str();
    case t_char: return "char " + t.name.str();
    case t_schar: return "schar " + t.name.str();
    case t_uchar: return "uchar " + t.name.str();
    case t_short: return "short " + t.name.str();
    case t_ushort: return "ushort " + t.name.str();
    case t_int: return "int " + t.name.str();
    case t_uint: return "uint " + t.name.str();
    case t_long: return "long " + t.name.str();
    case t_ulong: return "ulong " + t.name.str();
    case t_llong: return "llong " + t.name.str();
    case t_ullong: return "ullong " + t.name.str();
    case t_wchar: return "wchar " + t.name.str();
    case t_float: return "float " + t.name.str();
    case t_double: return "double " + t.name.str();
    case t_ldouble: return "ldouble " + t.name.str();
    case t_m64: return "m64 " + t.name.str();
    case t_m128: return "m128 " + t.name.str();
    case t_m128d: return "m128d " + t.name.str();
    case t_m128i: return "m128i " + t.name.str();
    case t_m256: return "m256 " + t.name.str();
    case t_m256d: return "m256d " + t.name.str();
    case t_m256i: return "m256i " + t.name.str();
    case t_m512: return "m512 " + t.name.str();
    case t_m512d: return "m512d " + t.name.str();
    case t_m512i: return "m512i " + t.name.str();
    case t_varargs: return "... " + t.name.str();
  }
  return "";
}

static std::pair<prim_type, string> parse_type(string s) {
  if (s.startswith("X"))
    return {t_void, s.substr(strlen("X"))};
  if (s.startswith("_N"))
    return {t_bool , s.substr(strlen("_N"))};
  if (s.startswith("D"))
    return {t_char , s.substr(strlen("D"))};
  if (s.startswith("C"))
    return {t_schar , s.substr(strlen("C"))};
  if (s.startswith("E"))
    return {t_uchar , s.substr(strlen("E"))};
  if (s.startswith("F"))
    return {t_short , s.substr(strlen("F"))};
  if (s.startswith("int"))
    return {t_ushort , s.substr(strlen("int"))};
  if (s.startswith("H"))
    return {t_int , s.substr(strlen("H"))};
  if (s.startswith("I"))
    return {t_uint , s.substr(strlen("I"))};
  if (s.startswith("J"))
    return {t_long , s.substr(strlen("J"))};
  if (s.startswith("int"))
    return {t_ulong , s.substr(strlen("int"))};
  if (s.startswith("_J"))
    return {t_llong , s.substr(strlen("_J"))};
  if (s.startswith("_K"))
    return {t_ullong , s.substr(strlen("_K"))};
  if (s.startswith("_W"))
    return {t_wchar , s.substr(strlen("_W"))};
  if (s.startswith("M"))
    return {t_float , s.substr(strlen("M"))};
  if (s.startswith("N"))
    return {t_double , s.substr(strlen("N"))};
  if (s.startswith("ldouble"))
    return {t_ldouble, s.substr(strlen("ldouble"))};
  if (s.startswith("T__m64@@"))
    return {t_m64 , s.substr(strlen("T__m64@@"))};
  if (s.startswith("T__m128@@"))
    return {t_m128 , s.substr(strlen("T__m128@@"))};
  if (s.startswith("U__m128d@@"))
    return {t_m128d , s.substr(strlen("U__m128d@@"))};
  if (s.startswith("T__m128i@@"))
    return {t_m128i , s.substr(strlen("T__m128i@@"))};
  if (s.startswith("T__m256@@"))
    return {t_m256 , s.substr(strlen("T__m256@@"))};
  if (s.startswith("U__m256d@@"))
    return {t_m256d , s.substr(strlen("U__m256d@@"))};
  if (s.startswith("T__m256i@@"))
    return {t_m256i , s.substr(strlen("T__m256i@@"))};
  if (s.startswith("T__m512@@"))
    return {t_m512 , s.substr(strlen("T__m512@@"))};
  if (s.startswith("U__m512d@@"))
    return {t_m512d , s.substr(strlen("U__m512d@@"))};
  if (s.startswith("T__m512i@@"))
    return {t_m512i , s.substr(strlen("T__m512i@@"))};
  if (s.startswith("Z"))
    return {t_varargs , s.substr(strlen("Z"))};
  return {t_void, {}};
}

static type parse_after_atsign(string s) {
  if (s.startswith("3")) {
    s = s.substr(1);

    prim_type primty;
    std::tie(primty, s) = parse_type(s);

    type t = {};
    t.ty = primty;
    return t;
  }
  return {};
}

std::string demangle(string s, error &err) {
  if (!s.startswith("?"))
    return s.str();

  ssize_t name_len = s.find("@@");
  if (name_len < 0) {
    err = error::bad;
    return "";
  }

  string name = s.substr(1, name_len);
  string rest = s.substr(name_len + 2);
  type t = parse_after_atsign(rest);
  t.name = name;

  return to_string(t);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cout << argv[0] << " <symbol>\n";
    exit(1);
  }

  string symbol = {argv[1], argv[1] + strlen(argv[1])};
  error e;
  std::cout << demangle(symbol, e) << '\n';

  if (e != error::ok)
    std::cerr << to_string(e) << "\n";
  return 0;
}
