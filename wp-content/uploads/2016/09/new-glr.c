                                                                    -*- C -*-

# GLR skeleton for Bison

# Copyright (C) 2002-2015 Free Software Foundation, Inc.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

m4_include(b4_pkgdatadir/[c.m4])

## ---------------- ##
## Default values.  ##
## ---------------- ##

# Stack parameters.
m4_define_default([b4_stack_depth_max], [10000])
m4_define_default([b4_stack_depth_init],  [200])



## ------------------------ ##
## Pure/impure interfaces.  ##
## ------------------------ ##

b4_define_flag_if([pure])
# If glr.cc is including this file and thus has already set b4_pure_flag,
# do not change the value of b4_pure_flag, and do not record a use of api.pure.
m4_ifndef([b4_pure_flag],
[b4_percent_define_default([[api.pure]], [[false]])
 m4_define([b4_pure_flag],
           [b4_percent_define_flag_if([[api.pure]], [[1]], [[0]])])])

# b4_user_formals
# ---------------
# The possible parse-params formal arguments preceded by a comma.
#
# This is not shared with yacc.c in c.m4 because  GLR relies on ISO C
# formal argument declarations.
m4_define([b4_user_formals],
[m4_ifset([b4_parse_param], [, b4_formals(b4_parse_param)])])


# b4_yyerror_args
# ---------------
# Optional effective arguments passed to yyerror: user args plus yylloc, and
# a trailing comma.
m4_define([b4_yyerror_args],
[b4_pure_if([b4_locations_if([yylocp, ])])dnl
m4_ifset([b4_parse_param], [b4_args(b4_parse_param), ])])


# b4_lyyerror_args
# ----------------
# Same as above, but on the lookahead, hence &yylloc instead of yylocp.
m4_define([b4_lyyerror_args],
[b4_pure_if([b4_locations_if([&yylloc, ])])dnl
m4_ifset([b4_parse_param], [b4_args(b4_parse_param), ])])


# b4_pure_args
# ------------
# Same as b4_yyerror_args, but with a leading comma.
m4_define([b4_pure_args],
[b4_pure_if([b4_locations_if([, yylocp])])[]b4_user_args])


# b4_lpure_args
# -------------
# Same as above, but on the lookahead, hence &yylloc instead of yylocp.
m4_define([b4_lpure_args],
[b4_pure_if([b4_locations_if([, &yylloc])])[]b4_user_args])



# b4_pure_formals
# ---------------
# Arguments passed to yyerror: user formals plus yylocp with leading comma.
m4_define([b4_pure_formals],
[b4_pure_if([b4_locations_if([, YYLTYPE *yylocp])])[]b4_user_formals])


# b4_locuser_formals(LOC = yylocp)
# --------------------------------
m4_define([b4_locuser_formals],
[b4_locations_if([, YYLTYPE *m4_default([$1], [yylocp])])[]b4_user_formals])


# b4_locuser_args(LOC = yylocp)
# -----------------------------
m4_define([b4_locuser_args],
[b4_locations_if([, m4_default([$1], [yylocp])])[]b4_user_args])



## ----------------- ##
## Semantic Values.  ##
## ----------------- ##


# b4_lhs_value([TYPE])
# --------------------
# Expansion of $<TYPE>$.
m4_define([b4_lhs_value],
[b4_symbol_value([(*yyvalp)], [$1])])


# b4_rhs_data(RULE-LENGTH, NUM)
# -----------------------------
# Expand to the semantic stack place that contains value and location
# of symbol number NUM in a rule of length RULE-LENGTH.
m4_define([b4_rhs_data],
[((yyGLRStackItem const *)yyvsp)@{YYFILL (b4_subtract([$2], [$1]))@}.yystate])


# b4_rhs_value(RULE-LENGTH, NUM, [TYPE])
# --------------------------------------
# Expansion of $<TYPE>NUM, where the current rule has RULE-LENGTH
# symbols on RHS.
m4_define([b4_rhs_value],
[b4_symbol_value([b4_rhs_data([$1], [$2]).yysemantics.yysval], [$3])])



## ----------- ##
## Locations.  ##
## ----------- ##

# b4_lhs_location()
# -----------------
# Expansion of @$.
m4_define([b4_lhs_location],
[(*yylocp)])


# b4_rhs_location(RULE-LENGTH, NUM)
# ---------------------------------
# Expansion of @NUM, where the current rule has RULE-LENGTH symbols
# on RHS.
m4_define([b4_rhs_location],
[(b4_rhs_data([$1], [$2]).yyloc)])


## -------------- ##
## Declarations.  ##
## -------------- ##

# b4_shared_declarations
# ----------------------
# Declaration that might either go into the header (if --defines)
# or open coded in the parser body.  glr.cc has its own definition.
m4_define([b4_shared_declarations],
[b4_declare_yydebug[
]b4_percent_code_get([[requires]])[
]b4_token_enums[
]b4_declare_yylstype[
]b4_function_declare(b4_prefix[parse], [int], b4_parse_param)[
]b4_percent_code_get([[provides]])[]dnl
])

## -------------- ##
## Output files.  ##
## -------------- ##

# Unfortunately the order of generation between the header and the
# implementation file matters (for glr.c) because of the current
# implementation of api.value.type=union.  In that case we still use a
# union for YYSTYPE, but we generate the contents of this union when
# setting up YYSTYPE.  This is needed for other aspects, such as
# defining yy_symbol_value_print, since we need to now the name of the
# members of this union.
#
# To avoid this issue, just generate the header before the
# implementation file.  But we should also make them more independant.

# ----------------- #
# The header file.  #
# ----------------- #

b4_output_begin([b4_spec_defines_file])
b4_copyright([Skeleton interface for Bison GLR parsers in C],
             [2002-2015])[

]b4_cpp_guard_open([b4_spec_defines_file])[
]b4_shared_declarations[
]b4_cpp_guard_close([b4_spec_defines_file])[
]b4_output_end()

# ------------------------- #
# The implementation file.  #
# ------------------------- #

b4_output_begin([b4_parser_file_name])
b4_copyright([Skeleton implementation for Bison GLR parsers in C],
             [2002-2015])[

/* C GLR parser skeleton written by Paul Hilfinger.  */

]b4_identification

b4_percent_code_get([[top]])[
]m4_if(b4_api_prefix, [yy], [],
[[/* Substitute the type names.  */
#define YYSTYPE ]b4_api_PREFIX[STYPE]b4_locations_if([[
#define YYLTYPE ]b4_api_PREFIX[LTYPE]])])[
]m4_if(b4_prefix, [yy], [],
[[/* Substitute the variable and function names.  */
#define yyparse ]b4_prefix[parse
#define yylex   ]b4_prefix[lex
#define yyerror ]b4_prefix[error
#define yydebug ]b4_prefix[debug
]]b4_pure_if([], [[
#define yylval  ]b4_prefix[lval
#define yychar  ]b4_prefix[char
#define yynerrs ]b4_prefix[nerrs]b4_locations_if([[
#define yylloc  ]b4_prefix[lloc]])]))[

/* First part of user declarations.  */
]b4_user_pre_prologue[

]b4_null_define[

]b4_defines_if([[#include "@basename(]b4_spec_defines_file[@)"]],
               [b4_shared_declarations])[

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE ]b4_error_verbose_if([1], [0])[
#endif

/* Default (constant) value used for initialization for null
   right-hand sides.  Unlike the standard yacc.c template, here we set
   the default value of $$ to a zeroed-out value.  Since the default
   value is undefined, this behavior is technically correct.  */
static YYSTYPE yyval_default;]b4_locations_if([[
static YYLTYPE yyloc_default][]b4_yyloc_default;])[

/* Copy the second part of user declarations.  */
]b4_user_post_prologue
b4_percent_code_get[]dnl

[#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YYFREE
# define YYFREE free
#endif
#ifndef YYMALLOC
# define YYMALLOC malloc
#endif
#ifndef YYREALLOC
# define YYREALLOC realloc
#endif

#define YYSIZEMAX ((size_t) -1)

#ifdef __cplusplus
   typedef bool yybool;
#else
   typedef unsigned char yybool;
#endif
#define yytrue 1
#define yyfalse 0

#ifndef YYSETJMP
# include <setjmp.h>
# define YYJMP_BUF jmp_buf
# define YYSETJMP(Env) setjmp (Env)
/* Pacify clang.  */
# define YYLONGJMP(Env, Val) (longjmp (Env, Val), YYASSERT (0))
#endif

]b4_attribute_define[

#ifndef YYASSERT
# define YYASSERT(Condition) ((void) ((Condition) || (abort (), 0)))
#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  ]b4_final_state_number[
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   ]b4_last[

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  ]b4_tokens_number[
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  ]b4_nterms_number[
/* YYNRULES -- Number of rules.  */
#define YYNRULES  ]b4_rules_number[
/* YYNRULES -- Number of states.  */
#define YYNSTATES  ]b4_states_number[
/* YYMAXRHS -- Maximum number of symbols on right-hand side of rule.  */
#define YYMAXRHS ]b4_r2_max[
/* YYMAXLEFT -- Maximum number of symbols to the left of a handle
   accessed by $0, $-1, etc., in any rule.  */
#define YYMAXLEFT ]b4_max_left_semantic_context[

/* YYTRANSLATE(X) -- Bison symbol number corresponding to X.  */
#define YYUNDEFTOK  ]b4_undef_token_number[
#define YYMAXUTOK   ]b4_user_token_number_max[

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const ]b4_int_type_for([b4_translate])[ yytranslate[] =
{
  ]b4_translate[
};

#if ]b4_api_PREFIX[DEBUG || YYERROR_VERBOSE || ]b4_token_table_flag[
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  ]b4_tname[
};
#endif

#define YYPACT_NINF ]b4_pact_ninf[
#define YYTABLE_NINF ]b4_table_ninf[

]b4_parser_tables_define[

/* YYDPREC[RULE-NUM] -- Dynamic precedence of rule #RULE-NUM (0 if none).  */
static const ]b4_int_type_for([b4_dprec])[ yydprec[] =
{
  ]b4_dprec[
};

/* YYMERGER[RULE-NUM] -- Index of merging function for rule #RULE-NUM.  */
static const ]b4_int_type_for([b4_merger])[ yymerger[] =
{
  ]b4_merger[
};

/* YYCONFLP[YYPACT[STATE-NUM]] -- Pointer into YYCONFL of start of
   list of conflicting reductions corresponding to action entry for
   state STATE-NUM in yytable.  0 means no conflicts.  The list in
   yyconfl is terminated by a rule number of 0.  */
static const ]b4_int_type_for([b4_conflict_list_heads])[ yyconflp[] =
{
  ]b4_conflict_list_heads[
};

/* YYCONFL[I] -- lists of conflicting rule numbers, each terminated by
   0, pointed into by YYCONFLP.  */
]dnl Do not use b4_int_type_for here, since there are places where
dnl pointers onto yyconfl are taken, whose type is "short int *".
dnl We probably ought to introduce a type for confl.
[static const short int yyconfl[] =
{
  ]b4_conflicting_rules[
};

/* Error token number */
#define YYTERROR 1

]b4_locations_if([[
]b4_yylloc_default_define[
# define YYRHSLOC(Rhs, K) ((Rhs)[K].yystate.yyloc)
]])[

]b4_pure_if(
[
#undef yynerrs
#define yynerrs (yystackp->yyerrcnt)
#undef yychar
#define yychar (yystackp->yyrawchar)
#undef yylval
#define yylval (yystackp->yyval)
#undef yylloc
#define yylloc (yystackp->yyloc)
m4_if(b4_prefix[], [yy], [],
[#define b4_prefix[]nerrs yynerrs
#define b4_prefix[]char yychar
#define b4_prefix[]lval yylval
#define b4_prefix[]lloc yylloc])],
[YYSTYPE yylval;]b4_locations_if([[
YYLTYPE yylloc;]])[

int yynerrs;
int yychar;])[

static const int YYEOF = 0;
static const int YYEMPTY = -2;

typedef enum { yyok, yyaccept, yyabort, yyerr } YYRESULTTAG;

#define YYCHK(YYE)                              \
  do {                                          \
    YYRESULTTAG yychk_flag = YYE;               \
    if (yychk_flag != yyok)                     \
      return yychk_flag;                        \
  } while (0)

#if ]b4_api_PREFIX[DEBUG

# ifndef YYFPRINTF
#  define YYFPRINTF fprintf
# endif

]b4_yy_location_print_define[

# define YYDPRINTF(Args)                        \
  do {                                          \
    if (yydebug)                                \
      YYFPRINTF Args;                           \
  } while (0)

]b4_yy_symbol_print_define[

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                  \
  do {                                                                  \
    if (yydebug)                                                        \
      {                                                                 \
        YYFPRINTF (stderr, "%s ", Title);                               \
        yy_symbol_print (stderr, Type, Value]b4_locuser_args([Location])[);        \
        YYFPRINTF (stderr, "\n");                                       \
      }                                                                 \
  } while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;

struct yyGLRStack;
static void yypstack (struct yyGLRStack* yystackp, size_t yyk)
  YY_ATTRIBUTE_UNUSED;
static void yypdumpstack (struct yyGLRStack* yystackp)
  YY_ATTRIBUTE_UNUSED;

#else /* !]b4_api_PREFIX[DEBUG */

# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)

#endif /* !]b4_api_PREFIX[DEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH ]b4_stack_depth_init[
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYMAXDEPTH * sizeof (GLRStackItem)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH ]b4_stack_depth_max[
#endif

/* Minimum number of free items on the stack allowed after an
   allocation.  This is to allow allocation and initialization
   to be completed by functions that call yyexpandGLRStack before the
   stack is expanded, thus insuring that all necessary pointers get
   properly redirected to new data.  */
#define YYHEADROOM 2

#if YYERROR_VERBOSE

#endif /* !YYERROR_VERBOSE */

/** State numbers, as in LALR(1) machine */
typedef int yyStateNum;

/** Rule numbers, as in LALR(1) machine */
typedef int yyRuleNum;

/** Grammar symbol */
typedef int yySymbol;

/** Item references, as in LALR(1) machine */
typedef short int yyItemNum;

typedef struct yyGLRState yyGLRState;
typedef struct yyGLRStateSet yyGLRStateSet;
typedef struct yySemanticOption yySemanticOption;
typedef union yyGLRStackItem yyGLRStackItem;
typedef struct yyGLRStack yyGLRStack;

struct yyGLRState {
  /** Type tag: always true.  */
  yybool yyisState;
  /** Type tag for yysemantics.  If true, yysval applies, otherwise
   *  yyfirstVal applies.  */
  yybool yyresolved;
  /** Number of corresponding LALR(1) machine state.  */
  yyStateNum yylrState;
  /** Preceding state in this stack */
  yyGLRState* yypred;
  /** Source position of the last token produced by my symbol */
  size_t yyposn;
  union {
    /** First in a chain of alternative reductions producing the
     *  non-terminal corresponding to this state, threaded through
     *  yynext.  */
    yySemanticOption* yyfirstVal;
    /** Semantic value for this state.  */
    YYSTYPE yysval;
  } yysemantics;]b4_locations_if([[
  /** Source location for this state.  */
  YYLTYPE yyloc;]])[
};

struct yyGLRStateSet {
  yyGLRState** yystates;
  /** During nondeterministic operation, yylookaheadNeeds tracks which
   *  stacks have actually needed the current lookahead.  During deterministic
   *  operation, yylookaheadNeeds[0] is not maintained since it would merely
   *  duplicate yychar != YYEMPTY.  */
  yybool* yylookaheadNeeds;
  size_t yysize, yycapacity;
};

struct yySemanticOption {
  /** Type tag: always false.  */
  yybool yyisState;
  /** Rule number for this reduction */
  yyRuleNum yyrule;
  /** The last RHS state in the list of states to be reduced.  */
  yyGLRState* yystate;
  /** The lookahead for this reduction.  */
  int yyrawchar;
  YYSTYPE yyval;]b4_locations_if([[
  YYLTYPE yyloc;]])[
  /** Next sibling in chain of options.  To facilitate merging,
   *  options are chained in decreasing order by address.  */
  yySemanticOption* yynext;
};

/** Type of the items in the GLR stack.  The yyisState field
 *  indicates which item of the union is valid.  */
union yyGLRStackItem {
  yyGLRState yystate;
  yySemanticOption yyoption;
};

struct yyGLRStack {
  int yyerrState;
]b4_locations_if([[  /* To compute the location of the error token.  */
  yyGLRStackItem yyerror_range[3];]])[
]b4_pure_if(
[
  int yyerrcnt;
  int yyrawchar;
  YYSTYPE yyval;]b4_locations_if([[
  YYLTYPE yyloc;]])[
])[
  YYJMP_BUF yyexception_buffer;
  yyGLRStackItem* yyitems;
  yyGLRStackItem* yynextFree;
  size_t yyspaceLeft;
  yyGLRState* yysplitPoint;
  yyGLRState* yylastDeleted;
  yyGLRStateSet yytops;
};

#if YYSTACKEXPANDABLE
static void yyexpandGLRStack (yyGLRStack* yystackp);
#endif

#if ]b4_api_PREFIX[DEBUG || YYERROR_VERBOSE
/** A printable representation of TOKEN.  */
static inline const char*
yytokenName (yySymbol yytoken)
{
  if (yytoken == YYEMPTY)
    return "";

  return yytname[yytoken];
}
#endif

/** Fill in YYVSP[YYLOW1 .. YYLOW0-1] from the chain of states starting
 *  at YYVSP[YYLOW0].yystate.yypred.  Leaves YYVSP[YYLOW1].yystate.yypred
 *  containing the pointer to the next state in the chain.  */
static void yyfillin (yyGLRStackItem *, int, int) YY_ATTRIBUTE_UNUSED;
static void
yyfillin (yyGLRStackItem *yyvsp, int yylow0, int yylow1)
{
  int i;
  yyGLRState *s = yyvsp[yylow0].yystate.yypred;
  for (i = yylow0-1; i >= yylow1; i -= 1)
    {
#if ]b4_api_PREFIX[DEBUG
      yyvsp[i].yystate.yylrState = s->yylrState;
#endif
      yyvsp[i].yystate.yyresolved = s->yyresolved;
      if (s->yyresolved)
        yyvsp[i].yystate.yysemantics.yysval = s->yysemantics.yysval;
      else
        /* The effect of using yysval or yyloc (in an immediate rule) is
         * undefined.  */
        yyvsp[i].yystate.yysemantics.yyfirstVal = YY_NULLPTR;]b4_locations_if([[
      yyvsp[i].yystate.yyloc = s->yyloc;]])[
      s = yyvsp[i].yystate.yypred = s->yypred;
    }
}

/* Do nothing if YYNORMAL or if *YYLOW <= YYLOW1.  Otherwise, fill in
 * YYVSP[YYLOW1 .. *YYLOW-1] as in yyfillin and set *YYLOW = YYLOW1.
 * For convenience, always return YYLOW1.  */
static inline int yyfill (yyGLRStackItem *, int *, int, yybool)
     YY_ATTRIBUTE_UNUSED;
static inline int
yyfill (yyGLRStackItem *yyvsp, int *yylow, int yylow1, yybool yynormal)
{
  if (!yynormal && yylow1 < *yylow)
    {
      yyfillin (yyvsp, *yylow, yylow1);
      *yylow = yylow1;
    }
  return yylow1;
}

static void
yyuserMerge (int yyn, YYSTYPE* yy0, YYSTYPE* yy1)
{
  YYUSE (yy0);
  YYUSE (yy1);

  switch (yyn)
    {
]b4_mergers[
      default: break;
    }
}

#if YYERROR_VERBOSE

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static size_t
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      size_t yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return strlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

#endif /* !YYERROR_VERBOSE */

                              /* Bison grammar-table manipulation.  */


/** Number of symbols composing the right hand side of rule #RULE.  */
static inline int
yyrhsLength (yyRuleNum yyrule)
{
  return yyr2[yyrule];
}

/** Left-hand-side symbol for rule #YYRULE.  */
static inline yySymbol
yylhsNonterm (yyRuleNum yyrule)
{
  return yyr1[yyrule];
}

#define yypact_value_is_default(Yystate) \
  ]b4_table_value_equals([[pact]], [[Yystate]], [b4_pact_ninf])[

/** True iff LR state YYSTATE has only a default reduction (regardless
 *  of token).  */
static inline yybool
yyisDefaultedState (yyStateNum yystate)
{
  return yypact_value_is_default (yypact[yystate]);
}

/** The default reduction for YYSTATE, assuming it has one.  */
static inline yyRuleNum
yydefaultAction (yyStateNum yystate)
{
  return yydefact[yystate];
}

#define yytable_value_is_error(Yytable_value) \
  ]b4_table_value_equals([[table]], [[Yytable_value]], [b4_table_ninf])[

/** Set *YYACTION to the action to take in YYSTATE on seeing YYTOKEN.
 *  Result R means
 *    R < 0:  Reduce on rule -R.
 *    R = 0:  Error.
 *    R > 0:  Shift to state R.
 *  Set *YYCONFLICTS to a pointer into yyconfl to a 0-terminated list
 *  of conflicting reductions.
 */
static inline void
yygetLRActions (yyStateNum yystate, int yytoken,
                int* yyaction, const short int** yyconflicts)
{
  int yyindex = yypact[yystate] + yytoken;
  if (yypact_value_is_default (yypact[yystate])
      || yyindex < 0 || YYLAST < yyindex || yycheck[yyindex] != yytoken)
    {
      *yyaction = -yydefact[yystate];
      *yyconflicts = yyconfl;
    }
  else if (! yytable_value_is_error (yytable[yyindex]))
    {
      *yyaction = yytable[yyindex];
      *yyconflicts = yyconfl + yyconflp[yyindex];
    }
  else
    {
      *yyaction = 0;
      *yyconflicts = yyconfl + yyconflp[yyindex];
    }
}

/** Compute post-reduction state.
 * \param yystate   the current state
 * \param yysym     the nonterminal to push on the stack
 */
static inline yyStateNum
yyLRgotoState (yyStateNum yystate, yySymbol yysym)
{
  int yyr = yypgoto[yysym - YYNTOKENS] + yystate;
  if (0 <= yyr && yyr <= YYLAST && yycheck[yyr] == yystate)
    return yytable[yyr];
  else
    return yydefgoto[yysym - YYNTOKENS];
}

static inline yybool
yyisShiftAction (int yyaction)
{
  return 0 < yyaction;
}

static inline yybool
yyisErrorAction (int yyaction)
{
  return yyaction == 0;
}

#define YYCHK1(YYE)                                                          \
  do {                                                                       \
    switch (YYE) {                                                           \
    case yyok:                                                               \
      break;                                                                 \
    case yyabort:                                                            \
      goto yyabortlab;                                                       \
    case yyaccept:                                                           \
      goto yyacceptlab;                                                      \
    case yyerr:                                                              \
      goto yyuser_error;                                                     \
    default:                                                                 \
      goto yybuglab;                                                         \
    }                                                                        \
  } while (0)

/*----------.
| yyparse.  |
`----------*/

#define CUSTOM_POOL(pool_name, data_type, pool_size) \
typedef union pool_name##_pool_item_tag pool_name##_pool_item_t; \
\
union pool_name##_pool_item_tag \
{ \
    data_type data; \
    int next; \
}; \
 \
typedef struct pool_name##_pool_tag pool_name##_pool_t; \
 \
struct pool_name##_pool_tag \
{ \
    int head; \
    int num_free_items; \
    int num_initialized; \
 \
    pool_name##_pool_item_t chunk[pool_size]; \
}; \
 \
static pool_name##_pool_t pool_name##_pool; \
 \
static inline void pool_name##_pool_init(void) \
{ \
    pool_name##_pool.head = 0; \
    pool_name##_pool.num_free_items = pool_size; \
    pool_name##_pool.num_initialized = 0; \
} \
 \
static inline data_type* pool_name##_pool_allocate(void) \
{ \
    if (pool_name##_pool.num_free_items == 0) \
        return NULL; \
 \
    if (pool_name##_pool.num_initialized < pool_size) \
    { \
        pool_name##_pool.chunk[pool_name##_pool.num_initialized].next \
            = pool_name##_pool.num_initialized + 1; \
        pool_name##_pool.num_initialized++; \
    } \
 \
    data_type* res = &(pool_name##_pool.chunk[pool_name##_pool.head].data); \
 \
    pool_name##_pool.num_free_items--; \
    if (pool_name##_pool.num_free_items > 0) \
    { \
        pool_name##_pool.head = pool_name##_pool.chunk[pool_name##_pool.head].next; \
    } \
    else \
    { \
        pool_name##_pool.head = -1; \
    } \
 \
    return res; \
} \
 \
static inline void pool_name##_pool_deallocate(data_type* p) \
{ \
    /* strict aliasing issues here? */ \
    pool_name##_pool_item_t* q = (pool_name##_pool_item_t*)p; \
    if (pool_name##_pool.head != -1) \
    { \
        q->next = pool_name##_pool.head; \
    } \
    else \
    { \
        q->next = pool_size; \
    } \
 \
    pool_name##_pool.head = q - pool_name##_pool.chunk; \
    pool_name##_pool.num_free_items++; \
}


enum {
    // Pool sizes
    STACK_POOL_SIZE = 128,
    STACK_NODE_EDGE_POOL_SIZE = 128,
    PAYLOAD_POOL_SIZE = 2048,

    // Immediate elements represented in data structures
    MAX_PAYLOAD_VALUES_IMEDIATES = 4,
    MAX_EDGES_PATH_IMEDIATES = 16,
    MAX_STACK_NODE_EDGES_IMMEDIATES = 2,
};

// A stack node represents a node of the GSS
struct stack_node_tag;
typedef struct stack_node_tag stack_node_t;

// A stack node edge represents an edge of the GSS
struct stack_node_edge_tag;
typedef struct stack_node_edge_tag stack_node_edge_t;

typedef struct ref_count_tag refcount_t;
struct ref_count_tag
{
    int refs;
    void (*walk)(void *, void (*)(void*));
    void (*destroy)(void*);
};

static inline void decref_(void *p)
{
    if (p == NULL)
        return;

    refcount_t* r = (refcount_t*)p;
    if (0 == --(r->refs))
    {
        (r->walk)(p, decref_);
        (r->destroy)(p);
    }
}

#define incref(p) ({ __typeof__((p)) _t = (p); _t->_ref.refs++; _t; })
#define decref(p) ({ __typeof__((p)) _t = (p); decref_(&(_t->_ref));  })

// p = q; but updating references
// #define assignref(p, q) ({ __typeof__(p) _p = p; decref(_p); __typeof__(q) _q = q; if (_q != NULL) incref(_q); _p = _q; })

#define newrefcounted(type) ({ \
        type *_t = YYMALLOC(sizeof(type)); \
        _t->_ref.refs = 1; \
        _t->_ref.walk = walk_##type; \
        _t->_ref.destroy = destroy_##type; \
        _t; })

// A stack node represents a state and a
// set of predecessors in the GSS. These
// predecessors are accessible through
// the edges of this node
struct stack_node_tag
{
    /* private */ refcount_t _ref;

    yyStateNum state;

    // predecessors in the GSS
    int num_preds;
    stack_node_edge_t **preds;
    stack_node_edge_t *preds_a[MAX_STACK_NODE_EDGES_IMMEDIATES];
};


// Payload of an edge
typedef struct payload_tag payload_t;

// Two kinds of payloads for edges: those that immediately encode the
// semantic value and those that defer the semantic value and only encode
// the arguments and the rule that can be used to compute the semantic value
typedef enum payload_kind_tag payload_kind_t;
enum payload_kind_tag
{
    P_INVALID = 0,
    P_DEFINITIVE_SHIFT,
    P_DEFINITIVE_REDUCE,
    P_DEFERRED_SHIFT,
    P_DEFERRED_REDUCE,
};

// Immediate payload
typedef struct payload_shift_tag payload_shift_t;
struct payload_shift_tag
{
    size_t yyposn;
    YYSTYPE yyval;
    ]b4_locations_if([YYLTYPE yyloc;])[
};

// Deferred payload
typedef struct payload_reduce_tag payload_reduce_t;
struct payload_reduce_tag
{
    yyRuleNum rule;
    YYSTYPE yyval;
    ]b4_locations_if([YYLTYPE yyloc;])[
    int num_values;
    payload_t **values;
    payload_t *values_a[MAX_PAYLOAD_VALUES_IMEDIATES];
};

// The payload of an edge
struct payload_tag
{
    /* private */ refcount_t _ref;
    payload_kind_t kind;
    payload_t *next;
    union {
        payload_shift_t s;
        payload_reduce_t r;
    };
};

static void walk_payload_t(void *p, void (*callback)(void*))
{
    if (p == NULL)
        return;

    payload_t* payload = (payload_t*)p;

    if (payload->kind == P_DEFERRED_REDUCE
            || payload->kind == P_DEFINITIVE_REDUCE)
    {
        int i;
        for (i = 0; i < payload->r.num_values; i++)
        {
            callback(payload->r.values[i]);
        }
    }

    callback(payload->next);
}

static void destroy_payload_t(void *p)
{
    payload_t* payload = (payload_t*)p;
    if ((payload->kind == P_DEFERRED_REDUCE
            || payload->kind == P_DEFINITIVE_REDUCE)
            && (payload->r.num_values >= MAX_PAYLOAD_VALUES_IMEDIATES))
    {
        YYFREE(payload->r.values);
    }
    YYFREE(payload);
}

static void empty_payload_t(payload_t* p)
{
    switch (p->kind)
    {
        case P_DEFERRED_SHIFT:
        case P_DEFINITIVE_SHIFT:
            {
                // Nothing to do
                break;
            }
        case P_DEFERRED_REDUCE:
        case P_DEFINITIVE_REDUCE:
            {
                int i;
                for (i = 0; i < p->r.num_values; i++)
                {
                    decref(p->r.values[i]);
                }
                if (p->r.num_values >= MAX_PAYLOAD_VALUES_IMEDIATES)
                {
                    YYFREE(p->r.values);
                }
                break;
            }
        default:
            YYASSERT(false);
    }
}

CUSTOM_POOL(payload, payload_t, PAYLOAD_POOL_SIZE);

static void destroy_payload_t_in_pool(void *p)
{
    payload_t* payload = (payload_t*)p;
    if ((payload->kind == P_DEFERRED_REDUCE
            || payload->kind == P_DEFINITIVE_REDUCE)
            && (payload->r.num_values >= MAX_PAYLOAD_VALUES_IMEDIATES))
    {
        YYFREE(payload->r.values);
    }
    payload_pool_deallocate(payload);
}

static inline char payload_pool_contains(payload_t* p)
{
    payload_pool_item_t* q = (payload_pool_item_t*)p;
    return (payload_pool.chunk <= q)
           && (q < (payload_pool.chunk + PAYLOAD_POOL_SIZE));
}

static inline payload_t* new_payload(payload_kind_t kind)
{
    payload_t *payload = payload_pool_allocate();
    if (payload == NULL)
    {
        payload = newrefcounted(payload_t);
    }
    else
    {
        payload->_ref.refs = 1;
        payload->_ref.walk = walk_payload_t;
        payload->_ref.destroy = destroy_payload_t_in_pool;
    }

    payload->kind = kind;
    payload->next = NULL;
    return payload;
}

static inline char payload_is_reduce(payload_t* p)
{
    return p->kind == P_DEFERRED_REDUCE
        || p->kind == P_DEFINITIVE_REDUCE;
}

static inline char payload_is_shift(payload_t* p)
{
    return p->kind == P_DEFERRED_SHIFT
        || p->kind == P_DEFINITIVE_SHIFT;
}

// A stack node edge refers to another stack node and has a payload
struct stack_node_edge_tag
{
    /* private */ refcount_t _ref;
    stack_node_t* target;

    payload_t *payload;
};

// The GSS itself, it keeps a list
// of active parsers which are themselves
// nodes of the GSS
typedef struct stack_node_set_tag stack_node_set_t;
struct stack_node_set_tag
{
    int num_active_stacks;
    stack_node_t **stacks;
};

static stack_node_set_t gss;

static inline void stack_pool_init(void);
static inline void stack_node_edge_pool_init(void);
static inline void payload_pool_init(void);

static inline void gss_init(void)
{
    gss.num_active_stacks = 0;
    gss.stacks = NULL;

    // Init pools
    stack_pool_init();
    stack_node_edge_pool_init();
    payload_pool_init();
}

static inline void gss_add_stack(stack_node_t* stack)
{
    gss.num_active_stacks++;
    gss.stacks = YYREALLOC(gss.stacks, gss.num_active_stacks * sizeof(*gss.stacks));
    // FIXME: check if memory has been exhausted
    gss.stacks[gss.num_active_stacks - 1] = stack;
}

static inline void gss_destroy(void)
{
    int i;
    for (i = 0; i < gss.num_active_stacks; i++)
    {
        decref(gss.stacks[i]);
    }
    YYFREE(gss.stacks);
}

// A path, computed when performing reductions
typedef
struct path_tag
{
    int length;
    stack_node_edge_t **edges;
    stack_node_edge_t *edges_a[MAX_EDGES_PATH_IMEDIATES];
} path_t;


typedef struct queue_item_tag queue_item_t;
struct queue_item_tag
{
    stack_node_t* stack;
    path_t path;
    int rule;
};

static void destroy_queue_item_t(queue_item_t* q)
{
    if (q->path.length >= MAX_EDGES_PATH_IMEDIATES)
    {
        YYFREE(q->path.edges);
    }
}

typedef struct path_queue_tag
{
    int capacity;
    int start;
    int end;
    queue_item_t* items;
} path_queue_t;

static path_queue_t path_queue;

static inline void path_queue_init(void)
{
    path_queue.capacity = 0;
    path_queue.start = 0;
    path_queue.end = 0;
    path_queue.items = NULL;
}

static inline bool path_queue_is_empty(void)
{
    return path_queue.start == path_queue.end;
}

static inline queue_item_t* path_queue_add(void)
{
    if (path_queue.end >= path_queue.capacity)
    {
        int previous_capacity = path_queue.capacity;
        path_queue.capacity = 2*(path_queue.capacity + 1);
        path_queue.items = YYREALLOC(path_queue.items,
                path_queue.capacity
                * sizeof(*path_queue.items));

        // Fix interior pointers
        int i;
        for (i = 0; i < previous_capacity; i++)
        {
            if (path_queue.items[i].path.length < MAX_EDGES_PATH_IMEDIATES)
            {
                path_queue.items[i].path.edges =
                    path_queue.items[i].path.edges_a;
            }
        }
    }

    queue_item_t* result = &path_queue.items[path_queue.end];
    path_queue.end++;

    return result;
}

static inline int path_queue_dequeue(void)
{
    YYASSERT(path_queue.start < path_queue.end);
    int result = path_queue.start;

    path_queue.start++;

    if (path_queue.start == path_queue.end)
    {
        // Compact an empty queue
        path_queue.start = 0;
        path_queue.end = 0;
    }

    return result;
}

static inline void path_queue_destroy(void)
{
    while (!path_queue_is_empty())
    {
        int qid = path_queue_dequeue();
        destroy_queue_item_t(&path_queue.items[qid]);
    }
    YYFREE(path_queue.items);
}

typedef struct path_predecessor_tag path_predecessor_t;

struct path_predecessor_tag
{
    path_predecessor_t* pred;
    stack_node_edge_t* edge;
};

static inline void compute_paths_rec(
        stack_node_t* stack,
        stack_node_edge_t* edge, int rule,
        int length,
        path_predecessor_t* pred,
        stack_node_edge_t* contains_link)
{
    path_predecessor_t p;
    p.pred = pred;
    p.edge = edge;

    if (length == 0)
    {
        if (contains_link != NULL)
        {
            // We have to check if the current path contains the link
            bool found = false;
            path_predecessor_t* it = &p;
            while (it != NULL)
            {
                if (it->edge == contains_link)
                {
                    found = true;
                    break;
                }

                it = it->pred;
            }
            // Do nothing
            if (!found)
                return;
        }
        // create the queue item
        queue_item_t* qi = path_queue_add();
        qi->stack = stack;
        qi->rule = rule;

        // assemble the path
        path_predecessor_t* it;

        int count = 0;
        // FIXME: count is just a constant offset of length
        for (it = &p; it != NULL; it = it->pred)
        {
            count++;
        }

        qi->path.length = count;
        if (qi->path.length < MAX_EDGES_PATH_IMEDIATES)
        {
            qi->path.edges = qi->path.edges_a;
        }
        else
        {
            qi->path.edges = YYMALLOC(qi->path.length * sizeof(*qi->path.edges));
        }

        // copy the path
        stack_node_edge_t** qi_s = qi->path.edges;
        it = &p;
        for (it = &p; it != NULL; it = it->pred, qi_s++)
        {
            *qi_s = it->edge;
        }
    }
    else
    {
        int num_preds = edge->target->num_preds;
        int i;
        for (i = 0; i < num_preds; i++)
        {
            stack_node_edge_t* pred_edge = edge->target->preds[i];
            compute_paths_rec(stack, pred_edge, rule, length - 1, &p, contains_link);
        }
    }
}


static inline void compute_paths(stack_node_t* current_stack, int rule, int length)
{
    YYDPRINTF((stderr, "Computing paths of length %d for rule r%d\n", length, rule - 1));
    YYASSERT(current_stack != NULL);
    YYASSERT(rule > 0);
    YYASSERT(length >= 0);

    if (length > 0)
    {
        int num_preds = current_stack->num_preds;
        int i;
        for (i = 0; i < num_preds; i++)
        {
            stack_node_edge_t* pred_edge = current_stack->preds[i];
            compute_paths_rec(current_stack, pred_edge, rule, length - 1, NULL, /* contains_link */ NULL);
        }
    }
    else
    {
        // Add a degenerate path
        queue_item_t* qi = path_queue_add();
        qi->stack = current_stack;
        qi->rule = rule;
        qi->path.length = 0;
        qi->path.edges = NULL;
    }
}

static inline void compute_paths_that_use_link(stack_node_t* current_stack,
        stack_node_edge_t* contains_link,
        int rule, int length)
{
    YYDPRINTF((stderr, "Computing paths of length %d for rule r%d that use a given link\n", length, rule - 1));
    YYASSERT(current_stack != NULL);
    YYASSERT(rule > 0);
    YYASSERT(length >= 0);

    // No path will be able to use that link
    if (length == 0)
        return;

    int num_preds = current_stack->num_preds;
    int i;
    for (i = 0; i < num_preds; i++)
    {
        stack_node_edge_t* pred_edge = current_stack->preds[i];
        compute_paths_rec(current_stack, pred_edge, rule, length - 1, NULL, contains_link);
    }
}

static inline int get_rule_length(int rule)
{
    return yyrhsLength(rule);
}

]

m4_undefine([b4_rhs_value])
m4_define([b4_rhs_value],
[b4_symbol_value([in_yyvalp@{$2 - 1@}], [$3])])

m4_undefine([b4_rhs_location])
m4_define([b4_rhs_location],
[in_yylocp@{$2 - 1@}])

[
static inline void
user_action (yyRuleNum yyn,
        /* out */
        YYSTYPE* yyvalp]b4_locations_if([, YYLTYPE *yylocp])[,
        /* in */
        size_t yyrhslen, 
        YYSTYPE* in_yyvalp]b4_locations_if([, YYLTYPE *in_yylocp])[
        /* user args */
        ]b4_user_formals[)
{
    YYDPRINTF((stderr, "Running user action for rule r%d\n", yyn - 1));
]b4_parse_param_use([yyvalp], [yylocp])dnl
[  YYUSE (yyrhslen);
   YYUSE(in_yyvalp);
   YYUSE(in_yylocp);
# undef yyerrok
# define yyerrok (yystackp->yyerrState = 0)
# undef YYACCEPT
# define YYACCEPT YYASSERT(0 && "YYACCEPT not supported")
# undef YYABORT
# define YYABORT YYASSERT(0 && "YYABORT not supported")
# undef YYERROR
# define YYERROR YYASSERT(0 && "YYERROR not supported")
# undef YYRECOVERING
# define YYRECOVERING() YYASSERT(0 && "YYRECOVERING not supported")
# undef yyclearin
# define yyclearin YYASSERT(0 && "yyclearin not supported")
# undef YYFILL
# define YYFILL(N) YYASSERT(0 && "YYFILL not supported")
# undef YYBACKUP
# define YYBACKUP(Token, Value) YYASSERT(0 && "YYBACKUP not supported")

  if (yyrhslen == 0)
    *yyvalp = yyval_default;
  else
    *yyvalp = yyvalp[0];
]b4_locations_if([[
  /* FIXME: compute default location */
  if (yyrhslen == 0)
  {
    *yylocp = yyloc_default;
    }
  else
  {
    *yylocp = in_yylocp[0];
    }
  /* YYLLOC_DEFAULT ((*yylocp), (yyvsp - yyrhslen), yyrhslen); */
]])[
  switch (yyn)
    {
      ]b4_user_actions[
      default: break;
    }
# undef yyerrok
# undef YYABORT
# undef YYACCEPT
# undef YYERROR
# undef YYBACKUP
# undef yyclearin
# undef YYRECOVERING
}

// FIXME: can we do this faster?
static inline stack_node_t* gss_find_stack_node(int dest_state)
{
    int i;
    for (i = 0; i < gss.num_active_stacks; i++)
    {
        stack_node_t* stack = gss.stacks[i];
        if (stack->state == dest_state)
            return stack;
    }
    return NULL;
}


#if ]b4_api_PREFIX[DEBUG

static inline void print_payload(payload_t* p);

static inline void print_single_payload(payload_t* p)
{
    switch (p->kind)
    {
        case P_DEFINITIVE_SHIFT:
        case P_DEFERRED_SHIFT:
            fprintf(stderr, "%s shift %zd",
                    p->kind == P_DEFINITIVE_SHIFT ? "definitive" : "deferred",
                    p->s.yyposn);
            break;
        case P_DEFINITIVE_REDUCE:
        case P_DEFERRED_REDUCE:
            fprintf(stderr, "%s reduce rule: r%d (",
                    p->kind == P_DEFINITIVE_REDUCE ? "definitive" : "deferred",
                    p->r.rule - 1);
            int i;
            for (i = 0; i < p->r.num_values; i++)
            {
                if (i > 0)
                    fprintf(stderr, ", ");

                print_payload(p->r.values[i]);
            }
            fprintf(stderr, ")");
            break;
        default:
            YYASSERT(false);
    }
}

static inline void print_payload(payload_t* p)
{
    payload_t* d;
    int num_items = 0;
    for (d = p; d != NULL; d = d->next)
    {
        num_items++;
    }
    fprintf(stderr, "[num-payloads: %d", num_items);
    for (d = p; d != NULL; d = d->next)
    {
        if (d != p)
            fprintf(stderr, ", ");

        fprintf(stderr, "(");
        print_single_payload(d);
        fprintf(stderr, ")");
    }
    fprintf(stderr, "]");
}

static inline void print_edge(stack_node_edge_t* edge)
{
    print_payload(edge->payload);
}

static inline void print_stack_rec(stack_node_t* stack)
{
    fprintf(stderr, "s%d [%p]", stack->state, stack);

    switch (stack->num_preds)
    {
        case 0:
            break;
        case 1:
            fprintf(stderr, "--");
            print_edge(stack->preds[0]);
            fprintf(stderr, "-->");
            print_stack_rec(stack->preds[0]->target);
            break;
        default:
            {
                fprintf(stderr, "->{{ ");
                int i;
                for (i = 0; i < stack->num_preds; i++)
                {
                    if (i > 0)
                        fprintf(stderr, " || ");

                    fprintf(stderr, "--");
                    print_edge(stack->preds[i]);
                    fprintf(stderr, "-->");

                    print_stack_rec(stack->preds[i]->target);
                }
                fprintf(stderr, " }}");
            }
            break;
    }
}

static inline void print_stacks(void)
{
    if (!yydebug)
        return;

    YYDPRINTF((stderr, "There are %d active stacks\n", gss.num_active_stacks));

    int i;
    for (i = 0; i < gss.num_active_stacks; i++)
    {
        stack_node_t* current_stack = gss.stacks[i];
        fprintf(stderr, "Stack #%d: ", i);
        print_stack_rec(current_stack);
        fprintf(stderr, "\n");
    }

}

static inline int stack_id(stack_node_t* stack)
{
    int i;
    for (i = 0; i < gss.num_active_stacks; i++)
    {
        stack_node_t* current_stack = gss.stacks[i];
        if (current_stack == stack)
            return i;
    }
    return -1;
}
#endif

static inline void enqueue_reductions(stack_node_t* stack, int token)
{
    YYDPRINTF((stderr, "Enqueueing reductions for stack #%d\n", stack_id(stack)));
    if (yyisDefaultedState(stack->state))
    {
        // Default reduction
        int rule = yydefaultAction (stack->state);
        if (rule != 0)
        {
            YYDPRINTF((stderr, "Stack #%d [%p] has a default action with rule r%d\n", stack_id(stack), stack, rule - 1));
            int len = get_rule_length(rule);
            compute_paths(stack, rule, len);
        }
    }
    else
    {
        int action = 0;
        const short int* conflicts;
        yygetLRActions(stack->state, token, &action, &conflicts);

        if (yyisShiftAction(action))
        {
            // Do nothing
        }
        else if (yyisErrorAction(action))
        {
            // Do nothing
        }
        else
        {
            int rule = -action;

            YYDPRINTF((stderr, "Immediate action for stack #%d [%p] is a reduce with rule r%d\n", stack_id(stack), stack, rule - 1));
            // It should be a reduce

            YYDPRINTF((stderr, "Reducing stack #%d [%p] by rule r%d\n", stack_id(stack), stack, rule - 1));
            int len = get_rule_length(rule);

            compute_paths(stack, rule, len);
        }

        if (*conflicts != 0)
        {
            YYDPRINTF((stderr, "Stack #%d [%p] has conflicts\n", stack_id(stack), stack));
        }

        while (*conflicts != 0)
        {
            // These are always reduces
            //
            // FIXME:
            // for each path p of lengh len(a) from stack
            //   enqueue(p, "N->a") into path_queue

            int rule = *conflicts;

            YYDPRINTF((stderr, "Stack #%d [%p] has a conflictive reduction with rule r%d\n", stack_id(stack), stack, rule - 1));

            int len = get_rule_length(rule);

            compute_paths(stack, rule, len);

            conflicts++;
        }
    }
    YYDPRINTF((stderr, "Done enqueuing reductions for stack #%d\n", stack_id(stack)));
}

static inline void enqueue_limited_reductions(stack_node_edge_t* link, int token)
{
#if ]b4_api_PREFIX[DEBUG
    print_stacks();
#endif

    int i;
    for (i = 0; i < gss.num_active_stacks; i++)
    {
        stack_node_t* current_stack = gss.stacks[i];

        if (yyisDefaultedState(current_stack->state))
        {
            // Default reduction
            int rule = yydefaultAction (current_stack->state);
            if (rule != 0)
            {
                int len = get_rule_length(rule);
                compute_paths_that_use_link(current_stack, link, rule, len);
            }
        }
        else
        {
            int action = 0;
            const short int* conflicts;
            yygetLRActions(current_stack->state, token, &action, &conflicts);

            if (yyisShiftAction(action))
            {
                // do nothing
            }
            else if (yyisErrorAction(action))
            {
                // do nothing
            }
            else
            {
                // It should be a reduce
                int rule = -action;
                int len = get_rule_length(rule);
                compute_paths_that_use_link(current_stack, link, rule, len);
            }

            while (*conflicts != 0)
            {
                // These are always reduces
                int rule = *conflicts;
                int len = get_rule_length(rule);
                compute_paths_that_use_link(current_stack, link, rule, len);

                conflicts++;
            }
        }
    }
}

static void walk_stack_node_edge_t(void *p, void (*callback)(void *p))
{
    if (p == NULL)
        return;

    stack_node_edge_t* edge = (stack_node_edge_t*)p;

    callback(edge->target);
    callback(edge->payload);
}

static void destroy_stack_node_edge_t(void *p)
{
    YYFREE(p);
}

CUSTOM_POOL(stack_node_edge, stack_node_edge_t, STACK_NODE_EDGE_POOL_SIZE);

static void destroy_stack_node_edge_t_in_pool(void *p)
{
    stack_node_edge_pool_deallocate(p);
}

static inline stack_node_edge_t* stack_node_add_link(stack_node_t* target,
        stack_node_t* source,
        payload_t *payload)
{
    YYASSERT(target != NULL);
    YYASSERT(source != NULL);

    if (source->num_preds == 0)
        source->preds = source->preds_a;

    source->num_preds++;

    if (source->num_preds > MAX_STACK_NODE_EDGES_IMMEDIATES)
    {
        if (source->num_preds == (MAX_STACK_NODE_EDGES_IMMEDIATES + 1))
        {
            source->preds
                = YYMALLOC(source->num_preds * sizeof(*source->preds));
            memcpy(source->preds,
                   source->preds_a,
                   sizeof(*source->preds) * MAX_STACK_NODE_EDGES_IMMEDIATES);
        }
        else
        {
            source->preds = YYREALLOC(
                source->preds, source->num_preds * sizeof(*source->preds));
        }
    }

    stack_node_edge_t *new_edge = stack_node_edge_pool_allocate();
    if (new_edge == NULL)
    {
        new_edge = newrefcounted(stack_node_edge_t);
    }
    else
    {
        new_edge->_ref.refs = 1;
        new_edge->_ref.walk = walk_stack_node_edge_t;
        new_edge->_ref.destroy = destroy_stack_node_edge_t_in_pool;
    }
    new_edge->target = incref(target);
    new_edge->payload = payload;

    int idx = source->num_preds - 1;
    source->preds[idx] = new_edge;

    return new_edge;
}

static void walk_stack_node_t(void *p, void (*callback)(void*))
{
    if (p == NULL)
        return;

    stack_node_t* stack = (stack_node_t*)p;

    int i;
    for (i = 0; i < stack->num_preds; i++)
    {
        callback(stack->preds[i]);
    }
}

static void destroy_stack_node_t(void *p)
{
    stack_node_t* stack = (stack_node_t*)p;
    if (stack->num_preds > MAX_STACK_NODE_EDGES_IMMEDIATES)
        YYFREE(stack->preds);
    YYFREE(stack);
}

/* Stack pool */

CUSTOM_POOL(stack, stack_node_t, STACK_POOL_SIZE)

/* End of stack pool */

static void destroy_stack_node_t_in_pool(void *p)
{
    stack_node_t* stack = (stack_node_t*)p;
    if (stack->num_preds > MAX_STACK_NODE_EDGES_IMMEDIATES)
        YYFREE(stack->preds);
    stack_pool_deallocate(stack);
}

static inline stack_node_t* new_stack(yyStateNum state)
{
    stack_node_t* stack = stack_pool_allocate();
    if (stack == NULL)
    {
        // The pool is full, use the free storage
        stack = newrefcounted(stack_node_t);
    }
    else
    {
        stack->_ref.refs = 1;
        stack->_ref.walk = walk_stack_node_t;
        stack->_ref.destroy = destroy_stack_node_t_in_pool;
    }
    stack->state = state;
    stack->num_preds = 0;
    stack->preds = NULL;

    YYDPRINTF((stderr, "New stack [%p]\n", stack));

    return stack;
}

static inline void reduce_via_path(queue_item_t* qi, int token)
{
    YYDPRINTF((stderr, "Reducing stack #%d via path for rule r%d\n", stack_id(qi->stack), qi->rule - 1));
#if ]b4_api_PREFIX[DEBUG
    if (yydebug)
    {
        fprintf(stderr, "Path has length %d: ", qi->path.length);
        int j;
        for (j = 0; j < qi->path.length; j++)
        {
            stack_node_edge_t* edge = qi->path.edges[j];
            fprintf(stderr, "s%d [%p]<-", edge->target->state, edge->target);
        }
        fprintf(stderr, "[s%d, %p]", qi->stack->state, qi->stack);
        fprintf(stderr, "\n");
    }
#endif

    stack_node_t* left_sibling = qi->stack;
    if (qi->path.length > 0)
        left_sibling = qi->path.edges[0]->target;

    YYDPRINTF((stderr, "Leftmost stack node in the path is in state s%d\n", left_sibling->state));

    yyStateNum state = yyLRgotoState (left_sibling->state,
            yylhsNonterm (qi->rule));

    YYDPRINTF((stderr, "Goto state for stack #%d [%p] in state s%d [%p] using rule r%d is s%d\n", stack_id(qi->stack), qi->stack, left_sibling->state, left_sibling, qi->rule - 1, state));

    // Create a deferred payload
    payload_t *payload = new_payload(P_DEFERRED_REDUCE);
    payload->r.rule = qi->rule;
    payload->r.num_values = qi->path.length;
    if (payload->r.num_values < MAX_PAYLOAD_VALUES_IMEDIATES)
    {
        payload->r.values = payload->r.values_a;
    }
    else
    {
        payload->r.values = YYMALLOC(qi->path.length * sizeof(*(payload->r.values)));
    }

    int i;
    for (i = 0; i < qi->path.length; i++)
    {
        payload->r.values[i] = incref(qi->path.edges[i]->payload);
    }

    if (yydebug)
    {
        YYDPRINTF((stderr, "New derred payload: "));
        print_payload(payload);
        YYDPRINTF((stderr, "\n"));
    }

    stack_node_t* right_sibling = gss_find_stack_node(state);
    if (right_sibling != NULL)
    {
        YYDPRINTF((stderr, "There is already the stack #%d [%p] in the state s%d\n", stack_id(right_sibling), right_sibling, state));
        // there is already a stack right_sibling with the goto state
        // of the current rule
        stack_node_edge_t* link = NULL;

        // look for a link from right_sibling to left_sibling
        for (i = 0; i < right_sibling->num_preds; i++)
        {
            if (right_sibling->preds[i]->target == left_sibling)
            {
                link = right_sibling->preds[i];
                break;
            }
        }

        if (link != NULL)
        {
            YYDPRINTF((stderr, "There is already a link from the stack #%d [%p] to the current stack node\n", stack_id(right_sibling), right_sibling));
            // there is already a link from right_sibling to left_sibling

            // Update in place so this propagates to other stacks that are referring to this payload
            payload_t* payload_tail = link->payload;
            while (payload_tail->next != NULL)
                payload_tail = payload_tail->next;
            payload_tail->next = payload;

            YYDPRINTF((stderr, "Merged payloads: "));
            if (yydebug)
            {
                print_payload(link->payload);
            }
            YYDPRINTF((stderr, "\n"));
        }
        else
        {
            YYDPRINTF((stderr, "There is NOT a link from the stack #%d [%p] to the current stack\n", stack_id(right_sibling), right_sibling));
            // FIXME:
            YYDPRINTF((stderr, "Adding the link\n"));
            link = stack_node_add_link(left_sibling, right_sibling, payload);

            YYDPRINTF((stderr, "Enqueueing reductions enabled by the new link\n"));
            enqueue_limited_reductions(link, token);
            YYDPRINTF((stderr, "Done\n"));
        }
    }
    else
    {
        YYDPRINTF((stderr, "There is NOT a stack already in state s%d\n", state));
        YYDPRINTF((stderr, "Creating new stack in state s%d\n", state));
        right_sibling = new_stack(state);

        YYDPRINTF((stderr, "Adding a link to the newly created stack\n"));
        // FIXME:
        stack_node_add_link(left_sibling, right_sibling, payload);

        YYDPRINTF((stderr, "Adding stack into set of stacks\n"));
        gss_add_stack(right_sibling);
    }
}

// Pretty lame approach that we may improve by materializing the "is linear"
// property elsewhere
static inline char gss_stack_is_linear_(stack_node_t* stack)
{
    while (stack != NULL
            && stack->num_preds == 1)
    {
        stack = stack->preds[0]->target;
    }

    return stack == NULL || stack->num_preds == 0;
}

static inline void do_reductions(int token)
{
    YYDPRINTF((stderr, "Doing reductions\n"));
#if ]b4_api_PREFIX[DEBUG
    print_stacks();
#endif

    YYASSERT(path_queue_is_empty());

    int first_stack_reduced = 0;
    for (;;)
    {
        int initial_active_stacks = gss.num_active_stacks;
        int i;
        for (i = first_stack_reduced; i < initial_active_stacks; i++)
        {
            stack_node_t* current_stack = gss.stacks[i];
            YYDPRINTF((stderr, "Reducing stack #%d [%p]. Currently in state %d\n", i, current_stack, current_stack->state));

            enqueue_reductions(current_stack, token);
        }

        YYDPRINTF((stderr, "\n"));

        while (!path_queue_is_empty())
        {
            int qid = path_queue_dequeue();

            reduce_via_path(&path_queue.items[qid], token);

            destroy_queue_item_t(&path_queue.items[qid]);
        }

        YYASSERT(path_queue_is_empty());

        YYASSERT(initial_active_stacks <= gss.num_active_stacks);
        if (initial_active_stacks == gss.num_active_stacks)
        {
            YYDPRINTF((stderr, "No more stacks to reduce\n\n"));
            break;
        }

        YYDPRINTF((stderr, "\n"));

        YYDPRINTF((stderr, "There are %d more stacks to potentially reduce\n", gss.num_active_stacks - initial_active_stacks));
        first_stack_reduced = initial_active_stacks;
    }
}

static inline void evaluate_payload(payload_t* p]b4_user_formals[);

static inline void do_shifts(int token,
        size_t yyposn,
        YYSTYPE *yyval
        ]b4_locations_if([, YYLTYPE *yyloc])[
        ]b4_user_formals[)
{
    YYDPRINTF((stderr, "Doing shifts\n"));

#if ]b4_api_PREFIX[DEBUG
    print_stacks();
#endif

    stack_node_set_t old_gss = gss;

    // clear gss
    gss.num_active_stacks = 0;
    gss.stacks = NULL;

    int currently_active = old_gss.num_active_stacks;
    stack_node_t* last_alive = NULL;

    int i;
    for (i = 0; i < old_gss.num_active_stacks; i++)
    {
        stack_node_t* current_stack = old_gss.stacks[i];

        int action = 0;
        const short int* conflicts;
        yygetLRActions(current_stack->state, token, &action, &conflicts);

        if (yyisShiftAction(action))
        {
            // we will process these in the next loop
            last_alive = current_stack;
        }
        else
        {
            currently_active--;
            old_gss.stacks[i] = NULL;
            decref(current_stack);
            YYDPRINTF((stderr, "Stack #%d s%d [%p] does not have to shift, so it dies\n", i, current_stack->state, current_stack));
        }
    }

    if (currently_active == 1
        && gss_stack_is_linear_(last_alive)
        && last_alive->preds != NULL)
    {
        evaluate_payload(last_alive->preds[0]->payload]b4_user_args[);
    }

    for (i = 0; i < old_gss.num_active_stacks; i++)
    {
        stack_node_t* current_stack = old_gss.stacks[i];

        // Already dead
        if (current_stack == NULL)
            continue;

        int action = 0;
        const short int* conflicts;
        yygetLRActions(current_stack->state, token, &action, &conflicts);

        // only real reductions at this point
        YYASSERT(yyisShiftAction(action));

        int dest_state = action;
        YYDPRINTF((stderr, "Shifting stack #%d [%p]. Currently in state s%d, New state is s%d\n", i, current_stack, current_stack->state, dest_state));

        stack_node_t* right_sibling = gss_find_stack_node(dest_state);
        if (right_sibling == NULL)
        {
            YYDPRINTF((stderr, "Adding new stack node in state s%d\n", dest_state));
            right_sibling = new_stack(dest_state);
            YYDPRINTF((stderr, "Adding newly created stack to set of stacks\n"));
            gss_add_stack(right_sibling);
        }
        else
        {
            YYDPRINTF((stderr, "Reusing existing stack node already in state s%d\n", dest_state));
        }

        payload_t *payload = new_payload(P_DEFERRED_SHIFT);
        payload->s.yyposn = yyposn;
        payload->s.yyval = *yyval;
        ]b4_locations_if([payload->s.yyloc = *yyloc;])[
        stack_node_add_link(current_stack, right_sibling, payload);
        decref(current_stack);
    }

    YYFREE(old_gss.stacks);
}

static inline YYSTYPE get_yyval_of_payload(payload_t* p)
{
    switch (p->kind)
    {
        case P_DEFINITIVE_SHIFT:
            return p->s.yyval;
        case P_DEFINITIVE_REDUCE:
            return p->r.yyval;
        default:
            YYASSERT(false);
    }
}

static inline void set_yyval_of_payload(payload_t* p, YYSTYPE yyval)
{
    switch (p->kind)
    {
        case P_DEFINITIVE_SHIFT:
            p->s.yyval = yyval;
            break;
        case P_DEFINITIVE_REDUCE:
            p->r.yyval = yyval;
            break;
        default:
            YYASSERT(false);
    }
}

]b4_locations_if([

static inline YYLTYPE get_yyloc_of_payload(payload_t* p)
{
    switch (p->kind)
    {
        case P_DEFINITIVE_SHIFT:
            return p->s.yyloc;
        case P_DEFINITIVE_REDUCE:
            return p->r.yyloc;
        default:
            YYASSERT(false);
    }
}

])[

static inline void evaluate_single_payload(payload_t* d]b4_user_formals[)
{
    switch (d->kind)
    {
        case P_DEFERRED_SHIFT:
            {
                d->kind = P_DEFINITIVE_SHIFT;
                break;
            }
        case P_DEFERRED_REDUCE:
            {
                YYASSERT(d->r.num_values >= 0);
                int i;
                for (i = 0; i < d->r.num_values; i++)
                {
                    evaluate_payload(d->r.values[i]]b4_user_args[);
                }

                int rule = d->r.rule;

                int len = get_rule_length(rule);
                YYASSERT(len == d->r.num_values);

                YYSTYPE yyval;
                ]b4_locations_if([YYLTYPE yyloc;])[
                YYSTYPE in_yyval[len + 1];
                ]b4_locations_if([
                YYLTYPE in_yyloc@{len + 1@};
                ])[

                for (i = 0; i < len; i++)
                {
                   in_yyval[i] = get_yyval_of_payload(d->r.values[i]);
                   ]b4_locations_if([in_yyloc@{i@} = get_yyloc_of_payload(d->r.values@{i@});])[
                }

                user_action(rule,
                        &yyval ]b4_locations_if([, &yyloc])[,
                        len, in_yyval ]b4_locations_if([, in_yyloc])[
                        ]b4_user_args[);

                d->kind = P_DEFINITIVE_REDUCE;
                d->r.yyval = yyval;
                ]b4_locations_if([d->r.yyloc = yyloc;])[

                for (i = 0; i < d->r.num_values; i++)
                {
                    decref(d->r.values[i]);
                    d->r.values[i] = NULL;
                }
                if (d->r.num_values >= MAX_PAYLOAD_VALUES_IMEDIATES)
                {
                    YYFREE(d->r.values);
                }
                d->r.num_values = 0;

                break;
            }
        case P_DEFINITIVE_SHIFT:
        case P_DEFINITIVE_REDUCE:
            break;
        default:
            YYASSERT(false);
    }
}

static inline void merge_payloads(int yyn, payload_t* p0, payload_t* p1)
{
  YYASSERT(p0->kind == P_DEFINITIVE_SHIFT
          || p0->kind == P_DEFINITIVE_REDUCE);
  YYASSERT(p1->kind == P_DEFINITIVE_SHIFT
          || p1->kind == P_DEFINITIVE_REDUCE);

  YYSTYPE yyval0 = get_yyval_of_payload(p0);
  YYSTYPE yyval1 = get_yyval_of_payload(p1);

  yyuserMerge(yyn, &yyval0, &yyval1);

  set_yyval_of_payload(p0, yyval0);
  set_yyval_of_payload(p1, yyval1);
}

static inline int get_rule_of_payload(payload_t* p)
{
  YYASSERT(p->kind == P_DEFERRED_REDUCE || p->kind == P_DEFINITIVE_REDUCE);

  return p->r.rule;
}

static inline int compare_rules(payload_t* pd0, payload_t* pd1)
{
  yyRuleNum r0 = get_rule_of_payload(pd0);
  yyRuleNum r1 = get_rule_of_payload(pd1);

  int p0 = yydprec[r0], p1 = yydprec[r1];

  if (p0 == p1)
    {
      if (yymerger[r0] == 0 || yymerger[r0] != yymerger[r1])
        return 0;
      else
        return 1;
    }
  if (p0 == 0 || p1 == 0)
    return 0;
  if (p0 < p1)
    return 3;
  if (p1 < p0)
    return 2;
  return 0;
}

static inline char identical_payloads(payload_t *pd0, payload_t *pd1)
{
    if (pd0 == pd1)
        return 1;

    if (payload_is_reduce(pd0)
            && payload_is_reduce(pd1))
    {
        if (pd0->r.rule != pd1->r.rule)
            return 0;
        if (pd0->r.num_values != pd1->r.num_values)
            return 0;
        int i;
        for (i = 0; i < pd0->r.num_values; i++)
        {
            if (!identical_payloads(pd0->r.values[i], pd1->r.values[i]))
                return 0;
        }
        return 1;
    }
    else if (payload_is_shift(pd0)
            && payload_is_shift(pd1))
    {
        return pd0->s.yyposn == pd1->s.yyposn;
    }
    else
    {
        return 0;
    }
}

// This can be largely improved
static inline void remove_repeated_payloads(payload_t* p)
{
    int count = 0;
    payload_t* p_current = p;
    while (p_current != NULL)
    {
        count++;
        p_current = p_current->next;
    }

    YYASSERT(count > 1);
    payload_t *set[count];

    p_current = p;
    int i = 0;
    while (p_current != NULL)
    {
        set[i] = p_current;
        i++;
        p_current = p_current->next;
    }

    int j;
    for (i = 0; i < count; i++)
    {
        if (set[i] == NULL)
            continue;

        for (j = i; j < count; j++)
        {
            if (i == j)
                continue;
            if (set[j] == NULL)
                continue;

            if (identical_payloads(set[i], set[j]))
            {
                payload_t* remove = set[j];

                remove->next = NULL;
                decref(remove);

                set[j] = NULL;
            }
        }
    }

    // Rebuild the links
    payload_t* p_previous = p;
    for (i = 1; i < count; i++)
    {
        if (set[i] != NULL)
        {
            p_previous->next = set[i];
            p_previous = set[i];
        }
    }
    p_previous->next = NULL;
}

static inline void evaluate_payload(payload_t* p]b4_user_formals[)
{
    YYASSERT(p != NULL);
    if (p->next != NULL)
    {
        // First check priority
        char need_to_merge = 0;

        // remove all repeated payloads
        remove_repeated_payloads(p);

        payload_t* pd_best = p;
        payload_t* pd_current = pd_best->next;

        while (pd_current != NULL)
        {
            YYASSERT(!identical_payloads(pd_current, pd_best));
            switch (compare_rules(pd_best, pd_current))
            {
                case 0:
                    YYASSERT(0 && "ambiguity");
                    break;
                case 1:
                    need_to_merge = 1;
                    break;
                case 2:
                    break;
                case 3:
                    pd_best = pd_current;
                    need_to_merge = 0;
                    break;
                default:
                    YYASSERT(0);
            }
            pd_current = pd_current->next;
        }

        if (need_to_merge)
        {
            int r = get_rule_of_payload(pd_best);
            int bestprec = yydprec[r];
            evaluate_single_payload(pd_best]b4_user_args[);

            for (pd_current = pd_best->next; pd_current != NULL; pd_current = pd_current->next)
            {
                r = get_rule_of_payload(pd_current);
                if (bestprec == yydprec[r])
                {
                    evaluate_single_payload(pd_current]b4_user_args[);
                    merge_payloads(yymerger[r], pd_best, pd_current);
                }
            }
        }
        else
        {
            evaluate_single_payload(pd_best]b4_user_args[);
        }

        if (p == pd_best)
        {
            // Easy case
            decref(p->next);
            p->next = NULL;
        }
        else
        {
            // Carefully split the list
            payload_t* split_point = p;
            while (split_point->next != pd_best)
            {
                split_point = split_point->next;
            }

            decref(pd_best->next);
            pd_best->next = NULL;

            split_point->next = NULL;
            decref(p->next);

            empty_payload_t(p);
            *p = *pd_best;
            if (p->kind == P_DEFERRED_REDUCE)
            {
                // Fix the interior pointer
                if (0 < p->r.num_values
                        && p->r.num_values < MAX_PAYLOAD_VALUES_IMEDIATES)
                {
                    p->r.values = p->r.values_a;
                }
            }

            // FIXME: This is definitely not nice
            if (pd_best->_ref.refs == 1)
            {
                if (payload_pool_contains(pd_best))
                {
                    payload_pool_deallocate(pd_best);
                }
                else
                {
                    YYFREE(pd_best);
                }
            }
        }
    }
    else
    {
        evaluate_single_payload(p]b4_user_args[);
    }
}

static inline void report_syntax_error(int token]b4_user_formals[)
{
    const char *token_desc = yytokenName(token);

    size_t length_token_name = yytnamerr(YY_NULLPTR, token_desc);
    char *token_name = YYMALLOC((length_token_name + 1) * sizeof(*token_name));
    yytnamerr(token_name, token_desc);
    token_name[length_token_name] = '\0';

    const char msg_pattern[] = "syntax error, unexpected ";
    int total_chars = strlen(msg_pattern) + strlen(token_name) + 1;
    char *message = YYMALLOC(total_chars * sizeof(char));
    snprintf(message, total_chars, "%s%s", msg_pattern, token_name);

    yyerror (]b4_lyyerror_args[message);

    YYFREE(message);
}

]b4_function_define([yyparse], [int], b4_parse_param)[
{
    // Initialize GSS
    gss_init();

    path_queue_init();
    stack_node_t* initial_stack = new_stack(0);
    gss_add_stack(initial_stack);

    size_t yyposn = 0;
    yychar = YYEMPTY;
    yylval = yyval_default;]b4_locations_if([
    yylloc = yyloc_default;])[
]m4_ifdef([b4_initial_action], [
b4_dollar_pushdef([yylval], [], [yylloc])dnl
  /* User initialization code.  */
  b4_user_initial_action
b4_dollar_popdef])[]dnl
[
    for (;;)
    {
        // Final state
        if (gss.num_active_stacks == 1
                && gss.stacks[0]->state == YYFINAL)
        {
            YYDPRINTF ((stderr, "Stack #0 [%p] in accepting state. Exiting\n", gss.stacks[0]));
            break;
        }

        yychar = ]b4_lex[;

        yySymbol yytoken;
        if (yychar <= YYEOF)
        {
            yychar = yytoken = YYEOF;
            YYDPRINTF ((stderr, "Now at end of input.\n"));
        }
        else
        {
            yytoken = YYTRANSLATE (yychar);
            YYDPRINTF((stderr, "----------------------------------------\n"));
            YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
            YYDPRINTF((stderr, "----------------------------------------\n"));
        }

        do_reductions(yytoken);
        do_shifts(yytoken, yyposn, &yylval]b4_locations_if([, &yylloc])[]b4_user_args[);
        yyposn++;

        if (gss.num_active_stacks == 0)
        {
            YYDPRINTF((stderr, "No stack is alive, leaving\n"));
            report_syntax_error(yytoken]b4_user_args[);
            break;
        }
        YYDPRINTF((stderr, "**************************************************************\n"));
        YYDPRINTF((stderr, "**************************************************************\n"));
    }

#if ]b4_api_PREFIX[DEBUG
    YYDPRINTF((stderr, "Final stacks\n"));
    print_stacks();
#endif

    int yyresult;

    if (gss.num_active_stacks == 1
            && gss.stacks[0]->state == YYFINAL)
    {
        fprintf(stderr, "Parse ok\n");
        if (gss.stacks[0]->preds != NULL)
            evaluate_payload(gss.stacks[0]->preds[0]->payload]b4_user_args[);
        yyresult = 0;
    }
    else
    {
        fprintf(stderr, "Parse failed\n");
        yyresult = 1;
    }

    path_queue_destroy();
    gss_destroy();

    return yyresult;

}

/* DEBUGGING ONLY */
#if ]b4_api_PREFIX[DEBUG
static void
yy_yypstack (yyGLRState* yys)
{
  if (yys->yypred)
    {
      yy_yypstack (yys->yypred);
      YYFPRINTF (stderr, " -> ");
    }
  YYFPRINTF (stderr, "%d@@%lu", yys->yylrState,
             (unsigned long int) yys->yyposn);
}

static void
yypstates (yyGLRState* yyst)
{
  if (yyst == YY_NULLPTR)
    YYFPRINTF (stderr, "<null>");
  else
    yy_yypstack (yyst);
  YYFPRINTF (stderr, "\n");
}

static void
yypstack (yyGLRStack* yystackp, size_t yyk)
{
  yypstates (yystackp->yytops.yystates[yyk]);
}

#define YYINDEX(YYX)                                                         \
    ((YYX) == YY_NULLPTR ? -1 : (yyGLRStackItem*) (YYX) - yystackp->yyitems)


static void
yypdumpstack (yyGLRStack* yystackp)
{
  yyGLRStackItem* yyp;
  size_t yyi;
  for (yyp = yystackp->yyitems; yyp < yystackp->yynextFree; yyp += 1)
    {
      YYFPRINTF (stderr, "%3lu. ",
                 (unsigned long int) (yyp - yystackp->yyitems));
      if (*(yybool *) yyp)
        {
          YYASSERT (yyp->yystate.yyisState);
          YYASSERT (yyp->yyoption.yyisState);
          YYFPRINTF (stderr, "Res: %d, LR State: %d, posn: %lu, pred: %ld",
                     yyp->yystate.yyresolved, yyp->yystate.yylrState,
                     (unsigned long int) yyp->yystate.yyposn,
                     (long int) YYINDEX (yyp->yystate.yypred));
          if (! yyp->yystate.yyresolved)
            YYFPRINTF (stderr, ", firstVal: %ld",
                       (long int) YYINDEX (yyp->yystate
                                             .yysemantics.yyfirstVal));
        }
      else
        {
          YYASSERT (!yyp->yystate.yyisState);
          YYASSERT (!yyp->yyoption.yyisState);
          YYFPRINTF (stderr, "Option. rule: %d, state: %ld, next: %ld",
                     yyp->yyoption.yyrule - 1,
                     (long int) YYINDEX (yyp->yyoption.yystate),
                     (long int) YYINDEX (yyp->yyoption.yynext));
        }
      YYFPRINTF (stderr, "\n");
    }
  YYFPRINTF (stderr, "Tops:");
  for (yyi = 0; yyi < yystackp->yytops.yysize; yyi += 1)
    YYFPRINTF (stderr, "%lu: %ld; ", (unsigned long int) yyi,
               (long int) YYINDEX (yystackp->yytops.yystates[yyi]));
  YYFPRINTF (stderr, "\n");
}
#endif

#undef yylval
#undef yychar
#undef yynerrs]b4_locations_if([
#undef yylloc])

m4_if(b4_prefix, [yy], [],
[[/* Substitute the variable and function names.  */
#define yyparse ]b4_prefix[parse
#define yylex   ]b4_prefix[lex
#define yyerror ]b4_prefix[error
#define yylval  ]b4_prefix[lval
#define yychar  ]b4_prefix[char
#define yydebug ]b4_prefix[debug
#define yynerrs ]b4_prefix[nerrs]b4_locations_if([[
#define yylloc  ]b4_prefix[lloc]])])[

]b4_epilogue[]dnl
b4_output_end()
