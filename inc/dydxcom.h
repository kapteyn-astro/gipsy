#if !defined (_dydxcom_h_)
#define _dydxcom_h_

                             /* rename externals */
#define yylval     dydx_val
#define dydxYlval  dydx_val
#define yyparse    dydx_parse
#define yylex      dydx_lex
#define dydxYlex   dydx_lex
#define main       dydx_lex  /* prevent undefined external reference ... */

                             /* error codes */
                             
#define DYDXBADSSYN  -1  /* scanner syntax error */
#define DYDXBADPSYN  -2  /* parser syntax error */
#define DYDXBADFUNC  -3  /* function has not been implemented */
#define DYDXFUNCLEN  -4  /* function too large for output argument */
#define DYDXTOOLARGE -5  /* derivative too large for output argument */
#define DYDXCONSTLEN -6  /* constant name too long */
#define DYDXTOOMANY  -7  /* too many constants for output argument */

typedef struct {
   int  var;         /* indicates variable involved */
   char *expr;
   char *deriv;
} _expr, *expr;

extern void dydx_init(char *);

#endif /* _dydxcom_h_ */
