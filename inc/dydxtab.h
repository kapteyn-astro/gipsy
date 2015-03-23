
typedef union  {
       char *text;
       expr value;
       } YYSTYPE;
extern YYSTYPE yylval;
# define NUMBER 257
# define IDENTIFIER 258
# define POWER 259
# define LEXERR 260
# define UMINUS 261
