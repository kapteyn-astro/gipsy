/* cola.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990, 1994
	All Rights Reserved.

#>            cola.dc1

Program:      COLA

Purpose:      Control language for use with Hermes.

Category:     UTILITY

File:         cola.c

Author:       K.G. Begeman

Keywords:

   NAME=      Name of COLA file.
              The contents of this file will be interpreted by COLA
              according to the syntax described below. Only the name
              of the file should be given, the extension of the file
              should be .col.
              Note: Hermes knows about COLA files, meaning that you may
              specify the name of the COLA file directly on Hermes'
              command line.

** LIST=      Make listing [NO] ?
              If YES the COLA makes a file with the same name as the
              COLA source file and extension .lis. The source and
              generated code will be listed.

   VAR=       Value for variable [current value of var.]
              (see explanation of READ statement below)

** STOP=      Stop execution of COLA [NO] ?

Description:  INTRODUCTION

              COLA is a small (but powerful) high-level control language for
              use with Hermes.  Using COLA you can write a program to execute
              a sequence of commands. In such a COLA program you can use a
              number of facilities, like:

              -  variables (and substitution of their values in commands)
              -  a conditional statement (IF)
              -  repetitive statements (FOR, WHILE, REPEAT)
              -  input/output with Hermes console (READ, WRITE)

              COLA compiles a source program to some intermediate code, and,
              if no errors are detected, starts to execute that code.
              If an error is found, the line containing the error is printed,
              together with an explanation and compilation is stopped.

              To introduce the general form and appearance, an example COLA
              program is presented, which shows a couple of maps on TV,
              prints them, and copies them to another set.

              !  example COLA program
              !  date: Nov 1, 94

              integer i                                    ! loop counter
              integer iret                                 ! error return
              integer nsubs                                ! number of subsets

              iret = RHEAD( "NGC4214", 0, "NAXIS3", nsubs )
              IF iret <> 0 THEN                            ! something wrong
                 WRITE "Something wrong with set NGC4214"
                 HALT
              CIF
              READ "Subset numbers [%nsubs] ?" nsubs       ! prompt user
              FOR i = 1 , nsubs                            ! for all subsets...
                 WRITE "Processing subset %i"              ! give message
                 "VIEW INSET=NGC4214 %i CLIP="             ! display
                 "PRINT INSET=NGC4214 %i BOX= FORMAT="     ! print
                 IF i < (nsubs/2) THEN
                    "COPY INSET=NGC4214 %i OUTSET=OUT1 %i" ! first half to OUT1
                 ELSE
                    "COPY INSET=NGC4214 %i OUTSET=OUT2 %i" ! second half to OUT2
                 CIF
              CFOR

              This example shows some aspects of COLA, all of which will be
              discussed in detail further on. But a few points can be made
              immediately:

              -  COLA makes no distinction between upper and lower case letters
              -  layout doesn't affect the meaning of a program
              -  comments can be added (the rest of a line, following ! is
                 skipped, except when the ! occurs in a string)
              -  COLA can read header items

              The next part of this document contains a detailed description
              of the features of COLA.


              CONSTANTS

              COLA knows 5 datatypes:

              integer                   1, 0, -12, etc.
              real                      3.14, -.01, 1e5, -2.5d-5 etc.
              double                    3.14, -.01, 1e5, -2.5d-5 etc.
              logical                   True, False
              string                    "This is a sample string"


              Note that reals are single precision floats and doubles
              double precision floats. All calculations are done in
              double precision.
              To put a double quote (") or a backslash in a string constant
              put a backslash in front of it; i.e.:
              "A double quote (\") is obtained by typing \\\""


              VARIABLES

              Variables which are introduced into a program are given
              "identifiers" by the programmer. An identifier is a name,
              made up from letters and digits, starting with a letter.
              The length of a name is physically determined by the length
              of a source line, although only the first 18 characters are
              significant.  A variable may be an array. Some words are
              "reserved", and have a special meaning in the language.
              Therefore they may not be used as names for variables.
              The reserved words are:

              ABS        ACOS       AND        ASIN       ATAN
              ATAN2      ATOF       ATOI       AXUNIT     BLANK
              CANCEL     CANCELKEYS CFOR       CIF        CLOSES
              COLUMNQ    COS        COSH       COTRANS    CREATEC
              CTYPE      CWHILE     DATE       DEBUG      DEG
              DELETEC    DELETEF    DELETEH    DELETES    DELETET
              DIMENSION  DOUBLE     ELSE       ELSIF      ENTERED
              EQ         ERF        ERFC       EXISTC     EXISTF
              EXISTH     EXISTS     EXISTT     EXP        EXTEND
              FACTOR     FALSE      FCLOSE     FOPEN      FOR
              FREAD      FUNNAME    FWRITE     GE         GETBOX
              GETPOS     GETSAS     GETSAX     GETSSS     GT
              HALT       HTYPE      IF         INTEGER    LE
              LEN        LGRID      LIST       LN         LOG
              LOGICAL    LT         MOD        NCOORDS    NE
              NINT       NOT        OR         RAD        RANG
              RANU       RCOLUMN    READ       REAL       REPEAT
              RFRCOM     RHEAD      RIMAGE     RTABLEC    SEED
              SETDEF     SIN        SINH       SIZEOF     SQRT
              STORYBOARD STRING     SUBSTR1    SUBSTR2    SYSTEM
              TABELQ     TAN        TANH       THEN       TRUE
              UGRID      UNTIL      WCOLUMN    WFRCOM     WHEAD
              WHILE      WIMAGE     WRITE      WTABLEC

              Before use, variables have to be declared first.  Declarations
              for the different types look like this:

              integer   i,j,nsets,subsets(100)
              real      f,pi,data(10)
              double    d,velo(64)
              logical   quit,okay(100)
              string    message,ngc(100)
              string*10 keyw

              The value of a variable can be substituted in a string by
              prefixing the name with a percent sign (%):

              "The number is: %num !!"

              At runtime the sequence '%num' will be replaced by the actual
              value of the variable num.
              Default C's format 'g' is used. You can specify the 'f' format
              using the syntax %10.2\\x.
              Example:
              x = 121212.3434
              write "%10.2\\x"
              x = 0.3434
              write "%10.2x"
              x = 0.03734
              write "%10.2x"
 
              Generates output:

              121212.34
              0.34
              0.037
                         

              Remarks:

              - to put a percent sign in a string, it should be prefixed by
                another percent sign
              - to get a variable substitution followed by a letter or digit,
                separate them with an underscore (_). If an underscore should
                be printed after a variable substitution, put a second
                underscore after the variable name.
              - if a minimum field width is wanted, add the wanted width after
                the percent sign, i.e: "The number is: %8num !!"
              - if a precision is wanted, add the wanted precision after
                a dot (after the percent sign or the wanted minimum width),
                i.e: "The number is: %.7num !!", or "It is: %10.7num !!"
              - if an array element is to be printed, you must add an index,
                which may be an integer constant or a variable, i.e:
                "The number is: %nums(10) !!", or "It is %nums(n) !!"


              EXPRESSIONS

              COLA has three kinds of expressions: arithmetic, logical and
              string. Arithmetic expressions are made up from integer or real
              constants, variables and the arithmetic operators:

              +      plus
              -      minus
              *      times
              /      divide (DIV for integer arguments)
              MOD    modulo function
              **     power

              For logical expressions the relational operators:

              =    or    EQ      equal
              <>   or    NE      not equal
              <    or    LT      less than
              <=   or    LE      less or equal
              >    or    GT      greater than
              >=   or    GE      greater or equal

              and the logical operators:  AND,  OR  and NOT can be used.

              Example expressions:

              5
              2**10
              num
              2*(num+3)
              num < 10
              (a LE b) OR (c > 0)


              ASSIGNMENT

              An assignment gives the value of an expression to a variable.

              Example:

              nr = 3
              subset = subset + 1
              subsets(1) = 0
              message = "Processing set: %iset"
              quit = ( num > last )


              TASK STATEMENT

              This statement is of course the most important one of COLA.
              It is used to start a servant task. The statement itself is
              just a string. Since these strings have to be passed through
              Hermes, they receive a special treatment:

              - multiple spaces are replaced by single spaces

              It is also possible to continue one string over more than one
              line.

              Examples:
              "RESET MEM=1 3 4"
              "VIEW INSET=%set %subset"
              "print inset=aurora freq 1
               box= format=xxx.xx"


              READ STATEMENT

              This statement is used to read a new value for a variable
              from the console. The user can enter a new value, or keep the
              current (as default).  You also have the option to specify a
              prompt string for the Task Status Area.

              Example:
              The statement sequence

              INSET = "AURORA"
              READ "Name of set [%INSET] ? " INSET

              leads to the following question at the console:

              - COLA  Name of set [AURORA] ?

              COLA INSET=

              If you omit the promptstring, COLA will display:
              "Value for variable: INSET". Of course it is also possible
              to supply a value when starting up the program with:
                 COLA,NAME=somename,INSET=AURORA


              CANCEL STATEMENT

              This statement cancels the previous READ, i.e. the next READ
              statement will prompt the user again. It is made up of the
              statement CANCEL followed by a variable.

              Example:
              CANCEL pos
              READ   "Next Position" pos


              WRITE STATEMENT

              This statement writes a string to the screen (and log file).
              It is made up of the keyword WRITE and a string.

              Example:
              WRITE "Now processing set %set %subset"


              HALT STATEMENT

              A HALT statement immediately stops the execution of a COLA
              program. This can be used to stop a program if a task stops
              abnormally.


              IF STATEMENT

              The IF statement allows a choice between two possible statement
              paths:

              IF logical_expression THEN
                 ...
              ELSIF logical_expression THEN
                 ...
	      ELSIF ...
                .
                .
              ELSE
                 ...
              CIF

              The ELSIF's and/or ELSE parts of this statement may be omitted.


              FOR STATEMENT

              This statement can be used to execute a body of statements a
              (fixed) number of times. It takes the following form:

              FOR cv = lwb , upb , step
                 ...
              CFOR

              The variable 'cv' is the control variable, 'lwb' and 'upb'
              (resp. the initial and final value) and step (the step size)
              are general (arithmetic) expressions. The last may be omitted,
              then step defaults to 1.

              Remarks:

              - although the control variable shouldn't be changed inside the
                loop, there's no checking against it. If it is changed, the
                outcome is not predictable.


              REPEAT and WHILE STATEMENT

              These two statements can be used to repeat a body of statements
              a (in advance unknown) number of times.
              The statements take the following form:

              WHILE logical_expression
                 ...
              CWHILE

              and

              REPEAT
                 ...
              UNTIL logical_expression


              OPTION STATEMENTS

              An option statement should appear before any declaration
              or executable statement. The options statement can be one or
              more of the following:

              CANCELKEYS      Cancel all keywords that are asked by
                              COLA's READ statement.
              DEBUG           Print out some extra information (like the
                              generated code) useful for debugging.
              FUNNAME         Generate special names for the TASKs to
                              run by COLA. The special names will be:
                              <name of task>_<name of cola task/file>
              STORYBOARD      Do not execute the TASK statements but list
                              them on screen and in a file with the name
                              of the cola source file and extension .str.


              APPENDIX I: syntax of COLA

              In this syntaxis, the following meta symbols are being used:

              x OPTION   ==   zero or one  x
              x SEQUENCE ==   one or more  x
              x CHAIN y  ==   x , ( y , x ) SEQUENCE OPTION


              program		: ( options ; declarations ; statements ) SEQ OPT .

              options           : option SEQ OPT .

              option            : 'CANCELKEYS' ; 'DEBUG' ; 'FUNNAME'
                  ; 'STORYBOARD'

              declarations      : declaration SEQ OPT .

              declaration	: type , variable .

              type		: 'INTEGER' ; 'REAL' ; 'DOUBLE'
                  ; 'STRING' lengthspec OPT ; 'LOGICAL' .

              lengthspec	: '*' , int_const .

              statements	: statement SEQ OPT .

              statement	        : assignment ; task ; read_stat ; write_stat
                  ; cancel_stat ; halt_stat ; if_stat ; for_stat ; while_stat
                  ; repeat_stat .

              assignment	: variable , '=' , expression .

              task		: string_const .

              read_stat	        : 'READ' , string_const OPT , variable .

              write_stat	: 'WRITE' , string_expr .

              cancel_stat       : 'CANCEL , variable .

              halt_stat	        : 'HALT' .

              if_stat		: 'IF' , logical_expr , 'THEN' , statements ,
		  ( 'ELSIF' , logical_expr , 'THEN' , statements ) SEQ OPT ,
		  ( 'ELSE' , statements ) OPT , 'CIF' .

              for_stat	        : 'FOR' , int_var , '=' , integer_expr , ',' ,
                  integer_expr ,  ( ',' , integer_expr ) OPT , statements ,
                  'CFOR' .

              while_stat	: 'WHILE' , logical_expr , statements ,
                  'CWHILE' .

              repeat_stat	: 'REPEAT' , statements , 'UNTIL' ,
                  logical_expr .

              expression	: string_expr ; logical_expr ; arith_expr .

              string_expr	: string_const ; string_var ; string_function .

              logical_expr	: logical_term ,
                  ( 'OR' , logical_term ) SEQ OPT .

              logical_term	: logical_factor ,
                  ( 'AND' , logical_factor ) SEQ OPT .

              logical_factor	: 'NOT' , logical_factor
		  ; '(' , logical_expression , ')'
		  ; arith_expr , rel_op , arith_expr
		  ; logical_var
		  ; 'TRUE' ; 'FALSE'
		  ; logical_function .

              rel_op		: '='  ; '<'  ; '<=' ; '<>' ; '>=' ; '>'
		  ; 'EQ' ; 'LT' ; 'LE' ; 'NE' ; 'GE' ; 'GT' .

              arith_expr	: arith_term , ( add_op , arith_term ) SEQ OPT .

              arith_term	: arith_factor , ( mul_op , arith_factor ) SEQ OPT .

              arith_factor	: add_op , arith_factor
                  ; '(' , arith_expr , ')' ; variable ; constant
                  ; arith_function .

              add_op            : '+' ; '-' .

              mul_op            : '*' ; '/' .

              variable          : letter , ( letter ; digit ) SEQ OPT .

              constant          : digit SEQ , ( '.' , digit SEQ OPT ,
                  ( 'E' , add_op OPT , digit SEQ ) OPT ) OPT .

              string_const      : '"' , character SEQ OPT , '"' .


              logical_function  : 'FUNCTIONNAME' , ( argument ) SEQ OPT .

              arith_function    : 'FUNCTIONNAME' , ( argument ) SEQ OPT .

              string_function   : 'FUNCTIONNAME' , ( argument ) SEQ OPT .

              argument          : expression ; variable .


              APPENDIX II: functions in COLA

              Notation: A1, A2, A...     arithmetic expression
                        I1, I2, I...     integer expression
                        L1, L2, L...     logical expression
                        S1, S2, S...     string expression
                        r1, r2, r...     real variable
                        d1, d2, d...     double precision variable
                        i1, i2, i...     integer variable
                        s1, s2, s...     string variable
                        x1, x2, x...     any variable

              Mathematical Functions:

              A = SIN( A1 )       sine of A1.
              A = COS( A1 )       cosine of A1.
              A = TAN( A1 )       tangent of A1.
              A = ASIN( A1 )      inverse sine of A1.
              A = ACOS( A1 )      inverse cosine of A1.
              A = ATAN( A1 )      inverse tangent of A1.
              A = ATAN2( A1, A2 ) inverse tangent of A1/A2.
              A = SINH( A1)       hyperbolic sine of A1.
              A = COSH( A1 )      hyperbolic cosine of A1.
              A = TANH( A1 )      hyperbolic tangent of A1.
              A = EXP( A1 )       exponential function 2.71828...**A1.
              A = LN( A1 )        natural logarithm of A1.
              A = LOG( A1 )       base 10 logarithm of A1.
              A = SQRT( A1 )      square root of A1.
              A = ABS( A1 )       absolute value of A1.
              A = ERF( A1 )       error function of A1.
              A = ERFC( A1 )      complementary error function of A1.
              A = RAD( A1 )       converts degrees to radians, A1 in degrees.
              A = DEG( A1 )       converts radians to degrees, A1 in radians.
              A = NINT( A1 )      nearest integer to A1.
              A = RANU( A1, A2 )  uniform random number in range A1 - A2.
              A = RANG( A1, A2 )  gaussian random number with mean A1,
                                  dispersion A2.
              A = SEED( A1 )      sets seed A1 for random number generator.

              Utility Functions:

              A = LEN( S1 )       Length of string S1.
              A = SIZEOF( x1 )    Size of variable x1.
              A = ENTERED         Number of items entered at last read.
              A = SETDEF( I1 )    Changes default level for read statements,
                                  returns the current default level.
                                  0: no default, [ 1: default], 2: hidden,
                                  4: exact number.
              A = ATOI( S1 )      Converts S1 to integer.
              A = ATOF( S1 )      Converts S1 to double.
              A = BLANK           Returns BLANK value.
              S = SUBSTR1( S1, A2 ) Get field A2 from string S1.
              S = SUBSTR2( S1, A2, A3 ) Get A3 characters from string S1
                                  starting at position A2.
              S = DATE            Returns current time and date.
              A = FACTOR( S1, S2 ) get conversion factor from units S1
                                  to units S2.
              A = LIST( I1 )      sets/gets Hermes' character output device
                                  state. The following device (I1) states are
                                  possible:
                                  0 both terminal and log file off;
                                  1 terminal on;
                                  2 log file on;
                                  3 both terminal and log file on;
                                  If an negative device state is given, the
                                  function only reports the current device
                                  state.
              A = SYSTEM( S1 )    issue the shell command S1.

              File handling:

              L = EXISTF( S1 )    Checks whether file S1 exists.
              L = DELETEF( S1 )   Deletes file S1.
              I = FOPEN( S1, S2 ) Opens file S1 with mode S2. Legal
                                  values for S2 are:
                                  "r"    open file for reading.
                                  "w"    create file for writing; discard
                                         previous contents if any.
                                  "a"    append; open or create file for
                                         writing at end of file.
                                  "r+"   open file for update (i.e. reading
                                         and writing).
                                  "w+"   create file for update; discard
                                         previous contents if any.
                                  "a+"   append; open or create file for
                                         update, writing at end.
                                  I is the file id which is non-negative and
                                  should be used for further reference to the
                                  file (i.e. in FREAD and FWRITE).
                                  If I is negative, an error occurred:
                                  -1 No more open files.
                                  -2 File could not be opened.
              I = FCLOSE( i1 )    Closes file with id i1. If I is non-negative,
                                  an error occurred:
                                  -1 Invalid id (i1).
                                  -2 File not open.
                                  -3 Error closing file.
              I = FREAD( i1, s2 ) Reads next line from file with id i1 into s2.
                                  I contains the number of characters read
                                  (excluding the new-line) or a negative error
                                  code:
                                  -1: Invalid id (i1).
                                  -2: File not open.
                                  -3: Cannot read from file.
              I = FWRITE( i1, S2 ) Writes a line with contents S2 to file with
                                  id i1. I contains the number of characters
                                  written (excluding the new-line) or a
                                  negative error code:
                                  -1: Invalid id (i1).
                                  -2: File not open.
                                  -3: Cannot write to file.

              Set, box and position parsing:

              A = GETSAS( S1, s2, i3 ) parses string S1 into set name (s2) and
                                  subsets coordinate words (i3). It returns
                                  the number of subsets or a negative value
                                  in case of error.
                                  See Appendix III for more information about
                                  subset coordinate words.
              A = GETBOX( S1, S2, A3, i4, i5 ) parses string S1 for set S2 and
                                  subset coordinate word A3 into lower and
                                  upper box grids. The results depend on the
                                  return value of GETBOX:
                                  <0 an error occurred;
                                   0 no position found in string S1;
                                   1 one position found, returned in i4;
                                   2 only size of box found, returned in i5;
                                   3 a complete box found, lower grids returned
                                     in i4, upper grids returned in i5;
              A = GETPOS( S1, S2, A3, d4 ) parses string S1 for set S2 and
                                  subset coordinate word A3 into grid positions
                                  returned in d4. GETPOS returns negative on
                                  error, else the number of positions decoded
                                  are returned.
              A = GETSSS( S1, i2, A3, s4 ) encodes set S1 and A3 subsets from
                                  i2 into string s4. GETSSS returns non-zero
                                  on error.

              Set and subset handling functions:

              L = DELETES( S1 )   Deletes set S1.
              L = EXISTS( S1 )    Checks for existence of set S1.
              A = DIMENSION( S1, A2 ) Returns dimension of subset coordinate
                                  word A2 in set S1.
              A = LGRID( S1, A2 ) Returns lower grid on axis number A2 of set
                                  S1.
              A = UGRID( S1, A2 ) Returns upper grid on axis number A2 of set
                                  S1.
              A = EXTEND( S1, S2, A3, A4 ) Creates or extends axis S2 of set
                                  S1 with origin A3 and size A4.
              A = GETSAX( S1, A2, i3 ) Gets the axis numbers in i3 from
                                  subset coordinate word A2 of set S1.
              A = CLOSES( S1 )    Closes set S1.

              Descriptor Handling Functions:

              A = DELETEH( S1, I2, S3 ) Removes descriptor S3 from subset level
                                  I2 of set S1.
              A = EXISTH( S1, I2, S3 ) Checks existence of descriptor item S3
                                  on subset level I2 of set S1. If present,
                                  the subset level on which S3 is found is
                                  returned.
              A = HTYPE( S1, I2, S3, s4 ) Returns type of descriptor S3 on
                                  subset level I2 of set S1 in s4. Type can be:
                                  CHAR, INT, LOG, REAL, DBLE.
              A = RHEAD( S1, I2, S3, x4 ) Reads descriptor S3 on subset level
                                  I2 of set S1 into x4.
              A = WHEAD( S1, I2, S3, x4 ) Writes descriptor S3 on subset level
                                  I2 of set S1 from x4.
              A = WFRCOM( S1, I2, S3, S4 ) Writes comments from S3 to FITS
                                  descriptor S3 on subset level I2 of set S1.
              A = RFRCOM( S1, I2, S3, S4 ) Reads comments in S3 from FITS
                                  descriptor S3 on subset level I2 of set S1.

              Image Handling Functions:

              A = RIMAGE( S1, I2, i3, i4, r5 ) Reads image data from subset
                                  level I2 of set S1 from lower grids i3 to
                                  upper grids i4 into r5.
              A = WIMAGE( S1, I2, i3, i4, r5 ) Writes image data to subset
                                  level I2 of set S1 from lower grids i3 to
                                  upper grids i4 from r5.

              Table handling Functions:

              A = DELETET( S1, I2, S3 ) Removes table S3 on subset level I2
                                  of set S1.
              A = EXISTT( S1, I2, S3 ) Checks existence of table S3 on
                                  subset level I2 of set S1. Returns number
                                  of columns in table S3.
              A = TABLEQ( S1, I2, S3, s4 ) Stores all columns of table S3 on
                                  subset level I2 of set S1 in s4. It returns
                                  the number of columns found.
              A = RTABLEC( S1, I2, S3, s4 ) Reads comments into s4 from
                                  table S3 on subset level I2 of set S1.
              A = WTABLEC( S1, I2, S3, s4 ) Writes comments from s4 to
                                  table S3 on subset level I2 of set S1.
              A = DELETEC( S1, I2, S3, S4 ) Removes column S4 from table S3
                                  on subset level I2 of set S1.
              A = EXISTC( S1, I2, S3, S4 ) Returns number of rows in Column
                                  S4, Table S3, at subset level I2 of set S1.
              A = CREATEC( S1, I2, S3, S4, S5, S6, S7 ) Creates column S4 in
                                  table S3 on subset level I2 of set S1 with
                                  type S5, comments S6 and units S7.
                                  See gdsa_crecol.dc2.
              A = CTYPE( S1, I2, S3, S4, s5 ) Returns type of column S4 of
                                  table S3 on subset level I2 of set S1 in s5.
                                  Type can be: CHAR*I, INT, LOG, REAL, DBLE.
              A = COLUMNQ( S1, I2, S3, S4, s5, s6, s7 ) Gets info about column
                                  S4 of table S3 on subset level I2 of set S1.
                                  In returns the number of rows in the
                                  column, and in s5 the column type, in s6
                                  the comments and in s7 the column units.
                                  See gdsa_colinq.dc2.
              A = RCOLUMN( S1, I2, S3, S4, x5, A6, A7 ) Reads data into x5
                                  from column S4 of table S3 on subset level
                                  I2 of set S1. Start row is in A6, number of
                                  rows in A7.
              A = WCOLUMN( S1, I2, S3, S4, x5, A6, A7 ) Writes data from x5
                                  to column S4 of table S3 on subset level
                                  I2 of set S1. Start row is in A6, number of
                                  rows in A7. If A6 is zero, appends data to
                                  the end of the column.

              Coordinate Handling Functions:

              A = COTRANS( S1, I2, d3, d4, I5 ) Does a transformation from
                                  grid coordinates to physical coordinates
                                  and vice versa. If I5 is zero d3 should
                                  contain the physical coordinates on subset
                                  level I2 of set S1, and d4 will contain
                                  the grids. If I5 is not zero d3 should
                                  contain the grid coordinates on subset level
                                  I2 of set S1, and d4 will contain the
                                  physical coordinates.
                                  Note that d4 contains all coordinates (i.e.
                                  including the subset coordinates and hidden
                                  coordinates). The first coordinate in d4
                                  refers to the first axis, the second to the
                                  second axis etc. Use NCOORDS to find how many
                                  coordinates are returned in d4.
                                  See cotrans.dc2.
              A = AXUNIT( S1, A2, s3 ) Returns in s3 the type of physical
                                  units used by COTRANS for axis number A2
                                  of set S1.
                                  See axunit.dc2.
              A = NCOORDS( S1 )   Returns the number of coordinates returned
                                  by COTRANS when used on set S1.
                                  See ncoords.dc2.


              APPENDIX III: Subset coordinate words.

              A subset coordinate word is a non-negative integer number which
              denotes a unique sub-structure of a GIPSY data set, a so called
              subset. It can be any pixel, any line or plane or the whole
              structure (set). The whole set is designated by a coordinate
              word which is zero.

              The following example should explain the use of subset
              coordinate words:

              !  example COLA program
              !  date: Nov 2, 94

              string	inset, set, file, line, bunit
              integer	subsets(1000), nsub, n, ier, fd, dstat, nblank
              real	datamax(1000), datamin(1000)
              real	amax, amin
              logical	plot

              ! message for the user
              write "Cola script to create a table out of the minimum
               and maximum of subsets"

              ! get the input set and subsets from the user
              repeat
                 read "Enter Set and subsets" inset
                         ! decode into gds subset levels
                 nsub = getsas( inset, set, subsets )
                 if ( nsub < 0 ) then
                    write "something wrong with your input set and subsets"
                         ! cancel inset to be asked again
                    cancel inset
                 cif
              until ( nsub > 0 )

              ! get the units of the data
              if ( rhead( set, 0, "BUNIT", bunit ) <> 0 ) then
                 bunit = "?"
              cif

              ! set to blank
              amin = blank
              amax = blank

              ! loop over all subsets to get DATAMIN and DATAMAX
              for n = 1, nsub
                 ier = rhead( set, subsets(n), "NBLANK", nblank )
                 if ( ier <> subsets(n) ) then
                    ier = getsss( set, subsets(n), 1, inset )
                    dstat = list( 0 )
                    "mnmx
                     inset=%inset"
                    dstat = list( dstat )
                 cif
                 ier = rhead( set, subsets(n), "DATAMIN", datamin(n) )
                 if ( ier < 0 ) then
                    datamin(n) = blank
                 else
                    ier = rhead( set, subsets(n), "DATAMAX", datamax(n) )
                    if ( ier < 0 ) then
                       datamax(n) = blank
                    cif
                 cif
                 if ( amin = blank and datamin(n) <> blank ) then
                    amin = datamin(n)
                    amax = datamax(n)
                 elsif ( datamin(n) <> blank ) then
                    if ( amin > datamin(n) ) then
                       amin = datamin(n)
                    cif
                    if ( amax < datamax(n) ) then
                       amax = datamax(n)
                    cif
                 cif
              cfor

              write "%inset Minimum = %amin Maximum = %amax"

              if ( existt( set, 0, "EXTREMA" ) > 0 ) then
              !   write "Removing table EXTREMA"
                 ier = deletet( set, 0, "EXTREMA" )
                  write "table EXTREMA already present, so we deleted it"
              cif

              ier = wtablec( set, 0, "EXTREMA",
                 "DATAMIN and DATAMAX for %inset" )
              ier = createc( set, 0, "EXTREMA",
                 "DATAMIN", "REAL", bunit, "minimum" )
              ier = wcolumn( set, 0, "EXTREMA",
                 "DATAMIN", datamin, 1, nsub )
              ier = createc( set, 0, "EXTREMA",
                 "DATAMAX", "REAL", bunit, "maximum" )
              ier = wcolumn( set, 0, "EXTREMA",
                 "DATAMAX", datamax, 1, nsub )

              plot = true
              read "Plot datamin [%plot]?" plot
              cancel plot
              if ( plot ) then
                 dstat = list( 0 )
                 "table
                 inset=%set
                 option=6;1
                 grdevice=x11
                 ytabcol=EXTREMA DATAMIN;
                 xtabcol=
                 xvalues=1:%nsub
                 connect=N
                 eytabcol=
                 header=
                 comment="
                 dstat = list( dstat )
              cif

              read "Plot datamax [%plot]?" plot
              cancel plot
              if ( plot ) then
                 dstat = list( 0 )
                 "table
                 inset=%set
                 option=6;1
                 grdevice=x11
                 ytabcol=EXTREMA DATAMAX;
                 xtabcol=
                 xvalues=1:%nsub
                 connect=N
                 eytabcol=
                 header=
                 comment="
                 dstat = list( dstat )
              cif
              ! The End

Updates:      Feb 26, 1991: KGB Document created.
              Nov  4, 1991: KGB RPK Bug repaired.
              Sep  1, 1993: KGB Bug in type conversion repaired.
              Jan 21, 1994: KGB New code, many enhancements.
              Nov  3, 1994: KGB Version 3.0.
              Dec 23, 1994: KGB Function setdef implemented.
              May  4, 1995: KGB Bug in prompting for characters strings
                                repaired.
              Jun 16, 1995: KGB Added an undocumented feature.
              Nov 15, 1995: KGB Bug in DELETES destroyed.
              Dec  8, 1995: KGB Dynamical allocation of all internal tables.
              Dec 21, 1995: KGB Added functions RFRCOM and WFRCOM.
              Jan 11, 1996: KGB Added function CLOSES.
              Feb  6, 2006: JPT Bug in PopReal repaired.
#<

Update History:

     (version 1.0)

     15 May, 1985   First draft
     22 May, 1985   First version (on PDP 11/70)
     19 Jun, 1985   FOR, READ and REPEAT added
     15 Jul, 1985   STOP=  and comment (!) added
      4 Oct, 1985   Errorcodes fixed
      7 Oct, 1985   Allow empty statements
      8 Oct, 1985   Stringhandling changed
     14 Oct, 1985   MOD added
     15 Oct, 1985   Detach from TI:
     17 Oct, 1985   Skip TAB's in input
     17 Feb, 1986   DATE and TIME added
     20 Feb, 1986   Bug in FOR (pop stack)
     20 Nov, 1986   Bug in stringscanning for tasks
     20 Nov, 1986   Bug in looping if last line is comment

     (version 2.0)

     14 Jan, 1987   First tries in C
     10 Feb, 1987   First "working" version
     23 Feb, 1987   First "under Hermes" version
     19 Mar, 1987   Added 'Storyboard' capability
      3 Mar, 1988   Fixed bug in If statement
     16 Mar, 1988   Fixed bug in If statement (DDT)
     19 Nov, 1988   Introduced STORY= keyword (BPW)
     26 Feb, 1991   Migrated to UNIX GIPSY (KGB)
      4 Nov, 1991   =+ changed into += (KGB)
     13 Nov, 1991   No scrambling of case sensitive text (KGB)
     10 Jul, 1992   StringMax changed into 1024 (WZ)
      8 Sep, 1992   Error messages changed to reflect XEQ codes (JPT)
     18 Mar, 1993   StringMax changed into 4000 (JPT)
      7 Jun, 1993   Problem with unsigned chars solved (KGB)
      1 Sep, 1993   Type conversion in assignments allowed (KGB)

     (version 3.0)
     Oct 22, 1993: KGB, Functions allowed.
     Nov 29, 1993: KGB, Completely rewritten, many enhancements.
     Nov  3, 1994: KGB, Version 3.0.
     Aug 24, 1990: VOG, Removed comma's as delimeters in examples
                        because "print inset=aurora freq 1,box=
                        gives a syntax error for gdsinp.  
     Jan 16, 2006: VOG, Documented how to use the 'f' format 
     Apr 12, 2009: VOG, Replaced NINT macro by one that uses floor()
                        which avoids problems with coordinates
                        and CRPIX values that end on .5
*/


/*
 * include files:
 */

#include	"ctype.h"			/* <ctype.h> */
#include	"math.h"			/* <math.h> defines pow */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"time.h"			/* <time.h> */
#include	"gipsyc.h"			/* GIPSY definitions */
#include	"anyout.h"			/* defines anyout_c */
#include	"axunit.h"			/* defines axunit_c */
#include	"cancel.h"			/* defines cancel_c */
#include	"cotrans.h"			/* defines cotrans_c */
#include	"cmain.h"			/* main C program */
#include	"dcdpos.h"			/* defines dcdpos_c */
#include	"dcdsetdef.h"			/* dcdset definitions */
#include	"ecdset.h"			/* defines ecdset_c */
#include	"error.h"			/* defines error_c */
#include	"factor.h"			/* defines factor_c */
#include	"finis.h"			/* defines finis_c */
#include	"gds_close.h"			/* defines gds_close_c */
#include	"gds_delete.h"			/* defines gds_delete_c */
#include	"gds_exist.h"			/* defines gds_exist_c */
#include	"gds_extend.h"			/* defines gds_extend_c */
#include	"gdsa_colinq.h"			/* defines gdsa_colinq_c */
#include	"gdsa_crecol.h"			/* defines gdsa_crecol_c */
#include	"gdsa_delcol.h"			/* defines gdsa_delcol_c */
#include	"gdsa_deltab.h"			/* defines gdsa_deltab_c */
#include	"gdsa_rcchar.h"			/* defines gdsa_rcchar_c */
#include	"gdsa_rcdble.h"			/* defines gdsa_rcdble_c */
#include	"gdsa_rcint.h"			/* defines gdsa_rcint_c */
#include	"gdsa_rclog.h"			/* defines gdsa_rclog_c */
#include	"gdsa_rcreal.h"			/* defines gdsa_rcreal_c */
#include	"gdsa_rdcom.h"			/* defines gdsa_rdcom_c */
#include	"gdsa_tabinq.h"			/* defines gdsa_tabinq_c */
#include	"gdsa_wcchar.h"			/* defines gdsa_wcchar_c */
#include	"gdsa_wcdble.h"			/* defines gdsa_wcdble_c */
#include	"gdsa_wcint.h"			/* defines gdsa_wcint_c */
#include	"gdsa_wclog.h"			/* defines gdsa_wclog_c */
#include	"gdsa_wcreal.h"			/* defines gdsa_wcreal_c */
#include	"gdsa_wrcom.h"			/* defines gdsa_wrcom_c */
#include	"gdsc_fill.h"			/* defines gdsc_fill_c */
#include	"gdsc_grid.h"			/* defines gdsc_grid_c */
#include	"gdsc_name.h"			/* defines gdsc_name_c */
#include	"gdsc_ndims.h"			/* defines gdsc_ndims_c */
#include	"gdsc_origin.h"			/* defines gdsc_origin_c */
#include	"gdsc_range.h"			/* defines gdsc_range_c */
#include	"gdsc_size.h"			/* defines gdsc_size_c */
#include	"gdsd_delete.h"			/* defines gdsd_delete_c */
#include	"gdsd_length.h"			/* defines gdsd_length_c */
#include	"gdsd_rchar.h"			/* defines gdsd_rchar_c */
#include	"gdsd_rdble.h"			/* defines gdsd_rdble_c */
#include	"gdsd_rfrcom.h"			/* defines gdsd_rfrcom_c */
#include	"gdsd_rint.h"			/* defines gdsd_rint_c */
#include	"gdsd_rlog.h"			/* defines gdsd_rlog_c */
#include	"gdsd_rreal.h"			/* defines gdsd_rreal_c */
#include	"gdsd_type.h"			/* defines gdsd_type_c */
#include	"gdsd_wchar.h"			/* defines gdsd_wchar_c */
#include	"gdsd_wdble.h"			/* defines gdsd_wdble_c */
#include	"gdsd_wfrcom.h"			/* defines gdsd_wfrcom_c */
#include	"gdsd_wint.h"			/* defines gdsd_wint_c */
#include	"gdsd_wlog.h"			/* defines gdsd_wlog_c */
#include	"gdsd_wreal.h"			/* defines gdsd_wreal_c */
#include	"gdsi_read.h"			/* defines gdsi_read_c */
#include	"gdsi_write.h"			/* defines gdsi_write_c */
#include	"init.h"			/* defines init_c */
#include	"listctrl.h"			/* defines listctrl_c */
#include	"myname.h"			/* defines myname_c */
#include	"ncoords.h"			/* defines ncoords_c */
#include	"nelc.h"			/* defines nelc_c */
#include	"setdblank.h"			/* defines setdblank_c */
#include	"setfblank.h"			/* defines setfblank_c */
#include	"status.h"			/* defines status_c */
#include	"userchar.h"			/* defines userchar_c */
#include	"userdble.h"			/* defines userdble_c */
#include	"userint.h"			/* defines userint_c */
#include	"userlog.h"			/* defines userlog_c */
#include	"userreal.h"			/* defines userreal_c */
#include	"usertext.h"			/* defines usertext_c */
#include	"xeq.h"				/* defines xeq_c */

/***************************************************************/
/*   some general definitions                                  */
/***************************************************************/

#define	VERSION	"3.0"

#define	false	0
#define	true	1

/* Old definition: #define	NINT(x)	( x > 0.0 ? (fint) ( x + 0.5 ) : (fint) ( x - 0.5 ) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

#define	GetAddr(v)	(SymTab[Nreserved+v].addr)
#define	GetId(v)	(SymTab[Nreserved+v].id)
#define	GetKind(v)	(SymTab[Nreserved+v].kind)
#define	GetLen(v)	(SymTab[Nreserved+v].code)
#define	GetSize(v)	(SymTab[Nreserved+v].size)

typedef	signed char byte;

/***************************************************************/
/*   declarations for the code generation                      */
/***************************************************************/

/*   the 'Instruction set' of the COLA machine   */

#define _HLT	0
#define _ADD	1
#define _SUB	2
#define _MUL	3
#define _DIV	4
#define	_POW	5
#define _NEG	6
#define _MOD	7
#define _AND	8
#define _OR	9
#define _NOT	10
#define _EQ	11
#define _NE	12
#define _LE	13
#define _LT	14
#define _GE	15
#define _GT	16
#define _DUP	17
#define	_DP2	18
#define _POP	19
#define	_IDX	20
#define _OUT	21
#define	_BRL	22
#define	_CAN	23
#define	_FIE	24
#define	_ADR	25
#define	_ADI	26
#define	_LDB	27
#define _LDI	28
#define	_LDR	29
#define _LDS	30
#define _LDV	31
#define _STV	32
#define _JMP	33
#define _BRF	34
#define _EXE	35
#define _IN	36

static	char	*Mnemonics[] = {
   "HLT", "ADD", "SUB", "MUL", "DIV", "POW", "NEG", "MOD", "AND", "OR ",
   "NOT", "EQ ", "NE ", "LE ", "LT ", "GE ", "GT ", "DUP", "DP2", "POP",
   "IDX", "OUT", "BRL", "CAN", "FIE", "ADR", "ADI", "LDB", "LDI", "LDR",
   "LDS", "LDV", "STV", "JMP", "BRF", "EXE", "IN "
};

#define	FilesMax	( FOPEN_MAX / 2 )	/* Max number of files */

static	FILE	*Files[FilesMax];		/* File descriptors */

#define	StringMax	15000			/* maximum size of strings */

#define	CodeIncr	(8192)			/* code increment */

static	byte	*Memory = NULL;			/* the code memory */
static	int	CodeMax = 0;			/* the size of code memory */
static	int	CP = 0;				/* the Code Pointer */

#define	IndexStackMax	100

static	int	IP = 0;				/* the index pointer */
static	int	IndexStack[IndexStackMax];


static	void	PushIndex( void );
static	int	PopIndex( int vn );
static	void	Index( void );

#define	AddressStackMax	100

static	int	AP = 0;
static	struct {
   int	vn;
   int	idx;
} AddressStack[AddressStackMax];

static	void	PushAddress( int vn, int idx );
static	int	PopAddress( int *idx );

static	int	PopIndex( int vn );

static	bool	Debug = false;			/* don't debug */
static	bool	Dump = false;			/* don't dump code */
static	bool	Funname = false;		/* no funnye exec. names */
static	bool	CancelKeys = false;		/* no keyword cancel */

/***************************************************************/
/*   definitions for the data table                           */
/***************************************************************/


#define	DataIncr 	(1024)		/* Memory increment in doubles */
#define	Offset( a, t )	\
( a % sizeof( t ) ? a / sizeof( t ) + 1 : a / sizeof( t ) )

static	int	DataMax = 0;		/* Size of data area */
static	int	DataPtr = 0;		/* Data Pointer */
static	union	{
   bool		*lp;			/* Logical pointer */
   char		*sp;			/* String pointer */
   double	*dp;			/* Double pointer */
   fint		*ip;			/* Integer pointer */
   float	*rp;			/* Real pointer */
} Data = { NULL };

/***************************************************************/
/*   definitions for the lexical scanner                       */
/***************************************************************/

#define IntSym		1
#define RealSym		2
#define	DoubleSym	3
#define LogicalSym	4
#define StringSym	5
#define Ident		10
#define StringConst	11
#define ReadSym		12
#define WriteSym	13
#define HaltSym		14
#define IfSym		15
#define ForSym		16
#define WhileSym	17
#define	CancelSym	18
#define RepeatSym	19
#define	FunctionSym	20
#define ThenSym		100
#define ElsifSym	101
#define ElseSym		102
#define CifSym		103
#define CforSym		104
#define CwhileSym	105
#define UntilSym	106
#define EqSym		107
#define NeSym		108
#define LeSym		109
#define LtSym		110
#define GeSym		111
#define GtSym		112
#define Comma		113
#define Lpar		114
#define Rpar		115
#define AndSym		116
#define OrSym		117
#define NotSym		118
#define PlusSym		119
#define MinusSym	120
#define TimesSym	121
#define DivSym		122
#define ModSym		123
#define	PowerSym	124
#define TrueSym		125
#define FalseSym	126
#define IntConst	127
#define RealConst	128
#define DebugSym	129
#define	StoryboardSym	130
#define	FunnameSym	131
#define	CancelKeysSym	132
#define EndOfFile	999

#define	MaxIdLen	18

static	char	ProgNam[StringMax+1];		/* name f program */

#define	IntConstsIncr	(64)			/* increment of integer consts. */
#define	RealConstsIncr	(64)			/* increment of real consts. */

static	int	IntConstsMax = 0;		/* current size of buffer */
static	int	RealConstsMax = 0;		/* currentsize of buffer */
static	int	NBoolConsts = 0;		/* # of logical constants */
static	int	NIntConsts = 0;			/* # of integer constants */
static	int	NRealConsts = 0;		/* # of real constants */
static	bool	BoolConsts[2];			/* Logical Constants */
static	fint	*IntConsts = NULL;		/* Integer Constants */
static	double	*RealConsts = NULL;		/* Real Constants */

static	char	Inline[StringMax+1];		/* the input line */
static	int	Inpos = 0;			/* position counter */
static	int	LineNr = 0;			/* Current Line Number */
static	bool	PrCode = false;			/* ... */
static	int	sym;				/* the current symbol */
static	char	ch = ' ';			/* the next character */
static	bool	EofSource = false; 		/* sic! */
static	int	CurInt;				/* current integer constant */
static	double	CurReal;			/* current real constant */
static	char 	CurString[StringMax+1];		/* current string constant */
static	char 	TmpString1[StringMax+1];	/* temporary string constant */
static	char 	TmpString2[StringMax+1];	/* temporary string constant */
static	char 	TmpString3[StringMax+1];	/* temporary string constant */
static	char 	TmpString4[StringMax+1];	/* temporary string constant */
static	char 	TmpString5[StringMax+1];	/* temporary string constant */
static	char 	TmpString6[StringMax+1];	/* temporary string constant */
static	int	CurStrPtr;			/* current string pointer */
static	int	CurStrLen;			/* current string length */
static	char 	CurIdent[MaxIdLen];		/* current identifier */
static	int 	CurVarNum;			/* current variable number */
static	int 	CurVarType;			/* current variable type */
static	int 	CurFunction;			/* current function type */
static	int	LastEntered = -1;		/* Last number of items entered */
static	fint	DefaultLevel = 1;		/* Default level read statements */

/***************************************************************/
/*   definitions for the symboltable administration            */
/***************************************************************/


#define Undef		-1
#define Reserved	0
#define IntVar		1
#define RealVar		2
#define	DoubleVar	3
#define LogicalVar	4
#define StringVar	5

static	char	*VarTypes[] = {
   "RESERVED", "INTEGER", "REAL", "DOUBLE", "LOGICAL", "STRING",
};

struct SymEntry {
   char	id[MaxIdLen];		/* identification */
   byte	kind;			/* kind of symbol */
   int	code;			/* symbol code */
   int	size;			/* size of array */
   int	addr;			/* address to variable */
};

#define	SymbolTableIncr	(256)	/* incement of symboltable */

static	int		SymbolTableMax = 0;
static	struct SymEntry	*SymTab = NULL;

static	int Nsym = 0;
static	int Nreserved = 0;

/***************************************************************/
/*   definitions for the runtime stack                         */
/***************************************************************/

#define StackSizeMax	100

struct StackEntry {
   byte	kind;
   union {
      bool	b;
      double	r;
      fint	i;
      int	s;
   } val;
   int	length;			/* length of character string */
   int	tmp;
   int	blank;
};

static	struct	StackEntry	Stack[StackSizeMax];

static	int	SP = 0;   			/* the Stack Pointer */


/***************************************************************/
/*   other definitions                                         */
/***************************************************************/

static	bool	makelist = false;
static	bool	makestoryboard = false;
static	FILE	*source, *list, *story;

typedef struct {
   int	s;
   int	l;
} Str;

static	Str	PopString( void );
static	fint	PopInt( void );
static	double	PopReal( void );
static	bool	PopBool( void );

static	void	IntConstsExtend( void );
static	void	RealConstsExtend( void );

static	void	CodeExtend( void );
static	void	DataExtend( void );
static	void	Output( char *, ... );
static	void	GetSubStr( int, char * );
static	void	GetSubStrS( Str, char * );
static	int	GetAddrI( int, int );
static	void	Quit( char *, ... );
static	void	Error( char * );
static	int	GetInt( fint *, int, int, char *, char * );
static	int	GetReal( float *, int, int, char *, char * );
static	int	GetDouble( double *, int, int, char *, char * );
static	int	GetLogical( bool *, int, int, char *, char * );
static	int	GetStr( char *, int, int, int, char *, char * );
static	void	GenByte( byte );
static	void	GenCode0( byte );
static	void	GenCode1( byte, int );
static	void	GenCode2( byte, int, int );
static	void	GenCode1At( int, byte, int );
static	void	PushBlank( void );
static	void	PushInt( int );
static	void	PushReal( double );
static	void	PushBool( bool );
static	void	Function( void );
static	void	PushString( int, int, int );
static	void	Pop( void );
static	void	PopVal( byte *, fint *, double *, bool *, int *, int *, int * );
static	int	PushBoolConst( bool );
static	int	PushIntConst( int );
static	int	PushRealConst( double );
static	int	PutBool( bool );
static	int	PutDouble( double );
static	int	PutInt( int );
static	int	PutReal( float );
static	int	PutString ( char * );
static	int	PutNBool( int );
static	int	PutNDouble( int );
static	int	PutNInt( int );
static	int	PutNReal( int );
static	int	PutNString( int );
static	void	ClearString( char * );
static	void	EnterReserved ( char *, int );
static	int	EnterVar( char *, int );
static	int	LookUp( char *, int * );
static	void	InitSymbolTable( void );
static	void	Nextch( void );
static	void	ReadIdent( void );
static	void	ScanString( void );
static	void	NextSym( void );
static	void	Declaration( void );
static	void	Statements( void );
static	void	Assignment( void );
static	void	HaltStat( void );
static	void	WriteStat( void );
static	void	ReadStat( void );
static	void	CancelStat( void );
static	void	Task( void );
static	void	WhileStat( void );
static	void	RepeatStat( void );
static	void	IfStat( void );
static	void	ForStat( void );
static	void	ArithExpression( char );
static	void	ArithTerm( char );
static	void	ArithFactor( char );
static	void	LogicalExpression( void );
static	void	LogicalTerm( void );
static	void	LogicalFactor( void );
static	void	StringExpression( void );
static	void	StringCompare( void );
static	void	Program( void );
static	void	Compile( void );
static	void	Execute( void );

typedef struct {
   char	kind;
   char	*name;
   char	*args;
   void	(*func)(void);
} func_struct;

/*
 * Standard functions:
 */

static	void	fie_add( void );
static	void	fie_and( void );
static	void	fie_div( void );
static	void	fie_eq( void );
static	void	fie_ge( void );
static	void	fie_gt( void );
static	void	fie_le( void );
static	void	fie_lt( void );
static	void	fie_mod( void );
static	void	fie_mul( void );
static	void	fie_ne( void );
static	void	fie_neg( void );
static	void	fie_not( void );
static	void	fie_or( void );
static	void	fie_pow( void );
static	void	fie_sub( void );

/*
 * Mathematical Functions:
 */

static	void	fie_sin( void );
static	void	fie_cos( void );
static	void	fie_tan( void );
static	void	fie_asin( void );
static	void	fie_acos( void );
static	void	fie_atan( void );
static	void	fie_atan2( void );
static	void	fie_sinh( void );
static	void	fie_cosh( void );
static	void	fie_tanh( void );
static	void	fie_exp( void );
static	void	fie_ln( void );
static	void	fie_log( void );
static	void	fie_sqrt( void );
static	void	fie_abs( void );
static	void	fie_erf( void );
static	void	fie_erfc( void );
static	void	fie_rad( void );
static	void	fie_deg( void );
static	void	fie_nint( void );
static	void	fie_ranu( void );
static	void	fie_rang( void );
static	void	fie_seed( void );

/*
 * Utility Functions:
 */

static	void	fie_len( void );
static	void	fie_sizeof( void );
static	void	fie_entered( void );
static	void	fie_setdef( void );
static	void	fie_atoi( void );
static	void	fie_atof( void );
static	void	fie_blank( void );
static	void	fie_substr1( void );
static	void	fie_substr2( void );
static	void	fie_date( void );
static	void	fie_factor( void );
static	void	fie_list( void );
static	void	fie_system( void );

/*
 * File handling functions:
 */

static	void	fie_existf( void );
static	void	fie_deletef( void );
static	void	fie_fopen( void );
static	void	fie_fclose( void );
static	void	fie_fread( void );
static	void	fie_fwrite( void );

/*
 * Set, Box and Position parsing functions:
 */

static	void	fie_getsas( void );
static	void	fie_getbox( void );
static	void	fie_getpos( void );
static	void	fie_getsss( void );


/*
 * Set and Subset Handling Functions:
 */

static	void	fie_deletes( void );
static	void	fie_exists( void );
static	void	fie_dimension( void );
static	void	fie_lgrid( void );
static	void	fie_ugrid( void );
static	void	fie_extend( void );
static	void	fie_getsax( void );
static	void	fie_closes( void );

/*
 * Descriptor Handling Functions:
 */

static	void	fie_deleteh( void );
static	void	fie_existh( void );
static	void	fie_htype( void );
static	void	fie_rhead( void );
static	void	fie_whead( void );
static	void	fie_rfrcom( void );
static	void	fie_wfrcom( void );

/*
 * Image Handling Functions:
 */

static	void	fie_rimage( void );
static	void	fie_wimage( void );

/*
 * Table Handling Functions:
 */

static	void	fie_deletet( void );
static	void	fie_existt( void );
static	void	fie_tableq( void );
static	void	fie_rtablec( void );
static	void	fie_wtablec( void );
static	void	fie_deletec( void );
static	void	fie_existc( void );
static	void	fie_createc( void );
static	void	fie_ctype( void );
static	void	fie_columnq( void );
static	void	fie_rcolumn( void );
static	void	fie_wcolumn( void );

/*
 * Coordinate handling functions:
 */

static	void	fie_cotrans( void );
static	void	fie_axunit( void );
static	void	fie_ncoords( void );

static	func_struct	Functions[] = {
   /* Mathematical Functions */
   { 'A',	"SIN",		"A",		fie_sin },	/* sin */
   { 'A',	"COS",		"A",		fie_cos },	/* cos */
   { 'A',	"TAN",		"A",		fie_tan },	/* tan */
   { 'A',	"ASIN",		"A",		fie_asin },	/* asin */
   { 'A',	"ACOS",		"A",		fie_acos },	/* acos */
   { 'A',	"ATAN",		"A",		fie_atan },	/* atan */
   { 'A',	"ATAN2",	"AA",		fie_atan2 },	/* atan2 */
   { 'A',	"SINH",		"A",		fie_sinh },	/* sinh */
   { 'A',	"COSH",		"A",		fie_cosh },	/* cosh */
   { 'A',	"TANH",		"A",		fie_tanh },	/* tanh */
   { 'A',	"EXP",		"A",		fie_exp },	/* exponential */
   { 'A',	"LN",		"A",		fie_ln	},	/* log base e */
   { 'A',	"LOG",		"A",		fie_log },	/* log base 10 */
   { 'A',	"SQRT",		"A",		fie_sqrt },	/* sqrt */
   { 'A',	"ABS",		"A",		fie_abs },	/* abs */
   { 'A',	"ERF",		"A",		fie_erf },	/* erf */
   { 'A',	"ERFC",		"A",		fie_erfc },	/* erfc */
   { 'A',	"RAD",		"A",		fie_rad },	/* to radians */
   { 'A',	"DEG",		"A",		fie_deg },	/* to degrees */
   { 'A',	"NINT",		"A",		fie_nint },	/* finds nearest integer */
   { 'A',	"RANU",		"AA",		fie_ranu },	/* uniform */
   { 'A',	"RANG",		"AA",		fie_rang },	/* gaussian */
   { 'A',	"SEED",		"I",		fie_seed },	/* set seed */
   /* Utility Functions: */
   { 'A',	"LEN",		"S",		fie_len },	/* length of string */
   { 'A',	"SIZEOF",	"X",		fie_sizeof },	/* size of var */
   { 'A',	"ENTERED",	"",		fie_entered },	/* items entered */
   { 'A',	"SETDEF",	"I",		fie_setdef },	/* set default level */
   { 'A',	"ATOI",		"S",		fie_atoi },	/* string to integer */
   { 'A',	"ATOF",		"S",		fie_atof },	/* string to real */
   { 'R',	"BLANK",	"",		fie_blank },	/* BLANK */
   { 'S',	"SUBSTR1",	"SA",		fie_substr1 },	/* get field from string */
   { 'S',	"SUBSTR2",	"SAA",		fie_substr2 },	/* part of string */
   { 'S',	"DATE",		"",		fie_date },	/* returns current time and date */
   { 'A',	"FACTOR",	"SS",		fie_factor },	/* conversion */
   { 'A',	"LIST",		"I",		fie_list },	/* list control */
   { 'A',	"SYSTEM",	"S",		fie_system },	/* issue a shell command */
   /* File Handling Functions: */
   { 'L',	"EXISTF",	"S",		fie_existf },	/* checks file existence */
   { 'L',	"DELETEF",	"S",		fie_deletef },	/* delete file */
   { 'I',	"FOPEN",	"SS",		fie_fopen },	/* opens a file */
   { 'I',	"FCLOSE",	"i",		fie_fclose },	/* closes a file */
   { 'I',	"FREAD",   	"is",		fie_fread },	/* read from file */
   { 'I',	"FWRITE",	"iS",		fie_fwrite },	/* writes to file */
   /* Set, Box and Position parsing: */
   { 'A',	"GETSAS",	"Ssi",		fie_getsas },	/* get set and subsets */
   { 'A',	"GETBOX",	"SSAii",	fie_getbox },	/* get box */
   { 'A',	"GETPOS",	"SSAd",		fie_getpos },	/* get positions */
   { 'A',	"GETSSS",	"SiAs",		fie_getsss },	/* get s&s as string */
   /* Set and subset handling: */
   { 'L',	"DELETES",	"S",		fie_deletes },	/* delete set */
   { 'L',	"EXISTS",	"S",		fie_exists },	/* checks set existence */
   { 'A',	"DIMENSION",	"SA",		fie_dimension },/* get dimension */
   { 'A',	"LGRID",	"SA",		fie_lgrid },	/* get lower grid on axis */
   { 'A',	"UGRID",	"SA",		fie_ugrid },	/* get upper grid on axis */
   { 'A',	"EXTEND",	"SSAA",		fie_extend },	/* extend a set */
   { 'A',	"GETSAX",	"SAi",		fie_getsax },	/* get subset axes */
   { 'A',	"CLOSES",	"S",		fie_closes },	/* closes a set */
   /* Descriptor Handling Functions: */
   { 'A',	"DELETEH",	"SIS",		fie_deleteh },	/* delete header item */
   { 'A',	"EXISTH",	"SAS",		fie_existh },	/* checks existence of header item */
   { 'A',	"HTYPE",	"SISs",		fie_htype },	/* get descriptor type */
   { 'A',	"RHEAD",	"SISx",		fie_rhead },	/* read descriptor item */
   { 'A',	"WHEAD",	"SISx",		fie_whead },	/* write header item */
   { 'A',	"RFRCOM",	"SISs",		fie_rfrcom },	/* gets comments */
   { 'A',	"WFRCOM",	"SISs",		fie_wfrcom },	/* puts comments */
   /* Image handling Functions: */
   { 'A',	"RIMAGE",	"SIiir",	fie_rimage },	/* read image */
   { 'A',	"WIMAGE",	"SIiir",	fie_wimage },	/* write image */
   /* Table handling Functions: */
   { 'A',	"DELETET",	"SIS",		fie_deletet },	/* delete table */
   { 'A',	"EXISTT",	"SIS",		fie_existt },	/* table exists */
   { 'A',	"TABLEQ",	"SISs",		fie_tableq },	/* table info */
   { 'A',	"RTABLEC",	"SISs",		fie_rtablec },	/* get comment from table */
   { 'A',	"WTABLEC",	"SISS",		fie_wtablec },	/* put comment to table */
   { 'A',	"DELETEC",	"SISS",		fie_deletec },	/* delete column */
   { 'A',	"EXISTC",	"SISS",		fie_existc },	/* column exists */
   { 'A',	"CREATEC",	"SISSSSS",	fie_createc },	/* create column */
   { 'A',	"CTYPE",	"SISSs",	fie_ctype },	/* get column type */
   { 'A',	"COLUMNQ",	"SISSsss",	fie_columnq },	/* column info */
   { 'A',	"RCOLUMN",	"SISSxAA",	fie_rcolumn },	/* read table column */
   { 'A',	"WCOLUMN",	"SISSxAA",	fie_wcolumn },	/* write table column */
   /* Coordinate Handling Functions: */
   { 'A',	"COTRANS",	"SIddI",	fie_cotrans },	/* coordinates */
   { 'A',	"AXUNIT",	"SAs",		fie_axunit },	/* units of axis */
   { 'A',	"NCOORDS",	"S",		fie_ncoords },	/* number of coords. */
};

static	double	DBLANK;				/* double BLANK */
static	float	FBLANK;				/* floating BLANK */

/*
 * StrUpp converts a string to upper case.
 */

static	void	StrUpp( char *string )
{
   do {
      *string = toupper( *string );
   } while ( *string++ );
}

/*
 * CodeExtend increase the code buffer.
 */

static	void	CodeExtend( void )
{
   CodeMax += CodeIncr;
   Memory = realloc( Memory, CodeMax );
   if ( Memory == NULL )  Quit( "CodeExtend: Cannot allocate sufficient memory!" );
}

/*
 * DataExtend increases the size of the data buffer.
 */

static	void	DataExtend( void )
{
   DataMax += DataIncr * sizeof( double );
   Data.rp = realloc( Data.rp , DataMax );
   if ( Data.rp == NULL ) Quit( "DataExtend: Cannot allocate sufficient memory!" );
}

/*
 * IntConstsExtend increases size of the buffer.
 */

static	void	IntConstsExtend( void )
{
   IntConstsMax += IntConstsIncr;
   IntConsts = realloc( IntConsts, IntConstsMax * sizeof( fint ) );
   if ( IntConsts == NULL ) Quit( "IntConstsExtend: Cannot allocate sufficient memory!" );
}

/*
 * RealConstsExtend increases size of the buffer.
 */

static	void	RealConstsExtend( void )
{
   RealConstsMax += RealConstsIncr;
   RealConsts = realloc( RealConsts, RealConstsMax * sizeof( double ) );
   if ( RealConsts == NULL ) Quit( "RealConstsExtend: Cannot allocate sufficient memory!" );
}

/*
 * SymbolTableExtend increase size of symboltable.
 */

static	void	SymbolTableExtend( void )
{
   SymbolTableMax += SymbolTableIncr;
   SymTab = realloc( SymTab, SymbolTableMax * sizeof( struct SymEntry ) );
   if ( SymTab == NULL ) Quit( "SymbolTableExtend: Cannot allocate sufficient memory!" );
}

/*
 * GetAddrI gets the address of an indexed variable .
 */

static	int	GetAddrI( int vn, int id )
{
   int	a;

   a = GetAddr( vn );
   switch( GetKind( vn ) ) {
      case StringVar: {
         a += id * GetLen( vn );
         break;
      }
      default: {
         a += id;
         break;
      }
   }
   return( a );
}

/*
 * PushAddress pushes an address on the address stack.
 */

static	void	PushAddress( int vn, int idx )
{
   if ( AP == AddressStackMax ) Quit( "PushAddress: Address Stack overflow!" );
   AddressStack[AP].vn = vn;
   AddressStack[AP].idx = idx;
   AP++;
}

/*
 * PopAddress pops an address off the address stack.
 */

static	int	PopAddress( int *idx )
{
   if ( AP == 0 ) Quit( "PopAddress: Address Stack underflow!" );
   *idx = AddressStack[--AP].idx;
   return( AddressStack[AP].vn );
}

/*
 * PushIndex pushes an index on the index stack.
 */

static	void	PushIndex( void )
{
   bool		b;
   byte		k;
   double	r;
   fint		i;
   int		l;
   int		s;
   int		t;

   if ( IP == IndexStackMax ) Quit( "PushIndex: Index Stack overflow!" );
   PopVal( &k, &i, &r, &b, &s, &l, &t );
   switch( k ) {
      case IntVar: {
         IndexStack[IP++] = i;
         break;
      }
      case RealVar: {
         IndexStack[IP++] = r;
         break;
      }
      default: {
         Quit( "PushIndex: Wrong index type!" );
         break;
      }
   }
}

/*
 * PopIndex pops an index from the index stack.
 */

static	int	PopIndex( int vn )
{
   int	idx = 0;

   if ( GetSize( vn ) != 1 ) {
      if ( IP == 0 ) Quit( "PopIndex: Index Stack underflow!" );
      idx = IndexStack[--IP];
      if ( idx < 1 || idx > GetSize( vn ) ) {
         Quit( "PopIndex: Index (%d) out of range for %.10s!", idx, GetId( vn ) );
      }
      idx = idx - 1;
   }
   return( idx );
}

/*
 * STRCMP compares string cs to string ct; return < 0 if cs < ct, 0 if cs == ct,
 * or > 0 if cs > ct. Case insensitive!
 */

static	int	STRCMP( const char *cs, const char *ct )
{
   int   d = 0;

   while (!(d = (toupper(*cs) - toupper(*ct)))) if ((!*cs++) || (!*ct++)) break;
   return( d );
}

/*
 * StrCmp compares two strings.
 */

static	int	StrCmp( Str s1, Str s2 )
{
   fchar	c1, c2;
   fint		l1, l2;

   c1.a = &Data.sp[s1.s]; c1.l = s1.l; l1 = nelc_c( c1 );
   c2.a = &Data.sp[s2.s]; c2.l = s2.l; l2 = nelc_c( c2 );
   strncpy( TmpString1, c1.a, l1 ); TmpString1[l1] = 0;
   strncpy( TmpString2, c2.a, l2 ); TmpString2[l2] = 0;
   return( STRCMP( TmpString1, TmpString2 ) );
}

/*
 * freadln reads characters from a stream.
 */

static	char	freadln( char Inline[], int nc, FILE *source )
{
   int		n = 0;
   int		eol = 0;
   static int	eof = 0;

   while ( ( n < nc ) && !eol && !eof ) {
      int	ch = fgetc( source );

      if ( ( ch == EOF ) && ( n == 0 )) {
      	  eof = 1;
      } else if ( ch == EOF ) {
      	  Inline[n++] = '\n';
      	  eof = eol = 1;
      } else if ( ch == '\n' ) {
      	  Inline[n++] = '\n';
      	  eol = 1;
      } else {
         Inline[n++] = ch;
      }
   }
   while ( n < nc ) Inline[n++] = '\0';
   if ( eol ) LineNr++;
   return( Inline[0] );
}

/*
 * Format formats an string.
 */

static	char	Format( char *sp, int *j, char *str, int *i, int max )
{
   char	fmt[StringMax+1];
   char	res[StringMax+1];
   int	fp = 0;
   int	ii = *i;
   int	jj = *j;
   int	kk = 0;

   if ( sp[jj] == '%' ) {
      if ( ii <= max ) str[ii++] = '%';
      *i = ii;
      *j = ++jj;
      return( sp[--jj] );
   }
   fmt[fp++] = '%';
   while ( sp[jj] && !isalpha( sp[jj] ) ) fmt[fp++] = sp[jj++];
   if ( isalpha( sp[jj] ) ) {
      char	Var[MaxIdLen+1];
      int	k = 0;
      int	code, kind;

      while ( isalnum( sp[jj] ) && ( k < MaxIdLen ) ) Var[k++] = sp[jj++];
      Var[k++] = 0;
      while ( isalnum( sp[jj] ) ) jj++;
      kind = LookUp( Var, &code );
      if ( ( kind >= IntVar ) && ( kind <= StringVar ) ) {
         int	idx = 0;

         if ( ( GetSize( code ) != 1 ) && ( sp[jj] == '(' ) ) {
            jj += 1;
            if ( isalpha( sp[jj] ) ) {
               char	Idx[MaxIdLen+1];
               int	code, kind;
               int	k = 0;

               while ( isalnum( sp[jj] ) && ( k < MaxIdLen ) ) {
                  Idx[k++] = sp[jj++];
               }
               Idx[k++] = 0;
               while ( isalnum( sp[jj] ) ) jj++;
               kind = LookUp( Idx, &code );
               if ( kind == IntVar ) {
                  if ( GetSize( code ) == 1 ) {
                     idx = Data.ip[GetAddr( code )];
                  }
               }
            } else while ( isdigit( sp[jj] ) ) {
               idx = 10 * idx + ( sp[jj++] - '0' );
            }
            if ( sp[jj] == ')' ) {
               jj += 1;
               if ( idx < 0 || idx > GetSize( code ) ) idx = 0;
            } else {
               idx = 0;
            }
         }
         if ( idx ) idx -= 1;
         if ( sp[jj] == '_' ) jj++;
         switch( kind ) {
            case IntVar: {
               fint	v = Data.ip[GetAddr( code ) + idx];

               fmt[fp++] = 'd'; fmt[fp] = 0;
               sprintf( res, fmt, v );
               break;
            }
            case RealVar: {
               float	v = Data.rp[GetAddr( code ) + idx];

               if ( v == FBLANK ) {
                  fmt[fp++] = 's'; fmt[fp] = 0;
                  sprintf( res, fmt, "BLANK" );
               } else {
                  if ( fmt[fp-1] == '\\' ) {
                     fmt[fp-1] = 'f';
                  } else {
                     fmt[fp++] = 'g';
                  }
                  fmt[fp] = 0;
                  sprintf( res, fmt, v );
               }
               break;
            }
            case DoubleVar: {
               double	v = Data.dp[GetAddr( code ) + idx];

               if ( v == DBLANK ) {
                  fmt[fp++] = 's'; fmt[fp] = 0;
                  sprintf( res, fmt, "BLANK" );
               } else {
                  if ( fmt[fp-1] == '\\' ) {
                     fmt[fp-1] = 'f';
                  } else {
                     fmt[fp++] = 'g';
                  }
                  fmt[fp] = 0;
                  sprintf( res, fmt, v );
               }
               break;
            }
            case LogicalVar: {
               bool	v = Data.lp[GetAddr( code ) + idx];

               fmt[fp++] = 's'; fmt[fp] = 0;
               sprintf( res, fmt, v ? "Y" : "N" );
               break;
            }
            case StringVar: {
               char	v[StringMax+1];
               int	l;

               l = GetLen( code );
               strncpy( v, &Data.sp[GetAddr( code ) + idx * l], l );
               v[l] = 0;
               fmt[fp++] = 's'; fmt[fp] = 0;
               sprintf( res, fmt, v );
               break;
            }
            default: {
               Quit( "Format: Internal error!" );
               break;
            }
         }
      } else {
         fmt[fp] = 0;
         sprintf( res, "[%s%s?]", fmt, Var );
      }
   } else {
      fmt[fp] = 0;
      sprintf( res, "[%s?]", fmt );
   }
   while ( res[kk] && ( ii <= StringMax ) ) {
      str[ii++] = res[kk++];
   }
   *i = ii;
   *j = jj;
   return( sp[--jj] );
}

/*
 * GetSubStrS substitutes the variables in a string.
 */

static	void	GetSubStrS( Str s, char str[] )
{
   char	String[StringMax+1];
   char	ch;
   int	i = 0, j = 0;

   strncpy( String, &Data.sp[s.s], s.l );
   String[s.l] = 0;
   do {
      ch = String[j++];
      if ( ch == '%' ) {
         ch = Format( String, &j, str, &i, StringMax );
      } else {
         str[i++] = ch;
      }
   } while ( ( i <= StringMax ) && ch );
   if ( ch ) str[i] = 0;
}

/*
 * GetSubStr substitutes variables in a string.
 */

static	void GetSubStr( int p, char str[] )
{
   Str	s;

   s.s = p; s.l = strlen( &Data.sp[p] );
   GetSubStrS( s, str );
}

/*
 * Blank checks whether there is a blank on the stack.
 */

static	bool	Blank( int sp )
{
   if ( ( SP - sp - 1 ) < 0 ) Quit( "Blank: Stack Underflow!" );
   return( Stack[SP-sp-1].blank );
}

/*
 * Kind returns the kind of variable on the stack.
 */

static	byte	Kind( int sp )
{
   if ( ( SP - sp - 1 ) < 0 ) Quit( "Kind: Stack Underflow!" );
   return( Stack[SP-sp-1].kind );
}

/*
 * Standard Functions:
 */

/*
 * add (+)
 */

static	void	fie_add( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushInt( i1 + i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Add: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushReal( r1 + r2 );
         break;
      }
      default: {
         Quit( "Add: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * substract (-)
 */

static	void	fie_sub( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushInt( i1 - i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Sub: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushReal( r1 - r2 );
         break;
      }
      default: {
         Quit( "Sub: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Multiply (*)
 */

static	void	fie_mul( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushInt( i1 * i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Mul: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushReal( r1 * r2 );
         break;
      }
      default: {
         Quit( "Mul: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Divide (/)
 */

static	void	fie_div( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( );
         if ( i2 == 0 ) Quit( "Div: Integer divide by zero!" );
         PushInt( i1 / i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Div: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( );
         if ( r2 == 0.0 ) Quit( "Div: Floating divide by zero!" );
         PushReal( r1 / r2 );
         break;
      }
      default: {
         Quit( "Div: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Power (**)
 */

static	void	fie_pow( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( );
         PushInt( pow( (double) i1, (double) i2 ) );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Pow: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushReal( pow( r1, r2 ) );
         break;
      }
      default: {
         Quit( "Pow: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Negate (-)
 */

static	void	fie_neg( void )
{
   int	k;

   k = Kind( 0 );
   switch( k ) {
      case IntVar: {
         int	i;

         i = PopInt( ); PushInt( -i );
         break;
      }
      case RealVar: {
         double	r;

         if ( Blank( 0 ) ) Quit( "Neg: Operation on BLANKS!" );
         r = PopReal( ); PushReal( -r );
         break;
      }
      default: {
         Quit( "Neg: Wrong type on Stack!" );
         break;
      }
   }
}

/*
 * Modulus ( mod )
 */

static	void	fie_mod( void )
{
   int	i1, i2;

   if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Mod: Operation on BLANKS!" );
   i2 = PopInt( );
   i1 = PopInt( );
   PushInt( i1 % i2 );
}

/*
 * logical and
 */

static	void	fie_and( void )
{
   bool	b1, b2;

   b2 = PopBool( ); b1 = PopBool( ); PushBool( b1 && b2 );
}

/*
 * logical or
 */

static	void	fie_or( void )
{
   bool	b1, b2;

   b2 = PopBool( ); b1 = PopBool( ); PushBool( b1 || b2 );
}

/*
 * logical not
 */

static	void	fie_not( void )
{
   bool	b;

   b = PopBool( ); PushBool( !b );
}

/*
 * Equal (=)
 */

static	void	fie_eq( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 == i2 );
         break;
      }
      case RealVar: {
         bool	b1, b2;
         double	r1, r2;

         b2 = Blank( 0 ); b1 = Blank( 1 );
         r2 = PopReal( ); r1 = PopReal( );
         if ( b1 && b2 ) {
            PushBool( true );
         } else if ( b1 || b1 ) {
            PushBool( false );
         } else {
            PushBool( r1 == r2 );
         }
         break;
      }
      case StringVar: {
         Str	s1, s2;

         s2 = PopString( ); s1 = PopString( ); PushBool( !StrCmp( s1, s2 ) );
         break;
      }
      default: {
         Quit( "Eq: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Not Equal (<>)
 */

static	void	fie_ne( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 != i2 );
         break;
      }
      case RealVar: {
         bool	b1, b2;
         double	r1, r2;

         b2 = Blank( 0 ); b1 = Blank( 1 );
         r2 = PopReal( ); r1 = PopReal( );
         if ( b1 && b2 ) {
            PushBool( false );
         } else if ( b1 || b2 ) {
            PushBool( true );
         } else {
            PushBool( r1 != r2 );
         }
         break;
      }
      case StringVar: {
         Str	s1, s2;

         s2 = PopString( ); s1 = PopString( ); PushBool( StrCmp( s1, s2 ) );
         break;
      }
      default: {
         Quit( "Ne: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Less than (<)
 */

static	void	fie_lt( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 < i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Lt: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushBool( r1 < r2 );
         break;
      }
      default: {
         Quit( "Lt: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Less than or equal (<=)
 */

static	void	fie_le( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 <= i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Le: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushBool( r1 <= r2 );
         break;
      }
      default: {
         Quit( "Le: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Greater than or equal (>=)
 */

static	void	fie_ge( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 >= i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Ge: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushBool( r1 >= r2 );
         break;
      }
      default: {
         Quit( "Ge: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Greater than (>)
 */

static	void	fie_gt( void )
{
   int	k1, k2;

   k2 = Kind( 0 );
   k1 = Kind( 1 );
   switch( k1 > k2 ? k1 : k2 ) {
      case IntVar: {
         int	i1, i2;

         i2 = PopInt( ); i1 = PopInt( ); PushBool( i1 > i2 );
         break;
      }
      case RealVar: {
         double	r1, r2;

         if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Gt: Operation on BLANKS!" );
         r2 = PopReal( ); r1 = PopReal( ); PushBool( r1 > r2 );
         break;
      }
      default: {
         Quit( "Gt: Wrong types on Stack!" );
         break;
      }
   }
}

/*
 * Mathematical Functions:
 */

/*
 * sin( )
 */

static	void	fie_sin( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Sin: Operation on BLANKS!" );
   PushReal( sin( r1 ) );
}

/*
 * cos( )
 */

static	void	fie_cos( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Cos: Operation on BLANKS!" );
   PushReal( cos( r1 ) );
}

/*
 * tan( )
 */

static	void	fie_tan( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Tan: Operation on BLANKS!" );
   PushReal( tan( r1 ) );
}

/*
 * asin( )
 */

static	void	fie_asin( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) {
      Quit( "Asin: Operation on BLANKS!" );
   } else if ( fabs( r1 ) > 1.0 ) {
      Quit( "Asin: Invalid argument!" );
   }
   PushReal( asin( r1 ) );
}

/*
 * acos( )
 */

static	void	fie_acos( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) {
      Quit( "Acos: Operation on BLANKS!" );
   } else if ( fabs( r1 ) > 1.0 ) {
      Quit( "Acos: Invalid argument!" );
   }
   PushReal( acos( r1 ) );
}

/*
 * atan( )
 */

static	void	fie_atan( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Atan: Operation on BLANKS!" );
   PushReal( atan( r1 ) );
}

/*
 * atan2( )
 */

static	void	fie_atan2( void )
{
   double	r1, r2;

   if ( Blank( 0 ) || Blank( 1 ) ) Quit( "Atan2: Operation on BLANKS!" );
   r2 = PopReal( ); r1 = PopReal( );
   PushReal( atan2( r1, r2 ) );
}

/*
 * sinh( )
 */

static	void	fie_sinh( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Sinh: Operation on BLANKS!" );
   PushReal( sinh( r1 ) );
}

/*
 * cosh( )
 */

static	void	fie_cosh( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Cosh: Operation on BLANKS!" );
   PushReal( cosh( r1 ) );
}

/*
 * tanh( )
 */

static	void	fie_tanh( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Tanh: Operation on BLANKS!" );
   PushReal( tanh( r1 ) );
}

/*
 * exp( )
 */

static	void	fie_exp( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Exp: Operation on BLANKS!" );
   PushReal( exp( r1 ) );
}

/*
 * ln( )
 */

static	void	fie_ln( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Ln: Operation on BLANKS!" );
   if ( r1 <= 0.0 ) Quit( "Ln: Argument <= 0.0!" );
   PushReal( log( r1 ) );
}

/*
 * log( )
 */

static	void	fie_log( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Log: Operation on BLANKS!" );
   if ( r1 <= 0.0 ) Quit( "Log: Argument <= 0.0!" );
   PushReal( log10( r1 ) );
}

/*
 * sqrt( )
 */

static	void	fie_sqrt( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Sqrt: Operation on BLANKS!" );
   if ( r1 < 0.0 ) Quit( "Sqrt: Argument is < 0.0!" );
   PushReal( sqrt( r1 ) );
}

/*
 * abs( )
 */

static	void	fie_abs( void )
{
   if ( Blank( 0 ) ) Quit( "Abs: Operation on BLANKS!" );
   switch( Kind( 0 ) ) {
      case IntVar: {
         fint	i = PopInt( );

         PushInt( labs( i ) );
         break;
      }
      case RealVar: {
         double	r = PopReal( );

         PushReal( fabs( r ) );
         break;
      }
      default: {
         Quit( "Abs: Wrong type on Stack!" );
         break;
      }
   }
}

/*
 * erf( )
 */

static	void	fie_erf( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) {
      Quit( "Erf: Operation on BLANKS!" );
   } else {
      double p  =  0.327591100;
      double a1 =  0.254829592;
      double a2 = -0.284496736;
      double a3 =  1.421413741;
      double a4 = -1.453152027;
      double a5 =  1.061405429;
      double t1 = 1.0 / ( 1.0 + p * fabs( r1 ));
      double t2 = t1*t1, t3 = t1*t2, t4 = t1*t3, t5 = t4*t1;

      if ( r1 > 0.0 ) {
         PushReal( 1.0 - (a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-r1*r1));
      } else {
         PushReal((a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-r1*r1) - 1.0);
      }
   }
}

/*
 * erf( )
 */

static	void	fie_erfc( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) {
      Quit( "Erfc: Operation on BLANKS!" );
   } else {
      double p  =  0.327591100;
      double a1 =  0.254829592;
      double a2 = -0.284496736;
      double a3 =  1.421413741;
      double a4 = -1.453152027;
      double a5 =  1.061405429;
      double t1 = 1.0 / ( 1.0 + p * fabs( r1 ));
      double t2 = t1*t1, t3 = t1*t2, t4 = t1*t3, t5 = t4*t1;

      if ( r1 > 0.0 ) {
         PushReal((a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-r1*r1));
      } else {
         PushReal(2.0-(a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-r1*r1));
      }
   }
}

/*
 * Converts degrees to radians.
 */

static	void	fie_rad( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Rad: Operation on BLANKS!" );
   PushReal( r1 * 0.017453292519943295769237 );
}

/*
 * Converts radians to degrees.
 */

static	void	fie_deg( void )
{
   double	r1 = PopReal( );

   if ( Blank( -1 ) ) Quit( "Deg: Operation on BLANKS!" );
   PushReal( r1 * 57.295779513082320876798155 );
}

/*
 * Returns nearest integer.
 */

static	void	fie_nint( void )
{
   double	r1;

   r1 = PopReal( );
   PushInt( NINT( r1 ) );
}

#define	RANDOM	( ( (double) rand( ) + 1.0 ) / ( (double) RAND_MAX + 1.0 ) )

/*
 * fie_ranu generates uniform noise.
 */

static	void	fie_ranu( void )
{
   double	a1, a2;
   double	r;

   a2 = PopReal( ); a1 = PopReal( );
   if ( Blank( -2 ) || Blank( -1 ) ) Quit( "Ranu: BLANK in argument!" );
   if ( a2 <= a1 ) Quit( "Ranu: Error in range!" );
   r = RANDOM;
   PushReal( a1 + r * ( a2 - a1 ) );
}

/*
 * fie_rang generates gaussian noise.
 */

static	void	fie_rang( void )
{
   double	a1, a2;
   double	r1, r2;
   double	val;
   static int	oddran = 0;

   a2 = PopReal( ); a1 = PopReal( );
   if ( Blank( -2 ) || Blank( -1 ) ) Quit( "Rang: BLANK in argument!" );
   r1 = RANDOM;
   r2 = RANDOM;
   if ( oddran ) {
      val = sqrt(-2*log(r1))*cos(6.283185307179586476925286*r2); oddran = 0;
   } else {
      val = sqrt(-2*log(r1))*cos(6.283185307179586476925286*r2); oddran = 1;
   }
   PushReal( a1 + fabs( a2 ) * val );
}

/*
 * Sets the seed for the system random number generator.
 */

static	void	fie_seed( void )
{
   fint	seed = PopInt( );

   if ( Blank( -1 ) ) Quit( "Seed: Seed is BLANK!" );
   if ( seed < 0 ) Quit( "Seed: Seed < 0!" );
   srand( seed );
   PushInt( seed );
}

/*
 * Utility Functions:
 */

/*
 * Returns length of substituted string.
 */

static	void	fie_len( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( nelc_c( tofchar( TmpString1 ) ) );
}

/*
 * returns size of variable.
 */

static	void	fie_sizeof( void )
{
   int	v1, x1;

   v1 = PopAddress( &x1 );
   PushInt( GetSize( v1 ) );
}

/*
 * Returns the number of items last entered by the user.
 */

static	void	fie_entered( void )
{
   PushInt( LastEntered );
}

/*
 * Sets default level.
 */

static	void	fie_setdef( void )
{
   fint	old_level = DefaultLevel;

   DefaultLevel = PopInt( );
   PushInt( old_level );
}

/*
 * Converts string into integer.
 */

static	void	fie_atoi( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( strtol( TmpString1, NULL, 10 ) );
}

/*
 * Converts string into real.
 */

static	void	fie_atof( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   PushReal( strtod( TmpString1, NULL ) );
}

/*
 * Puts a blank on the stack.
 */

static	void	fie_blank( void )
{
   PushBlank( );
}

/*
 * Extracts substring.
 */

static	void	fie_substr1( void )
{
   fint		i2;
   char		*ptr = NULL;
   char		*sep = "\n\t ";
   int		sp;

   i2 = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   ptr = strtok( TmpString1, sep );
   while ( ( ptr != NULL ) && (--i2) ) {
      ptr = strtok( NULL, sep );
   }
   if ( ptr != NULL ) {
      sp = PutString( ptr );
      PushString( sp, strlen( ptr ), 1 );
   } else {
      sp = PutString( "" );
      PushString( sp, 1, 1 );
   }
}

/*
 * Gets part of string.
 */

static	void	fie_substr2( void )
{
   char	*ptr;
   fint	i1, i2;
   int	l;
   int	sp;

   i2 = PopInt( );
   i1 = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   l = strlen( TmpString1 );
   if ( i1 < 1 || i2 < 1 || i1 > l ) {
      sp = PutString( "" );
      PushString( sp, 1, 1 );
   } else {
      ptr = &TmpString1[i1-1];
      ptr[i2] = 0;
      sp = PutString( ptr );
      PushString( sp, strlen( ptr ), 1 );
   }
}

/*
 * Returns current time and date.
 */

static	void	fie_date( void )
{
   char		*ptr;
   int		sp;
   time_t	now = time( NULL );

   ptr = ctime( &now );
   if ( ptr != NULL ) {
      int	l = strlen( ptr );

      if ( ptr[l-1] == '\n' ) ptr[l-1] = 0;
      sp = PutString( ptr );
      PushString( sp, strlen( ptr ), 1 );
   } else {
      sp = PutString( "" );
      PushString( sp, 1, 1 );
   }
}

/*
 * Conversion.
 */

static	void	fie_factor( void )
{
   double	f;

   GetSubStrS( PopString( ), TmpString2 );
   GetSubStrS( PopString( ), TmpString1 );
   if ( factor_c( tofchar( TmpString1 ), tofchar( TmpString2 ), &f ) ) {
      PushReal( 0.0 );
   } else {
      PushReal( f );
   }
}

/*
 * Sets/Gets list device (terminal and/or logfile).
 */

static	void	fie_list( void )
{
   fint	devs;

   devs = PopInt( );
   PushInt( listctrl_c( &devs ) );
}

/*
 * Issue a shell command.
 */

static	void	fie_system( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( system( TmpString1 ) );
}

/*
 * File Handling Functions:
 */

/*
 * Checks whether a file exists.
 */

static	void	fie_existf( void )
{
   FILE	*f;

   GetSubStrS( PopString( ), TmpString1 );
   f = fopen( TmpString1, "r" );
   if ( f == NULL ) {
      PushBool( false );
   } else {
      fclose( f );
      PushBool( true );
   }
}

/*
 * Deletes a file.
 */

static	void	fie_deletef( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   if ( remove( TmpString1 ) ) {
      PushBool( false );
   } else {
      PushBool( true );
   }
}

/*
 * Opens a file.
 */

static	void	fie_fopen( void )
{
   int	fd = 0;

   GetSubStrS( PopString( ), TmpString2 );
   GetSubStrS( PopString( ), TmpString1 );
   while ( ( fd < FilesMax ) && Files[fd] != NULL ) fd++;
   if ( fd == FilesMax ) {
      fd = -1;
   } else {
      Files[fd] = fopen( TmpString1, TmpString2 );
      if ( Files[fd] == NULL ) fd = -2;
   }
   PushInt( fd );
}

/*
 * Closes a file.
 */

static	void	fie_fclose( void )
{
   int	a1, v1, x1;
   int	fd;

   v1 = PopAddress( &x1 ); a1 = GetAddrI( v1, x1 );
   fd = Data.ip[a1];
   if ( fd < 0 || fd >= FilesMax ) {
      fd = -1;
   } else if ( Files[fd] == NULL ) {
      fd = -2;
   } else {
      if ( !fclose( Files[fd] ) ) {
         Files[fd] = NULL;
         fd = 0;
      } else {
         fd = -3;
      }
   }
   PushInt( fd );
}

/*
 * Reads from file.
 */

static	void	fie_fread( void )
{
   int	a1, v1, x1;
   int	a2, v2, x2, l2;
   int	fd;

   v2 = PopAddress( &x2 ); a2 = GetAddrI( v2, x2 ); l2 = GetLen( v2 );
   v1 = PopAddress( &x1 ); a1 = GetAddrI( v1, x1 );
   fd = Data.ip[a1];
   if ( fd < 0 || fd >= FilesMax ) {
      fd = -1;
   } else if ( Files[fd] == NULL ) {
      fd = -2;
   } else {
      int	ch;
      int	n = 0;

      while ( ( ( ch = fgetc( Files[fd] ) ) != '\n' ) && ( ch != EOF ) ) {
         if ( n < l2 ) Data.sp[a2+n] = ch;
         n += 1;
      }
      if ( ch == EOF && !n ) {
         fd = -3;
      } else {
         fd = n;
         if ( n < l2 ) Data.sp[a2+n] = 0;
      }
   }
   PushInt( fd );
}

/*
 * Writes to file.
 */

static	void	fie_fwrite( void )
{
   int	a1, v1, x1;
   int	fd;

   GetSubStrS( PopString( ), TmpString1 );
   v1 = PopAddress( &x1 ); a1 = GetAddrI( v1, x1 );
   fd = Data.ip[a1];
   if ( fd < 0 || fd >= FilesMax ) {
      fd = -1;
   } else if ( Files[fd] == NULL ) {
      fd = -2;
   } else {
      fd = fprintf( Files[fd], "%s\n", TmpString1 );
      if ( fd == EOF ) {
         fd = -3;
      } else {
         fd = fd - 1;
      }
   }
   PushInt( fd );
}

/*
 * Set, Box and Position Parsing Functions:
 */

/*
 * Decodes set input into set and subsets.
 */

static	void	fie_getsas( void )
{
   int			a2, v2, x2;
   int			a3, v3, x3;
   dcdset_struct	*p;

   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 );
   v2 = PopAddress( &x2 ); a2 = GetAddrI( v2, x2 );
   GetSubStrS( PopString( ), TmpString1 );
   if ( GetKind( v2 ) != StringVar ) {
      Quit( "Getsas: Argument 2 wrong type!" );
   }
   if ( GetKind( v3 ) != IntVar ) {
      Quit( "Getsas: Argument 3 wrong type!" );
   }
   p = dcdset( TmpString1 );
   if ( p->status < 1 ) {
      PushInt( p->status );
      return;
   }
   if ( strlen( p->setname ) >= GetLen( v2 ) ) {
      Quit( "Getsas: Set name too long!" );
   }
   if ( p->status > ( GetSize( v3 ) - x3 ) ) {
      Quit( "Getsas: Too many subsets!" );
   }
   strncpy( &Data.sp[a2], p->setname, GetLen( v2 ) );
   memmove( &Data.ip[a3], p->subsets, p->status * sizeof( fint ) );
   PushInt( p->status );
}

/*
 * Decodes a box.
 */

static	void	fie_getbox( void )
{
   double	pos[100];
   fchar	set;
   int		n;
   int		a4, v4, x4;
   int		a5, v5, x5;
   fint		boxstat;
   fint		option = 0;
   fint		subset;
   fint		ndims;
   fint		*blo = NULL, *bhi = NULL;

   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString2 );
   set = tofchar( TmpString2 );
   GetSubStrS( PopString( ), TmpString1 );
   ndims = gdsc_ndims_c( set, &subset );
   if ( ndims > ( GetSize( v4 ) + x4 ) ) {
      Quit( "Getbox: Exceeding size of %.10s!", GetId( v4 ) );
   } else {
      blo = &Data.ip[a4];
   }
   if ( ndims > ( GetSize( v5 ) + x5 ) ) {
      Quit( "GetBox: Exceeding size of %.10s!", GetId( v5 ) );
   } else {
      bhi = &Data.ip[a5];
   }
   boxstat = dcdpos_c( set, &subset, tofchar( TmpString1 ), pos, &option );
   switch( boxstat ) {
      case 0: {
         break;
      }
      case 1: {
         for ( n = 0; n < ndims; n++ ) {
            blo[n] = NINT( pos[n] );
         }
         break;
      }
      case 2: {
         for ( n = 0; n < ndims; n++ ) {
            bhi[n] = NINT( pos[n] );
         }
         break;
      }
      case 3: {
         for ( n = 0; n < ndims; n++ ) {
            fint	c = NINT( pos[n] );
            fint	s = NINT( pos[n+ndims] );

            if ( s < 1 ) s = 1;
            bhi[n] = c + s / 2;
            blo[n] = bhi[n] + 1 - s;
         }
         break;
      }
      case 4: {
         for ( n = 0; n < ndims; n++ ) {
            fint	l = NINT( pos[n] );
            fint	u = NINT( pos[n+ndims] );

            if ( l > u ) {
               blo[n] = u;
               bhi[n] = l;
            } else {
               blo[n] = l;
               bhi[n] = u;
            }
         }
         boxstat = 3;
         break;
      }
      default: {
         break;
      }
   }
   PushInt( boxstat );
}

/*
 * Decodes positions.
 */

static	void	fie_getpos( void )
{
   double	*pos;
   int		a4, v4, x4;
   fint		boxstat;
   fint		option;
   fint		subset;
   fint		ndims;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 ); pos = &Data.dp[a4];
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString2 );
   GetSubStrS( PopString( ), TmpString1 );
   ndims = gdsc_ndims_c( tofchar( TmpString2 ), &subset );
   option = ( GetSize( v4 ) + x4 ) / ndims;
   if ( option < 1 ) {
      Quit( "Getpos: Exceeding size of %.10s!", GetId( v4 ) );
   }
   boxstat = dcdpos_c( tofchar( TmpString2 ), &subset, tofchar( TmpString1 ), pos, &option );
   PushInt( boxstat );
}

/*
 * fie_getsss encodes set and subset numbers into a text string.
 */

static	void	fie_getsss( void )
{
   fchar	string;
   int		a4, v4, x4;
   int		a2, v2, x2;
   fint		nsubs;
   fint		*subsets;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   string.a = &Data.sp[a4]; string.l = GetLen( v4 );
   nsubs = PopInt( );
   v2 = PopAddress( &x2 ); a2 = GetAddrI( v2, x2 );
   subsets = &Data.ip[a2];
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( ecdset_c( string, tofchar( TmpString1 ), subsets, &nsubs ) );
}

/*
 * Set and Subset handling Functions:
 */

/*
 * Deletes a set.
 */

static	void	fie_deletes( void )
{
   fint	gerror = 0;

   GetSubStrS( PopString( ), TmpString1 );
   gds_delete_c( tofchar( TmpString1 ), &gerror );
   PushBool( gerror >= 0 );
}

/*
 * Checks whether a set exists.
 */

static	void	fie_exists( void )
{
   fint		gerror = 0;

   GetSubStrS( PopString( ), TmpString1 );
   if ( tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) && ( gerror == 0 ) ) {
      PushBool( true );
      gds_close_c( tofchar( TmpString1 ), &gerror );
   } else {
      PushBool( false );
   }
}

/*
 * Returns the dimension of a subset.
 */

static	void	fie_dimension( void )
{
   fint		subset;

   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( gdsc_ndims_c( tofchar( TmpString1 ), &subset ) );
}

/*
 * Returns lower grid on axis.
 */

static	void	fie_lgrid( void )
{
   fint		i2;
   fint		clow = 0, cupp = 0;
   fint		gerror = 0;
   fint		grid;
   fint		level = 0;

   i2 = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsc_range_c( tofchar( TmpString1 ), &level, &clow, &cupp, &gerror );
   if ( gerror != 0 ) Quit( "Lgrid: GDSC_RANGE error in LGRID!" );
   grid = gdsc_grid_c( tofchar( TmpString1 ), &i2, &clow, &gerror );
   if ( gerror != 0 ) Quit( "Lgrid: GDSC_GRID error in LGRID!" );
   PushInt( grid );
}

/*
 * Returns upper grid on axis.
 */

static	void	fie_ugrid( void )
{
   fint		i2;
   fint		clow = 0, cupp = 0;
   fint		gerror = 0;
   fint		grid;
   fint		level = 0;

   i2 = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsc_range_c( tofchar( TmpString1 ), &level, &clow, &cupp, &gerror );
   if ( gerror != 0 ) Quit( "Ugrid: GDSC_RANGE error in UGRID!" );
   grid = gdsc_grid_c( tofchar( TmpString1 ), &i2, &cupp, &gerror );
   if ( gerror != 0 ) Quit( "Ugrid: GDSC_GRID error in UGRID!" );
   PushInt( grid );
}

/*
 * Adds an axis to a set.
 */

static	void	fie_extend( void )
{
   double	crpix;
   fint		*ptr;
   fint		size;
   fint		gerror = 0;

   size = PopInt( );
   if ( size > 0 ) ptr = &size; else ptr = NULL;
   crpix = PopReal( );
   GetSubStrS( PopString( ), TmpString2 );
   GetSubStrS( PopString( ), TmpString1 );
   gds_extend_c( tofchar( TmpString1 ), tofchar( TmpString2 ), &crpix, ptr,
      &gerror );
   PushInt( gerror );
}

/*
 * Gets subset axes.
 */

static	void	fie_getsax( void )
{
   fint		*axes;
   fint		gerror = 0;
   fint		m, n, ndim;
   fint		subset;
   int		a3, v3, x3;

   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 ); axes = &Data.ip[a3];
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( !tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) ) {
      PushInt( -5 );
      return;
   }
   ndim = gdsc_ndims_c( tofchar( TmpString1 ), &subset );
   if ( ndim > ( GetSize( v3 ) + x3 ) ) {
      Quit( "GetSax: Exceeding size of %.10s!", GetId( v3 ) );
   }
   for ( m = n = 0; m < ndim; n++ ) {
      fint	axnum = n + 1;
      fint	grid;

      grid = gdsc_grid_c( tofchar( TmpString1 ), &axnum, &subset, &gerror );
      if ( gerror ) {
         gerror = 0;
         axes[m++] = axnum;
      }
   }
   PushInt( 0 );
}

/*
 * Closes a set.
 */

static	void	fie_closes( void )
{
   fint	gerror = 0;

   GetSubStrS( PopString( ), TmpString1 );
   gds_close_c( tofchar( TmpString1 ), &gerror );
   PushInt( gerror );
}

/*
 * Descriptor Handling Functions:
 */

/*
 * Deletes a header item.
 */

static	void	fie_deleteh( void )
{
   fint	gerror = 0;
   fint	subset;

   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsd_delete_c( tofchar( TmpString1 ), tofchar( TmpString2 ), &subset,
      &gerror );
   PushInt( gerror );
}

/*
 * Checks existence of header item.
 */

static	void	fie_existh( void )
{
   fchar	type;
   fint		gerror = 0;
   fint		subset;

   type.a = TmpString3; type.l = sizeof( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsd_type_c( type, tofchar( TmpString1 ), tofchar( TmpString2 ),
      &subset, &gerror );
   PushInt( gerror );
}

/*
 * Returns the type of a descriptor item.
 */

static	void	fie_htype( void )
{
   fchar	type;
   fint		gerror = 0;
   fint		subset;
   int		a4, v4, x4;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   type.a = &Data.sp[a4]; type.l = GetLen( v4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsd_type_c( type, tofchar( TmpString1 ), tofchar( TmpString2 ), &subset, &gerror );
   PushInt( gerror );
}

/*
 * Read header item.
 */

static	void	fie_rhead( void )
{
   int			a4, v4, x4;
   fint			gerror = 0;
   fint			subset;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( !tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) ) {
      if ( gerror == 0 ) gerror = -39;
      PushInt( gerror );
      return;
   }
   switch( GetKind( v4 ) ) {
      case IntVar: {
         fint	value;

         if ( !strncmp( TmpString2, "NAXIS", 5 ) && isdigit( TmpString2[5] ) ) {
            fint	axnum;

            axnum = atoi( &TmpString2[5] );
            value = gdsc_size_c( tofchar( TmpString1 ), &axnum, &gerror );
         } else if ( !strncmp( TmpString2, "NAXIS", 5 ) && ( TmpString2[5] == ' ' || TmpString2[5] == '\0' ) ) {
            subset = 0;
            value = gdsc_ndims_c( tofchar( TmpString1 ), &subset );
         } else {
            gdsd_rint_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
               &subset, &value, &gerror );
         }
         Data.ip[a4] = value;
         break;
      }
      case LogicalVar: {
         bool	value;

         gdsd_rlog_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, &value, &gerror );
         Data.lp[a4] = tobool( value );
         break;
      }
      case RealVar: {
         float	value;

        gdsd_rreal_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, &value, &gerror );
         Data.rp[a4] = value;
         break;
      }
      case DoubleVar: {
         double	value;

         if ( !strncmp( TmpString2, "CRPIX", 5 ) && isdigit( TmpString2[5] ) ) {
            fint	axnum;

            axnum = atoi( &TmpString2[5] );
            value = gdsc_origin_c( tofchar( TmpString1 ), &axnum, &gerror );
         } else {
            gdsd_rdble_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
               &subset, &value, &gerror );
         }
         Data.dp[a4] = value;
         break;
      }
      case StringVar: {
         fchar	value;

         value.a = &Data.sp[a4];
         value.l = GetLen( v4 );
         gdsd_rchar_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, value, &gerror );
         break;
      }
      default: {
         Quit( "Rhead: Internal Error!" );
         break;
      }
   }
   PushInt( gerror );
}

/*
 * Writes a descriptor item.
 */

static	void	fie_whead( void )
{
   int			a4, v4, x4;
   fint			gerror = 0;
   fint			subset;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( !tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) ) {
      if ( gerror == 0 ) gerror = -39;
      PushInt( gerror );
      return;
   }
   switch( GetKind( v4 ) ) {
      case IntVar: {
         fint	value = Data.ip[a4];

         if ( !strncmp( TmpString2, "NAXIS", 5 ) && isdigit( TmpString2[5] ) ) {
            double	crpix;
            char	buffer[70];
            fchar	ctype;
            fint	axnum;

            ctype.a = buffer; ctype.l = sizeof( buffer );
            axnum = atoi( &TmpString2[5] );
            gdsc_name_c( ctype, tofchar( TmpString1 ), &axnum, &gerror );
            if ( gerror == 0 ) {
               crpix = gdsc_origin_c( tofchar( TmpString1 ), &axnum, &gerror );
               if ( gerror == 0 ) {
                  gds_extend_c( tofchar( TmpString1 ), ctype, &crpix, &value, &gerror );
               }
            }
         } else {
            gdsd_wint_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
               &subset, &value, &gerror );
         }
         break;
      }
      case LogicalVar: {
         bool	value = toflog( Data.lp[a4] );

         gdsd_wlog_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, &value, &gerror );
         break;
      }
      case RealVar: {
         float	value = Data.rp[a4];

         gdsd_wreal_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, &value, &gerror );
         break;
      }
      case DoubleVar: {
         double	value = Data.dp[a4];

         if ( !strncmp( TmpString2, "CRPIX", 5 ) && isdigit( TmpString2[5] ) ) {
            char	buffer[70];
            fchar	ctype;
            fint	axnum;

            ctype.a = buffer; ctype.l = sizeof( buffer );
            axnum = atoi( &TmpString2[5] );
            gdsc_name_c( ctype, tofchar( TmpString1 ), &axnum, &gerror );
            if ( gerror == 0 ) {
               gds_extend_c( tofchar( TmpString1 ), ctype, &value, NULL, &gerror );
            }
         } else {
            gdsd_wdble_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
               &subset, &value, &gerror );
         }
         break;
      }
      case StringVar: {
         fchar	value;

         value.a = &Data.sp[a4];
         value.l = GetLen( v4 );
         gdsd_wchar_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, value, &gerror );
         break;
      }
      default: {
         break;
      }
   }
   PushInt( gerror );
}

/*
 * Gets FITS record comments.
 */

static	void	fie_rfrcom( void )
{
   fchar	value;
   int		a4, v4, x4;
   fint		gerror = 0;
   fint		subset;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( !tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) ) {
      if ( gerror == 0 ) gerror = -39;
      PushInt( gerror );
      return;
   }
   value.a = &Data.sp[a4];
   value.l = GetLen( v4 );
   gdsd_rfrcom_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, value, &gerror );
   PushInt( gerror );
}

/*
 * Puts FITS record comments.
 */

static	void	fie_wfrcom( void )
{
   fchar	value;
   int		a4, v4, x4;
   fint		gerror = 0;
   fint		subset;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( !tobool( gds_exist_c( tofchar( TmpString1 ), &gerror ) ) ) {
      if ( gerror == 0 ) gerror = -39;
      PushInt( gerror );
      return;
   }
   value.a = &Data.sp[a4];
   value.l = GetLen( v4 );
   gdsd_wfrcom_c( tofchar( TmpString1 ), tofchar( TmpString2 ),
            &subset, value, &gerror );
   PushInt( gerror );
}

/*
 * Image Handling Functions:
 */

/*
 * Reads from an image.
 */

static	void	fie_rimage( void )
{
   fchar	set;
   int		n;
   int		a3, v3, x3;
   int		a4, v4, x4;
   int		a5, v5, x5;
   fint		cwlo, cwhi;
   fint		subset;
   fint		gerror = 0;
   fint		ndims;
   fint		ndone;
   fint		ntotal = 1;
   fint		*blo = NULL, *bhi = NULL;
   float	*data = NULL;

   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   set = tofchar( TmpString1 );
   ndims = gdsc_ndims_c( set, &subset );
   if ( ndims > ( GetSize( v3 ) + x3 ) ) {
      Quit( "Rimage: Exceeding size of %.10s!", GetId( v3 ) );
   } else {
      blo = &Data.ip[a3];
   }
   if ( ndims > ( GetSize( v4 ) + x4 ) ) {
      Quit( "Rimage: Exceeding size of %.10s!", GetId( v4 ) );
   } else {
      bhi = &Data.ip[a4];
   }
   for ( n = 0; n < ndims; n++ ) ntotal *= ( bhi[n] - blo[n] + 1 );
   if ( ntotal > ( GetSize( v5 ) + x5 ) ) {
      Quit( "Rimage: Exceeding size of %.10s!", GetId( v5 ) );
   } else {
      data = &Data.rp[a5];
   }
   cwlo = gdsc_fill_c( set, &subset, blo );
   cwhi = gdsc_fill_c( set, &subset, bhi );
   gdsi_read_c( set, &cwlo, &cwhi, data, &ntotal, &ndone, &gerror );
   if ( gerror == 0 ) {
      PushInt( ndone );
   } else {
      PushInt( gerror );
   }
}

/*
 * Writes to an image.
 */

static	void	fie_wimage( void )
{
   fchar	set;
   int		n;
   int		a3, v3, x3;
   int		a4, v4, x4;
   int		a5, v5, x5;
   fint		cwlo, cwhi;
   fint		subset;
   fint		gerror = 0;
   fint		ndims;
   fint		ndone;
   fint		ntotal = 1;
   fint		*blo = NULL, *bhi = NULL;
   float	*data = NULL;

   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   set = tofchar( TmpString1 );
   ndims = gdsc_ndims_c( set, &subset );
   if ( ndims > ( GetSize( v3 ) + x3 ) ) {
      Quit( "Wimage: Exceeding size of %.10s!", GetId( v3 ) );
   } else {
      blo = &Data.ip[a3];
   }
   if ( ndims > ( GetSize( v4 ) + x4 ) ) {
      Quit( "Wimage: Exceeding size of %.10s!", GetId( v4 ) );
   } else {
      bhi = &Data.ip[a4];
   }
   for ( n = 0; n < ndims; n++ ) ntotal *= ( bhi[n] - blo[n] + 1 );
   if ( ntotal > ( GetSize( v5 ) + x5 ) ) {
      Quit( "Wimage: Exceeding size of %.10s!", GetId( v5 ) );
   } else {
      data = &Data.rp[a5];
   }
   cwlo = gdsc_fill_c( set, &subset, blo );
   cwhi = gdsc_fill_c( set, &subset, bhi );
   gdsi_write_c( set, &cwlo, &cwhi, data, &ntotal, &ndone, &gerror );
   if ( gerror == 0 ) {
      PushInt( ndone );
   } else {
      PushInt( gerror );
   }
}

/*
 * Table Handling Functions:
 */

/*
 * Deletes a table.
 */

static	void	fie_deletet( void )
{
   fint	gerror = 0;
   fint	subset;

   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_deltab_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      &gerror );
   PushInt( gerror );
}

/*
 * Checks the existence of a table.
 */

static	void	fie_existt( void )
{
   fchar	cnames;
   fint		gerror = 0;
   fint		nfound = 0;
   fint		nitems;
   fint		subset;

   cnames.a = TmpString3; cnames.l = 8;
   nitems = sizeof( TmpString3 ) / 8;
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_tabinq_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      cnames, &nitems, &nfound, &gerror );
   if ( gerror < 0 ) {
      PushInt( gerror );
   } else {
      PushInt( nfound );
   }
}

/*
 * Gets info about a table.
 */

static	void	fie_tableq( void )
{
   fchar	cnames;
   int		a4, v4, x4;
   fint		subset;
   fint		gerror = 0;
   fint		nfound = 0;
   fint		nitems;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   cnames.a = &Data.sp[a4]; cnames.l = GetLen( v4 );
   nitems = GetSize( v4 ) - x4;
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_tabinq_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      cnames, &nitems, &nfound, &gerror );
   if ( gerror < 0 ) {
      PushInt( gerror );
   } else {
      PushInt( nfound );
   }
}

/*
 * Reads comments from a table.
 */

static	void	fie_rtablec( void )
{
   fchar	tcomm;
   fint		subset;
   fint		gerror = 0;
   int		a4, v4, x4;

   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 );
   tcomm.a = &Data.sp[a4]; tcomm.l = GetLen( v4 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_rdcom_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tcomm, &gerror );
   PushInt( gerror );
}

/*
 * Writes comments to a table.
 */

static	void	fie_wtablec( void )
{
   fint	subset;
   fint	gerror = 0;

   GetSubStrS( PopString( ), TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_wrcom_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), &gerror );
   PushInt( gerror );
}

/*
 * Deletes a column from a table.
 */

static	void	fie_deletec( void )
{
   fint	gerror = 0;
   fint	subset;


   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_delcol_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), &gerror );
   PushInt( gerror );
}

/*
 * Checks the existence of a column.
 */

static	void	fie_existc( void )
{
   char		ctypeb[132];
   char		ccommb[132];
   char		cuntsb[132];
   fchar	ctype;
   fchar	ccomm;
   fchar	cunts;
   fint		gerror = 0;
   fint		nrows;
   fint		subset;

   ctype.a = ctypeb; ctype.l = sizeof( ctypeb );
   ccomm.a = ccommb; ccomm.l = sizeof( ccommb );
   cunts.a = cuntsb; cunts.l = sizeof( cuntsb );
   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_colinq_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), ctype, ccomm, cunts, &nrows, &gerror );
   if ( gerror < 0 ) {
      PushInt( gerror );
   } else {
      PushInt( nrows );
   }
}

/*
 * Creates a column.
 */

static	void	fie_createc( void )
{
   fint	gerror = 0;
   fint	subset;

   GetSubStrS( PopString( ), TmpString6 );
   GetSubStrS( PopString( ), TmpString5 );
   GetSubStrS( PopString( ), TmpString4 ); StrUpp( TmpString4 );
   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_crecol_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), tofchar( TmpString4 ), tofchar( TmpString5 ),
      tofchar( TmpString6 ), &gerror );
   PushInt( gerror );
}

/*
 * Returns the column data type.
 */

static	void	fie_ctype( void )
{
   char		ccommb[132];
   char		cuntsb[132];
   fchar	ctype;
   fchar	ccomm;
   fchar	cunts;
   fint		gerror = 0;
   fint		nrows;
   fint		subset;
   int		a5, v5, x5;

   ccomm.a = ccommb; ccomm.l = sizeof( ccommb );
   cunts.a = cuntsb; cunts.l = sizeof( cuntsb );
   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   ctype.a = &Data.sp[a5]; ctype.l = GetLen( v5 );
   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_colinq_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), ctype, ccomm, cunts, &nrows, &gerror );
   PushInt( gerror );
}

/*
 * Gets info about a column.
 */

static	void	fie_columnq( void )
{
   fchar	ctype;
   fchar	ccomm;
   fchar	cunts;
   int		a5, v5, x5;
   int		a6, v6, x6;
   int		a7, v7, x7;
   fint		subset;
   fint		gerror = 0;
   fint		nrows = 0;

   v7 = PopAddress( &x7 ); a7 = GetAddrI( v7, x7 );
   cunts.a = &Data.sp[a7]; cunts.l = GetLen( v7 );
   v6 = PopAddress( &x6 ); a6 = GetAddrI( v6, x6 );
   ccomm.a = &Data.sp[a6]; ccomm.l = GetLen( v6 );
   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   ctype.a = &Data.sp[a5]; ctype.l = GetLen( v5 );
   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   gdsa_colinq_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
      tofchar( TmpString3 ), ctype, ccomm, cunts, &nrows, &gerror );
   if ( gerror < 0 ) {
      PushInt( gerror );
   } else {
      PushInt( nrows );
   }
}

/*
 * Reads from a column.
 */

static	void	fie_rcolumn( void )
{
   int		a5, v5, x5;
   fint		subset;
   fint		gerror = 0;
   fint		item;
   fint		nitems;

   nitems = PopInt( );
   item = PopInt( );
   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   GetSubStrS( PopString( ), TmpString3 ); StrUpp( TmpString3 );
   GetSubStrS( PopString( ), TmpString2 ); StrUpp( TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( nitems > ( GetSize( v5 ) + x5 ) ) {
      Quit( "Rcolumn: Exceeding size of output buffer!" );
   }
   switch( GetKind( v5 ) ) {
      case IntVar: {
         fint	*ptr = &Data.ip[a5];

         gdsa_rcint_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case LogicalVar: {
         bool	*ptr = &Data.lp[a5];
         int	n;

         gdsa_rclog_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         for ( n = 0; n < nitems; n++ ) {
            ptr[n] = tobool( ptr[n] );
         }
         break;
      }
      case RealVar: {
         float	*ptr = &Data.rp[a5];

         gdsa_rcreal_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case DoubleVar: {
         double	*ptr = &Data.dp[a5];

         gdsa_rcdble_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case StringVar: {
         fchar	ptr;

         ptr.a = &Data.sp[a5];
         ptr.l = GetLen( v5 );
         gdsa_rcchar_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      default: {
         Quit( "Rcolumn: Internal Error!" );
         break;
      }
   }
   PushInt( gerror );
}

/*
 * Writes to a column.
 */

static	void	fie_wcolumn( void )
{
   int		a5, v5, x5;
   fint		subset;
   fint		gerror = 0;
   fint		item;
   fint		nitems;

   nitems = PopInt( );
   item = PopInt( );
   v5 = PopAddress( &x5 ); a5 = GetAddrI( v5, x5 );
   GetSubStrS( PopString( ), TmpString3 );
   GetSubStrS( PopString( ), TmpString2 );
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   if ( nitems > ( GetSize( v5 ) + x5 ) ) {
      Quit( "Wcolumn: Exceeding size of %.10s!", GetId( v5 ) );
   }
   switch( GetKind( v5 ) ) {
      case IntVar: {
         fint	*ptr = &Data.ip[a5];

         gdsa_wcint_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case LogicalVar: {
         bool	*ptr = &Data.lp[a5];
         int	n;

         for ( n = 0; n < nitems; n++ ) {
            ptr[n] = toflog( ptr[n] );
         }
         gdsa_wclog_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case RealVar: {
         float	*ptr = &Data.rp[a5];

         gdsa_wcreal_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case DoubleVar: {
         double	*ptr = &Data.dp[a5];

         gdsa_wcdble_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      case StringVar: {
         fchar	ptr;

         ptr.a = &Data.sp[a5];
         ptr.l = GetLen( v5 );
         gdsa_wcchar_c( tofchar( TmpString1 ), &subset, tofchar( TmpString2 ),
            tofchar( TmpString3 ), ptr, &item, &nitems, &gerror );
         break;
      }
      default: {
         Quit( "Wcolumn: Internal Error!" );
         break;
      }
   }
   PushInt( gerror );
}

/*
 * Coordinate Handling Functions:
 */

/*
 * Does a coordinate transformarion.
 */

static	void	fie_cotrans( void )
{
   double	*in;
   double	*ou;
   fint		dir;
   fint		subset;
   int		a4, v4, x4;
   int		a3, v3, x3;

   dir = PopInt( );
   v4 = PopAddress( &x4 ); a4 = GetAddrI( v4, x4 ); ou = &Data.dp[a4];
   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 ); in = &Data.dp[a3];
   subset = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( cotrans_c( tofchar( TmpString1 ), &subset, in, ou, &dir ) );
}

/*
 * Gets units of axis.
 */

static	void	fie_axunit( void )
{
   fchar	cunit;
   fint		axnum;
   int		a3, v3, x3;

   v3 = PopAddress( &x3 ); a3 = GetAddrI( v3, x3 );
   cunit.a = &Data.sp[a3]; cunit.l = GetLen( v3 );
   axnum = PopInt( );
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( axunit_c( tofchar( TmpString1 ), &axnum, cunit ) );
}

/*
 * Returns number of output coordinates for cotrans.
 */

static	void	fie_ncoords( void )
{
   GetSubStrS( PopString( ), TmpString1 );
   PushInt( ncoords_c( tofchar( TmpString1 ) ) );
}

/*
 * Quit generates a fatal error message.
 */

static	void	Quit( char fmt[], ... )
{
   char		string[StringMax+1];		/* buffer for output string */
   fint		error_level = 4;		/* FATAL error */
   va_list	args;				/* the argument list */

   va_start( args, fmt );
   vsprintf( string, fmt, args );
   error_c( &error_level, tofchar( string ) );
}

/*
 * Output puts out a text string.
 */

static	void	Output( char fmt[], ... )
{
   char		string[StringMax+1];
   fint		output_level = 0;
   va_list	args;

   va_start( args, fmt );
   vsprintf( string, fmt, args );
   anyout_c( &output_level, tofchar( string ) );
}

/*
 * Error generates an error message and then quits.
 */

static	void	Error( char *msg )
{
   if (!EofSource) {
      Output( "Line %5.5d [ %s]", LineNr, Inline );
   }
   Quit( "Error: %s!", msg );
}

/*
 * GetInt prompts the user for integers.
 */

static	int	GetInt( fint iarray[], int n, int d, char *key, char *prompt )
{
   fint	input_level = d;
   fint	nitems = n;
   int	r;

   r = userint_c( iarray, &nitems, &input_level, tofchar( key ), tofchar( prompt ) );
   return( r );
}

/*
 * GetReal prompts the user for reals.
 */

static	int	GetReal( float rarray[], int n, int d, char *key, char *prompt )
{
   fint	input_level = d;
   fint	nitems = n;
   int	r;

   r = userreal_c( rarray, &nitems, &input_level, tofchar( key ), tofchar( prompt ) );
   return( r );
}

/*
 * GetDouble prompts the user for doubles.
 */

static	int	GetDouble( double darray[], int n, int d, char *key, char *prompt )
{
   fint	input_level = d;
   fint	nitems = n;
   int	r;

   r = userdble_c( darray, &nitems, &input_level, tofchar( key ), tofchar( prompt ) );
   return( r );
}

/*
 * GetLogical prompts the user for logicals.
 */

static	int	GetLogical( bool larray[], int n, int d, char *key, char *prompt )
{
   fint	input_level = d;
   fint	nitems = n;
   int	r;
   int	i;

   for ( i = 0; i < n; i++ ) larray[i] = toflog( larray[i] );
   r= userlog_c( larray, &nitems, &input_level, tofchar( key ), tofchar( prompt ) );
   for ( i = 0; i < n; i++ ) larray[i] = tobool( larray[i] );
   return( r );
}

/*
 * GetStr prompts the user for string values.
 */

static	int	GetStr( char *s, int len, int n, int d, char *key, char *prompt )
{
   fchar	string;
   fint		input_level = d;
   fint		nitems = n;
   int		r;

   string.a = s;
   string.l = len;
   if ( n == 1 ) {
      r = usertext_c( string, &input_level, tofchar( key ), tofchar( prompt ) );
      if ( r && r < len ) string.a[r] = 0;
   } else {
      int	i, j;

      r = userchar_c( string, &nitems, &input_level, tofchar( key ), tofchar( prompt ) );
      for ( i = 0; i < r; i++ ) {
         string.a = &s[i*len];
         j = nelc_c( string );
         if ( j < len ) string.a[j] = 0;
      }
   }
   return( r );
}

/*
 * GenByte puts a byte in the code buffer.
 */

static	void	GenByte( byte b )
{
   if ( CP == CodeMax ) CodeExtend( );
   Memory[CP++] = b;
}

/*
 * GenCode0 generates code.
 */

static	void	GenCode0( byte opc )
{
   if ( makelist ) {
      fprintf( list, "%5.5d         [ %s                     ]\n", CP,
         Mnemonics[opc] );
   }
   PrCode = true;
   GenByte( opc );
}

/*
 * GenCode1 generates code with 1 operand.
 */

static	void	GenCode1( byte opc, int oper )
{
   int		i;
   union {
      byte	b[sizeof(int)];
      int	i;
   } u;

   if ( makelist ) {
      fprintf( list, "%5.5d         [ %s  %8d           ]\n", CP,
         Mnemonics[opc], oper );
   }
   PrCode = true;
   GenByte( opc );
   u.i = oper;
   for ( i = 0; i < sizeof( int ); i++ ) GenByte( u.b[i] );
}

/*
 * GenCode2 generates code with 2 operands.
 */

static	void	GenCode2( byte opc, int oper1, int oper2 )
{
   int		i;
   union {
      byte	b[sizeof(int)];
      int	i;
   } u;

   if ( makelist ) {
      fprintf( list, "%5.5d         [ %s  %8d  %8d ]\n", CP,
         Mnemonics[opc], oper1, oper2 );
   }
   PrCode = true;
   GenByte( opc );
   u.i = oper1;
   for ( i = 0; i < sizeof( int ); i++ ) GenByte( u.b[i] );
   u.i = oper2;
   for ( i = 0; i < sizeof( int ); i++ ) GenByte( u.b[i] );
}

/*
 * GenCode1At generates code oper at addr.
 */

static	void	GenCode1At ( int addr, byte opc, int oper )
{
   int	saveCP = CP;

   if ( makelist ) fprintf( list, "*****  *****\n" );
   CP = addr;
   GenCode1( opc, oper );
   CP = saveCP;
}

/*
 * PushInt puts an integer on the stack.
 */

static	void	PushInt( int i )
{
   if ( SP == StackSizeMax ) Quit( "PushInt: Stack overflow!" );
   Stack[SP].kind  = IntVar;
   Stack[SP].val.i = i;
   Stack[SP].blank = false;
   if ( Debug ) Output( "PushInt:    SP=%4d (%d)", SP, i );
   SP++;
}

/*
 * PushReal puts a real on the stack.
 */

static	void	PushReal( double r )
{
   if ( SP == StackSizeMax ) Quit( "PushReal: Stack overflow!" );
   Stack[SP].kind  = RealVar;
   Stack[SP].val.r = r;
   Stack[SP].blank = false;
   if ( Debug ) Output( "PushReal:   SP=%4d (%f)", SP, r );
   SP++;
}

/*
 * PushBlank puts a BLANK on the stack.
 */

static	void	PushBlank( void )
{
   if ( SP == StackSizeMax ) Quit( "PushBlank: Stack overflow!" );
   Stack[SP].kind  = RealVar;
   Stack[SP].val.r = 0.0;
   Stack[SP].blank = true;
   if ( Debug ) Output( "PushReal:   SP=%4d (BLANK)", SP );
   SP++;
}

/*
 * PushBool puts a logical on the stack.
 */

static	void	PushBool( bool b )
{
   if ( SP == StackSizeMax ) Quit( "PushBool: Stack overflow!" );
   Stack[SP].kind  = LogicalVar;
   Stack[SP].val.b = b;
   Stack[SP].blank = false;
   if ( Debug ) Output( "PushBool:   SP=%4d (%d)", SP, b );
   SP++;
}

/*
 * PushString puts a string on the stack.
 */

static	void	PushString( int s, int l, int t )
{
   if (SP == StackSizeMax) Quit( "PushString: Stack overflow!" );
   Stack[SP].kind   = StringVar;
   Stack[SP].val.s  = s;
   Stack[SP].length = l;
   Stack[SP].tmp    = t;
   Stack[SP].blank  = false;
   if ( Debug ) Output( "PushString: SP=%4d (%d,%d,%d)", SP, s, l, t );
   SP++;
}

/*
 * Dup duplicates the stack.
 */

static	void	Dup( void )
{
   if ( SP == StackSizeMax ) {
      Quit( "Dup: Stack overflow!" );
   } else if ( SP <= 0 ) {
      Quit( "Dup: Stack underflow!" );
   }
   Stack[SP] = Stack[SP-1];
   SP++;
}

/*
 * Dp2 duplicates two values on the stack.
 */

static	void	Dp2( void )
{
   if ( ( SP + 1 ) == StackSizeMax ) {
      Quit( "Dp2: Stack overflow!" );
   } else if ( SP <= 1 ) {
      Quit( "Dp2: Stack underflow!" );
   }
   Stack[SP] = Stack[SP-2];
   SP++;
   Stack[SP] = Stack[SP-2];
   SP++;
}

/*
 * Pop pops the stack.
 */

static	void	Pop( void )
{
   if ( SP == 0 ) Quit( "Pop: Stack underflow!" );
   SP--;
   if ( Debug ) Output( "Pop:        SP=%4d", SP );
}

/*
 * Lds puts a string value on the stack.
 */

static	void	Lds( int s )
{
   if ( SP == StackSizeMax ) Quit( "Lds: Stack overflow!" );
   Stack[SP].kind = StringVar;
   Stack[SP].val.s = s;
   Stack[SP].length = strlen( &Data.sp[s] );
   SP++;
}

/*
 * Ldv puts a variable on the stack.
 */

static	void	Ldv( int v )
{
   int	a;
   int	x;

   x = PopIndex( v ); a = GetAddrI( v, x );
   switch( GetKind( v ) ) {
      case IntVar: {
         PushInt( Data.ip[a] );
         break;
      }
      case LogicalVar: {
         PushBool( Data.lp[a] );
         break;
      }
      case RealVar: {
         if ( Data.rp[a] == FBLANK ) PushBlank( ); else PushReal( Data.rp[a] );
         break;
      }
      case DoubleVar: {
         if ( Data.dp[a] == FBLANK ) PushBlank( ); else PushReal( Data.dp[a] );
         break;
      }
      case StringVar: {
         PushString( a, GetLen( v ), 0 );
         break;
      }
      default: {
         Quit( "Ldv: Internal Error!" );
         break;
      }
   }
}

/*
 * Stv stores the next value from stack into a variable.
 */

static	void	Stv( int v )
{
   int	a;
   int	x;

   x = PopIndex( v ); a = GetAddrI( v, x );
   switch( GetKind( v ) ) {
      case IntVar: {
         Data.ip[a] = PopInt( );
         break;
      }
      case RealVar: {
         double	v = PopReal( );

         if ( Blank( -1 ) ) Data.rp[a] = FBLANK; else Data.rp[a] = v;
         break;
      }
      case DoubleVar: {
         double	v = PopReal( );

         if ( Blank( -1 ) ) Data.dp[a] = DBLANK; else Data.dp[a] = v;
         break;
      }
      case LogicalVar: {
         Data.lp[a] = PopBool( );
         break;
      }
      case StringVar: {
         GetSubStrS( PopString( ), TmpString1 );
         strncpy( &Data.sp[a], TmpString1, GetLen( v ) );
         break;
      }
      default: {
         Quit( "Stv: Internal Error!" );
         break;
      }
   }
}

/*
 * Can cancels the keyword associated with a variable.
 */

static	void	Can( int oper1 )
{
   char	keyword[MaxIdLen+2];

   sprintf( keyword, "%s=", GetId( oper1 ) );
   cancel_c( tofchar( keyword ) );
}

/*
 * Out puts out some text.
 */

static	void	Out( int oper1 )
{
   fint		output_level = 0;

   GetSubStr( oper1, CurString );
   anyout_c( &output_level, tofchar( CurString ) );
}

/*
 * Brl tests the loop.
 */

static	void	Brl( int vn )
{
   bool	next = false;
   int	v, s, e;

   v = Data.ip[GetAddr( vn )];
   s = PopInt( );
   e = PopInt( );
   if ( s == 0 ) {
      Quit( "Brl: Loop Increment = 0!" );
   } else if ( s < 0 ) {
      if ( v >= e ) next = true;
   } else if ( s > 0 ) {
      if ( v <= e ) next = true;
   }
   PushBool( next );
}
/*
 * Exe executes a GIPSY command.
 */

static	void	Exe( int oper1 )
{
   fint	ier;

   GetSubStr( oper1, CurString );
   ClearString( CurString );
   if ( Funname ) {
      char	*s;
      int	subst = 1;
      int	n = 0, m;

      s = strpbrk( CurString, " ,(" );
      if ( s != NULL ) {
         while ( *s == ' ' || *s == ',' ) s++;
         if ( *s == '(' ) subst = 0;
      }
      if ( subst ) {
         strcpy( TmpString1, CurString );
         while ( TmpString1[n] && TmpString1[n] != ' ' && TmpString1[n] != ',' ) {
            TmpString2[n] = TmpString1[n]; n++;
         }
         TmpString2[n] = 0; m = n;
         while ( m-- && TmpString2[m] != '/' ); m++;
         sprintf( CurString, "%s_%s(%s)%s", &TmpString2[m], ProgNam, &TmpString2[m], &TmpString1[n] );
      }
   }
   if ( !makestoryboard ) {
      void	gds_closeall_c( );

      gds_closeall_c( );
      xeq_c( tofchar(CurString), &ier );
      switch( ier ) {
         case 1: {
            break;
         }
         case -1: {
            Quit( "Exe: Task Cannot be run!" );
            break;
         }
         case -2: {
            Quit( "Exe: Task exited before calling INIT!" );
            break;
         }
         case -3: {
            Quit( "Exe: Fatal execution error!" );
            break;
         }
         case -4: {
            Quit( "Exe: Task crashed!" );
            break;
         }
         case -5: {
            Quit( "Exe: User abort!" );
            break;
         }
         default: {
            Quit( "Exe: Undocumented error (%d) from xeq!", (int) ier );
            break;
         }
      }
   } else {
      fint	output_level = 0;

      anyout_c( &output_level, tofchar( CurString ) );
      fprintf( story, "\"%s\"\n", CurString );
   }
}

/*
 * In allows the user to fill a variable.
 */

static	void	In( int oper1, int oper2 )
{
   char	keyword[MaxIdLen+2];
   int	addr;
   int	size;

   if ( oper1 != 0 ) {
      GetSubStr( oper1, CurString );
   } else {
      sprintf( CurString, "Value for variable: %s", GetId( oper2 ) );
   }
   sprintf( keyword, "%s=", GetId( oper2 ) );
   addr = GetAddr( oper2 );
   size = GetSize( oper2 );
   switch( GetKind( oper2 ) ) {
      case IntVar: {
         LastEntered = GetInt( &Data.ip[addr], size, DefaultLevel, keyword,
            CurString );
         break;
      }
      case RealVar: {
         LastEntered = GetReal( &Data.rp[addr], size, DefaultLevel, keyword,
            CurString );
         break;
      }
      case DoubleVar: {
         LastEntered = GetDouble( &Data.dp[addr], size, DefaultLevel, keyword,
            CurString );
         break;
      }
      case LogicalVar: {
         LastEntered = GetLogical( &Data.lp[addr], size, DefaultLevel, keyword,
            CurString );
         break;
      }
      case StringVar: {
         LastEntered = GetStr( &Data.sp[addr], GetLen( oper2 ), size,
            DefaultLevel, keyword, CurString );
         break;
      }
      default: {
         Quit( "In: Internal Error!" );
         break;
      }
   };
   if ( CancelKeys ) cancel_c( tofchar( keyword ) );
}

/*
 * PopVal gets the next value from the stack.
 */

static	void PopVal( byte *kind, fint *i, double *r, bool *b, int *s, int *l, int *t )
{
   if ( SP == 0 ) Quit( "PopVal: Stack underflow!" );
   *kind = Stack[--SP].kind;
   switch( *kind ) {
      case IntVar: {
         *i = Stack[SP].val.i;
         break;
      }
      case RealVar: {
         *r = Stack[SP].val.r;
         break;
      }
      case LogicalVar: {
         *b = Stack[SP].val.b;
         break;
      }
      case StringVar: {
         *s = Stack[SP].val.s;
         *l = Stack[SP].length;
         *t = Stack[SP].tmp;
         break;
      }
      default: {
         break;
      }
   }
}

/*
 * PopString gets a string from the stack.
 */

static	Str	PopString( void )
{
   bool		b1;
   byte		k1;
   double	r1;
   fint		i1;
   int		l1;
   int		s1;
   int		t1;
   Str		r;

   PopVal( &k1, &i1, &r1, &b1, &s1, &l1, &t1 );
   if ( k1 != StringVar ) Quit( "PopString: Wrong type on stack!" );
   r.s = s1; r.l = l1;
   if ( ( ( s1 + l1 + 1 ) == DataPtr ) && t1 ) {
      DataPtr -= ( l1 + 1 );
   }
   return( r );
}

/*
 * PopReal gets a real from the stack.
 */

static	double	PopReal( void )
{
   bool		b1;
   byte		k1;
   double	r1;
   fint		i1;
   int		l1;
   int		s1;
   int		t1;
   double	r = 0.0;

   PopVal( &k1, &i1, &r1, &b1, &s1, &l1, &t1 );
   switch( k1 ) {
      case IntVar: {
         r = i1;
         break;
      }
      case RealVar: {
         r = r1;
         break;
      }
      default: {
         Quit( "PopReal: Wrong type on stack!" );
         break;
      }
   }
   return( r );
}

/*
 * PopInt gets an integer from the stack.
 */

static	fint	PopInt( void )
{
   bool		b1;
   byte		k1;
   double	r1;
   fint		i1;
   int		l1;
   int		s1;
   int		t1;
   fint		r = 0;

   PopVal( &k1, &i1, &r1, &b1, &s1, &l1, &t1 );
   switch( k1 ) {
      case IntVar: {
         r = i1;
         break;
      }
      case RealVar: {
         if ( Blank( -1 ) ) Quit( "PopInt: Operation on BLANK!" );
         r = r1;
         break;
      }
      default: {
         Quit( "PopInt: Wrong type on stack!" );
         break;
      }
   }
   return( r );
}

/*
 * PopBool gets a logical from the stack.
 */

static	bool	PopBool( void )
{
   bool		b1;
   byte		k1;
   double	r1;
   fint		i1;
   int		l1;
   int		s1;
   int		t1;
   bool		r = 0;

   PopVal( &k1, &i1, &r1, &b1, &s1, &l1, &t1 );
   switch( k1 ) {
      case LogicalVar: {
         r = b1;
         break;
      }
      default: {
         Quit( "PopBool: Wrong type on stack!" );
         break;
      }
   }
   return( r );
}

/*
 * PushBoolConst store a logical constant.
 */

static	int	PushBoolConst( bool bcst )
{
   int	i = 0;

   while ( ( i < NBoolConsts ) && ( BoolConsts[i] != bcst ) ) i++;
   if ( i == NBoolConsts ) {
      if ( NBoolConsts < ( sizeof(BoolConsts) / sizeof(bool) ) ) {
         BoolConsts[NBoolConsts++] = bcst;
      } else {
         Error( "[PushBoolConst] Out of space for Logical Constants" );
      }
   }
   return( i );
}

/*
 * PushIntConst stores an integer constant.
 */

static	int	PushIntConst( int icst )
{
   int	i = 0;

   while ( ( i < NIntConsts ) && ( IntConsts[i] != icst ) ) i++;
   if ( i == NIntConsts ) {
      if ( NIntConsts == IntConstsMax ) {
         IntConstsExtend( );
      }
      IntConsts[NIntConsts++] = icst;
   }
   return( i );
}

/*
 * PushRealConst stores a real constant.
 */

static	int	PushRealConst( double rcst )
{
   int	i = 0;

   while ( ( i < NRealConsts ) && ( RealConsts[i] != rcst ) ) i++;
   if ( i == NRealConsts ) {
      if ( NRealConsts == RealConstsMax ) {
         RealConstsExtend( );
      }
      RealConsts[NRealConsts++] = rcst;
   }
   return( i );
}

/*
 * PutBool stores a logical in the DATA buffer.
 */

static	int	PutBool( bool b )
{
   int	p = Offset( DataPtr, bool );		/* get logical offset */

   DataPtr = p * sizeof( bool );		/* new data pointer */
   if ( DataPtr == DataMax ) DataExtend( );	/* extend data area */
   Data.lp[p] = b;				/* assign */
   DataPtr += sizeof( bool );			/* increment pointer */
   return( p );
}

/*
 * PutNBool reserves space in the DATA buffer.
 */

static	int	PutNBool( int n )
{
   int	i;
   int	r;

   r = PutBool( false );
   for ( i = 1; i < n; i++ ) PutBool( false );
   return( r );
}

/*
 * PutInt stores an integer in the DATA buffer.
 */

static	int	PutInt( int i )
{
   int	p = Offset( DataPtr, int );

   DataPtr = p * sizeof( int );
   if ( DataPtr == DataMax ) DataExtend( );
   Data.ip[p] = i;
   DataPtr += sizeof( int );
   return( p );
}

/*
 * PutNInt reserves space for an integer variable.
 */

static	int	PutNInt( int n )
{
   int	i;
   int	r;

   r = PutInt( 0 );
   for ( i = 1; i < n; i++ ) PutInt( 0 );
   return( r );
}

/*
 * PutReal stores a real in the DATA buffer.
 */

static	int	PutReal( float r )
{
   int	p = Offset( DataPtr, float );

   DataPtr = p * sizeof( float );
   if ( DataPtr == DataMax ) DataExtend( );
   Data.rp[p] = r;
   DataPtr += sizeof( float );
   return( p );
}

/*
 * PutNReal reserves space for an real variable.
 */

static	int	PutNReal( int n )
{
   int	i;
   int	r;

   r = PutReal( 0.0 );
   for ( i = 1; i < n; i++ ) PutReal( 0.0 );
   return( r );
}

/*
 * PutDouble stores a double in the DATA buffer.
 */

static	int	PutDouble( double d )
{
   int	p = Offset( DataPtr, double );

   DataPtr = p * sizeof( double );
   if ( DataPtr == DataMax ) DataExtend( );
   Data.dp[p] = d;
   DataPtr += sizeof( double );
   return( p );
}

/*
 * PutNDouble reserves space for a double variable.
 */

static	int	PutNDouble( int n )
{
   int	i;
   int	r;

   r = PutDouble( 0.0 );
   for ( i = 1; i < n; i++ ) PutDouble( 0.0 );
   return( r );
}

/*
 * PutNString reserves space for a string variable.
 */

static	int	PutNString( int n )
{
   int	i;
   int	r = DataPtr;

   for ( i = 0; i < n; i++ ) {
      if ( DataPtr == DataMax ) DataExtend( );
      Data.sp[DataPtr++] = '\0';
   }
   return( r );
}

/*
 * PutString stores a string in the DATA buffer.
 */

static	int	PutString ( char s[] )
{
   int	r = DataPtr;
   int	i = 0;

   do {
      if ( DataPtr == DataMax ) DataExtend( );
      Data.sp[DataPtr++] = s[i++];
   } while ( s[i-1] != 0 );
   return ( r );
}

/*
 * ClearString clears a string from weird printable characters.
 */

static	void	ClearString( char str[] )
{
   int	i = 0, u = 0;
   char	ch;

   while ( str[i] != 0 ) {
      ch = str[i];
      if ( ( ch=='\n' ) || ( ch=='\t' ) ) ch = ' ';
      if ( isalpha( ch ) ) {
         str[u] = ch;
         i++ ; u++ ;
      } else if ( ch <= 1 && ch >= 4 ) {
         str[u++] = ch;  i++;
         str[u++] = str[i++];
      } else if ( ( ch == ' ' ) || ( ch == ',' ) ) {
         while ( ( str[i]==' ') || ( str[i]==',') ||
            ( str[i]=='\n' ) || ( str[i]=='\t' ) ) i++;
         str[u++] = ch;
      } else {
         str[u++] = ch;
         i++;
      };
   };
   str[u] = 0;
}

/*
 * EnterReserved puts a reserved symbol in the symbol table.
 */

static	void EnterReserved ( char id[] , int sym )
{
   int i;

   if ( Nsym == SymbolTableMax ) SymbolTableExtend( );
   for ( i = 0 ; i < MaxIdLen && id[i]; i++ ) SymTab[Nsym].id[i] = toupper(id[i]);
   while ( i < MaxIdLen ) SymTab[Nsym].id[i++] = 0;
   SymTab[Nsym].kind = Reserved;
   SymTab[Nsym].code = sym;
   Nsym++;
   Nreserved++;
}

/*
 * EnterVar puts an identifier in the symbol table.
 */

static	int	EnterVar( char id[] , int vt )
{
   int i;

   if ( Nsym == SymbolTableMax ) SymbolTableExtend( );
   /*
    * Convert id to uppercase.
    *
    * Nov 13, 1991: KGB
    */
   for ( i = 0 ; i < MaxIdLen && id[i] ; i++ ) SymTab[Nsym].id[i] = toupper(id[i]);
   while ( i < MaxIdLen ) SymTab[Nsym].id[i++] = 0;
   switch( vt ) {
      case IntSym: {
         vt = IntVar;
         break;
      }
      case LogicalSym: {
         vt = LogicalVar;
         break;
      }
      case RealSym: {
         vt = RealVar;
         break;
      }
      case DoubleSym: {
         vt = DoubleVar;
         break;
      }
      case StringSym: {
         vt = StringVar;
         break;
      }
      default: {
         Error( "[EnterVar] Internal error" );
         break;
      }
   }
   SymTab[Nsym].kind = vt;
   Nsym++;
   return( Nsym - Nreserved - 1 );
}

/*
 * LookUp searches the symbol table.
 */

static	int	LookUp( char id[] , int *code )
{
   int i = Nsym;

   *code = -1;
   while ( i > 0 ) {
      i--;
      if ( !STRCMP( SymTab[i].id, id ) ) {
         if ( SymTab[i].kind == Reserved ) {
            *code = SymTab[i].code;
         } else {
            *code = i - Nreserved;
         }
         return( SymTab[i].kind );
      };
   };
   return( Undef );
}

/*
 * InitSymbolTable generates the table with the known symbols.
 */

static	void InitSymbolTable( void )
{
   EnterReserved( "INTEGER",IntSym );
   EnterReserved( "REAL", RealSym );
   EnterReserved( "DOUBLE", DoubleSym );
   EnterReserved( "LOGICAL", LogicalSym );
   EnterReserved( "STRING", StringSym );
   EnterReserved( "READ", ReadSym );
   EnterReserved( "WRITE", WriteSym );
   EnterReserved( "HALT", HaltSym );
   EnterReserved( "CANCEL", CancelSym );
   EnterReserved( "IF", IfSym );
   EnterReserved( "FOR", ForSym );
   EnterReserved( "WHILE", WhileSym );
   EnterReserved( "REPEAT", RepeatSym );
   EnterReserved( "THEN", ThenSym );
   EnterReserved( "ELSIF", ElsifSym );
   EnterReserved( "ELSE", ElseSym );
   EnterReserved( "CIF", CifSym );
   EnterReserved( "CFOR", CforSym );
   EnterReserved( "CWHILE", CwhileSym );
   EnterReserved( "UNTIL", UntilSym );
   EnterReserved( "EQ", EqSym );
   EnterReserved( "NE", NeSym );
   EnterReserved( "LE", LeSym );
   EnterReserved( "LT", LtSym );
   EnterReserved( "GE", GeSym );
   EnterReserved( "GT", GtSym );
   EnterReserved( "AND", AndSym );
   EnterReserved( "OR", OrSym );
   EnterReserved( "NOT", NotSym );
   EnterReserved( "MOD", ModSym );
   EnterReserved( "TRUE", TrueSym );
   EnterReserved( "FALSE", FalseSym );
   EnterReserved( "DEBUG", DebugSym );
   EnterReserved( "STORYBOARD", StoryboardSym );
   EnterReserved( "FUNNAME", FunnameSym );
   EnterReserved( "CANCELKEYS", CancelKeysSym );
   {
      int	k;

      for ( k = 0; k < sizeof(Functions)/sizeof(func_struct); k++ ) {
         EnterReserved( Functions[k].name, FunctionSym );
      }
   }
}

/*
 * NextCh gets the next character.
 */

static	void Nextch( void )
{
   ch = Inline[Inpos++];
   if ( ch == '\0' ) {
      if ( ( ch = freadln( Inline, StringMax, source ) ) == 0 ) {
         EofSource = true;
      } else {
         if ( PrCode ) {
            PrCode = false;
            if ( makelist ) {
               fprintf( list,"%5.5d\n%5.5d  %5.5d  %s", CP, CP, LineNr, Inline );
            }
         } else if ( makelist ) {
            fprintf( list,"%5.5d  %5.5d  %s", CP, LineNr, Inline );
         }
         Inpos = 0;
         ch = Inline[Inpos++];
      }
   }
}

/*
 * ReadIdent gets the name of the identifier.
 */

static	void ReadIdent( void )
{
   int	i = 0;

   while ( ( isalnum( ch ) ) && ( i < MaxIdLen ) ) {
      CurIdent[i++] = ch;
      Nextch( );
   };
   CurIdent[i++] = 0;
   while ( isalnum( ch ) ) Nextch( );
   CurVarType = LookUp( CurIdent, &CurVarNum );
}

/*
 * ScanString scans a string for output variables.
 */

static	void ScanString( void )
{
   int	i = 0;

   sym = StringConst;
   Nextch( );
   while ( ch != '"' ) {
      if ( i >= StringMax ) Error( "[ScanString] String too long" );
      if ( ch == '\\' ) Nextch( );				/* escaped character */
      if ( ( ( ch >= 1 ) && ( ch < ' ' ) ) || ( ch > '~' ) ) {
         ch = ' ';
      } else {
         CurString[i++] = ch;
         Nextch( );
      };
   };
   CurString[i++] = 0;
   CurStrPtr = PutString( CurString );
   CurStrLen = strlen( CurString ) + 1;
   Nextch( );
}

/*
 * NextSym gets the next symbol.
 */

static	void NextSym( void )
{
   int kind, code;

   while ( !EofSource && ( ( ch == ' ' ) || ( ch == '\t' ) || ( ch == '\n' ) || ( ch == '!') ) ) {
      if ( ch == '!' ) do { Nextch( ); } while ( ch != '\n' );
      Nextch( );
   };
   if ( EofSource ) {
      sym = EndOfFile;
   } else if ( isalpha( ch ) ) {
      ReadIdent( );
      if ( ( kind = LookUp( CurIdent, &code ) ) == Reserved ) {
         sym = code;
         if ( sym == FunctionSym ) {
            CurFunction = 0;
            while ( STRCMP( Functions[CurFunction].name, CurIdent ) ) CurFunction++;
         }
      } else {
         sym = Ident;
         CurVarType = kind;
         CurVarNum = code;
      }
   } else if ( isdigit( ch ) || ( ch == '.' ) ) {
      if ( isdigit( ch ) ) {
         sym = IntConst;
         CurInt = 0;
         while ( isdigit( ch ) ) {
            CurInt = 10 * CurInt + ch - '0';
            Nextch( );
         };
      };
      if ( ch == '.' ) {
         double r = 10.0;

         sym = RealConst;
         CurReal = CurInt;
         Nextch( );
         while ( isdigit( ch ) ) {
            CurReal = CurReal + ( ch - '0' ) / r;
            r = r * 10.0;
            Nextch( );
         };
      };
      if ( ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D' ) {
         int	e = 0, sign = 1;

         if ( sym == IntConst ) {
            sym = RealConst;
            CurReal = CurInt;
         }
         Nextch( );
	 while ( ch == '-' || ch == '+' ) {
	    if ( ch == '-' ) sign = -sign;
	    Nextch( );
	 }
	 while ( isdigit( ch ) ) {
	    e = 10*e + (ch - '0');
	    Nextch( );
	 }
	 if ( sign == -1 ) e = -e;
         CurReal *= pow( 10.0, (double) e );
      }
   } else switch( ch ) {
      case '(': {
         sym = Lpar;
         Nextch( );
         break;
      }
      case ')': {
         sym = Rpar;
         Nextch( );
         break;
      }
      case ',': {
         sym = Comma;
         Nextch( );
         break;
      }
      case '=': {
         sym = EqSym;
         Nextch( );
         break;
      }
      case '<': {
         Nextch( );
         if (ch == '=') {
            sym = LeSym ; Nextch( );
         } else if (ch == '>') {
            sym = NeSym ; Nextch( );
         } else {
            sym = LtSym;
         }
         break;
      }
      case '>': {
         Nextch( );
         if (ch == '=') {
            sym = GeSym ; Nextch( );
         } else {
            sym = GtSym;
         }
         break;
      }
      case '+': {
         sym = PlusSym;
         Nextch( );
         break;
      }
      case '-': {
         sym = MinusSym;
         Nextch( );
         break;
      }
      case '*': {
         sym = TimesSym;
         Nextch( );
         if ( ch == '*' ) { sym = PowerSym; Nextch( ); }
         break;
      }
      case '/': {
         sym = DivSym;
         Nextch( );
         break;
      }
      case '"': {
         ScanString( );
         break;
      }
      case ' ':
      case '\t':
      case '\0':
      case '\n': {
         break;
      }
      default: {
         char	errmes[StringMax+1];

         sprintf( errmes, "[NextSym] %d Illegal character", ch );
         Error( errmes );
         break;
      }
   }
}

/*
 * Declaration generates code for the declaration of variables.
 */

static	void Declaration( void )
{
   int	addr;
   int	s = sym;
   int	size;
   int	len = 80, code;
   bool	LenSeen = false;

   do {
      NextSym( );
      if ( s == StringSym ) {
         if ( !LenSeen ) {
            if ( sym == TimesSym ) {
               NextSym( );
               if ( sym != IntConst ) Error( "[Declaration] Integer expected" );
               len = CurInt;
               NextSym( );
            } else {
               len = 80;
            }
            LenSeen = true;
         }
      }
      if ( sym != Ident ) Error( "[Declaration] Identifier expected" );
      if ( CurVarType != Undef ) Error( "[Declaration] Variable declared twice" );
      code = CurVarNum = EnterVar( CurIdent , s );
      NextSym( );
      if ( sym == Lpar) {
         NextSym( );
         if ( sym != IntConst ) Error( "[Declaration] Integer expected" );
         size = CurInt;
         if ( size < 1 ) Error( "[Declaration] Illegal size declaration" );
         NextSym( );
         if ( sym != Rpar ) Error ( "[Declaration] ) expected" );
         NextSym( );
      } else {
         size = 1;
      }
      switch( s ) {
         case IntSym: {
            addr = PutNInt( size );
            SymTab[Nreserved+code].addr = addr;
            SymTab[Nreserved+code].size = size;
            if ( Debug ) Output( "Enter '%10s' VarNum: %4d Addr: %4d Size: %4d",
               GetId( code ), code, addr, size );
            break;
         }
         case LogicalSym: {
            addr = PutNBool( size );
            SymTab[Nreserved+code].size = size;
            SymTab[Nreserved+code].addr = addr;
            if ( Debug ) Output( "Enter '%10s' VarNum: %4d Addr: %4d Size: %4d",
                GetId( code ), code, addr, size );
            break;
         }
         case RealSym: {
            addr = PutNReal( size );
            SymTab[Nreserved+code].size = size;
            SymTab[Nreserved+code].addr = addr;
            if ( Debug ) Output( "Enter '%10s' VarNum: %4d Addr: %4d Size: %4d",
               GetId( code ), code, addr, size );
            break;
         }
         case DoubleSym: {
            addr = PutNDouble( size );
            SymTab[Nreserved+code].size = size;
            SymTab[Nreserved+code].addr = addr;
            if ( Debug ) Output( "Enter '%10s' VarNum: %4d Addr: %4d Size: %4d",
               GetId( code ), code, addr, size );
            break;
         }
         case StringSym: {
            if ( len > StringMax ) {
               Quit( "Declaration: String Length too large (max = %d)!", StringMax );
            }
            addr = PutNString( size * len );
            SymTab[Nreserved+code].size = size;
            SymTab[Nreserved+code].code = len;
            SymTab[Nreserved+code].addr = addr;
            if ( Debug ) Output( "Enter '%10s' VarNum: %4d Addr: %4d Size: %4d",
                GetId( code ), code, addr, size );
            break;
         }
         default: {
            Quit( "Declaration: Internal Error!" );
            break;
         }
      }
   } while ( sym == Comma );
}

/*
 * Statements generates code for the statements.
 */

static	void Statements( void )
{
   while ( ( sym >= Ident ) && ( sym <= RepeatSym ) ) {
      switch( sym ) {
         case Ident: Assignment( ); break;
         case StringConst: Task( ); break;
         case ReadSym: ReadStat( ); break;
         case WriteSym: WriteStat( ); break;
         case HaltSym: HaltStat( ); break;
         case IfSym: IfStat( ); break;
         case ForSym: ForStat( ); break;
         case WhileSym: WhileStat( ); break;
         case RepeatSym: RepeatStat( ); break;
         case CancelSym: CancelStat( ); break;
         default: break;
      }
   }
}

/*
 * Index generates code for an index argument.
 */

static	void	Index( void )
{
   int	vn = CurVarNum;

   if ( GetSize( vn ) == 1 ) return;
   NextSym( );
   if ( sym != Lpar ) Error( "[Index] Index '(' expected" );
   NextSym( );
   ArithExpression( 'A' );
   if ( sym != Rpar ) Error( "[Index] Close Index ')' expected" );
   CurVarNum = vn;
   GenCode0( _IDX );
}

/*
 * Assignment generates code for an assignment.
 */

static	void Assignment( void )
{
   int	vn = CurVarNum;
   int	vt = CurVarType;

   if ( vt == Undef ) Error( "[Assignment] Variable not declared" );
   Index( );
   NextSym( );
   if ( sym == EqSym ) NextSym( ); else Error( "[Assignment] '=' expected" );
   switch( vt ) {
      case IntVar:
      case RealVar:
      case DoubleVar: {
         ArithExpression( 'A' );
         break;
      }
      case LogicalVar: {
         LogicalExpression( );
         break;
      }
      case StringVar: {
         StringExpression( );
         break;
      }
      default: {
         Error( "[Assignment] Internal Error" );
         break;
      }
   }
   GenCode1( _STV , vn );
}

/*
 * HaltStat generates code to stop execution.
 */

static	void HaltStat( void )
{
   NextSym( );
   GenCode0( _HLT );
}

/*
 * WriteStat generates code for the output.
 */

static	void WriteStat( void )
{
   NextSym( );
   if ( sym != StringConst ) Error( "[WriteStat] Stringconstant expected" );
   GenCode1( _OUT , CurStrPtr );
   NextSym( );
}

/*
 * CancelStat generates code to cancel the user input for a variable.
 */

static	void	CancelStat( void )
{
   NextSym( );
   if ( sym != Ident ) Error( "[CancelStat] Identifier Expected" );
   if ( CurVarType == Undef ) Error( "[CancelStat] Variable not declared" );
   GenCode1( _CAN, CurVarNum );
   NextSym( );
}

/*
 * ReadStat generates code to obtain a value from the user.
 */

static	void ReadStat( void )
{
   int	s;

   NextSym( );
   if ( sym == StringConst ) {
      s = CurStrPtr;
      NextSym( );
   } else {
      s = 0;
   }
   if ( sym != Ident ) Error( "[ReadStat] Variable expected" );
   if ( CurVarType == Undef ) Error( "[ReadStat] Variable not declared" );
   GenCode2( _IN, s, CurVarNum );
   NextSym( );
}

/*
 * Task generates code for the execution of a command.
 */

static	void Task( void )
{
   GenCode1( _EXE , CurStrPtr );
   if ( Debug ) Output( "Task:       (%s)", &Data.sp[CurStrPtr] );
   NextSym( );
}

/*
 * WhileStat generates code for a while-loop.
 */
static	void WhileStat( void )
{
   int	l1 = CP;
   int	l2;

   NextSym( );
   LogicalExpression( );
   l2 = CP;
   GenCode1( _BRF, 0 );				/*  filler  */
   Statements( );
   GenCode1( _JMP , l1 );
   GenCode1At( l2 , _BRF , CP );
   if ( sym == CwhileSym ) NextSym( ); else Error( "[WhileStat] CWHILE expected" );
}

/*
 * RepeatStat generates code for repeat-loop.
 */

static	void RepeatStat( void )
{
   int	l = CP;

   NextSym( );
   Statements( );
   if ( sym == UntilSym ) NextSym( ); else Error( "[RepeatStat] UNTIL expected" );
   LogicalExpression( );
   GenCode1( _BRF , l );
}

/*
 * IfStat generates code for an if-statement.
 */

static	void IfStat( void )
{
   int	lab[11];
   int	l;
   int	ilab = 0;

   NextSym( );
   LogicalExpression( );
   if ( sym == ThenSym ) NextSym( ); else Error( "[IfStat] THEN expected" );
   l = CP;
   GenCode1( _BRF , 0 );
   Statements( );
   if ( sym == CifSym ) GenCode1At( l , _BRF , CP );
   while ( sym == ElsifSym ) {
      NextSym( );
      if ( ilab == 10 ) Error( "[IfStat] Too many ELSIF's" );
      lab[ilab++] = CP;
      GenCode1( _JMP , 0 );
      GenCode1At( l , _BRF , CP );
      LogicalExpression( );
      if ( sym == ThenSym ) NextSym( ); else Error( "[IfStat] THEN expected" );
      l = CP;
      GenCode1( _BRF , 0 );
      Statements( );
   };
   if ( sym == ElseSym ) {
      NextSym( );
      lab[ilab++] = CP;
      GenCode1( _JMP , 0 );
      GenCode1At( l , _BRF , CP );
      Statements( );
   } else {
      GenCode1At( l , _BRF , CP );
   }
   for ( ; ilab > 0 ; ) GenCode1At( lab[--ilab] , _JMP , CP );
   if ( sym == CifSym ) NextSym( ); else Error( "[IfStat] CIF expected" );
}

/*
 * ForStat generates code for a do-loop.
 */

static	void ForStat( void )
{
   int	l, l1;
   int	vn;

   NextSym( );
   if ( (sym != Ident ) || ( CurVarType != IntVar ) ) {
      Error( "[ForStat] Integer variable expected" );
   }
   vn = CurVarNum;
   NextSym( );
   if ( sym == EqSym ) NextSym( ); else Error( "[ForStat] '=' expected" );
   ArithExpression( 'I' );			/*!! check for int here! */
   GenCode1( _STV , vn );
   if ( sym == Comma ) NextSym( ); else Error( "[ForStat] ',' expected" );
   ArithExpression( 'I' );			/* dito */
   if ( sym == Comma ) {
      NextSym( );
      ArithExpression( 'I' );
   } else {
      GenCode1( _LDI , PushIntConst( 1 ) );
   }
   l = CP;
   GenCode0( _DP2 );
   GenCode1( _BRL, vn );
   l1 = CP;
   GenCode1( _BRF , 0 );
   Statements( );
   GenCode0( _DUP );
   GenCode1( _LDV , vn );
   GenCode0( _ADD );
   GenCode1( _STV , vn );
   GenCode1( _JMP , l );
   GenCode1At( l1 , _BRF , CP );
   GenCode0( _POP );
   GenCode0( _POP );
   if (sym == CforSym) NextSym( ); else Error( "[ForStat] CFOR expected" );
}

/*
 * Function generates code for a function call.
 */

static	void Function( void )
{
   int	f = CurFunction;
   int	n;
   int	m;

   m = n = strlen( Functions[f].args );
   NextSym( );
   if ( n > 0 ) {
      if ( sym == Lpar ) {
         NextSym( );
      } else {
         char	String[80];

         sprintf( String, "[Function] Wrong number of arguments for %s", Functions[f].name );
         Error( String );
      }
      while( n > 0 ) {
         char	at = Functions[f].args[(m-n)];
         int	kind = 0;

         switch( at ) {
            case 'L': {				/* need logical expression */
               LogicalExpression( );
               break;
            }
            case 'S': {				/* need string expression */
               StringExpression( );
               break;
            }
            case 'A': {				/* need arith expression */
               ArithExpression( 'A' );
               break;
            }
            case 'R': {
               ArithExpression( 'R' );
               break;
            }
            case 'I': {
               ArithExpression( 'I' );
               break;
            }
            case 's': kind++;			/* need address of string */
            case 'l': kind++;			/* need address of logical */
            case 'd': kind++;			/* need address of double */
            case 'r': kind++;			/* need address of real */
            case 'i': {				/* need address of integer */
               int	vn = CurVarNum;

               if ( sym != Ident ) {
                  Error( "[Function] Identifier Expected" );
               }
               if ( CurVarType == Undef ) {
                  char	errmes[80];

                  sprintf( errmes, "[Function] %.*s not defined", MaxIdLen, CurIdent );
                  Error( errmes );
               }
               if ( ++kind != CurVarType ) {
                  Error( "[Function] Wrong type of identifier" );
               }
               NextSym( );
               if ( GetSize( vn ) != 1 && sym == Lpar ) {
                  NextSym( );
                  ArithExpression( 'I' );
                  if ( sym != Rpar ) {
                     Error( "[Function] Close Index ')' expected" );
                  }
                  NextSym( );
                  GenCode0( _IDX );
                  GenCode1( _ADI, vn );
               } else {
                  GenCode1( _ADR, vn );
               }
               break;
            }
            case 'X':
            case 'x': {				/* need any address */
               int	vn = CurVarNum;

               if ( sym != Ident ) {
                  Error( "[Function] Identifier Expected" );
               }
               if ( CurVarType == Undef ) {
                  char	errmes[80];

                  sprintf( errmes, "[Function] %.*s not defined", MaxIdLen, CurIdent );
                  Error( errmes );
               }
               NextSym( );
               if ( at == 'x' && GetSize( vn ) != 1 && sym == Lpar ) {
                  NextSym( );
                  ArithExpression( 'I' );
                  if ( sym != Rpar ) {
                     Error( "[Function] Close Index ')' expected" );
                  }
                  NextSym( );
                  GenCode0( _IDX );
                  GenCode1( _ADI, vn );
               } else {
                  GenCode1( _ADR, vn );
               }
               break;
            }
            default: {
               Error( "[Function] Internal Error" );
               break;
            }
         }
         if ( --n > 0 ) {
            if ( sym == Comma ) {
               NextSym( );
            } else {
               Error( "[Function] Wrong number of arguments" );
            }
         }
      }
      if ( sym == Rpar ) {
         NextSym( );
      } else {
         Error( "[Function] Wrong number of arguments" );
      }
   }
   GenCode1( _FIE, f );
}

/*
 * ArithExpression.
 */

static	void ArithExpression( char at )
{
   int s;

   ArithTerm( at );
   while ( ( sym == PlusSym ) || ( sym == MinusSym ) ) {
      s = sym;
      NextSym( );
      ArithTerm( at );
      if ( s == PlusSym ) GenCode0( _ADD ); else GenCode0( _SUB );
   }
}

/*
 * ArithTerm.
 */

static	void ArithTerm( char at )
{
   int s;

   ArithFactor( at );
   while ( ( sym == TimesSym ) || ( sym == DivSym ) || ( sym == ModSym ) ) {
      s = sym;
      NextSym( );
      ArithFactor( at );
      if ( s == TimesSym ) {
         GenCode0( _MUL );
      } else if ( s == DivSym ) {
         GenCode0( _DIV );
      } else {
         GenCode0( _MOD );
      }
   }
}

/*
 * ArithFactor generates code for a arithmetic factor.
 */

static	void ArithFactor( char at )
{
   switch( sym ) {
      case PlusSym: {
         NextSym( );
         ArithFactor( at );
         break;
      }
      case MinusSym: {
         NextSym( );
         ArithFactor( at );
         GenCode0( _NEG );
         break;
      }
      case Lpar: {
         NextSym( );
         ArithExpression( at );
         if ( sym == Rpar ) NextSym( ); else Error( "[ArithFactor] ')' expected" );
         break;
      }
      case Ident: {
         if ( CurVarType == Undef ) Error( "[ArithFactor] Variable undeclared" );
         if ( ( CurVarType < IntVar ) || ( CurVarType > DoubleVar ) ) {
            Error( "[ArithFactor] Type Conflict" );
         } else if ( ( CurVarType == IntVar ) && ( at == 'R' ) ) {
            Error( "[ArithFactor] Type Conflict" );
         } else if ( ( CurVarType == RealVar ) && ( at == 'I' ) ) {
            Error( "[ArithFactor] Type Conflict" );
         } else if ( ( CurVarType == DoubleVar ) && ( at == 'I' ) ) {
            Error( "[ArithFactor] Type Conflict" );
         }
         Index( );
         GenCode1( _LDV , CurVarNum );
         NextSym( );
         break;
      }
      case IntConst: {
         GenCode1( _LDI , PushIntConst( CurInt ) );
         NextSym( );
         break;
      }
      case RealConst: {
         GenCode1( _LDR , PushRealConst( CurReal ) );
         NextSym( );
         break;
      }
      case FunctionSym: {
         int	fk = Functions[CurFunction].kind;

         if ( at == 'A' ) {
            if ( ( fk != 'I' ) && ( fk != 'R' ) && ( fk != 'A' ) ) {
               Error( "[ArithFactor] Function Type Conflict" );
            }
         } else if ( at == 'I' ) {
            if ( ( fk != 'A' ) && ( fk != 'I' ) ) {
               Error( "[ArithFactor] Function Type Conflict" );
            }
         } else if ( at == 'R' ) {
            if ( ( fk != 'A' ) && ( fk != 'R' ) ) {
               Error( "[ArithFactor] Function Type Conflict" );
            }
         } else {
            Error( "[ArithFactor] Function Type Conflict" );
         }
         Function( );
         break;
      }
      default: {
         Error( "[ArithFactor] Wrong Syntax" );
         break;
      }
   }
   if ( sym == PowerSym ) {
      NextSym( );
      ArithFactor( at );
      GenCode0( _POW );
   }
}

/*
 * LogicalExpression generates code for a logical expression.
 */

static	void LogicalExpression( void )
{
   LogicalTerm( );
   while ( sym == OrSym ) {
      NextSym( );
      LogicalTerm( );
      GenCode0( _OR );
   }
}

/*
 * LogicalTerm generates code for a logical term.
 */

static	void LogicalTerm( void )
{
   LogicalFactor( );
   while ( sym == AndSym ) {
      NextSym( );
      LogicalFactor( );
      GenCode0( _AND );
   };
}

/*
 * LogicalFactor generates code for a logical factor.
 */

static	void LogicalFactor( void )
{
   int s = 0;

   switch( sym ) {
      case NotSym: {
         NextSym( );
         LogicalFactor( );
         GenCode0( _NOT );
         break;
      }
      case Lpar: {
         NextSym( );
         LogicalExpression( );
         if ( sym == Rpar ) NextSym( ); else Error( "[LogicalFactor] ')' expected" );
         break;
      }
      case TrueSym: {
         GenCode1( _LDB , PushBoolConst( true ) );
         NextSym( );
         break;
      }
      case FalseSym: {
         GenCode1( _LDB , PushBoolConst( false ) );
         NextSym( );
         break;
      }
      default: {
         if ( ( sym == FunctionSym ) && ( Functions[CurFunction].kind == 'L' ) ) {
            Function( );
         } else if ( ( sym == Ident ) && ( CurVarType == LogicalVar ) ) {
            Index( );
            GenCode1( _LDV , CurVarNum );
            NextSym( );
         } else if ( ( sym == Ident ) && ( CurVarType == StringVar ) ) {
            StringCompare( );
         } else if ( ( sym == StringConst ) ) {
            StringCompare( );
         } else if ( ( sym == FunctionSym ) && ( Functions[CurFunction].kind == 'S' ) ) {
            StringCompare( );
         } else {
            ArithExpression( 'A' );
            if ( ( sym >= EqSym ) && ( sym <= GtSym ) ) {
               s = sym;
               NextSym( );
            } else {
               Error( "[LogicalFactor] 'relop' expected" );
            }
            ArithExpression( 'A' );
            switch ( s ) {
               case EqSym: GenCode0( _EQ ); break;
               case NeSym: GenCode0( _NE ); break;
               case LeSym: GenCode0( _LE ); break;
               case LtSym: GenCode0( _LT ); break;
               case GeSym: GenCode0( _GE ); break;
               case GtSym: GenCode0( _GT ); break;
               default: break;
            }
         }
      }
   }
}

/*
 * StringExpresion generates code for a string.
 */

static	void StringExpression( void )
{
   switch( sym ) {
      case StringConst: {
         GenCode1( _LDS , CurStrPtr );
         NextSym( );
         break;
      }
      case Ident: {
         if ( CurVarType == Undef ) Error( "[StringExpression] Variable undeclared" );
         if ( CurVarType != StringVar ) Error( "[StringExpression] String Identifier expected" );
         Index( );
         GenCode1( _LDV , CurVarNum );
         NextSym( );
         break;
      }
      case FunctionSym: {
         if ( Functions[CurFunction].kind != 'S' ) {
            Error( "[StringExpression] String Function Expected" );
         }
         Function( );
         break;
      }
      default: {
         Error( "[StringExpression] Stringexpression expected" );
         break;
      }
   }
}

/*
 * StringCompare generates code for comparing strings.
 */

static	void StringCompare( void )
{
   int s;

   StringExpression( );
   s = sym;
   if ( ( s == EqSym ) || ( s == NeSym ) ) {
      NextSym( );
   } else {
      Error( "[StringCompare] '=' or '<>' expected" );
   }
   StringExpression( );
   if ( s == EqSym ) GenCode0( _EQ ); else GenCode0( _NE );
}

/*
 * Program does the compilation of the COLA source.
 */

static	void	Program( void )
{
   while ( ( sym >= IntSym ) && ( sym <= RepeatSym ) ) {
      if ( sym <= StringSym ) {
         Declaration( );
      } else {
         Statements( );
      }
   }
}

/*
 * Compile Does the setup for the compilation of the COLA source.
 */

static	void Compile( void )
{
   status_c( tofchar( "Compiling ...." ) );
   InitSymbolTable( );
   Inline[Inpos] = 0;
   while ( 1 ) {
      NextSym( );
      if ( sym == DebugSym ) {
         Debug = true;
         Output( "Debugmode..." );
      } else if ( sym == StoryboardSym ) {
         makestoryboard = true;
         Output( "Logging RENDER info..." );
      } else if ( sym == FunnameSym ) {
         Funname = true;
         Output( "Generating Funny Names..." );
      } else if ( sym == CancelKeysSym ) {
         CancelKeys = true;
         Output( "Cancelling all keywords..." );
      } else {
         break;
      }
   }
   Program( );
   if ( sym != EndOfFile ) Error( "[Compile] statement expected" );
   GenCode0( _HLT );
   if ( makelist ) {
      int	n;

      fprintf( list, "\n" );
      fprintf( list, "Codesize:   %6d   (Allocated = %6d)   (%d Lines)\n",
         CP, CodeMax, LineNr );
      fprintf( list, "Memorysize: %6d   (Allocated = %6d)\n",
         DataPtr, DataMax );
      fprintf( list, "Symbols:    %6d   (Allocated = %6d)\n",
         Nsym - Nreserved, SymbolTableMax - Nreserved );
      fprintf( list, "IConstants: %6d   (Allocated = %6d)\n",
         NIntConsts, IntConstsMax );
      fprintf( list, "LConstants: %6d   (Allocated = %6d)\n",
         NBoolConsts, sizeof(BoolConsts)/sizeof(bool) );
      fprintf( list, "RConstants: %6d   (Allocated = %6d)\n",
         NRealConsts, RealConstsMax );
      fprintf( list, "\nVariable    Type        Size\n" );
      for ( n = Nreserved; n < Nsym; n++ ) {
         if ( SymTab[n].kind == StringVar ) {
            fprintf( list, "%-10.10s  %s*%3.3d  %5.5d\n", SymTab[n].id, VarTypes[SymTab[n].kind], SymTab[n].code, SymTab[n].size );
         } else {
            fprintf( list, "%-10.10s  %-10s  %5.5d\n", SymTab[n].id, VarTypes[SymTab[n].kind], SymTab[n].size );
         }
      }
   }
}

/*
 * Execute executes the compiled code.
 */

static	void Execute( void )
{
   bool		quits = false;		/* quit ? */
   int		PC = 0;			/* the program counter */
   int		opc;			/* the opcode */
   int		oper1 = 0, oper2 = 0;   /* the operands */

   status_c( tofchar( "Executing ..." ) );
   while ( !quits ) {
      int	curPC = PC;

      opc = Memory[PC++];
      if ( opc >= _OUT ) {		/* at least one operand */
         int		i;
         union {
            byte	b[sizeof(int)];
            int	i;
         } u;

         for ( i = 0; i < sizeof( int ); i++ ) u.b[i] = Memory[PC++];
         oper1 = u.i;
      };
      if ( opc >= _IN ) {		/* two operands */
         int		i;
         union {
            byte	b[sizeof(int)];
            int	i;
         } u;

         for ( i = 0; i < sizeof( int ); i++ ) u.b[i] = Memory[PC++];
         oper2 = u.i;
      }
      if ( Debug || Dump ) {
         if ( opc >= _IN ) {
            Output( "PC=%4d  Opc=%4d  SP=%4d [%s %4d %4d]",
               curPC, opc, SP, Mnemonics[opc], oper1, oper2 );
         } else if ( opc >= _OUT ) {
            Output( "PC=%4d  Opc=%4d  SP=%4d [%s %4d]",
               curPC, opc, SP, Mnemonics[opc], oper1 );
         } else {
            Output( "PC=%4d  Opc=%4d  SP=%4d [%s]",
               curPC, opc, SP, Mnemonics[opc] );
         }
      };
      switch( opc ) {
         case _HLT: { quits = true; break; }
         case _ADD: { fie_add( ); break; }
         case _SUB: { fie_sub( ); break; }
         case _MUL: { fie_mul( ); break; }
         case _DIV: { fie_div( ); break; }
         case _POW: { fie_pow( ); break; }
         case _NEG: { fie_neg( ); break; }
         case _MOD: { fie_mod( ); break; }
         case _AND: { fie_and( ); break; }
         case _OR: { fie_or( ); break; }
         case _NOT: { fie_not( ); break; }
         case _EQ: { fie_eq( ); break; }
         case _NE: { fie_ne( ); break; }
         case _LT: { fie_lt( ); break; }
         case _LE: { fie_le( ); break; }
         case _GE: { fie_ge( ); break; }
         case _GT: { fie_gt( ); break; }
         case _DUP: { Dup( ); break; }
         case _DP2: { Dp2( ); break; }
         case _POP: { Pop( ); break; }
         case _CAN: { Can( oper1 ); break; }
         case _IDX: { PushIndex( ); break; }
         case _ADR: { PushAddress( oper1, 0 ); break; }
         case _ADI: { PushAddress( oper1, PopIndex( oper1 ) ); break; }
         case _OUT: { Out( oper1 ); break; }
         case _BRL: { Brl( oper1 ); break; }
         case _LDB: { PushBool( BoolConsts[oper1] ); break; }
         case _LDI: { PushInt( IntConsts[oper1] ); break; }
         case _LDR: { PushReal( RealConsts[oper1] ); break; }
         case _LDS: { Lds( oper1 ); break; }
         case _LDV: { Ldv( oper1 ); break; }
         case _STV: { Stv( oper1 ); break; }
         case _JMP: { PC = oper1; break; }
         case _BRF: { if (!Stack[--SP].val.b) PC = oper1; break; }
         case _EXE: { Exe( oper1 ); break; }
         case _IN: { In( oper1, oper2 ); break; }
         case _FIE: { Functions[oper1].func( ); break; }
         default: { Quit( "Execute: Internal Error!" ); break; };
      }
      GetLogical( &quits, 1, 2, "STOP=", "quit CoLa [NO] ?" );
      cancel_c( tofchar( "STOP=" ) );
   }
}

/*
 * M A I N   P R O G R A M
 */

MAIN_PROGRAM_ENTRY
{
   char		filename[StringMax+1];		/* name of cola file */
   char		sourcename[StringMax+1];	/* name of source */
   char		listname[StringMax+1];		/* name of listing */
   char		storyname[StringMax+1];		/* name of story */
   fchar	program;			/* name of program */

   program.a = ProgNam; program.l = sizeof( ProgNam ) - 1;
   init_c( );					/* START */
   setdblank_c( &DBLANK );			/* get double BLANK */
   setfblank_c( &FBLANK );			/* get floating BLANK */
   myname_c( program );				/* get name of program */
   ProgNam[nelc_c( program )] = 0;		/* add zero byte */
   if ( !strcmp( ProgNam, "COLA" ) ) {
      IDENTIFICATION( ProgNam, VERSION );	/* say hello */
   }
   GetStr( filename, StringMax - 4, 1, 1, "NAME=", "Name of COLA file ?" );
   GetLogical( &Dump, 1, 2, "DUMP=", "Dump Code [NO] ?" );
   GetLogical( &makelist, 1, 2, "LIST=", "Make listing [NO] ?" );
   strcpy( sourcename, filename );
   {
      int	m, n = strlen( filename );

      m = n;
      while ( n-- && filename[n] != '/' ); n++;
      if ( n ) { memmove( filename, &filename[n], m - n + 1 ); }
   }
   strcpy( listname, filename );
   strcpy( storyname, filename );
   strcat( sourcename, ".col" );
   strcat( listname, ".lis" );
   strcat( storyname, ".str" );

   if ( ( source = fopen( sourcename, "r" ) ) == NULL ) {
      Quit( "Main: Can't open inputfile [%s]!", sourcename );
   }
   if ( makelist ) {
      if ( ( list = fopen( listname, "w" ) ) == NULL ) {
         Quit( "Main: Can't open listing file [%s]!", listname );
      }
   }
   Compile( );
   fclose( source );
   if ( makelist ) fclose( list );
   if ( makestoryboard ) {
      if ( ( story = fopen( storyname, "w" )) == NULL) {
         Quit( "Main: Can't open storyboard file [%s]!", storyname );
      }
   }
   Execute( );
   if ( makestoryboard ) {
      fclose( story );
   };
   finis_c( );					/* END */
   return( 0 );
}
